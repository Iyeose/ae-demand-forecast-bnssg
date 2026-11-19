# 03_explore.R ------------------------------------------------------------------
# Question 1: how fast is Type 1 demand growing, and how much higher (or lower)
# is it in winter than in summer?
#
# Window: July 2021 onwards (after Covid). Covid months would distort both the
# trend and the seasonal pattern, and post-Covid behaviour is what matters for
# planning 2027-28.
#
# Two independent methods, so each answer is cross-checked:
#  A. STL decomposition: splits each series into trend + seasonal + remainder.
#  B. Regression: log(attendances per day) ~ time trend + calendar month.
#     The time coefficient gives annual growth (with a 95% confidence interval);
#     the month coefficients give the seasonal pattern.

library(tidyverse)
library(tsibble)
library(feasts)
library(fabletools)
library(fs)
source("R/00_config.R")

monthly <- read_rds("data/processed/ae_type1_monthly.rds")
dir_create("outputs/figures")
dir_create("outputs/tables")

post <- monthly |>
  filter(month >= yearmonth("2021 Jul")) |>
  mutate(adm_per_day = emerg_adm_t1 / days)

winter <- c("Dec", "Jan", "Feb")
summer <- c("Jun", "Jul", "Aug")

# A. STL decomposition -------------------------------------------------------------
# season(window = "periodic") assumes the same seasonal shape every year, which is
# sensible with only ~5 years of post-Covid data. robust = TRUE stops one odd
# month from bending the trend.
stl_fit <- post |>
  model(stl = STL(att_per_day ~ trend(window = 13) + season(window = "periodic"),
                  robust = TRUE))
comp <- components(stl_fit)

stl_summary <- comp |>
  as_tibble() |>
  arrange(provider, month) |>
  group_by(provider) |>
  mutate(cal_month = month.abb[lubridate::month(as.Date(month))],
         season_pct = season_year / mean(trend)) |>
  summarise(
    months          = n(),
    trend_start     = first(trend),
    trend_end       = last(trend),
    stl_growth_pa   = (last(trend) / first(trend))^(12 / (n() - 1)) - 1,
    stl_growth_l12m = last(trend) / nth(trend, n() - 12) - 1,
    stl_winter_vs_summer = (1 + mean(season_pct[cal_month %in% winter])) /
      (1 + mean(season_pct[cal_month %in% summer])) - 1,
    .groups = "drop"
  )

# B. Regression ----------------------------------------------------------------------
seasonal_reg <- function(d, var, log_scale = TRUE) {
  d <- d |>
    arrange(month) |>
    mutate(t = as.numeric(month) - min(as.numeric(month)),
           cal_month = factor(month.abb[lubridate::month(as.Date(month))], levels = month.abb),
           yy = .data[[var]])
  if (log_scale) {
    if (any(d$yy <= 0, na.rm = TRUE)) stop(var, " has zero or negative values")
    d$yy <- log(d$yy)
  }
  fit <- lm(yy ~ t + cal_month, data = d)
  b   <- coef(fit)
  ci  <- confint(fit)["t", ]
  eff <- c(0, b[paste0("cal_month", month.abb[-1])])  # January is the baseline
  names(eff) <- month.abb
  if (log_scale) {
    list(growth = exp(12 * b[["t"]]) - 1,
         growth_lo = exp(12 * ci[[1]]) - 1,
         growth_hi = exp(12 * ci[[2]]) - 1,
         month_index = exp(eff - mean(eff)) - 1,     # % above/below an average month
         winter_vs_summer = exp(mean(eff[winter]) - mean(eff[summer])) - 1)
  } else {
    list(growth = 12 * b[["t"]],
         month_index = eff - mean(eff),
         winter_vs_summer = mean(eff[winter]) - mean(eff[summer]))
  }
}

providers <- unique(post$provider)

reg_summary <- map(providers, \(p) {
  d    <- post |> as_tibble() |> filter(provider == p)
  att  <- seasonal_reg(d, "att_per_day")
  adm  <- seasonal_reg(d, "adm_per_day")
  perf <- seasonal_reg(mutate(d, pct = 100 * pct_within_4h), "pct", log_scale = FALSE)
  tibble(provider = p,
         reg_growth_pa = att$growth,
         reg_growth_lo = att$growth_lo,
         reg_growth_hi = att$growth_hi,
         reg_winter_vs_summer_att = att$winter_vs_summer,
         reg_winter_vs_summer_adm = adm$winter_vs_summer,
         # share of attendances admitted, winter vs summer
         reg_winter_vs_summer_admrate = (1 + adm$winter_vs_summer) / (1 + att$winter_vs_summer) - 1,
         winter_minus_summer_4h_pts = perf$winter_vs_summer)
}) |> list_rbind()

month_profile <- map(providers, \(p) {
  d <- post |> as_tibble() |> filter(provider == p)
  bind_rows(
    tibble(provider = p, measure = "Attendances", cal_month = month.abb,
           index = seasonal_reg(d, "att_per_day")$month_index),
    tibble(provider = p, measure = "Emergency admissions", cal_month = month.abb,
           index = seasonal_reg(d, "adm_per_day")$month_index)
  )
}) |> list_rbind() |>
  mutate(cal_month = factor(cal_month, levels = month.abb))

# Context: today versus before Covid ---------------------------------------------------
levels_vs_pre <- monthly |>
  as_tibble() |>
  arrange(provider, month) |>
  group_by(provider) |>
  summarise(
    pre_covid_per_day = mean(att_per_day[month >= yearmonth("2019 Apr") &
                                           month <= yearmonth("2020 Feb")]),
    last_12m_per_day  = mean(tail(att_per_day, 12)),
    change_vs_pre     = last_12m_per_day / pre_covid_per_day - 1,
    .groups = "drop"
  )

# Results ---------------------------------------------------------------------------------
q1 <- stl_summary |>
  left_join(reg_summary, by = "provider") |>
  left_join(levels_vs_pre, by = "provider")

write_csv(q1, "outputs/tables/q1_growth_seasonality.csv")
write_csv(month_profile, "outputs/tables/q1_month_profile.csv")

pct <- \(x) scales::percent(x, accuracy = 0.1)
q1 |>
  transmute(
    provider,
    `Growth/yr (STL)`        = pct(stl_growth_pa),
    `Growth/yr (regression)` = paste0(pct(reg_growth_pa), " [", pct(reg_growth_lo), " to ", pct(reg_growth_hi), "]"),
    `Last 12m trend growth`  = pct(stl_growth_l12m),
    `Winter vs summer (STL)` = pct(stl_winter_vs_summer),
    `Winter vs summer (reg)` = pct(reg_winter_vs_summer_att),
    `Admissions winter vs summer` = pct(reg_winter_vs_summer_adm),
    `Admission rate winter vs summer` = pct(reg_winter_vs_summer_admrate),
    `Type 1 4h perf winter - summer (pts)` = as.character(round(winter_minus_summer_4h_pts, 1)),
    `Per day: pre-Covid -> last 12m` = paste0(round(pre_covid_per_day), " -> ", round(last_12m_per_day),
                                              " (", pct(change_vs_pre), ")")
  ) |>
  pivot_longer(-provider) |>
  pivot_wider(names_from = provider, values_from = value) |>
  print(n = Inf, width = 150)

# Charts ------------------------------------------------------------------------------------
p_stl <- autoplot(comp) +
  labs(title = "STL decomposition: Type 1 attendances per day (Jul 2021 onwards)",
       subtitle = "Trend, seasonal pattern and remainder for each provider") +
  theme_minimal(base_size = 11)
ggsave("outputs/figures/02_stl_components.png", p_stl, width = 10, height = 8, dpi = 150)

p_season <- month_profile |>
  ggplot(aes(cal_month, index, colour = measure, group = measure)) +
  geom_hline(yintercept = 0, colour = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point() +
  facet_wrap(~provider, ncol = 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_colour_manual(values = c("Attendances" = "#2c6fbb", "Emergency admissions" = "#d1495b")) +
  labs(title = "Seasonal pattern after removing the growth trend (Jul 2021 onwards)",
       subtitle = "% above or below an average month",
       x = NULL, y = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
ggsave("outputs/figures/03_seasonal_profile.png", p_season, width = 9, height = 6.5, dpi = 150)

message("Q1 results saved to outputs/tables/ and charts to outputs/figures/.")

