# 04_forecast.R ----------------------------------------------------------------
# Question 2: how many Type 1 attendances should the ICBs plan for, month by
# month, in 2027-28, with a realistic range?
#
# Approach
#  1. Rolling-origin cross-validation. From July 2021 (post-Covid), each model is
#     trained on the first 36 months, forecasts the next 12, then the training
#     window grows by one month and it forecasts again: 17 forecast origins in all.
#     Five models are compared:
#       snaive : same month last year (the baseline any model must beat)
#       ets    : exponential smoothing, automatically chosen form
#       arima  : seasonal ARIMA, automatically chosen orders
#       tslm   : regression on log scale with linear trend + month effects
#       dynreg : the same regression with ARIMA errors (models runs of high or
#                low months)
#     Scored on MAPE (average % miss) across all 17 x 12 forecasts, the error in
#     each 12-month total, and how often actuals fell inside the 80% and 95% ranges.
#     Many origins, not one test year, so the choice does not hinge on one year.
#  2. Choice: for each trust, the lowest-MAPE model whose residuals pass the
#     Ljung-Box test (p > 0.05) when fitted to all post-Covid data.
#  3. Honest ranges. The models' own ranges include month-to-month noise but not
#     uncertainty in the growth rate, so they come out too narrow. They are widened
#     by a factor k, set so that the back-test errors fall inside the 80% and 95%
#     ranges as often as they should. The annual range is also never narrower than
#     the back-tests' own errors in 12-month totals, and the two-trust total's
#     range is the two trusts' ranges added (a bad winter tends to hit both).
#
# Covid: training starts in July 2021 rather than using a Covid dummy, because
# demand came back at a different level and mix.

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(urca)       # used by ARIMA unit-root tests; loading it keeps it in renv.lock
library(fs)
source("R/00_config.R")

set.seed(2027)
dir_create("outputs/figures")
dir_create("outputs/tables")

monthly <- read_rds("data/processed/ae_type1_monthly.rds")
post <- monthly |>
  filter(month >= yearmonth("2021 Jul"), month <= DATA_END) |>
  select(provider, month, att_per_day, type1_att, days)

fit_models <- function(data) {
  data |>
    model(
      snaive = SNAIVE(att_per_day),
      ets    = ETS(att_per_day),
      arima  = ARIMA(att_per_day),
      tslm   = TSLM(log(att_per_day) ~ trend() + season()),
      dynreg = ARIMA(log(att_per_day) ~ trend() + season() + pdq(d = 0) + PDQ(0, 0, 0))
    )
}

# 1. Rolling-origin cross-validation ----------------------------------------------
h_cv <- 12
init <- 36
cv_data <- post |>
  filter(month <= max(month) - h_cv) |>
  stretch_tsibble(.init = init, .step = 1)
n_origins <- n_distinct(cv_data$.id)
message("Cross-validation: ", n_origins, " forecast origins x ", h_cv, " months ahead")

fc_cv <- fit_models(cv_data) |> forecast(h = h_cv)

actual <- as_tibble(post) |> select(provider, month, actual = att_per_day, days)
cv <- fc_cv |>
  hilo(level = c(80, 95)) |>
  unpack_hilo(c(`80%`, `95%`)) |>
  as_tibble() |>
  arrange(.id, provider, .model, month) |>
  group_by(.id, provider, .model) |>
  mutate(h = row_number()) |>
  ungroup() |>
  left_join(actual, by = c("provider", "month")) |>
  mutate(ape  = abs(actual - .mean) / actual,
         sd80 = (`80%_upper` - `80%_lower`) / (2 * qnorm(0.9)),
         z    = (actual - .mean) / sd80,
         in80 = actual >= `80%_lower` & actual <= `80%_upper`,
         in95 = actual >= `95%_lower` & actual <= `95%_upper`)

cv_scores <- cv |>
  group_by(provider, .model) |>
  summarise(MAPE        = 100 * mean(ape),
            MAPE_1to3   = 100 * mean(ape[h <= 3]),
            MAPE_10to12 = 100 * mean(ape[h >= 10]),
            in_80 = mean(in80), in_95 = mean(in95), .groups = "drop")

annual_cv <- cv |>
  group_by(provider, .model, .id) |>
  summarise(fc_total = sum(.mean * days), act_total = sum(actual * days), .groups = "drop") |>
  mutate(pct_err = fc_total / act_total - 1)

annual_scores <- annual_cv |>
  group_by(provider, .model) |>
  summarise(annual_MAPE = 100 * mean(abs(pct_err)),
            annual_bias = 100 * mean(pct_err), .groups = "drop")

# 2. Residual check on all data, and the choice -----------------------------------
fit_full <- fit_models(post)
resid_check <- augment(fit_full) |>
  features(.innov, ljung_box, lag = 12) |>
  select(provider, .model, lb_pvalue)

model_table <- cv_scores |>
  left_join(annual_scores, by = c("provider", ".model")) |>
  left_join(resid_check, by = c("provider", ".model")) |>
  arrange(provider, MAPE)
write_csv(model_table, "outputs/tables/q2_model_accuracy.csv")

cat("\n== Cross-validation accuracy (", n_origins, " origins x 12 months), best first ==\n", sep = "")
model_table |>
  mutate(across(c(MAPE, MAPE_1to3, MAPE_10to12, annual_MAPE, annual_bias), \(x) round(x, 2)),
         lb_pvalue = round(lb_pvalue, 3),
         across(c(in_80, in_95), \(x) scales::percent(x, 1))) |>
  print(n = Inf, width = 200)

best <- model_table |>
  group_by(provider) |>
  arrange(lb_pvalue <= 0.05, MAPE, .by_group = TRUE) |>   # passing models first, then by MAPE
  slice(1) |>
  ungroup() |>
  select(provider, .model)
write_csv(best, "outputs/tables/q2_best_model.csv")
keep_best <- \(x) filter(x, paste(provider, .model) %in% paste(best$provider, best$.model))
cat("\n== Chosen model ==\n"); print(best)
cat("\n== Fitted model forms (all data) ==\n"); print(fit_full)

# 3. Range calibration from the back-tests ------------------------------------------
# k = how much wider the chosen model's ranges must be for back-test errors to land
# inside the 80% and 95% ranges as often as promised (never narrower than k = 1).
range_k <- cv |>
  semi_join(best, by = c("provider", ".model")) |>
  group_by(provider) |>
  summarise(k80 = quantile(abs(z), 0.80, na.rm = TRUE) / qnorm(0.90),
            k95 = quantile(abs(z), 0.95, na.rm = TRUE) / qnorm(0.975),
            in_80_before = mean(in80), .groups = "drop") |>
  mutate(k = pmax(1, k80, k95))
write_csv(range_k, "outputs/tables/q2_range_calibration.csv")
cat("\n== Range calibration (k = widening factor) ==\n"); print(range_k)

# 4. Forecast and simulate to March 2028 ---------------------------------------------
h_out <- as.numeric(yearmonth("2028 Mar")) - as.numeric(max(post$month))

sims_all <- fit_full |>
  generate(h = h_out, times = 2000) |>
  as_tibble() |>
  keep_best() |>
  left_join(select(range_k, provider, k), by = "provider") |>
  group_by(provider, month) |>
  mutate(centre = mean(.sim)) |>
  ungroup() |>
  mutate(.sim = centre + k * (.sim - centre),            # widened around the centre
         days = lubridate::days_in_month(as.Date(month)),
         att  = .sim * days)                             # back to monthly attendances

q_summ <- function(x) {
  tibble(mean = mean(x),
         lo95 = quantile(x, 0.025), lo80 = quantile(x, 0.10),
         hi80 = quantile(x, 0.90),  hi95 = quantile(x, 0.975))
}

# Cluster total: the two trusts added within each simulated future; its range is
# then widened below so it is never narrower than the two trusts' ranges added.
monthly_ranges <- bind_rows(
  sims_all |> group_by(provider, month) |> summarise(q_summ(att), .groups = "drop"),
  sims_all |> group_by(month, .rep) |> summarise(att = sum(att), .groups = "drop") |>
    group_by(month) |> summarise(q_summ(att), .groups = "drop") |>
    mutate(provider = "Cluster total")
)

# The cluster range adds the two trusts' ranges: a bad winter tends to hit both,
# so treating them as independent would make the total look more certain than it is.
widen_cluster <- function(tbl, by = NULL) {
  comp <- tbl |> filter(provider != "Cluster total") |>
    group_by(across(all_of(by))) |>
    summarise(d_lo80 = sum(mean - lo80), d_hi80 = sum(hi80 - mean),
              d_lo95 = sum(mean - lo95), d_hi95 = sum(hi95 - mean), .groups = "drop")
  joined <- if (is.null(by)) cross_join(tbl, comp) else left_join(tbl, comp, by = by)
  joined |>
    mutate(is_total = provider == "Cluster total",
           lo80 = if_else(is_total, pmin(lo80, mean - d_lo80), lo80),
           hi80 = if_else(is_total, pmax(hi80, mean + d_hi80), hi80),
           lo95 = if_else(is_total, pmin(lo95, mean - d_lo95), lo95),
           hi95 = if_else(is_total, pmax(hi95, mean + d_hi95), hi95)) |>
    select(-starts_with("d_"), -is_total)
}
monthly_ranges <- widen_cluster(monthly_ranges, by = "month")

fy <- \(x) filter(x, month >= yearmonth("2027 Apr"), month <= yearmonth("2028 Mar"))
plan_monthly <- fy(monthly_ranges)
sims_fy <- fy(sims_all)

plan_annual_sim <- bind_rows(
  sims_fy |> group_by(provider, .rep) |> summarise(att = sum(att), .groups = "drop") |>
    group_by(provider) |> summarise(q_summ(att), .groups = "drop"),
  sims_fy |> group_by(.rep) |> summarise(att = sum(att), .groups = "drop") |>
    summarise(q_summ(att)) |> mutate(provider = "Cluster total")
)

# Back-test errors in 12-month totals for the chosen models (and both trusts added)
annual_err <- annual_cv |>
  semi_join(best, by = c("provider", ".model")) |>
  select(provider, .id, fc_total, act_total)
annual_err <- bind_rows(
  annual_err,
  annual_err |> group_by(.id) |>
    summarise(fc_total = sum(fc_total), act_total = sum(act_total), .groups = "drop") |>
    mutate(provider = "Cluster total")
) |>
  mutate(pct_err = fc_total / act_total - 1) |>
  group_by(provider) |>
  summarise(err80 = quantile(abs(pct_err), 0.80), err95 = quantile(abs(pct_err), 0.95),
            .groups = "drop")

last_12m <- post |>
  as_tibble() |>
  group_by(provider) |>
  slice_max(month, n = 12) |>
  summarise(last_12m_actual = sum(type1_att), .groups = "drop")
last_12m <- bind_rows(last_12m,
                      tibble(provider = "Cluster total",
                             last_12m_actual = sum(last_12m$last_12m_actual)))

# Annual range = the wider of the simulation and the back-test errors
plan_annual <- plan_annual_sim |>
  left_join(annual_err, by = "provider") |>
  mutate(lo80 = pmin(lo80, mean * (1 - err80)), hi80 = pmax(hi80, mean * (1 + err80)),
         lo95 = pmin(lo95, mean * (1 - err95)), hi95 = pmax(hi95, mean * (1 + err95))) |>
  widen_cluster() |>
  left_join(last_12m, by = "provider") |>
  mutate(change_vs_last_12m = mean / last_12m_actual - 1)

write_csv(mutate(plan_monthly, month = as.character(month)),
          "outputs/tables/q2_plan_monthly_2027_28.csv")
write_csv(plan_annual, "outputs/tables/q2_plan_annual_2027_28.csv")
write_rds(monthly_ranges, "data/processed/forecast_monthly_ranges.rds")  # used by the simulation

cat("\n== 2027-28 plan: total Type 1 attendances ==\n")
plan_annual |>
  transmute(provider,
            plan = scales::comma(mean),
            `80% range` = paste(scales::comma(lo80), "to", scales::comma(hi80)),
            `95% range` = paste(scales::comma(lo95), "to", scales::comma(hi95)),
            `+/- 80%` = scales::percent((hi80 - lo80) / 2 / mean, 0.1),
            `last 12m actual` = scales::comma(last_12m_actual),
            change = scales::percent(change_vs_last_12m, 0.1)) |>
  print(width = 200)

cat("\n== 2027-28 plan by month (mean, 80% range) ==\n")
plan_monthly |>
  transmute(provider, month = as.character(month),
            plan = round(mean, -1),
            range_80 = paste(round(lo80, -1), "-", round(hi80, -1))) |>
  pivot_wider(names_from = provider, values_from = c(plan, range_80)) |>
  print(n = Inf, width = 200)

# Charts ------------------------------------------------------------------------------------
model_names <- c(snaive = "Seasonal naive (baseline)", ets = "ETS", arima = "ARIMA",
                 tslm = "Regression (trend + month)", dynreg = "Regression with ARIMA errors")
p_cv <- cv |>
  group_by(provider, .model, h) |>
  summarise(MAPE = 100 * mean(ape), .groups = "drop") |>
  mutate(model = model_names[.model]) |>
  ggplot(aes(h, MAPE, colour = model)) +
  geom_line(linewidth = 0.9) + geom_point(size = 1.5) +
  facet_wrap(~provider, ncol = 1) +
  scale_x_continuous(breaks = 1:12) +
  labs(title = paste("Forecast error by months ahead, averaged over", n_origins, "back-tests"),
       subtitle = "Rolling-origin cross-validation, July 2021 onwards. Lower is better.",
       x = "Months ahead", y = "MAPE (%)", colour = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
ggsave("outputs/figures/04_model_cv.png", p_cv, width = 10, height = 7, dpi = 150)

fc_plot_data <- monthly_ranges |> filter(provider != "Cluster total")
p_fc <- ggplot() +
  annotate("rect", xmin = as.Date("2027-04-01"), xmax = as.Date("2028-03-31"),
           ymin = -Inf, ymax = Inf, alpha = 0.06) +
  geom_ribbon(data = fc_plot_data, aes(as.Date(month), ymin = lo95, ymax = hi95),
              fill = "#2c6fbb", alpha = 0.15) +
  geom_ribbon(data = fc_plot_data, aes(as.Date(month), ymin = lo80, ymax = hi80),
              fill = "#2c6fbb", alpha = 0.30) +
  geom_line(data = fc_plot_data, aes(as.Date(month), mean), colour = "#2c6fbb", linewidth = 0.9) +
  geom_line(data = as_tibble(post) |> filter(month >= yearmonth("2023 Apr")),
            aes(as.Date(month), type1_att), colour = "black", linewidth = 0.8) +
  facet_wrap(~provider, ncol = 1, scales = "free_y") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Type 1 A&E attendances: forecast to March 2028",
       subtitle = "Line = central forecast; dark band = 80% range; light band = 95% range (calibrated on back-tests); shaded = 2027-28",
       x = NULL, y = "Attendances per month") +
  theme_minimal(base_size = 12)
ggsave("outputs/figures/05_forecast_2027_28.png", p_fc, width = 10, height = 7, dpi = 150)

message("Q2 tables saved to outputs/tables/ and charts to outputs/figures/.")
