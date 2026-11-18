# 04_forecast.R ----------------------------------------------------------------
# Question 2: how many Type 1 attendances should the ICBs plan for, month by
# month, in 2027-28, with a realistic range?
#
# Approach
#  1. Train on Jul 2021 - Oct 2025 (post-Covid), hold back the last 12 months
#     (Nov 2025 - Oct 2026) and test five models on data they have not seen:
#       snaive : same month last year (the baseline any model must beat)
#       ets    : exponential smoothing, automatically chosen trend/season form
#       arima  : seasonal ARIMA, automatically chosen orders
#       tslm   : regression on log scale with linear trend + month effects
#       dynreg : the same regression, but with ARIMA errors, so runs of high or
#                low months (autocorrelation) are modelled and ranges are honest
#     Accuracy: MAPE (average % miss), RMSE, MASE, and the Winkler score for
#     the 95% interval. We also check how often actuals fell inside the ranges.
#  2. Pick, for each provider, the lowest-MAPE model whose residuals pass the
#     Ljung-Box test (p > 0.05). A model with leftover pattern in its errors
#     gives over-confident ranges. Refit it on all post-Covid data and forecast
#     to March 2028.
#  3. Simulate 2,000 futures from that model to get monthly and annual ranges,
#     and to add the two providers together for a cluster total.
#
# Covid choice: training starts in July 2021 rather than using a Covid dummy.
# Demand came back at a different level and mix after Covid, and 64 months
# (5 winters) is enough for monthly seasonal models.

library(tidyverse)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(urca)       # used by ARIMA unit-root tests; loading it keeps it in renv.lock
library(fs)

set.seed(2027)
dir_create("outputs/figures")
dir_create("outputs/tables")

monthly <- read_rds("data/processed/ae_type1_monthly.rds")
post <- monthly |>
  filter(month >= yearmonth("2021 Jul")) |>
  select(provider, month, att_per_day, type1_att, days)

test_start <- max(post$month) - 11
train <- post |> filter(month < test_start)

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

# 1. Test on the held-back 12 months ------------------------------------------------
fit_train <- fit_models(train)
fc_test   <- fit_train |> forecast(h = 12)

acc <- fc_test |>
  accuracy(post, measures = list(point_accuracy_measures, interval_accuracy_measures)) |>
  select(provider, .model, any_of(c("MAPE", "RMSE", "MASE", "winkler")))

coverage <- fc_test |>
  hilo(level = c(80, 95)) |>
  unpack_hilo(c(`80%`, `95%`)) |>
  as_tibble() |>
  left_join(as_tibble(post) |> select(provider, month, actual = att_per_day),
            by = c("provider", "month")) |>
  group_by(provider, .model) |>
  summarise(in_80 = mean(actual >= `80%_lower` & actual <= `80%_upper`),
            in_95 = mean(actual >= `95%_lower` & actual <= `95%_upper`),
            .groups = "drop")

resid_check <- augment(fit_train) |>
  features(.innov, ljung_box, lag = 12) |>
  select(provider, .model, lb_pvalue)

model_table <- acc |>
  left_join(coverage, by = c("provider", ".model")) |>
  left_join(resid_check, by = c("provider", ".model")) |>
  arrange(provider, MAPE)
write_csv(model_table, "outputs/tables/q2_model_accuracy.csv")

cat("\n== Test-period accuracy (Nov 2025 - Oct 2026), best first ==\n")
model_table |>
  mutate(across(any_of(c("MAPE", "RMSE", "MASE", "winkler", "lb_pvalue")), \(x) round(x, 3)),
         across(c(in_80, in_95), \(x) scales::percent(x, 1))) |>
  print(n = Inf, width = 150)

best <- model_table |>
  group_by(provider) |>
  arrange(lb_pvalue <= 0.05, MAPE, .by_group = TRUE) |>   # passing models first, then by MAPE
  slice(1) |>
  ungroup() |>
  select(provider, .model)
write_csv(best, "outputs/tables/q2_best_model.csv")
keep_best <- \(x) filter(x, paste(provider, .model) %in% paste(best$provider, best$.model))
cat("\n== Chosen model ==\n"); print(best)

# 2. Refit on all post-Covid data -----------------------------------------------------
fit_full <- fit_models(post)
cat("\n== Fitted model forms (all data) ==\n")
print(fit_full)

# Residual check: Ljung-Box p > 0.05 means no leftover pattern the model missed
cat("\n== Residual check (Ljung-Box, lag 12) ==\n")
augment(fit_full) |>
  features(.innov, ljung_box, lag = 12) |>
  keep_best() |>
  print()

# 3. Forecast and simulate to March 2028 ---------------------------------------------
h_out <- as.numeric(yearmonth("2028 Mar")) - as.numeric(max(post$month))

sims_all <- fit_full |>
  generate(h = h_out, times = 2000) |>
  as_tibble() |>
  keep_best() |>
  mutate(days = lubridate::days_in_month(as.Date(month)),
         att  = .sim * days)                       # back to monthly attendances

q_summ <- function(x) {
  tibble(mean = mean(x),
         lo95 = quantile(x, 0.025), lo80 = quantile(x, 0.10),
         hi80 = quantile(x, 0.90),  hi95 = quantile(x, 0.975))
}

# Cluster total: add the providers within each simulated future. This treats the
# two trusts' surprises as independent; in reality a bad flu winter hits both, so
# the total range is if anything slightly too narrow.
monthly_ranges <- bind_rows(
  sims_all |> group_by(provider, month) |> summarise(q_summ(att), .groups = "drop"),
  sims_all |> group_by(month, .rep) |> summarise(att = sum(att), .groups = "drop") |>
    group_by(month) |> summarise(q_summ(att), .groups = "drop") |>
    mutate(provider = "Cluster total")
)

fy <- \(x) filter(x, month >= yearmonth("2027 Apr"), month <= yearmonth("2028 Mar"))
plan_monthly <- fy(monthly_ranges)
sims_fy <- fy(sims_all)

plan_annual <- bind_rows(
  sims_fy |> group_by(provider, .rep) |> summarise(att = sum(att), .groups = "drop") |>
    group_by(provider) |> summarise(q_summ(att), .groups = "drop"),
  sims_fy |> group_by(.rep) |> summarise(att = sum(att), .groups = "drop") |>
    summarise(q_summ(att)) |> mutate(provider = "Cluster total")
)

last_12m <- post |>
  as_tibble() |>
  group_by(provider) |>
  slice_max(month, n = 12) |>
  summarise(last_12m_actual = sum(type1_att), .groups = "drop")
last_12m <- bind_rows(last_12m,
                      tibble(provider = "Cluster total",
                             last_12m_actual = sum(last_12m$last_12m_actual)))

plan_annual <- plan_annual |>
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
            `last 12m actual` = scales::comma(last_12m_actual),
            change = scales::percent(change_vs_last_12m, 0.1)) |>
  print(width = 150)

cat("\n== 2027-28 plan by month (mean, 80% range) ==\n")
plan_monthly |>
  transmute(provider, month = as.character(month),
            plan = round(mean, -1),
            range_80 = paste(round(lo80, -1), "-", round(hi80, -1))) |>
  pivot_wider(names_from = provider, values_from = c(plan, range_80)) |>
  print(n = Inf, width = 200)

# Charts ------------------------------------------------------------------------------------
test_plot_data <- fc_test |>
  as_tibble() |>
  select(provider, .model, month, .mean)

p_test <- ggplot() +
  geom_line(data = as_tibble(post) |> filter(month >= yearmonth("2023 Apr")),
            aes(as.Date(month), att_per_day), colour = "black", linewidth = 0.8) +
  geom_line(data = test_plot_data,
            aes(as.Date(month), .mean, colour = .model), linewidth = 0.8) +
  annotate("rect", xmin = as.Date(test_start), xmax = as.Date(max(post$month)),
           ymin = -Inf, ymax = Inf, alpha = 0.08) +
  facet_wrap(~provider, ncol = 1, scales = "free_y") +
  labs(title = "Model test: forecasts for the 12 held-back months vs what actually happened",
       subtitle = "Black = actual. Shaded = test period (models never saw it)",
       x = NULL, y = "Type 1 attendances per day", colour = "Model") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")
ggsave("outputs/figures/04_model_test.png", p_test, width = 10, height = 7, dpi = 150)

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
       subtitle = "Line = central forecast; dark band = 80% range; light band = 95% range; shaded = 2027-28",
       x = NULL, y = "Attendances per month") +
  theme_minimal(base_size = 12)
ggsave("outputs/figures/05_forecast_2027_28.png", p_fc, width = 10, height = 7, dpi = 150)

message("Q2 tables saved to outputs/tables/ and charts to outputs/figures/.")
