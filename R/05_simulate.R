# 05_simulate.R -----------------------------------------------------------------
# Question 3: if January 2028 demand arrives as forecast, how does the share of
# Type 1 patients who leave A&E within 4 hours change with one extra clinician
# 10:00-22:00?
#
# Scope (decision): Gloucestershire Hospitals (GRH + Cheltenham) modelled as ONE
# combined Type 1 emergency department. This overstates how flexibly staff can
# move between sites, so results describe "a department of this size".
#
# Patient pathway (discrete-event simulation with simmer):
#   arrive -> triage nurse -> clinician assessment -> investigations/treatment
#          -> clinician review/decision -> discharged, or admitted after a bed wait
# The 4-hour measure is arrival to leaving A&E (the NHS standard), Type 1 only.
#
# How the numbers are set:
#   * Demand: January 2028 forecast from 04_forecast.R, spread over the day with
#     an hourly arrival profile. Each run simulates a whole January (31 days)
#     after 3 warm-up days.
#   * Admission rate and bed waits: Gloucestershire's own January 2026 data. Bed
#     waits use three bands matching the published decision-to-admit figures:
#     under 4 hours, 4-12 hours, over 12 hours (tail capped at 48 hours).
#   * Clinician capacity: real staffing is not published. The rota is a stated
#     MODELLED capacity (22 clinician-equivalents at peak), and the clinician
#     minutes per patient are CALIBRATED continuously so the model reproduces the
#     actual January 2026 Type 1 4-hour performance. "22" is therefore not a
#     headcount: it only means something together with the calibrated minutes.
#   * Patients still in the department when a run ends count as breaches.
#   * Scenarios share random numbers run by run, so the effect of the extra
#     clinician is measured as a paired difference, run against run.
# Not modelled: day-of-week patterns (busy Mondays), cubicle space, seniority.

library(tidyverse)
library(tsibble)
library(janitor)
library(simmer)
library(simmer.plot)
library(parallel)
library(fs)
source("R/00_config.R")

set.seed(2028)
dir_create("outputs/figures")
dir_create("outputs/tables")
provider_name <- "Gloucestershire (GHFT)"

# 1. Inputs from the data ----------------------------------------------------------
monthly  <- read_rds("data/processed/ae_type1_monthly.rds")
fc_rng   <- read_rds("data/processed/forecast_monthly_ranges.rds")

obs_jan26 <- monthly |> as_tibble() |>
  filter(provider == provider_name, month == yearmonth("2026 Jan"))
jan28 <- fc_rng |> filter(provider == provider_name, month == yearmonth("2028 Jan"))

# Decision-to-admit (DTA) waits, January 2026, straight from the raw file
idx  <- read_csv("data/raw/_file_index.csv", show_col_types = FALSE)
f_j26 <- idx |> filter(str_detect(month_label, regex("January 2026", ignore_case = TRUE))) |>
  arrange(desc(revised)) |> slice(1) |> pull(file)
raw_j26 <- read_csv(f_j26, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
  clean_names() |> filter(org_code == "RTE")
num  <- \(pattern) sum(as.numeric(str_remove_all(unlist(raw_j26[str_subset(names(raw_j26), pattern)]), ",")), na.rm = TRUE)
dta_4_12 <- num("4_12_h.*dta")
dta_12   <- num("waited_12_h.*dta")
adm_all  <- num("^emergency_admissions_via_a_e")
p_4_12   <- dta_4_12 / adm_all          # share of admissions waiting 4-12h for a bed
p_over12 <- dta_12 / adm_all            # share waiting over 12h
p_under4 <- 1 - p_4_12 - p_over12

p_admit   <- obs_jan26$emerg_adm_t1 / obs_jan26$type1_att
target_4h <- obs_jan26$pct_within_4h

cat("\n== Inputs from Gloucestershire data (Type 1) ==\n")
cat(sprintf("Jan 2026 actual: %.0f arrivals/day, %.1f%% within 4h (Type 1), %.1f%% admitted\n",
            obs_jan26$att_per_day, 100 * target_4h, 100 * p_admit))
cat(sprintf("Bed waits after decision to admit: %.1f%% under 4h, %.1f%% 4-12h, %.1f%% over 12h\n",
            100 * p_under4, 100 * p_4_12, 100 * p_over12))
cat(sprintf("Jan 2028 forecast: %.0f/day (95%% upper: %.0f/day)\n",
            jan28$mean / 31, jan28$hi95 / 31))

# 2. Assumptions ------------------------------------------------------------------------
# Hourly arrival profile (share of a day's arrivals, 00:00-23:00). Illustrative shape
# consistent with NHS Digital HES A&E reports: quietest 03:00-06:00, peak 10:00-12:00.
hour_share <- c(2.6, 2.1, 1.8, 1.5, 1.3, 1.3, 1.6, 2.4, 3.9, 5.3, 6.1, 6.2,
                6.0, 5.8, 5.7, 5.6, 5.6, 5.6, 5.4, 5.1, 4.7, 4.3, 3.8, 3.2)
hour_share <- hour_share / sum(hour_share)

base_params <- list(
  per_day      = jan28$mean / 31,
  hour_share   = hour_share,
  triage_mean  = 10,                    # min, exponential
  assess_mean  = NA,                    # min, clinician assessment: CALIBRATED below
  assess_cv    = 25 / 45,               # spread of assessment time (SD / mean)
  invest_mean  = 90, invest_sd = 60,    # min, tests/treatment/results (no clinician time)
  review_mean  = 10, review_sd = 5,     # min, clinician review and decision
  p_admit      = p_admit,
  bed_p        = c(p_under4, p_4_12, p_over12),
  bed_tail_mean = 360,                  # min beyond 12h, exponential, for the over-12h band
  bed_cap      = 48 * 60,               # no bed wait longer than 48h
  bed_scale    = 1,                     # sensitivity: multiplies every bed wait
  triage_rota  = c(night = 3, day = 5), # pooled across both sites (~75% busy)
  rota_peak    = 22,                    # MODELLED capacity at 10:00-22:00 (not a headcount)
  extra_clin   = 0,                     # +1 scenario adds here, 10:00-22:00 only
  warm_days = 3, measure_days = 31, cool_days = 1
)

# Rota shape: clinicians on duty by period, as a share of the 10:00-22:00 peak
rota_for <- function(peak, extra = 0) {
  list(times  = c(0, 480, 600, 1320),                      # 00:00, 08:00, 10:00, 22:00
       values = c(round(0.55 * peak), round(0.80 * peak), peak + extra, round(0.75 * peak)))
}

# 3. One simulated run (self-contained so it can run on parallel workers) ------------------
run_one <- function(rep, p) {
  library(simmer)
  set.seed(10000 + rep)
  rln <- function(m, s) rlnorm(1, log(m^2 / sqrt(s^2 + m^2)), sqrt(log(1 + s^2 / m^2)))
  bed <- function() {                     # three bands from the published DTA waits
    u <- runif(1)
    w <- if (u < p$bed_p[1]) runif(1, 0, 240)
         else if (u < p$bed_p[1] + p$bed_p[2]) runif(1, 240, 720)
         else min(720 + rexp(1, 1 / p$bed_tail_mean), p$bed_cap)
    w * p$bed_scale
  }

  total_days <- p$warm_days + p$measure_days + p$cool_days
  arr <- unlist(lapply(seq_len(total_days * 24) - 1, function(h) {
    k <- rpois(1, p$per_day * p$hour_share[(h %% 24) + 1])
    h * 60 + sort(runif(k, 0, 60))
  }))

  env <- simmer()
  patient <- trajectory() |>
    seize("triage") |> timeout(function() rexp(1, 1 / p$triage_mean)) |> release("triage") |>
    seize("clinician") |>
    set_attribute("t_seen", function() now(env)) |>
    timeout(function() rln(p$assess_mean, p$assess_cv * p$assess_mean)) |>
    release("clinician") |>
    timeout(function() rln(p$invest_mean, p$invest_sd)) |>
    seize("clinician") |> timeout(function() rln(p$review_mean, p$review_sd)) |> release("clinician") |>
    branch(function() as.numeric(runif(1) < p$p_admit), continue = TRUE,
           trajectory() |>
             set_attribute("admitted", 1) |>
             timeout(bed))

  rota <- p$rota
  env |>
    add_resource("triage", schedule(c(0, 480, 1320), c(p$triage_rota[["night"]], p$triage_rota[["day"]],
                                                       p$triage_rota[["night"]]), period = 1440)) |>
    add_resource("clinician", schedule(rota$times, rota$values, period = 1440)) |>
    add_generator("pt", patient, at(arr), mon = 2) |>
    run(until = total_days * 1440 + 3 * 1440) |> invisible()
  t_end <- now(env)

  # ongoing = TRUE keeps patients still in the department at the end: they breached
  a  <- get_mon_arrivals(env, ongoing = TRUE)
  at <- get_mon_attributes(env)
  seen <- at[at$key == "t_seen", c("name", "value")]
  adm  <- at[at$key == "admitted", "name"]
  a <- merge(a, seen, by = "name", all.x = TRUE)
  a <- a[a$start_time >= p$warm_days * 1440 &
           a$start_time <  (p$warm_days + p$measure_days) * 1440, ]
  a$end_time[is.na(a$end_time)] <- Inf
  a$los   <- a$end_time - a$start_time
  a$wait  <- ifelse(is.na(a$value), t_end - a$start_time, a$value - a$start_time)
  a$adm   <- a$name %in% adm
  a$hour  <- floor((a$start_time %% 1440) / 60)
  data.frame(
    rep = rep, n = nrow(a),
    pct_4h = mean(a$los <= 240),
    pct_4h_nonadm = mean(a$los[!a$adm] <= 240),
    pct_4h_adm = mean(a$los[a$adm] <= 240),
    pct_unfinished = mean(!is.finite(a$los)),
    mean_wait = mean(a$wait),
    mean_los = mean(a$los[is.finite(a$los)]),
    t(setNames(as.numeric(tapply(a$wait, factor(a$hour, levels = 0:23), mean)),
               paste0("wait_h", 0:23)))
  )
}

cl <- makeCluster(max(1, detectCores() - 1))
clusterCall(cl, function(paths) .libPaths(paths), .libPaths())  # workers use the project's renv library
run_scenario <- function(p, reps) {
  p$rota <- rota_for(p$rota_peak, p$extra_clin)
  do.call(rbind, parLapply(cl, seq_len(reps), run_one, p = p))
}

# 4. Calibrate clinician minutes per patient to January 2026 ------------------------------
# A continuous setting, so the baseline can match the target closely (an integer
# number of clinicians jumps about 15 points at a time near capacity).
calib_params <- modifyList(base_params, list(per_day = obs_jan26$att_per_day,
                                             warm_days = 3, measure_days = 14))
calib <- map(seq(30, 60, by = 2), \(m) {
  r <- run_scenario(modifyList(calib_params, list(assess_mean = m)), reps = 20)
  tibble(assess_mean = m, sim_4h = mean(r$pct_4h), mean_wait = mean(r$mean_wait))
}) |> list_rbind()
smooth  <- loess(sim_4h ~ assess_mean, data = calib, span = 0.5)
fine    <- tibble(assess_mean = seq(30, 60, by = 0.1)) |>
  mutate(sim_4h = predict(smooth, newdata = data.frame(assess_mean = assess_mean)))
assess_star <- fine$assess_mean[which.min(abs(fine$sim_4h - target_4h))]
base_params$assess_mean <- assess_star

check <- run_scenario(modifyList(calib_params, list(assess_mean = assess_star)), reps = 100)
calib_check <- tibble(assess_mean = assess_star, target_4h = target_4h,
                      sim_4h = mean(check$pct_4h),
                      sim_4h_lo = quantile(check$pct_4h, 0.025), sim_4h_hi = quantile(check$pct_4h, 0.975))
write_csv(calib, "outputs/tables/q3_calibration.csv")
write_csv(calib_check, "outputs/tables/q3_calibration_check.csv")
cat("\n== Calibration: clinician minutes per patient vs simulated Type 1 4h % (Jan 2026 demand) ==\n")
calib |> mutate(sim_4h = scales::percent(sim_4h, 0.1), mean_wait = round(mean_wait)) |> print(n = Inf)
cat(sprintf("Calibrated: %.1f min per patient -> %.1f%% simulated (100 runs) vs %.1f%% actual\n",
            assess_star, 100 * calib_check$sim_4h, 100 * target_4h))

# 5. Scenarios: a whole simulated January each run ------------------------------------------
summarise_runs <- function(r, label) {
  tibble(scenario = label,
         pct_4h = mean(r$pct_4h), pct_4h_lo = quantile(r$pct_4h, 0.025), pct_4h_hi = quantile(r$pct_4h, 0.975),
         pct_4h_nonadmitted = mean(r$pct_4h_nonadm), pct_4h_admitted = mean(r$pct_4h_adm),
         pct_unfinished = mean(r$pct_unfinished),
         mean_wait_clinician = mean(r$mean_wait), mean_time_in_ae = mean(r$mean_los))
}

scen_defs <- list(
  "1. Jan 2028 forecast, current capacity" = list(),
  "2. Jan 2028 forecast, +1 clinician 10:00-22:00" = list(extra_clin = 1),
  "3. Jan 2028 high demand (95% upper), current capacity" = list(per_day = jan28$hi95 / 31),
  "4. Jan 2028 high demand, +1 clinician 10:00-22:00" = list(per_day = jan28$hi95 / 31, extra_clin = 1)
)
scen_runs <- imap(scen_defs, \(mods, label) run_scenario(modifyList(base_params, mods), reps = 100))
scenarios <- imap(scen_runs, summarise_runs) |> list_rbind()
write_csv(scenarios, "outputs/tables/q3_scenarios.csv")

# Paired effect of the extra clinician: same random patients, run against run
paired <- function(with, without, label) {
  d <- with$pct_4h - without$pct_4h
  w <- with$mean_wait - without$mean_wait
  tibble(comparison = label, gain_pts = 100 * mean(d),
         gain_lo = 100 * quantile(d, 0.025), gain_hi = 100 * quantile(d, 0.975),
         wait_change_min = mean(w), wait_lo = quantile(w, 0.025), wait_hi = quantile(w, 0.975))
}
improvement <- bind_rows(
  paired(scen_runs[[2]], scen_runs[[1]], "Forecast demand: +1 clinician vs current"),
  paired(scen_runs[[4]], scen_runs[[3]], "High demand: +1 clinician vs current")
)
write_csv(improvement, "outputs/tables/q3_improvement.csv")

cat("\n== Scenario results (100 simulated Januaries each; Type 1) ==\n")
scenarios |>
  transmute(scenario,
            `within 4h` = paste0(scales::percent(pct_4h, 0.1), " [", scales::percent(pct_4h_lo, 0.1),
                                 "-", scales::percent(pct_4h_hi, 0.1), "]"),
            `non-admitted` = scales::percent(pct_4h_nonadmitted, 0.1),
            admitted = scales::percent(pct_4h_admitted, 0.1),
            unfinished = scales::percent(pct_unfinished, 0.01),
            `wait to clinician (min)` = round(mean_wait_clinician),
            `time in A&E (min)` = round(mean_time_in_ae)) |>
  print(width = 200)
cat("\n== Effect of +1 clinician (paired, run against run) ==\n")
improvement |> mutate(across(where(is.numeric), \(x) round(x, 1))) |> print(width = 200)

# 6. Sensitivity: each key assumption +/-20%, everything else at baseline ----------------------
sens_defs <- list(
  "Arrivals per day"            = \(p, f) modifyList(p, list(per_day = p$per_day * f)),
  "Clinician time per patient"  = \(p, f) modifyList(p, list(assess_mean = p$assess_mean * f)),
  "Investigation time"          = \(p, f) modifyList(p, list(invest_mean = p$invest_mean * f, invest_sd = p$invest_sd * f)),
  "Admission rate"              = \(p, f) modifyList(p, list(p_admit = p$p_admit * f)),
  "Bed waits"                   = \(p, f) modifyList(p, list(bed_scale = f))
)
base_4h <- scenarios$pct_4h[1]
sensitivity <- imap(sens_defs, \(fn, label) {
  map(c(0.8, 1.2), \(f) {
    r <- run_scenario(fn(base_params, f), reps = 50)
    tibble(assumption = label, change = ifelse(f < 1, "-20%", "+20%"), pct_4h = mean(r$pct_4h))
  }) |> list_rbind()
}) |> list_rbind() |>
  mutate(effect_pts = 100 * (pct_4h - base_4h))
write_csv(sensitivity, "outputs/tables/q3_sensitivity.csv")

cat("\n== Sensitivity: change in % within 4h (percentage points) ==\n")
sensitivity |> dplyr::select(assumption, change, effect_pts) |>  # dplyr:: because simmer masks select()
  pivot_wider(names_from = change, values_from = effect_pts) |>
  mutate(across(-assumption, \(x) round(x, 1)), swing = abs(`+20%` - `-20%`)) |>
  arrange(desc(swing)) |> print()

# 7. Assumptions table (for the report) ----------------------------------------------------------
rota <- rota_for(base_params$rota_peak)$values
assumptions <- tribble(
  ~parameter, ~value, ~source,
  "Department", "GRH + Cheltenham modelled as one combined Type 1 ED", "Scoping decision",
  "Arrivals (Jan 2028)", sprintf("%.0f per day (95%% upper %.0f)", jan28$mean / 31, jan28$hi95 / 31), "Forecast, 04_forecast.R",
  "Arrival pattern by hour", "Low 03:00-06:00, peak 10:00-12:00", "Illustrative; consistent with NHS Digital HES A&E reports",
  "Triage", "10 min average; 5 nurses 08:00-22:00, 3 overnight (both sites)", "Stated assumption",
  "Clinician capacity (modelled)", sprintf("%d clinician-equivalents 10:00-22:00, %d 08:00-10:00, %d 22:00-24:00, %d overnight", rota[3], rota[2], rota[4], rota[1]),
  "Stated assumption; not a headcount",
  "Clinician time per patient", sprintf("%.1f min average assessment + 10 min review", assess_star),
  "Calibrated so the model reproduces GHFT's January 2026 Type 1 4-hour performance",
  "Investigations / treatment", "90 min average (SD 60), no clinician time", "Stated assumption",
  "Admission rate", scales::percent(p_admit, 0.1), "GHFT Type 1, January 2026 (NHS England monthly A&E)",
  "Bed wait after decision to admit", sprintf("%.0f%% under 4h (spread evenly 0-4h), %.0f%% 4-12h (evenly), %.0f%% over 12h (12h + average 6h more, capped at 48h)",
                                              100 * p_under4, 100 * p_4_12, 100 * p_over12),
  "Band shares: GHFT DTA waits, January 2026; shapes within bands: stated assumption",
  "Patients still in A&E at the end of a run", "Counted as 4-hour breaches", "Simulation design",
  "Replications", "100 simulated Januaries per scenario (50 for sensitivity); 3 warm-up days", "Simulation design"
)
write_csv(assumptions, "outputs/tables/q3_assumptions.csv")

# 8. Charts ---------------------------------------------------------------------------------------
wait_by_hour <- imap(scen_runs[1:2], \(r, label) {
  tibble(scenario = label, hour = 0:23,
         wait = colMeans(r[, paste0("wait_h", 0:23)], na.rm = TRUE))
}) |> list_rbind()

p_wait <- ggplot(wait_by_hour, aes(hour, wait, colour = scenario)) +
  annotate("rect", xmin = 10, xmax = 22, ymin = -Inf, ymax = Inf, alpha = 0.07) +
  geom_line(linewidth = 1) + geom_point() +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  scale_colour_manual(values = c("#7a7a7a", "#2c6fbb")) +
  labs(title = "Average wait to see a clinician, by hour of arrival (January 2028 forecast)",
       subtitle = "Shaded: 10:00-22:00, when the extra clinician is on duty",
       x = "Hour of arrival", y = "Minutes", colour = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "top", legend.direction = "vertical")
ggsave("outputs/figures/06_wait_by_hour.png", p_wait, width = 9, height = 5.5, dpi = 150)

p_sens <- sensitivity |>
  mutate(assumption = fct_reorder(assumption, abs(effect_pts), .fun = max)) |>
  ggplot(aes(effect_pts, assumption, fill = change)) +
  geom_col(position = "identity", alpha = 0.85) +
  geom_vline(xintercept = 0) +
  scale_fill_manual(values = c("-20%" = "#2c6fbb", "+20%" = "#d1495b")) +
  labs(title = "Which assumption matters most? (+/-20% change, January 2028)",
       subtitle = sprintf("Change in %% of Type 1 patients within 4 hours vs baseline of %.1f%%", 100 * base_4h),
       x = "Percentage points", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12)
ggsave("outputs/figures/07_sensitivity.png", p_sens, width = 9, height = 5, dpi = 150)

# One monitored baseline run for a resource-usage chart (simmer.plot)
p1 <- base_params; p1$rota <- rota_for(p1$rota_peak)
run_env <- function(p) {
  rln <- function(m, s) rlnorm(1, log(m^2 / sqrt(s^2 + m^2)), sqrt(log(1 + s^2 / m^2)))
  days <- 3
  arr <- unlist(lapply(seq_len(days * 24) - 1, function(h) {
    k <- rpois(1, p$per_day * p$hour_share[(h %% 24) + 1]); h * 60 + sort(runif(k, 0, 60)) }))
  env <- simmer()
  patient <- trajectory() |>
    seize("triage") |> timeout(function() rexp(1, 1 / p$triage_mean)) |> release("triage") |>
    seize("clinician") |> timeout(function() rln(p$assess_mean, p$assess_cv * p$assess_mean)) |> release("clinician") |>
    timeout(function() rln(p$invest_mean, p$invest_sd)) |>
    seize("clinician") |> timeout(function() rln(p$review_mean, p$review_sd)) |> release("clinician")
  env |>
    add_resource("triage", schedule(c(0, 480, 1320), c(3, 5, 3), period = 1440)) |>
    add_resource("clinician", schedule(p$rota$times, p$rota$values, period = 1440)) |>
    add_generator("pt", patient, at(arr)) |>
    run(until = days * 1440)
}
env1 <- run_env(p1)
p_use <- plot(get_mon_resources(env1), metric = "usage", names = "clinician", items = c("queue", "server")) +
  labs(title = "Clinicians busy and patients queuing over 3 simulated January days (baseline)",
       x = "Minutes from start", y = "Patients") +
  theme_minimal(base_size = 12)
ggsave("outputs/figures/08_clinician_usage.png", p_use, width = 10, height = 5, dpi = 150)

stopCluster(cl)
message("Q3 tables saved to outputs/tables/ and charts to outputs/figures/.")
