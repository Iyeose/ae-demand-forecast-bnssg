# 05_simulate.R -----------------------------------------------------------------
# Question 3: if January 2028 demand arrives as forecast, how does the share of
# patients who leave A&E within 4 hours change with one extra clinician 10:00-22:00?
#
# Scope (decision): Gloucestershire Hospitals (GRH + Cheltenham) modelled as ONE
# combined emergency department. This overstates how flexibly staff can move
# between sites, so results are best read as "a department of this size".
#
# Patient pathway (discrete-event simulation with simmer):
#   arrive -> triage nurse -> clinician assessment -> investigations/treatment
#          -> clinician review/decision -> discharged, or admitted after a bed wait
# The 4-hour measure is arrival to leaving A&E (the NHS standard).
#
# How the numbers are set:
#   * Demand: January 2028 forecast from 04_forecast.R, spread over the day with
#     an hourly arrival profile.
#   * Admission rate and bed waits: from Gloucestershire's own January 2026 data
#     (emergency admissions; 4-12h and 12h+ waits from decision to admit).
#   * Clinical times: published / stated assumptions (see assumptions table).
#   * Clinician numbers: not public, so CALIBRATED - we pick the rota size that
#     reproduces Gloucestershire's actual January 2026 4-hour performance, then
#     test changes against that calibrated baseline.

library(tidyverse)
library(tsibble)
library(janitor)
library(simmer)
library(simmer.plot)
library(parallel)
library(fs)

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
p_over4  <- max((dta_4_12 + dta_12) / adm_all, 0.01)     # share of admissions waiting > 4h for a bed
p_over12 <- max(dta_12 / adm_all, 0.005)       # share waiting > 12h

# Fit a log-normal bed wait that matches both shares exactly
bed_sigma <- (log(720) - log(240)) / (qnorm(1 - p_over12) - qnorm(1 - p_over4))
bed_mu    <- log(240) - bed_sigma * qnorm(1 - p_over4)

p_admit   <- obs_jan26$emerg_adm_t1 / obs_jan26$type1_att
target_4h <- obs_jan26$pct_within_4h

cat("\n== Inputs from Gloucestershire data ==\n")
cat(sprintf("Jan 2026 actual: %.0f arrivals/day, %.1f%% within 4h, %.1f%% admitted\n",
            obs_jan26$att_per_day, 100 * target_4h, 100 * p_admit))
cat(sprintf("Bed waits (DTA): %.1f%% > 4h, %.1f%% > 12h -> median bed wait %.0f min\n",
            100 * p_over4, 100 * p_over12, exp(bed_mu)))
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
  assess_mean  = 45, assess_sd = 25,    # min, first clinician assessment (~1.1 pts/hr incl. review)
  invest_mean  = 90, invest_sd = 60,    # min, tests/treatment/results (no clinician time)
  review_mean  = 10, review_sd = 5,     # min, clinician review and decision
  p_admit      = p_admit,
  bed_mu       = bed_mu, bed_sigma = bed_sigma,
  triage_rota  = c(night = 3, day = 5), # pooled across both sites (~75% busy)
  rota_peak    = NA,                    # clinicians on duty 10:00-22:00 (calibrated)
  extra_clin   = 0,                     # +1 scenario adds here, 10:00-22:00 only
  warm_days = 2, measure_days = 7, cool_days = 1
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
    timeout(function() rln(p$assess_mean, p$assess_sd)) |>
    release("clinician") |>
    timeout(function() rln(p$invest_mean, p$invest_sd)) |>
    seize("clinician") |> timeout(function() rln(p$review_mean, p$review_sd)) |> release("clinician") |>
    branch(function() as.numeric(runif(1) < p$p_admit), continue = TRUE,
           trajectory() |>
             set_attribute("admitted", 1) |>
             timeout(function() rlnorm(1, p$bed_mu, p$bed_sigma)))
  
  rota <- p$rota
  env |>
    add_resource("triage", schedule(c(0, 480, 1320), c(p$triage_rota[["night"]], p$triage_rota[["day"]],
                                                       p$triage_rota[["night"]]), period = 1440)) |>
    add_resource("clinician", schedule(rota$times, rota$values, period = 1440)) |>
    add_generator("pt", patient, at(arr), mon = 2) |>
    run(until = total_days * 1440 + 5 * 1440) |> invisible()
  
  a  <- get_mon_arrivals(env)
  at <- get_mon_attributes(env)
  seen <- at[at$key == "t_seen", c("name", "value")]
  adm  <- at[at$key == "admitted", "name"]
  a <- merge(a, seen, by = "name", all.x = TRUE)
  a <- a[a$start_time >= p$warm_days * 1440 &
           a$start_time <  (p$warm_days + p$measure_days) * 1440, ]
  a$los   <- a$end_time - a$start_time
  a$wait  <- a$value - a$start_time
  a$adm   <- a$name %in% adm
  a$hour  <- floor((a$start_time %% 1440) / 60)
  data.frame(
    rep = rep, n = nrow(a),
    pct_4h = mean(a$los <= 240),
    pct_4h_nonadm = mean(a$los[!a$adm] <= 240),
    pct_4h_adm = mean(a$los[a$adm] <= 240),
    mean_wait = mean(a$wait),
    mean_los = mean(a$los),
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

# 4. Calibrate the rota to January 2026 ---------------------------------------------------
calib_params <- modifyList(base_params, list(per_day = obs_jan26$att_per_day, measure_days = 5))
calib <- map(16:32, \(n) {
  r <- run_scenario(modifyList(calib_params, list(rota_peak = n)), reps = 8)
  tibble(rota_peak = n, sim_4h = mean(r$pct_4h), mean_wait = mean(r$mean_wait))
}) |> list_rbind() |>
  mutate(gap = abs(sim_4h - target_4h))
rota_peak <- calib$rota_peak[which.min(calib$gap)]
write_csv(calib, "outputs/tables/q3_calibration.csv")
cat("\n== Calibration: clinicians 10:00-22:00 vs simulated 4h % (Jan 2026 demand) ==\n")
calib |> mutate(sim_4h = scales::percent(sim_4h, 0.1), mean_wait = round(mean_wait)) |> print(n = Inf)
cat(sprintf("Chosen rota peak: %d clinicians (actual Jan 2026 4h = %.1f%%)\n", rota_peak, 100 * target_4h))
base_params$rota_peak <- rota_peak

# 5. Scenarios -------------------------------------------------------------------------------
summarise_runs <- function(r, label) {
  tibble(scenario = label,
         pct_4h = mean(r$pct_4h), pct_4h_lo = quantile(r$pct_4h, 0.025), pct_4h_hi = quantile(r$pct_4h, 0.975),
         pct_4h_nonadmitted = mean(r$pct_4h_nonadm), pct_4h_admitted = mean(r$pct_4h_adm),
         mean_wait_clinician = mean(r$mean_wait), mean_time_in_ae = mean(r$mean_los))
}

scen_defs <- list(
  "1. Jan 2028 forecast, current staffing" = list(),
  "2. Jan 2028 forecast, +1 clinician 10:00-22:00" = list(extra_clin = 1),
  "3. Jan 2028 high demand (95% upper), current staffing" = list(per_day = jan28$hi95 / 31),
  "4. Jan 2028 high demand, +1 clinician 10:00-22:00" = list(per_day = jan28$hi95 / 31, extra_clin = 1)
)
scen_runs <- imap(scen_defs, \(mods, label) run_scenario(modifyList(base_params, mods), reps = 100))
scenarios <- imap(scen_runs, summarise_runs) |> list_rbind()
write_csv(scenarios, "outputs/tables/q3_scenarios.csv")

cat("\n== Scenario results (100 replications each) ==\n")
scenarios |>
  transmute(scenario,
            `within 4h` = paste0(scales::percent(pct_4h, 0.1), " [", scales::percent(pct_4h_lo, 0.1),
                                 "-", scales::percent(pct_4h_hi, 0.1), "]"),
            `non-admitted` = scales::percent(pct_4h_nonadmitted, 0.1),
            admitted = scales::percent(pct_4h_admitted, 0.1),
            `avg wait to clinician (min)` = round(mean_wait_clinician),
            `avg time in A&E (min)` = round(mean_time_in_ae)) |>
  print(width = 200)

# 6. Sensitivity: each key assumption +/-20%, everything else at baseline ----------------------
sens_defs <- list(
  "Arrivals per day"       = \(p, f) modifyList(p, list(per_day = p$per_day * f)),
  "Clinician assessment time" = \(p, f) modifyList(p, list(assess_mean = p$assess_mean * f, assess_sd = p$assess_sd * f)),
  "Investigation time"     = \(p, f) modifyList(p, list(invest_mean = p$invest_mean * f, invest_sd = p$invest_sd * f)),
  "Admission rate"         = \(p, f) modifyList(p, list(p_admit = p$p_admit * f)),
  "Bed wait (median)"      = \(p, f) modifyList(p, list(bed_mu = p$bed_mu + log(f)))
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
assumptions <- tribble(
  ~parameter, ~value, ~source,
  "Department", "GRH + Cheltenham modelled as one combined ED", "Scoping decision",
  "Arrivals (Jan 2028)", sprintf("%.0f per day (95%% upper %.0f)", jan28$mean / 31, jan28$hi95 / 31), "Forecast, 04_forecast.R",
  "Arrival pattern by hour", "Low 03:00-06:00, peak 10:00-12:00", "Illustrative; consistent with NHS Digital HES A&E reports",
  "Triage", "10 min average; 5 nurses 08:00-22:00, 3 overnight (both sites)", "Stated assumption",
  "Clinician assessment", "45 min average (SD 25)", "Stated assumption; ~1 patient/hour per clinician is in line with published UK ED productivity",
  "Investigations / treatment", "90 min average (SD 60), no clinician time", "Stated assumption",
  "Clinician review / decision", "10 min average (SD 5)", "Stated assumption",
  "Admission rate", scales::percent(p_admit, 0.1), "GHFT Type 1, January 2026 (NHS England monthly A&E)",
  "Bed wait after decision to admit", sprintf("Log-normal, median %.0f min; %.0f%% > 4h, %.1f%% > 12h", exp(bed_mu), 100 * p_over4, 100 * p_over12), "GHFT DTA waits, January 2026 (NHS England monthly A&E)",
  "Clinician rota", sprintf("%d (10:00-22:00), %d (08:00-10:00), %d (22:00-24:00), %d (00:00-08:00)",
                            rota_peak, round(0.80 * rota_peak), round(0.75 * rota_peak), round(0.55 * rota_peak)),
  "Calibrated to GHFT's actual January 2026 4-hour performance",
  "Replications", "100 per scenario (50 for sensitivity); 2 warm-up days, 7 measured days", "Simulation design"
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
       subtitle = sprintf("Change in %% of patients within 4 hours vs baseline of %.1f%%", 100 * base_4h),
       x = "Percentage points", y = NULL, fill = NULL) +
  theme_minimal(base_size = 12)
ggsave("outputs/figures/07_sensitivity.png", p_sens, width = 9, height = 5, dpi = 150)

# One monitored baseline run for a resource-usage chart (simmer.plot)
p1 <- base_params; p1$rota <- rota_for(rota_peak)
run_env <- function(p) {
  rln <- function(m, s) rlnorm(1, log(m^2 / sqrt(s^2 + m^2)), sqrt(log(1 + s^2 / m^2)))
  days <- 3
  arr <- unlist(lapply(seq_len(days * 24) - 1, function(h) {
    k <- rpois(1, p$per_day * p$hour_share[(h %% 24) + 1]); h * 60 + sort(runif(k, 0, 60)) }))
  env <- simmer()
  patient <- trajectory() |>
    seize("triage") |> timeout(function() rexp(1, 1 / p$triage_mean)) |> release("triage") |>
    seize("clinician") |> timeout(function() rln(p$assess_mean, p$assess_sd)) |> release("clinician") |>
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

