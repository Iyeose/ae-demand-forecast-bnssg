# 02_clean_data.R ---------------------------------------------------------------
# Reads the raw monthly CSVs, keeps the Bristol and Gloucestershire acute trusts,
# and builds a clean monthly Type 1 series per provider.
#
# Decisions made here (each one is worth explaining at interview):
#  * Bristol merger: North Bristol (RVJ) was acquired by UHBW (RA7) on 1 July
#    2026 and UHBW was renamed Bristol NHS Foundation Trust, keeping code RA7.
#    Summing RVJ + RA7 every month gives one continuous Bristol series, both
#    before and after the merger. Weston Area Health NHS Trust (RA3) reported
#    separately until UH Bristol acquired it on 1 April 2020 (creating UHBW), so
#    RA3 is added too; without it Bristol shows a false ~140/day step in 2020.
#  * Type 1 only: Urgent Treatment Centres and other types changed reporting
#    over time, so Type 1 (major EDs) is the consistent measure.
#  * Revisions: if a month appears in more than one file, keep the revised one.
#  * Days in month: February has fewer days, so we model attendances per day.
#  * Covid: April 2020 to June 2021 is flagged, not deleted. The modelling step
#    decides whether to use a dummy or start training after it.

library(tidyverse)
library(janitor)
library(tsibble)
library(fs)
source("R/00_config.R")

providers <- tribble(
  ~org_code, ~provider,
  "RVJ",     "Bristol (NBT + UHBW)",
  "RA7",     "Bristol (NBT + UHBW)",
  "RA3",     "Bristol (NBT + UHBW)",   # Weston Area Health: joined UH Bristol 1 Apr 2020
  "RTE",     "Gloucestershire (GHFT)"
)

# Helper: first column whose cleaned name matches a pattern (NA if none).
# Column names drift slightly between years, so we match by pattern.
pick <- function(df, pattern) {
  hit <- str_subset(names(df), pattern)
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  df[[hit[1]]]
}

read_one <- function(path) {
  df <- read_csv(path, col_types = cols(.default = col_character()),
                 show_col_types = FALSE, progress = FALSE) |>
    clean_names()

  tibble(
    period        = pick(df, "^period$"),
    org_code      = pick(df, "^org_code$"),
    org_name      = pick(df, "^org_name$"),
    # Files before April 2020 prefix these columns with "number_of_"
    type1_att     = pick(df, "^(number_of_)?a_e_attendances_type_1$"),
    type1_booked  = pick(df, "^(number_of_)?a_e_attendances_booked_appointments_type_1$"),
    type1_over4h  = pick(df, "^(number_of_)?attendances_over_4hrs_type_1$"),
    emerg_adm_t1  = pick(df, "^emergency_admissions_via_a_e_type_1$"),
    source_file   = basename(path)
  )
}

files <- dir_ls("data/raw", glob = "*.csv") |> setdiff("data/raw/_file_index.csv")
index <- read_csv("data/raw/_file_index.csv", show_col_types = FALSE) |>
  transmute(source_file = basename(file), revised)

raw <- map(files, read_one) |>
  list_rbind() |>
  left_join(index, by = "source_file")

# Parse the period ("MSitAE-JANUARY-2025") and keep our providers
ae <- raw |>
  filter(org_code %in% providers$org_code) |>
  mutate(
    month = yearmonth(lubridate::my(str_extract(period, "[A-Za-z]+-\\d{4}$"))),
    across(c(type1_att, type1_booked, type1_over4h, emerg_adm_t1),
           \(x) as.numeric(str_remove_all(x, ",")))
  ) |>
  filter(!is.na(month), month >= DATA_START, month <= DATA_END)   # frozen window

# Revisions: one row per org per month, preferring revised files
ae <- ae |>
  arrange(org_code, month, desc(revised), desc(source_file)) |>
  distinct(org_code, month, .keep_all = TRUE)

# Combine to provider level (this is where RVJ + RA7 become one Bristol series)
monthly <- ae |>
  left_join(providers, by = "org_code") |>
  group_by(provider, month) |>
  summarise(
    n_orgs        = n_distinct(org_code),
    type1_att     = sum(type1_att, na.rm = TRUE),
    type1_booked  = sum(type1_booked, na.rm = TRUE),
    type1_over4h  = sum(type1_over4h, na.rm = TRUE),
    emerg_adm_t1  = sum(emerg_adm_t1, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    days          = lubridate::days_in_month(as.Date(month)),
    att_per_day   = type1_att / days,
    pct_within_4h = 1 - type1_over4h / type1_att,
    covid         = month >= yearmonth("2020 Apr") & month <= yearmonth("2021 Jun")
  ) |>
  as_tsibble(key = provider, index = month)

# Checks -------------------------------------------------------------------------
# 1. Gaps in each series (should be none, or only before the first CSV month)
gaps <- monthly |> count_gaps(.full = TRUE)
if (nrow(gaps) > 0) {
  message("Missing months found:")
  print(gaps)
}

# 2. Bristol should have 2 orgs before July 2026 and 1 after
monthly |>
  as_tibble() |>
  filter(str_detect(provider, "Bristol")) |>
  count(after_merger = month >= yearmonth("2026 Jul"), n_orgs) |>
  print()

# 3. Coverage
monthly |>
  as_tibble() |>
  group_by(provider) |>
  summarise(first = min(month), last = max(month), months = n()) |>
  print()

# Save ---------------------------------------------------------------------------
dir_create("data/processed")
write_rds(monthly, "data/processed/ae_type1_monthly.rds")
write_csv(as_tibble(monthly) |> mutate(month = as.character(month)),
          "data/processed/ae_type1_monthly.csv")

# First look -----------------------------------------------------------------------
dir_create("outputs/figures")
p <- monthly |>
  ggplot(aes(as.Date(month), att_per_day)) +
  annotate("rect", xmin = as.Date("2020-04-01"), xmax = as.Date("2021-06-30"),
           ymin = -Inf, ymax = Inf, alpha = 0.15) +
  geom_line(colour = "#2c6fbb") +
  facet_wrap(~provider, ncol = 1, scales = "free_y") +
  labs(title = "Type 1 A&E attendances per day",
       subtitle = "Shaded: Covid period (Apr 2020 – Jun 2021)",
       x = NULL, y = "Attendances per day",
       caption = "Source: NHS England Monthly A&E Attendances and Emergency Admissions") +
  theme_minimal(base_size = 12)

ggsave("outputs/figures/01_type1_per_day.png", p, width = 9, height = 6, dpi = 150)
message("Clean data saved to data/processed/. Chart saved to outputs/figures/.")
