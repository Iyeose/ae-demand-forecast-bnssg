# 01_download_data.R ------------------------------------------------------------
# Finds every monthly provider-level A&E CSV on the NHS England yearly pages
# (2015-16 onwards) and downloads them to data/raw/. Skipped when the frozen
# raw files up to DATA_END (R/00_config.R) are already present.
#
# Why scrape rather than hard-code links: NHS England stores each file in an
# upload folder named after its publication date, and revised files get new
# names. Reading the yearly pages always picks up the current (latest) version.

library(tidyverse)
library(rvest)
library(fs)
source("R/00_config.R")

# Frozen data: the raw CSVs up to DATA_END are committed to the repository, so
# by default nothing is downloaded and every rerun uses exactly the same files.
have_frozen_data <- function() {
  index_path <- "data/raw/_file_index.csv"
  if (!file_exists(index_path)) return(FALSE)
  idx <- read_csv(index_path, show_col_types = FALSE)
  m <- tsibble::yearmonth(lubridate::my(idx$month_label))
  need <- seq(DATA_START, DATA_END, by = 1)
  all(need %in% m) && all(file_exists(idx$file[!is.na(m) & m >= DATA_START & m <= DATA_END]))
}

if (!REFRESH_DATA && have_frozen_data()) {
  message("Raw files up to ", DATA_END, " are already in data/raw/ (frozen data). ",
          "Skipping download. Set REFRESH_DATA <- TRUE in R/00_config.R to re-check NHS England.")
} else {

raw_dir <- "data/raw"
dir_create(raw_dir)

base_url <- paste0(
  "https://www.england.nhs.uk/statistics/statistical-work-areas/",
  "ae-waiting-times-and-activity/"
)

# 1. Find the yearly pages ------------------------------------------------------
# Take them from the links on the main page, so older pages with unusual
# web addresses are still found.
main_links <- read_html(base_url) |>
  html_elements("a") |>
  (\(a) tibble(text = html_text2(a), href = html_attr(a, "href")))()

year_pages <- main_links |>
  filter(str_detect(text, regex("^Monthly A&E Attendances and Emergency Admissions \\d{4}-\\d{2}",
                                ignore_case = TRUE))) |>
  mutate(fin_year = str_extract(text, "\\d{4}-\\d{2}"),
         href = xml2::url_absolute(href, base_url)) |>
  filter(fin_year >= "2015-16") |>
  distinct(fin_year, .keep_all = TRUE)

# Safety net: the newest year page may not be linked from the main page yet.
latest <- "2026-27"
if (!latest %in% year_pages$fin_year) {
  year_pages <- bind_rows(year_pages, tibble(
    text = paste("Monthly A&E Attendances and Emergency Admissions", latest),
    href = paste0(base_url, "ae-attendances-and-emergency-admissions-", latest, "/"),
    fin_year = latest
  ))
}

message("Year pages found: ", paste(sort(year_pages$fin_year), collapse = ", "))

# 2. Collect the monthly CSV links from each year page --------------------------
get_csv_links <- function(page_url, fin_year) {
  Sys.sleep(1)  # be polite to the server
  page <- tryCatch(read_html(page_url), error = function(e) NULL)
  if (is.null(page)) {
    warning("Could not read ", page_url)
    return(tibble())
  }
  a <- html_elements(page, "a")
  tibble(text = html_text2(a), href = html_attr(a, "href")) |>
    filter(
      str_detect(href, regex("\\.csv$", ignore_case = TRUE)),
      str_detect(text, regex("Monthly", ignore_case = TRUE)),  # some labels omit A&E (e.g. July 2021)
      !str_detect(text, regex("quarter|time ?series|ECDS", ignore_case = TRUE))
    ) |>
    mutate(href = xml2::url_absolute(href, page_url),
           fin_year = fin_year)
}

csv_links <- map2(year_pages$href, year_pages$fin_year, get_csv_links) |>
  list_rbind() |>
  distinct(href, .keep_all = TRUE) |>
  mutate(
    month_label = str_extract(text, regex("(January|February|March|April|May|June|July|August|September|October|November|December)\\s+\\d{4}",
                                          ignore_case = TRUE)),
    revised     = str_detect(text, regex("revised", ignore_case = TRUE)),
    file        = path(raw_dir, basename(href))
  )

message(nrow(csv_links), " CSV links found.")

# 3. Download anything we don't already have ------------------------------------
to_get <- filter(csv_links, !file_exists(file))
walk2(to_get$href, to_get$file, \(u, f) {
  Sys.sleep(0.5)
  tryCatch(download.file(u, f, mode = "wb", quiet = TRUE),
           error = function(e) warning("Failed: ", u))
})

# Keep a record of where every file came from (useful for the README and for
# handling revisions in the cleaning step)
write_csv(csv_links, "data/raw/_file_index.csv")
message("Done. ", nrow(to_get), " new files downloaded to ", raw_dir)
}  # end of download branch
