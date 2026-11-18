# run_all.R ---------------------------------------------------------------------
# Runs the whole project in order, from raw data to the HTML report.
# First time on a new computer: renv::restore() to install the exact packages.

source("R/01_download_data.R")   # download monthly NHS England A&E CSVs
source("R/02_clean_data.R")      # clean Type 1 series per trust
source("R/03_explore.R")         # Q1: growth and seasonality
source("R/04_forecast.R")        # Q2: model test and 2027-28 forecast
source("R/05_simulate.R")        # Q3: patient-flow simulation (a few minutes)
quarto::quarto_render("report/report.qmd")   # technical report -> report/report.html
quarto::quarto_render("report/briefing_note.qmd", output_format = "all")   # one-page note -> HTML + Word
