# 00_setup.R ------------------------------------------------------------------
# One-off setup: creates a project-specific package library with renv and
# installs everything the project uses. Run once, from the project root.

# Posit Public Package Manager: a CRAN mirror that keeps ready-made Windows
# packages for older R versions, so nothing needs compiling (no Rtools needed).
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Start renv in this project if it isn't already (bare = don't scan code yet)
if (!file.exists("renv.lock")) renv::init(bare = TRUE, restart = FALSE)

pkgs <- c(
  # data handling
  "tidyverse", "janitor", "rvest", "fs", "here",
  # time series and forecasting
  "tsibble", "fable", "feasts", "fabletools", "urca",
  # simulation
  "simmer", "simmer.plot",
  # reporting
  "quarto", "knitr", "rmarkdown", "gt", "scales"
)

renv::install(pkgs)
renv::snapshot(packages = pkgs, prompt = FALSE)

message("Setup complete. renv.lock written.")
