# Setup

How to run this project on a new computer. Tested on Windows with R 4.4.2.

## Requirements

- [R](https://cran.r-project.org) 4.4 or later
- [RStudio Desktop](https://posit.co/download/rstudio-desktop/) (includes Quarto, used for the reports)
- [Git](https://git-scm.com/downloads), to clone the repository

## Steps

1. Clone the repository and open `ae-demand-forecast-bnssg.Rproj` in RStudio.
   Opening the project file sets the working folder, so every path in the scripts works.
2. Install the exact package versions recorded in `renv.lock`:
   ```r
   renv::restore()
   ```
   Packages come from the Posit Public Package Manager, which provides ready-made
   Windows packages for older R versions, so no compiler (Rtools) is needed.
3. Run everything:
   ```r
   source("run_all.R")
   ```
   This cleans the data, fits the models, runs the simulation (a few minutes) and
   renders the technical report and briefing note into `report/`.

## Data

The raw NHS England files up to October 2026 are included in `data/raw/`, and the
cut-off is fixed in `R/00_config.R` (`DATA_END`). A rerun therefore uses exactly the
same data and, with fixed random seeds, gives the same results.

To update the analysis with newer months, move `DATA_END` and set `REFRESH_DATA <- TRUE`.
The download script will then fetch new and revised files from NHS England, and
every number will change accordingly.

## First-time package setup without the lockfile

`R/00_setup.R` builds the package library from scratch and writes a new `renv.lock`.
It is only needed if `renv.lock` is missing.
