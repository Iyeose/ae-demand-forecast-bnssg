# Setting up your computer (one-off, about 30–45 minutes)

Do these in order. Everything is free.

## 1. Install R
- Go to https://cran.r-project.org and choose your system (Windows or macOS).
- Windows: click "base", then "Download R-x.x.x for Windows". Run the installer and accept the defaults.
- macOS: pick the .pkg that matches your chip (Apple silicon "arm64" or Intel "x86_64").

## 2. Install RStudio Desktop
- Go to https://posit.co/download/rstudio-desktop/ and install RStudio.
- RStudio already includes Quarto, so you can render the report without a separate install.
  (Optional: install Quarto on its own from https://quarto.org if you want to render from a terminal.)

## 3. Install Git and create a GitHub account
- Git: https://git-scm.com/downloads (accept the defaults).
- GitHub: sign up at https://github.com if you don't have an account.
- Tell Git who you are. In RStudio, open the **Terminal** tab and run:
  ```
  git config --global user.name "Iyeose Simon"
  git config --global user.email "you@example.com"
  ```

## 4. Open the project
1. Unzip / copy the `ae-demand-forecast-bnssg` folder somewhere sensible (e.g. Documents).
2. In RStudio: File → New Project → Existing Directory → choose that folder.
   This creates `ae-demand-forecast-bnssg.Rproj`. Always open the project through that file.

## 5. Install the packages (first time only)
In the RStudio **Console**, run:
```r
source("R/00_setup.R")
```
This installs `renv`, creates a private package library for this project and installs
everything the project needs. It takes 10–20 minutes the first time. When it finishes it
writes `renv.lock`, the file that lets anyone else reinstall exactly the same versions.

If you see a question like "Do you want to proceed? [Y/n]", type `Y` and press Enter.

## 6. Run the data pipeline
```r
source("R/01_download_data.R")   # downloads the monthly CSVs from NHS England
source("R/02_clean_data.R")      # builds the clean Type 1 series and a first chart
```
Then open `outputs/figures/01_type1_per_day.png` and check the lines look sensible.

## If something goes wrong
Copy the full red error message from the Console and send it to me, along with which
script you were running. Most first-run problems are a package that failed to install or a
column name NHS England changed, and both are quick to fix.
