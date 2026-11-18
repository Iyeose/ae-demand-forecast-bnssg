# Forecasting and simulating A&E demand in BNSSG & Gloucestershire

Forecasts Type 1 (major emergency department) A&E attendances for the acute trusts serving
Bristol, North Somerset & South Gloucestershire (BNSSG) and Gloucestershire, then simulates
what January 2028 demand means for 4-hour performance. Built in R.

**Questions answered**
1. How fast is Type 1 demand growing at each trust, and how does winter compare with summer?
2. How many attendances should the ICBs plan for, month by month, in 2027-28 (with a range)?
3. If January 2028 demand arrives as forecast, how does one extra clinician (10:00-22:00)
   change the share of patients leaving A&E within 4 hours?

The full answers, methods and limitations are in the technical report: `report/report.html`.
A one-page plain-English summary for commissioners is in `report/briefing_note.html` (and `.docx`).

## Rerun everything

```r
renv::restore()        # 1. install the exact package versions
source("run_all.R")    # 2. download data, clean, model, simulate, render the report
```

## Methods in brief

| Part | Method |
|---|---|
| Data | NHS England monthly A&E CSVs, April 2018 onwards, downloaded automatically |
| Growth and seasonality | STL decomposition and log-linear regression, July 2021 onwards (post-Covid) |
| Forecast | Seasonal naive, ETS, ARIMA, regression and regression with ARIMA errors (`fable`), tested on the last 12 months; best model with clean residuals chosen; 2,000 simulated futures for ranges |
| Simulation | Discrete-event model (`simmer`) of Gloucestershire's emergency departments as one combined department, calibrated to January 2026 performance; 100 replications per scenario; +/-20% sensitivity analysis |

## Trusts and organisation codes

| Series | Codes | Notes |
|---|---|---|
| Bristol | RVJ + RA7 + RA3 | North Bristol (RVJ) joined UHBW (RA7) on 1 July 2026, forming Bristol NHS Foundation Trust under code RA7. Weston Area Health (RA3) joined UH Bristol on 1 April 2020. All three are added every month so the series covers the same hospitals throughout. |
| Gloucestershire | RTE | Gloucestershire Hospitals NHS Foundation Trust |

## Structure

```
R/01_download_data.R   download monthly CSVs from NHS England
R/02_clean_data.R      clean Type 1 series per trust
R/03_explore.R         Q1: growth and seasonality
R/04_forecast.R        Q2: model comparison and 2027-28 forecast
R/05_simulate.R        Q3: patient-flow simulation
report/report.qmd      technical report (renders to report.html)
outputs/tables/        results tables (CSV)
outputs/figures/       charts (PNG)
data/processed/        clean data used by the models
data/raw/              downloaded CSVs (not committed; rebuilt by the scripts)
```

## Data source

NHS England, *A&E Attendances and Emergency Admissions*, monthly provider-level data.
