# Forecasting and simulating A&E demand in BNSSG & Gloucestershire

Forecasts Type 1 (major emergency department) A&E attendances for the acute trusts serving
Bristol, North Somerset & South Gloucestershire (BNSSG) and Gloucestershire, then simulates
what January 2028 demand means for 4-hour performance. Built in R.

**Questions answered**
1. How fast is Type 1 demand growing at each trust, and how does winter compare with summer?
2. How many attendances should the ICBs plan for, month by month, in 2027-28 (with a range)?
3. If January 2028 demand arrives as forecast, how does one extra clinician (10:00-22:00)
   change the share of patients leaving A&E within 4 hours?

**Read the results**
- [Technical report](https://iyeose.github.io/ae-demand-forecast-bnssg/report/report.html): methods, model comparison, forecasts, simulation and limitations
- [One-page briefing note](https://iyeose.github.io/ae-demand-forecast-bnssg/report/briefing_note.html): plain-English summary for commissioners ([Word version](report/briefing_note.docx))

## Rerun everything

```r
renv::restore()        # 1. install the exact package versions
source("run_all.R")    # 2. clean, model, simulate, render the reports
```

The raw NHS England files are included in `data/raw/` and the analysis is frozen at
October 2026 (`R/00_config.R`), so a rerun gives the same results. See `SETUP.md`
for details and for how to update to newer months.

## Methods in brief

| Part | Method |
|---|---|
| Data | NHS England monthly A&E CSVs, April 2018 onwards, downloaded automatically |
| Growth and seasonality | STL decomposition and log-linear regression, July 2021 onwards (post-Covid) |
| Forecast | Seasonal naive, ETS, ARIMA, regression and regression with ARIMA errors (`fable`), compared by rolling-origin cross-validation (17 back-tests, 12 months ahead); best model with clean residuals chosen; ranges widened to match back-test errors |
| Simulation | Discrete-event model (`simmer`) of Gloucestershire's Type 1 emergency departments as one combined department; bed waits from published decision-to-admit data; clinician time calibrated to January 2026 Type 1 performance; 100 simulated Januaries per scenario; +/-20% sensitivity analysis |

## Trusts and organisation codes

| Series | Codes | Notes |
|---|---|---|
| Bristol | RVJ + RA7 + RA3 | North Bristol (RVJ) joined UHBW (RA7) on 1 July 2026, forming Bristol NHS Foundation Trust under code RA7. Weston Area Health (RA3) joined UH Bristol on 1 April 2020. All three are added every month so the series covers the same hospitals throughout. |
| Gloucestershire | RTE | Gloucestershire Hospitals NHS Foundation Trust |

## Structure

```
R/00_config.R          data cut-off (October 2026) shared by all scripts
R/01_download_data.R   download monthly CSVs from NHS England (skipped when frozen)
R/02_clean_data.R      clean Type 1 series per trust
R/03_explore.R         Q1: growth and seasonality
R/04_forecast.R        Q2: model comparison and 2027-28 forecast
R/05_simulate.R        Q3: patient-flow simulation
report/report.qmd      technical report (renders to report.html)
outputs/tables/        results tables (CSV)
outputs/figures/       charts (PNG)
data/processed/        clean data used by the models
data/raw/              NHS England monthly CSVs, April 2018 to October 2026 (frozen)
```

## Data source

NHS England, *A&E Attendances and Emergency Admissions*, monthly provider-level data.
