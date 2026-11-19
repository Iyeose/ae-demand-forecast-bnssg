# 00_config.R -------------------------------------------------------------------
# Settings shared by every script. Sourced at the top of each one.
#
# DATA_END freezes the analysis: every script ignores months after it, so a rerun
# gives the same results even after NHS England publishes new or revised months.
# To update the analysis, move DATA_END deliberately and set REFRESH_DATA to TRUE.

DATA_START   <- tsibble::yearmonth("2018 Apr")   # first month published as CSV
DATA_END     <- tsibble::yearmonth("2026 Oct")   # data cut-off for this analysis
REFRESH_DATA <- FALSE                            # TRUE = re-download from NHS England
