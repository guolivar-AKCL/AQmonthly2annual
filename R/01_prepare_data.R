# ============================================================================
# 01 - Prepare monthly, annual and rolling means
#
# Reads the raw hourly air quality data, resolves duplicate timestamps, applies
# the data completeness rules and writes the tidy means the rest of the
# analysis works from, for the pollutants and period set in R/00_config.R.
#
# The rules themselves live in R/functions.R so that R/04_method_checks.R can
# re-run them under different settings.
#
# Outputs: output/monthly_means.csv, output/annual_means.csv,
#          output/rolling_means.csv, output/prep_summary.csv
# ============================================================================

source("R/00_config.R")
source("R/functions.R")

library(dplyr)
library(lubridate)

# ============================================================================
# Hourly data
#
# Duplicate site/pollutant/hour records (two instruments running in parallel)
# are averaged, and the synthetic network-wide series is added, so that it
# behaves like a single extra monitoring site in everything that follows.
# ============================================================================

hourly <- prepare_hourly(
  RAW_DATA,
  metrics = METRICS,
  start_year = START_YEAR,
  end_year = END_YEAR,
  city_site = CITY_SITE
)

# Counts of what the duplicate rule touched, for the methods document
prep_summary <- data.frame(
  n_hourly_raw = attr(hourly, "n_hourly_raw"),
  n_duplicated = attr(hourly, "n_duplicated"),
  min_month_completeness = MIN_MONTH_COMPLETENESS,
  start_year = START_YEAR,
  end_year = END_YEAR
)

# ============================================================================
# Monthly means and completeness
# ============================================================================

monthly <- monthly_from_hourly(hourly, MIN_MONTH_COMPLETENESS)

# ============================================================================
# Calendar-year means
#
# The annual mean is the mean of the twelve monthly means rather than the mean
# of all hours, so that months with more data do not pull the annual figure
# towards their own level. A year counts only if all twelve months are
# complete, which keeps the target value comparable across sites and years.
# ============================================================================

annual <- annual_from_monthly(monthly, weighting = "month")

# ============================================================================
# Rolling twelve-month means
#
# The second target of the analysis. A rolling year is the twelve months
# beginning with a given month, so a campaign that crosses the end of a
# calendar year still has a twelve-month period it sits inside. The same
# completeness rule applies: all twelve months must be complete.
# ============================================================================

rolling <- consecutive_means(
  monthly |> filter(complete), 12,
  value_name = "rolling_mean"
) |>
  filter(start_year >= START_YEAR, start_year <= END_YEAR)

# ============================================================================
# Save
# ============================================================================

write.csv(monthly, MONTHLY_MEANS, row.names = FALSE)
write.csv(annual, ANNUAL_MEANS, row.names = FALSE)
write.csv(rolling, ROLLING_MEANS, row.names = FALSE)
write.csv(prep_summary, "output/prep_summary.csv", row.names = FALSE)

cat(
  "01_prepare_data: ", nrow(monthly), " monthly means (",
  sum(monthly$complete), " complete), ", nrow(annual),
  " complete site-years, ", nrow(rolling), " rolling years\n",
  sep = ""
)
