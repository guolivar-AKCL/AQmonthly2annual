# ============================================================================
# 01 - Prepare monthly and annual means
#
# Reads the raw hourly air quality data, resolves duplicate timestamps, applies
# the data completeness rules and writes tidy monthly and annual means for the
# pollutants and period set in R/00_config.R.
#
# Outputs: output/monthly_means.csv, output/annual_means.csv,
#          output/rolling_means.csv
# ============================================================================

source("R/00_config.R")
source("R/functions.R")

library(dplyr)
library(lubridate)

load(RAW_DATA) # provides all.data.long

# ============================================================================
# Subset to the pollutants and period of interest
# ============================================================================

hourly <- all.data.long |>
  filter(
    Parameter %in% METRICS,
    !is.na(Value),
    year(date) >= START_YEAR,
    year(date) <= END_YEAR
  ) |>
  select(Parameter, site, date, Value)

# Some site-pollutant combinations have two instruments reporting the same
# hour (for example NO2 at queen_st in 2016). Average them so a single hour
# contributes once to the monthly mean.
n_duplicated <- sum(duplicated(hourly[c("Parameter", "site", "date")]))
if (n_duplicated > 0) {
  message(
    "Averaging ", n_duplicated, " duplicated site/pollutant/hour records"
  )
  hourly <- hourly |>
    group_by(Parameter, site, date) |>
    summarise(Value = mean(Value), .groups = "drop")
}

# ============================================================================
# Add the synthetic network-wide series
# ============================================================================

# The city series is the mean across all sites reporting a given hour, so it
# behaves like a single extra monitoring site in everything that follows.
city <- hourly |>
  group_by(Parameter, date) |>
  summarise(Value = mean(Value), .groups = "drop") |>
  mutate(site = CITY_SITE)

hourly <- bind_rows(hourly, city)

# ============================================================================
# Monthly means and completeness
# ============================================================================

monthly <- hourly |>
  mutate(year = year(date), month = month(date)) |>
  group_by(Parameter, site, year, month) |>
  summarise(monthly_mean = mean(Value), n_hours = n(), .groups = "drop") |>
  mutate(
    # Hours in the month, ignoring the one-hour daylight saving shifts
    possible_hours = days_in_month(make_date(year, month, 1)) * 24,
    completeness = n_hours / possible_hours,
    complete = completeness >= MIN_MONTH_COMPLETENESS,
    # Months numbered continuously from year 0, so that runs of months can
    # cross the end of a calendar year
    month_index = year * 12 + month - 1
  )

# ============================================================================
# Annual means
# ============================================================================

# The annual mean is the mean of the twelve monthly means rather than the mean
# of all hours, so that months with more data do not pull the annual figure
# towards their own level. A year counts only if all twelve months are
# complete, which keeps the target value comparable across sites and years.
annual <- monthly |>
  filter(complete) |>
  group_by(Parameter, site, year) |>
  summarise(
    annual_mean = mean(monthly_mean),
    n_months = n(),
    .groups = "drop"
  ) |>
  filter(n_months == 12) |>
  select(-n_months)

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

cat(
  "01_prepare_data: ", nrow(monthly), " monthly means (",
  sum(monthly$complete), " complete), ", nrow(annual),
  " complete site-years, ", nrow(rolling), " rolling years\n",
  sep = ""
)
