# ============================================================================
# Shared configuration for the monthly-to-annual scaling factor analysis
#
# Sourced by every numbered script. Change a setting here and re-run the
# pipeline from 01 onwards.
# ============================================================================

# Pollutants to analyse (as named in the `Parameter` column of the raw data)
METRICS <- c("pm2.5", "pm10", "no2")

# Display labels for figures and tables
METRIC_LABELS <- c(
  "pm2.5" = "PM2.5",
  "pm10"  = "PM10",
  "no2"   = "NO2"
)

# Measurement units, for axis labels
METRIC_UNITS <- c(
  "pm2.5" = "µg/m³",
  "pm10"  = "µg/m³",
  "no2"   = "µg/m³"
)

# First and last calendar year of the analysis period. 2015-2024 matches the
# reporting period of TR 2025/26.
START_YEAR <- 2015
END_YEAR <- 2024

# Longest sampling window to evaluate, in consecutive months
MAX_WINDOW <- 6

# Data completeness rules ----------------------------------------------------
# A month is usable if at least this fraction of its possible hours were
# measured; a year is usable only if all 12 of its months are usable.
MIN_MONTH_COMPLETENESS <- 0.75

# A site-window combination needs at least this many usable years before a
# scaling factor is fitted.
MIN_YEARS <- 5

# Name given to the synthetic network-wide series (mean across all sites)
CITY_SITE <- "auckland"

# Paths ----------------------------------------------------------------------
RAW_DATA <- "data/aq_data_SOE2025.RData"
MONTHLY_MEANS <- "output/monthly_means.csv"
ANNUAL_MEANS <- "output/annual_means.csv"
ROLLING_MEANS <- "output/rolling_means.csv"
SCALING_FACTORS <- "output/scaling_factors.csv"
PLOT_DIR <- "output/plots"
