# ============================================================================
# Sampling Analysis: Find optimal sampling months for annual estimates
#
# This script evaluates different month combinations to identify which sampling
# periods best estimate the true annual average, using linear regression
# correction and multiple error metrics (RMSE, MAE, % MAE).
# ============================================================================

source("./R/functions.R")

# Load required packages
required_packages <- c("dplyr", "lubridate", "purrr")
invisible(lapply(required_packages, load_or_install))

# Ensure lubridate functions are available
library(dplyr)
library(lubridate)

# Load pre-processed air quality data
load("./data/aq_data_SOE2025.RData")

# Configuration
METRIC <- "pm2.5"
START_YEAR <- 2015
MAX_MONTHS <- 4 # Maximum consecutive months to test

# ============================================================================
# Data Preparation: Create city aggregate
# ============================================================================

# Create city-wide average by aggregating across all sites
auckland_avg <- all.data.long |>
  filter(Parameter == METRIC) |>
  group_by(date, Parameter) |>
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
  mutate(site = "auckland")

# Add aggregated data to main dataset
all.data.long <- bind_rows(all.data.long, auckland_avg)

# ============================================================================
# Analysis Function: Evaluate month combinations for a single site
# ============================================================================

analyze_site_sampling <- function(site_data, site_name, max_months) {
  # Early exit if no data
  if (nrow(site_data) == 0) {
    warning(paste("No data for site:", site_name))
    return(data.frame())
  }

  # Calculate monthly and annual averages once
  monthly_stats <- site_data |>
    mutate(month = month(date), year = year(date)) |>
    group_by(year, month) |>
    summarise(
      monthly_avg = mean(Value, na.rm = TRUE),
      n_obs = n(),
      .groups = "drop"
    )

  annual_avg <- site_data |>
    mutate(year = year(date)) |>
    group_by(year) |>
    summarise(
      annual_avg = mean(Value, na.rm = TRUE),
      .groups = "drop"
    )

  # Generate all consecutive month combinations (1 to max_months)
  month_combos <- lapply(1:max_months, function(n_months) {
    lapply(1:(13 - n_months), function(start_month) {
      start_month:(start_month + n_months - 1)
    })
  }) |> unlist(recursive = FALSE)

  # Evaluate each month combination
  results <- map_df(month_combos, function(months) {
    n_months <- length(months)

    # Estimate annual average from selected months
    est_data <- monthly_stats |>
      filter(month %in% months) |>
      group_by(year) |>
      summarise(est_annual_avg = mean(monthly_avg, na.rm = TRUE), .groups = "drop") |>
      inner_join(annual_avg, by = "year")

    # Skip if insufficient data
    if (nrow(est_data) < 2) {
      return(data.frame())
    }

    # Fit linear regression for bias correction
    tryCatch(
      {
        lm_model <- lm(annual_avg ~ est_annual_avg, data = est_data)
        r2 <- summary(lm_model)$r.squared

        # Skip if r-squared is NA
        if (is.na(r2)) {
          return(data.frame())
        }

        # Calculate residuals for error metrics
        residuals <- lm_model$residuals
        rmse <- sqrt(mean(residuals^2))
        mae <- mean(abs(residuals))
        pct_mae <- mean(abs(residuals) / est_data$annual_avg * 100)

        data.frame(
          site = site_name,
          months = paste(months, collapse = "-"),
          n_months = n_months,
          r_squared = r2,
          rmse = rmse,
          mae = mae,
          pct_mae = pct_mae,
          stringsAsFactors = FALSE
        )
      },
      error = function(e) data.frame() # Return empty df on error
    )
  })

  return(results)
}

# ============================================================================
# Main Analysis Loop: Process all sites
# ============================================================================

sites <- unique(all.data.long$site)
results_list <- list()

for (site_name in sites) {
  # Filter to metric, time period, and current site; compute month/year once
  site_data <- all.data.long |>
    filter(
      site == site_name,
      year(date) >= START_YEAR,
      Parameter == METRIC
    ) |>
    select(date, Value) # Keep only needed columns for memory efficiency

  # Run analysis for this site
  results_list[[site_name]] <- analyze_site_sampling(site_data, site_name, MAX_MONTHS)
}

# ============================================================================
# Combine results and save
# ============================================================================

results_df <- bind_rows(results_list) |>
  filter(nrow(results_list[[1]]) > 0) # Remove empty results

write.csv(
  results_df,
  paste0("./output/pm2.5_sampling_analysis_results_starting_", START_YEAR, ".csv"),
  row.names = FALSE
)

cat("Analysis complete. Results saved to output/\n")
