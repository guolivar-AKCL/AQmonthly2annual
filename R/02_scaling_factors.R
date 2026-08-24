# ============================================================================
# 02 - Fit monthly-to-annual scaling factors
#
# For every pollutant, site and consecutive month window, fits the factor that
# converts the mean of the sampled months into the annual mean, and measures
# how well it does so on years it was not fitted on.
#
# Outputs: output/scaling_factors.csv
# ============================================================================

source("R/00_config.R")
source("R/functions.R")

library(dplyr)
library(purrr)

monthly <- read.csv(MONTHLY_MEANS)
annual <- read.csv(ANNUAL_MEANS)

windows <- month_windows(MAX_WINDOW)

# ============================================================================
# Window means
#
# For each window, average the monthly means it covers. Only complete months
# count, and the window is dropped for that year if any of its months is
# incomplete, so a factor is never fitted on a partly-missing window.
# ============================================================================

window_means <- map_df(names(windows), function(label) {
  months <- windows[[label]]

  monthly |>
    filter(month %in% months, complete) |>
    group_by(Parameter, site, year) |>
    summarise(sampled_mean = mean(monthly_mean), n_used = n(), .groups = "drop") |>
    filter(n_used == length(months)) |>
    mutate(window = label, n_months = length(months)) |>
    select(Parameter, site, year, window, n_months, sampled_mean)
})

# Keep only years that also have a usable annual mean to compare against
paired <- window_means |>
  inner_join(annual, by = c("Parameter", "site", "year"))

write.csv(paired, "output/window_year_pairs.csv", row.names = FALSE)

# ============================================================================
# Fit one factor per pollutant, site and window
# ============================================================================

fit_group <- function(group_data) {
  bind_cols(
    data.frame(
      n_years = nrow(group_data),
      first_year = min(group_data$year),
      last_year = max(group_data$year)
    ),
    fit_scaling_factor(group_data$sampled_mean, group_data$annual_mean),
    fit_linear_model(group_data$sampled_mean, group_data$annual_mean)
  )
}

site_factors <- paired |>
  group_by(Parameter, site, window, n_months) |>
  filter(n() >= MIN_YEARS) |>
  group_modify(~ fit_group(.x)) |>
  ungroup()

# ============================================================================
# Pooled factors
#
# Fitting across all site-years at once gives a single factor per pollutant and
# window that does not depend on a particular site. This is the factor to reach
# for at a location with no long monitoring record of its own.
# ============================================================================

pooled_factors <- paired |>
  filter(site != CITY_SITE) |>
  group_by(Parameter, window, n_months) |>
  filter(n() >= MIN_YEARS) |>
  group_modify(~ bind_cols(fit_group(.x), loso_error(.x))) |>
  ungroup() |>
  mutate(site = "pooled")

# ============================================================================
# Combine, rank and save
# ============================================================================

# Rank windows within each pollutant and site by out-of-sample error, so rank 1
# is the window to sample if you have a free choice.
factors <- bind_rows(site_factors, pooled_factors) |>
  group_by(Parameter, site) |>
  mutate(rank_loo = rank(loo_pct_mae, ties.method = "min")) |>
  ungroup() |>
  arrange(Parameter, site, n_months, window) |>
  select(
    metric = Parameter, site, window, n_months, n_years, first_year, last_year,
    factor, factor_se, factor_lwr, factor_upr,
    rmse, pct_mae, loo_rmse, loo_pct_mae, loo_pct_max, rank_loo,
    loso_pct_mae, loso_pct_max, loso_n_sites,
    lm_intercept, lm_slope, lm_r_squared, lm_loo_pct_mae
  )

write.csv(factors, SCALING_FACTORS, row.names = FALSE)

cat(
  "02_scaling_factors: ", nrow(factors), " factors fitted (",
  nrow(pooled_factors), " pooled)\n",
  sep = ""
)
