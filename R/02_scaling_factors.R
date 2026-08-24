# ============================================================================
# 02 - Fit monthly-to-annual scaling factors
#
# For every pollutant, site and window of consecutive months, fits the factor
# that converts the mean of the sampled months into the annual mean, and
# measures how well it does so on years it was not fitted on.
#
# Two targets are fitted:
#   calendar - the mean of the calendar year the window sits in. Windows are
#              confined to a single calendar year.
#   rolling  - the mean of the twelve months beginning with the window's first
#              month. Windows may cross the end of a year, which is what a
#              summer campaign does.
#
# Outputs: output/scaling_factors.csv, output/window_year_pairs.csv
# ============================================================================

source("R/00_config.R")
source("R/functions.R")

library(dplyr)
library(purrr)

monthly <- read.csv(MONTHLY_MEANS)
annual <- read.csv(ANNUAL_MEANS)
rolling <- read.csv(ROLLING_MEANS)

monthly_complete <- monthly |> filter(complete)
windows <- month_windows(MAX_WINDOW)

# ============================================================================
# Calendar-year target
#
# For each window, average the monthly means it covers. Only complete months
# count, and the window is dropped for that year if any of its months is
# incomplete, so a factor is never fitted on a partly-missing window.
# ============================================================================

calendar_pairs <- calendar_window_pairs(monthly_complete, annual, windows) |>
  mutate(target = "calendar", wraps = FALSE)

# ============================================================================
# Rolling twelve-month target
#
# The window is n consecutive months starting at any month of the year, and
# the target is the twelve months starting with the same month. Windows that
# run past December continue into the following year.
# ============================================================================

rolling_pairs <- map_df(seq_len(MAX_WINDOW), function(n) {
  consecutive_means(monthly_complete, n, value_name = "sampled_mean") |>
    inner_join(
      rolling |> select(Parameter, site, start_index, rolling_mean),
      by = c("Parameter", "site", "start_index")
    ) |>
    mutate(
      n_months = n,
      window = vapply(start_month, function(m) {
        window_label(wrap_months(m, n))
      }, character(1)),
      # A window wraps when its last month falls in the next calendar year
      wraps = start_month + n - 1 > 12
    )
}) |>
  transmute(
    Parameter, site, year = start_year, window, n_months,
    target = "rolling", wraps,
    sampled_mean, target_mean = rolling_mean
  )

paired <- bind_rows(calendar_pairs, rolling_pairs)

write.csv(paired, "output/window_year_pairs.csv", row.names = FALSE)

# ============================================================================
# Fit one factor per pollutant, site, window and target
# ============================================================================

fit_group <- function(group_data) {
  bind_cols(
    data.frame(
      n_years = nrow(group_data),
      first_year = min(group_data$year),
      last_year = max(group_data$year)
    ),
    fit_scaling_factor(group_data$sampled_mean, group_data$target_mean),
    fit_linear_model(group_data$sampled_mean, group_data$target_mean)
  )
}

site_factors <- paired |>
  group_by(Parameter, target, site, window, n_months, wraps) |>
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
  group_by(Parameter, target, window, n_months, wraps) |>
  filter(n() >= MIN_YEARS) |>
  group_modify(~ bind_cols(fit_group(.x), loso_error(.x))) |>
  ungroup() |>
  mutate(site = "pooled")

# ============================================================================
# Combine, rank and save
# ============================================================================

# Rank windows within each pollutant, site and target by out-of-sample error,
# so rank 1 is the window to sample if you have a free choice.
factors <- bind_rows(site_factors, pooled_factors) |>
  group_by(Parameter, target, site) |>
  mutate(rank_loo = rank(loo_pct_mae, ties.method = "min")) |>
  ungroup() |>
  arrange(Parameter, target, site, n_months, window) |>
  select(
    metric = Parameter, target, site, window, n_months, wraps,
    n_years, first_year, last_year,
    factor, factor_se, factor_lwr, factor_upr,
    ratio_factor, ratio_factor_se, ratio_loo_pct_mae,
    rmse, pct_mae, loo_rmse, loo_pct_mae, loo_pct_max, rank_loo,
    loso_pct_mae, loso_pct_max, loso_n_sites,
    lm_intercept, lm_slope, lm_r_squared, lm_loo_pct_mae
  )

write.csv(factors, SCALING_FACTORS, row.names = FALSE)

cat(
  "02_scaling_factors: ", nrow(factors), " factors fitted (",
  sum(factors$target == "rolling"), " against a rolling year, ",
  sum(factors$wraps), " crossing the year end)\n",
  sep = ""
)
