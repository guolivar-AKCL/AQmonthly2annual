# ============================================================================
# 04 - Method checks: sensitivity, estimator comparison and residuals
#
# Everything a reviewer needs to see that the factors in output/ are not an
# artefact of the choices made to produce them. Three checks:
#
#   sensitivity  - refit the pooled factors under different completeness
#                  thresholds, periods and annual-mean definitions
#   estimators   - compare the through-origin factor against three
#                  alternatives on identical data
#   residuals    - per-year residuals and diagnostics for the fitted factors
#
# Reads the raw data directly, because most of the checks change a rule that
# 01_prepare_data.R applies. Slower than the other scripts; a few minutes.
#
# Outputs: output/sensitivity.csv, output/estimator_comparison.csv,
#          output/residuals.csv, output/residual_diagnostics.csv
# ============================================================================

source("R/00_config.R")
source("R/functions.R")

library(dplyr)
library(purrr)

windows <- month_windows(MAX_WINDOW)

# Hourly data is reused across variants, so load each period once
hourly_base <- prepare_hourly(
  RAW_DATA, METRICS, START_YEAR, END_YEAR, CITY_SITE
)
LONG_START <- 2006 # earliest year with most of the network reporting
hourly_long <- prepare_hourly(
  RAW_DATA, METRICS, LONG_START, END_YEAR, CITY_SITE
)

#' Pooled factors for one set of settings, as a comparable table
run_variant <- function(hourly, min_completeness, weighting = "month",
                        years = NULL, label, group) {
  monthly <- monthly_from_hourly(hourly, min_completeness)
  if (!is.null(years)) {
    monthly <- monthly |> filter(year %in% years)
  }
  annual <- annual_from_monthly(monthly, weighting = weighting)

  calendar_window_pairs(monthly |> filter(complete), annual, windows) |>
    fit_pooled_factors(MIN_YEARS, exclude_site = CITY_SITE) |>
    transmute(
      check = group, variant = label,
      metric = Parameter, window, n_months, n_years, factor, loo_pct_mae
    )
}

# ============================================================================
# Sensitivity
# ============================================================================

sensitivity <- bind_rows(
  run_variant(hourly_base, MIN_MONTH_COMPLETENESS,
    label = "base", group = "reference"
  ),
  run_variant(hourly_base, 0.50,
    label = "completeness 50%", group = "completeness"
  ),
  run_variant(hourly_base, 0.90,
    label = "completeness 90%", group = "completeness"
  ),
  run_variant(hourly_base, MIN_MONTH_COMPLETENESS,
    weighting = "hour", label = "hour-weighted annual mean", group = "target"
  ),
  run_variant(hourly_long, MIN_MONTH_COMPLETENESS,
    label = paste0("period ", LONG_START, "-", END_YEAR), group = "period"
  ),
  run_variant(hourly_base, MIN_MONTH_COMPLETENESS,
    years = START_YEAR:(START_YEAR + 4),
    label = paste0("period ", START_YEAR, "-", START_YEAR + 4), group = "period"
  ),
  run_variant(hourly_base, MIN_MONTH_COMPLETENESS,
    years = (END_YEAR - 4):END_YEAR,
    label = paste0("period ", END_YEAR - 4, "-", END_YEAR), group = "period"
  )
)

# Express every variant as a shift from the reference fit
reference <- sensitivity |>
  filter(variant == "base") |>
  select(metric, window, base_factor = factor, base_loo = loo_pct_mae)

sensitivity <- sensitivity |>
  left_join(reference, by = c("metric", "window")) |>
  mutate(
    factor_shift = factor - base_factor,
    pct_factor_shift = factor_shift / base_factor * 100,
    loo_shift = loo_pct_mae - base_loo
  )

write.csv(sensitivity, "output/sensitivity.csv", row.names = FALSE)

# ============================================================================
# Estimator comparison
#
# Four ways of turning the same site-years into a scaling factor, each scored
# by the same leave-one-out procedure so the comparison is like for like.
# ============================================================================

base_pairs <- {
  monthly <- monthly_from_hourly(hourly_base, MIN_MONTH_COMPLETENESS)
  annual <- annual_from_monthly(monthly, weighting = "month")
  calendar_window_pairs(monthly |> filter(complete), annual, windows) |>
    filter(site != CITY_SITE)
}

compare_estimators <- function(group_data) {
  x <- group_data$sampled_mean
  y <- group_data$target_mean
  n <- length(x)

  # Each estimator as a function of a subset, so leave-one-out is one code path
  estimators <- list(
    "through-origin OLS" = function(i) sum(x[i] * y[i]) / sum(x[i]^2),
    "ratio of means" = function(i) sum(y[i]) / sum(x[i]),
    "mean of ratios" = function(i) mean(y[i] / x[i])
  )

  factor_rows <- imap_dfr(estimators, function(f, name) {
    loo_pred <- vapply(seq_len(n), function(k) f(-k) * x[k], numeric(1))
    data.frame(
      estimator = name,
      factor = f(seq_len(n)),
      loo_pct_mae = mean(abs(loo_pred - y) / y) * 100,
      stringsAsFactors = FALSE
    )
  })

  # The two-parameter alternative does not reduce to a single factor
  model <- lm(y ~ x)
  loo_resid <- residuals(model) / (1 - hatvalues(model))

  bind_rows(
    factor_rows,
    data.frame(
      estimator = "OLS with intercept",
      factor = NA_real_,
      loo_pct_mae = mean(abs(loo_resid) / y) * 100,
      stringsAsFactors = FALSE
    )
  ) |>
    mutate(n_years = n)
}

estimator_comparison <- base_pairs |>
  group_by(Parameter, window, n_months) |>
  filter(n() >= MIN_YEARS) |>
  group_modify(~ compare_estimators(.x)) |>
  ungroup() |>
  rename(metric = Parameter)

write.csv(estimator_comparison, "output/estimator_comparison.csv", row.names = FALSE)

# ============================================================================
# Residuals and diagnostics
#
# One row per site-year for the pooled fits, then the standard checks on the
# assumptions behind the standard error: constant spread, no year-to-year
# dependence, roughly symmetric errors.
# ============================================================================

residuals_by_year <- base_pairs |>
  group_by(Parameter, window, n_months) |>
  filter(n() >= MIN_YEARS) |>
  mutate(
    factor = sum(sampled_mean * target_mean) / sum(sampled_mean^2),
    fitted = factor * sampled_mean,
    residual = target_mean - fitted,
    rel_residual = residual / target_mean * 100
  ) |>
  ungroup() |>
  rename(metric = Parameter)

write.csv(
  residuals_by_year |>
    select(metric, site, year, window, n_months, sampled_mean, target_mean,
           factor, fitted, residual, rel_residual),
  "output/residuals.csv",
  row.names = FALSE
)

diagnostics <- residuals_by_year |>
  group_by(metric, window, n_months) |>
  summarise(
    n_obs = n(),
    # Spread of the residuals against the fitted level: a positive correlation
    # means the absolute error grows with concentration, which is the case the
    # multiplicative form is meant to handle
    scale_corr = cor(abs(residual), fitted, method = "spearman"),
    rel_scale_corr = cor(abs(rel_residual), fitted, method = "spearman"),
    # Symmetry and tails
    mean_rel = mean(rel_residual),
    skewness = mean(((residual - mean(residual)) / sd(residual))^3),
    shapiro_p = if (n() >= 3 && n() <= 5000) shapiro.test(residual)$p.value else NA_real_,
    .groups = "drop"
  )

# Year-to-year dependence, within a site: successive years of the same site
# sharing an error would inflate the effective sample size
autocorrelation <- residuals_by_year |>
  arrange(metric, window, site, year) |>
  group_by(metric, window, n_months, site) |>
  filter(n() >= 4) |>
  summarise(
    lag1 = cor(residual[-n()], residual[-1]),
    .groups = "drop"
  ) |>
  group_by(metric, window, n_months) |>
  summarise(
    mean_lag1 = mean(lag1, na.rm = TRUE),
    n_sites_lag1 = sum(!is.na(lag1)),
    .groups = "drop"
  )

diagnostics <- diagnostics |>
  left_join(autocorrelation, by = c("metric", "window", "n_months"))

write.csv(diagnostics, "output/residual_diagnostics.csv", row.names = FALSE)

cat(
  "04_method_checks: ", nrow(sensitivity), " sensitivity rows, ",
  nrow(estimator_comparison), " estimator rows, ",
  nrow(residuals_by_year), " residuals\n",
  sep = ""
)
