# ============================================================================
# Monthly-to-annual scaling factor helpers
# ============================================================================

#' Enumerate consecutive month windows
#'
#' @param max_window Longest window to generate, in months.
#' @return A named list; each element is an integer vector of month numbers and
#'   the name is the window label (e.g. "May-Aug").
month_windows <- function(max_window = 6) {
  windows <- list()
  for (n in seq_len(max_window)) {
    for (start in seq_len(13 - n)) {
      months <- start:(start + n - 1)
      windows[[window_label(months)]] <- months
    }
  }
  windows
}

#' The n months starting at a given month, wrapping past December
#'
#' @param start Starting month number.
#' @param n Number of months.
#' @return An integer vector of month numbers, e.g. `wrap_months(11, 4)` is
#'   November, December, January, February.
wrap_months <- function(start, n) {
  (start - 1 + seq_len(n) - 1) %% 12 + 1
}

#' Short human-readable label for a set of months
#'
#' @param months Integer vector of month numbers.
#' @return A single string, e.g. "Jun" or "May-Aug".
window_label <- function(months) {
  abb <- month.abb[months]
  if (length(months) == 1) abb else paste0(abb[1], "-", abb[length(abb)])
}

#' Fit a through-origin scaling factor with leave-one-out error metrics
#'
#' Fits `annual = factor * sampled` (no intercept), which is the form used to
#' scale a campaign result up to an annual mean. Errors are reported both for
#' the fitted model and leave-one-out, the latter being the honest estimate of
#' how the factor performs on a year it was not fitted on.
#'
#' @param sampled Numeric vector of window means.
#' @param annual Numeric vector of matching annual means.
#' @return A one-row data frame of coefficients and error metrics.
fit_scaling_factor <- function(sampled, annual) {
  sxx <- sum(sampled^2)
  sxy <- sum(sampled * annual)
  factor <- sxy / sxx

  fitted <- factor * sampled
  resid <- annual - fitted
  # Standard error of a through-origin slope, n - 1 degrees of freedom
  se <- sqrt(sum(resid^2) / (length(sampled) - 1) / sxx)

  # Leave-one-out: refit the factor without each year in turn
  loo_factor <- (sxy - sampled * annual) / (sxx - sampled^2)
  loo_resid <- annual - loo_factor * sampled

  # The same idea fitted as the mean of the year-by-year ratios. It weights
  # every year equally instead of by concentration, which matters when the
  # error is judged in percentage terms. R/04_method_checks.R compares them.
  ratios <- annual / sampled
  ratio_factor <- mean(ratios)
  loo_ratio <- (sum(ratios) - ratios) / (length(ratios) - 1)
  ratio_loo_resid <- annual - loo_ratio * sampled

  data.frame(
    factor = factor,
    factor_se = se,
    factor_lwr = factor - 1.96 * se,
    factor_upr = factor + 1.96 * se,
    ratio_factor = ratio_factor,
    ratio_factor_se = sd(ratios) / sqrt(length(ratios)),
    ratio_loo_pct_mae = mean(abs(ratio_loo_resid) / annual) * 100,
    rmse = sqrt(mean(resid^2)),
    pct_mae = mean(abs(resid) / annual) * 100,
    loo_rmse = sqrt(mean(loo_resid^2)),
    loo_pct_mae = mean(abs(loo_resid) / annual) * 100,
    loo_pct_max = max(abs(loo_resid) / annual) * 100,
    stringsAsFactors = FALSE
  )
}

#' Fit the two-parameter regression alternative to a scaling factor
#'
#' @param sampled Numeric vector of window means.
#' @param annual Numeric vector of matching annual means.
#' @return A one-row data frame with intercept, slope, R-squared and
#'   leave-one-out percentage error.
fit_linear_model <- function(sampled, annual) {
  model <- lm(annual ~ sampled)
  coefs <- coef(model)
  # Leave-one-out residuals from the hat values, no refitting needed
  h <- stats::hatvalues(model)
  loo_resid <- stats::residuals(model) / (1 - h)

  data.frame(
    lm_intercept = unname(coefs[1]),
    lm_slope = unname(coefs[2]),
    lm_r_squared = summary(model)$r.squared,
    lm_loo_pct_mae = mean(abs(loo_resid) / annual) * 100,
    stringsAsFactors = FALSE
  )
}

#' Leave-one-site-out error for a pooled scaling factor
#'
#' Refits the pooled factor with one site held out and applies it to that
#' site's years, repeated for every site. This is the error to quote when the
#' factor is applied at a location that contributed nothing to fitting it.
#'
#' @param data Data frame with `site`, `sampled_mean` and `target_mean`.
#' @return A one-row data frame with the mean and maximum percentage error and
#'   the number of sites tested.
loso_error <- function(data) {
  sites <- unique(data$site)
  if (length(sites) < 2) {
    return(data.frame(loso_pct_mae = NA_real_, loso_pct_max = NA_real_, loso_n_sites = length(sites)))
  }

  errors <- unlist(lapply(sites, function(s) {
    held_out <- data[data$site == s, ]
    rest <- data[data$site != s, ]
    if (nrow(rest) < 2) return(numeric(0))
    factor <- sum(rest$sampled_mean * rest$target_mean) / sum(rest$sampled_mean^2)
    abs(factor * held_out$sampled_mean - held_out$target_mean) / held_out$target_mean * 100
  }))

  data.frame(
    loso_pct_mae = mean(errors),
    loso_pct_max = max(errors),
    loso_n_sites = length(sites),
    stringsAsFactors = FALSE
  )
}

#' Mean of n consecutive complete months, at every possible start
#'
#' Works on a month index rather than on calendar years, so a run of months is
#' allowed to cross the end of a year. Runs containing an incomplete month are
#' dropped rather than averaged over the gap.
#'
#' @param monthly Data frame of monthly means with `Parameter`, `site`,
#'   `month_index` and `monthly_mean`, already filtered to complete months.
#' @param n Number of consecutive months to average.
#' @param value_name Name to give the resulting column.
#' @return A data frame with one row per pollutant, site and start month that
#'   has all `n` months available.
consecutive_means <- function(monthly, n, value_name = "mean_value") {
  # Offset the series n times and keep only the starts that line up n deep
  runs <- lapply(seq_len(n) - 1, function(offset) {
    monthly |>
      dplyr::mutate(start_index = month_index - offset) |>
      dplyr::select(Parameter, site, start_index, monthly_mean)
  })

  dplyr::bind_rows(runs) |>
    dplyr::group_by(Parameter, site, start_index) |>
    dplyr::filter(dplyr::n() == n) |>
    dplyr::summarise(value = mean(monthly_mean), .groups = "drop") |>
    dplyr::mutate(
      start_year = start_index %/% 12,
      start_month = start_index %% 12 + 1
    ) |>
    stats::setNames(c(
      "Parameter", "site", "start_index", value_name, "start_year", "start_month"
    ))
}

# ============================================================================
# Data preparation
#
# These wrap the rules described in the methods document. They take their
# thresholds as arguments rather than reading the configuration directly, so
# that R/04_method_checks.R can re-run the pipeline under different settings.
# ============================================================================

#' Load the hourly data, resolve duplicates and add the network-average series
#'
#' @param path Path to the .RData file holding `all.data.long`.
#' @param metrics Pollutants to keep.
#' @param start_year,end_year Calendar years to keep, inclusive.
#' @param city_site Name for the synthetic network-average series, or NULL to
#'   leave it out.
#' @return A data frame of `Parameter`, `site`, `date`, `Value`, with one row
#'   per pollutant, site and hour.
prepare_hourly <- function(path, metrics, start_year, end_year, city_site = NULL) {
  env <- new.env()
  load(path, envir = env)
  raw <- get("all.data.long", envir = env)

  hourly <- raw |>
    dplyr::filter(
      Parameter %in% metrics,
      !is.na(Value),
      lubridate::year(date) >= start_year,
      lubridate::year(date) <= end_year
    ) |>
    dplyr::select(Parameter, site, date, Value)

  # Some site-pollutant combinations have two instruments reporting the same
  # hour. Average them so a single hour contributes once to the monthly mean.
  # The counts are recorded as attributes for the methods document to report.
  n_raw <- nrow(hourly)
  n_duplicated <- sum(duplicated(hourly[c("Parameter", "site", "date")]))
  if (n_duplicated > 0) {
    hourly <- hourly |>
      dplyr::group_by(Parameter, site, date) |>
      dplyr::summarise(Value = mean(Value), .groups = "drop")
  }
  attr(hourly, "n_hourly_raw") <- n_raw
  attr(hourly, "n_duplicated") <- n_duplicated

  if (!is.null(city_site)) {
    city <- hourly |>
      dplyr::group_by(Parameter, date) |>
      dplyr::summarise(Value = mean(Value), .groups = "drop") |>
      dplyr::mutate(site = city_site)
    hourly <- dplyr::bind_rows(hourly, city)
  }

  hourly
}

#' Monthly means with a completeness flag
#'
#' @param hourly Output of `prepare_hourly()`.
#' @param min_completeness Fraction of the month's hours needed for the month
#'   to be usable.
#' @return A data frame with one row per pollutant, site, year and month.
monthly_from_hourly <- function(hourly, min_completeness) {
  hourly |>
    dplyr::mutate(year = lubridate::year(date), month = lubridate::month(date)) |>
    dplyr::group_by(Parameter, site, year, month) |>
    dplyr::summarise(
      monthly_mean = mean(Value),
      n_hours = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # Hours in the month, ignoring the one-hour daylight saving shifts
      possible_hours = lubridate::days_in_month(
        lubridate::make_date(year, month, 1)
      ) * 24,
      completeness = n_hours / possible_hours,
      complete = completeness >= min_completeness,
      # Months numbered continuously from year 0, so that runs of months can
      # cross the end of a calendar year
      month_index = year * 12 + month - 1
    )
}

#' Calendar-year means from monthly means
#'
#' @param monthly Output of `monthly_from_hourly()`.
#' @param weighting `"month"` averages the twelve monthly means equally;
#'   `"hour"` weights each month by the hours actually measured in it, which is
#'   equivalent to averaging all hours of the year.
#' @return A data frame with one row per pollutant, site and complete year.
annual_from_monthly <- function(monthly, weighting = c("month", "hour")) {
  weighting <- match.arg(weighting)

  monthly |>
    dplyr::filter(complete) |>
    dplyr::group_by(Parameter, site, year) |>
    dplyr::summarise(
      annual_mean = if (weighting == "month") {
        mean(monthly_mean)
      } else {
        stats::weighted.mean(monthly_mean, n_hours)
      },
      n_months = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(n_months == 12) |>
    dplyr::select(-n_months)
}

#' Pair each calendar-year window mean with its annual mean
#'
#' @param monthly_complete Monthly means, already filtered to complete months.
#' @param annual Calendar-year means from `annual_from_monthly()`.
#' @param windows Named list of month vectors from `month_windows()`.
#' @return A data frame with one row per pollutant, site, year and window.
calendar_window_pairs <- function(monthly_complete, annual, windows) {
  purrr::map_df(names(windows), function(label) {
    months <- windows[[label]]

    monthly_complete |>
      dplyr::filter(month %in% months) |>
      dplyr::group_by(Parameter, site, year) |>
      dplyr::summarise(
        sampled_mean = mean(monthly_mean),
        n_used = dplyr::n(),
        .groups = "drop"
      ) |>
      dplyr::filter(n_used == length(months)) |>
      dplyr::mutate(window = label, n_months = length(months))
  }) |>
    dplyr::inner_join(annual, by = c("Parameter", "site", "year")) |>
    dplyr::transmute(
      Parameter, site, year, window, n_months,
      sampled_mean, target_mean = annual_mean
    )
}

#' Pooled scaling factors for every window
#'
#' A thin wrapper used by the sensitivity analysis, which needs the pooled
#' factors under a range of settings without the site-level detail.
#'
#' @param paired Output of `calendar_window_pairs()`.
#' @param min_years Minimum number of years before a factor is fitted.
#' @param exclude_site Site to leave out of the pool (the network average).
#' @return A data frame of pooled factors and their leave-one-out errors.
fit_pooled_factors <- function(paired, min_years, exclude_site = NULL) {
  if (!is.null(exclude_site)) {
    paired <- paired |> dplyr::filter(site != exclude_site)
  }

  paired |>
    dplyr::group_by(Parameter, window, n_months) |>
    dplyr::filter(dplyr::n() >= min_years) |>
    dplyr::group_modify(~ dplyr::bind_cols(
      data.frame(n_years = nrow(.x)),
      fit_scaling_factor(.x$sampled_mean, .x$target_mean)
    )) |>
    dplyr::ungroup()
}
