# Function definitions
# analysis-template/R/
# Matt Dray

# Use this file for storing functions to be used in the script files


# ============================================================================
# Get timeseries from KiWIS
# Example usage:
# get_timeseries_data(c("WDR", "WSP", "TEMP"), station_parameters, data_from = "2024-01-01", data_to = "2024-10-01")

get_timeseries_data <- function(
  metrics = c("PM10", "PM2.5"), # vector of parameter names to INCLUDE (grepl pattern)
  exclude = c("RAW"), # vector of parameter names to EXCLUDE (grepl pattern)
  stations = NULL, # vector of station names to include (optional)
  stations_parameters = data.frame(ts_name = "this-is-a/sample"),
  data_from = "2024-01-01",
  data_to = "2024-09-18"
) {
  # Libraries
  library(rjson)
  library(curl)
  library(dplyr)


  # Tidy the time series names
  stations_parameters$ts_name <- sub(".*/", "", stations_parameters$ts_name)

  # Subset the stations parameters if a set of stations were given only request data for those
  if (!is.null(stations)) {
    stations_parameters <- subset(stations_parameters, station_name %in% stations)
  }

  # Validate metrics and exclude arguments
  if (length(metrics) == 0 || all(is.na(metrics))) {
    stop("At least one metric must be specified.")
  }
  if (!is.null(exclude) && length(exclude) > 0 && any(is.na(exclude))) {
    exclude <- exclude[!is.na(exclude)]
  }

  # Base URL for calls
  base_url <- "http://aklc.hydrotel.co.nz:8080/KiWIS/KiWIS?service=kisters&type=queryServices&datasource=1&"


  # Choose which parameter(s) to fetch
  param_include <- grepl(paste0(metrics, collapse = "|"), stations_parameters$ts_name)
  if (!is.null(exclude) && length(exclude) > 0) {
    param_exclude <- grepl(paste0(exclude, collapse = "|"), stations_parameters$ts_name)
  } else {
    param_exclude <- rep(FALSE, nrow(stations_parameters))
  }
  ts_to_choose <- unique(stations_parameters$ts_id[param_include & !param_exclude])

  if (length(ts_to_choose) == 0) {
    stop("No time series match the specified metrics and exclude filters.")
  }

  # Split ts_to_choose into batches of 20
  ts_batches <- split(ts_to_choose, ceiling(seq_along(ts_to_choose) / 20))
  all_timeseries <- list()

  for (batch in ts_batches) {
    get_timeseries <- paste0(
      base_url,
      "request=getTimeseriesValues&",
      "ts_id=", paste0(batch, collapse = ","), "&",
      "from=", data_from, "&",
      "to=", data_to, "&",
      "format=dajson"
    )
    req1 <- curl_fetch_memory(get_timeseries)
    timeseries_list <- fromJSON(rawToChar(req1$content))
    all_timeseries <- c(all_timeseries, timeseries_list)
  }

  # Process the timeseries data
  time_series <- bind_rows(lapply(all_timeseries, function(ts) {
    if (length(ts$data) > 0) {
      ts_info <- stations_parameters %>% filter(ts_id == ts$ts_id)
      data.frame(
        Timestamp = sapply(ts$data, `[[`, 1),
        ts_name = sub(".*/", "", ts_info$ts_name),
        station_name = ts_info$station_name,
        Value = sapply(ts$data, function(x) ifelse(is.null(x[[2]]), NA, x[[2]]))
      )
    } else {
      NULL
    }
  }))

  return(time_series)
}

# ============================================================================
# Package installation helper function
load_or_install <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    message(paste("Package", pkg, "not found. Attempting to install..."))
    tryCatch(
      {
        install.packages(pkg, dependencies = TRUE)
        library(pkg, character.only = TRUE)
      },
      error = function(e) {
        stop(paste("Failed to install package:", pkg))
      }
    )
  }
}
# ============================================================================

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

  data.frame(
    factor = factor,
    factor_se = se,
    factor_lwr = factor - 1.96 * se,
    factor_upr = factor + 1.96 * se,
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
