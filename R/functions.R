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
