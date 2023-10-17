#' @title dailyAgg
#'
#' @description Aggregates sub daily time series into a daily resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return An aggregated dataset taken to the daily resolution, data are not stored in a `HydroAggs` container.
#' @export
#'
#' @import data.table
#'
#' @examples
#' data(bewdley)
#'
#' # Daily maximum flows
#' dailyAgg(bewdley, method = "max")
dailyAgg <- function(x, method = "mean", ...) {
  UseMethod("dailyAgg", x)
}

#' @rdname dailyAgg
#' @export
dailyAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    dt <- x[, .(dailyMean = mean(value, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  if (method == "median") {
    dt <- x[, .(dailyMedian = median(value, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  if (method == "min") {
    dt <- x[, .(dailyMin = min(value, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  if (method == "max") {
    dt <- x[, .(dailyMax = max(value, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  if (method == "sum") {
    dt <- x[, .(dailySum = sum(volume, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  return(dt)
}

#' @rdname dailyAgg
#' @export
dailyAgg.HydroImport <- function(x, method = "mean", ...) {
  if (method == "mean") {
    dt <- x$data[, .(dailyMean = mean(value, na.rm = TRUE)),
                 .(dateTime = as.Date(dateTime))]
  }
  if (method == "median") {
    dt <- x$data[, .(dailyMedian = median(value, na.rm = TRUE)),
                 .(dateTime = as.Date(dateTime))]
  }
  if (method == "min") {
    dt <- x$data[, .(dailyMin = min(value, na.rm = TRUE)),
                 .(dateTime = as.Date(dateTime))]
  }
  if (method == "max") {
    dt <- x$data[, .(dailyMax = max(value, na.rm = TRUE)),
                 .(dateTime = as.Date(dateTime))]
  }
  if (method == "sum") {
    dt <- x$data[, .(dailySum = sum(volume, na.rm = TRUE)),
                 .(dateTime = as.Date(dateTime))]
  }
  return(dt)
}
