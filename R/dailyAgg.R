#' @title dailyAgg
#'
#' @description Aggregates sub daily time series into a daily resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return Data are aggregated to a daily resolution
#' @export
#'
#' @import data.table
#'
#' @examples
#' ## Do not run
#' # dailyAgg(Buildwas)
dailyAgg <- function(x, method = "mean", ...) {
  UseMethod("dailyAgg", x)
}

#' @rdname dailyAgg
#' @export
dailyAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Daily <- x[, .(dailyMean = mean(value, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  if (method == "median") {
    Daily <- x[, .(dailyMedian = median(value, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  if (method == "min") {
    Daily <- x[, .(dailyMin = min(value, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  if (method == "max") {
    Daily <- x[, .(dailyMax = max(value, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  if (method == "sum") {
    Daily <- x[, .(dailySum = sum(volume, na.rm = TRUE)),
               .(dateTime = as.Date(dateTime))]
  }
  return(Daily)
}
