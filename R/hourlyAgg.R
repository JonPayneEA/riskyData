#' @title hourlyAgg
#'
#' @description Aggregates sub hourly time series into an hourly resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return Hourly aggregated data.table
#' @export
#'
#' @import data.table
#'
#' @examples
#' hourlyAgg(Buildwas)
hourlyAgg <- function(x, method = "mean", ...) {
  UseMethod("hourlyAgg", x)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Hourly <- x[, .(hourlyMean = mean(value, na.rm = TRUE)),
                .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "median") {
    Hourly <- x[, .(hourlyMedian = median(value, na.rm = TRUE)),
                .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "min") {
    Hourly <- x[, .(hourlyMin = min(value, na.rm = TRUE)),
                .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "max") {
    Hourly <- x[, .(hourlyMax = max(value, na.rm = TRUE)),
                .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "sum") {
    Hourly <- x[, .(hourlySum = sum(volume, na.rm = TRUE)),
                .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  return(Hourly)
}
