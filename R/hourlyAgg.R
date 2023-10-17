#' @title hourlyAgg
#'
#' @description Aggregates sub hourly time series into an hourly resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return A dataset of hourly aggregated, data are not stored in a `HydroAggs` container.
#' @export
#'
#' @import data.table
#'
#' @examples
#' data(bewdley)
#'
#' ## Calculate median flow
#' hourlyAgg(bewdley, method = "median")
hourlyAgg <- function(x, method = "mean", ...) {
  UseMethod("hourlyAgg", x)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    dt <- x[, .(hourlyMean = mean(value, na.rm = TRUE)),
            .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "median") {
    dt <- x[, .(hourlyMedian = median(value, na.rm = TRUE)),
            .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "min") {
    dt <- x[, .(hourlyMin = min(value, na.rm = TRUE)),
            .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "max") {
    dt <- x[, .(hourlyMax = max(value, na.rm = TRUE)),
            .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "sum") {
    dt <- x[, .(hourlySum = sum(volume, na.rm = TRUE)),
            .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  return(dt)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.HydroImport <- function(x, method = "mean", ...) {
  if (method == "mean") {
    dt <- x$data[, .(hourlyMean = mean(value, na.rm = TRUE)),
                 .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "median") {
    dt <- x$data[, .(hourlyMedian = median(value, na.rm = TRUE)),
                 .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "min") {
    dt <- x$data[, .(hourlyMin = min(value, na.rm = TRUE)),
                 .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "max") {
    dt <- x$data[, .(hourlyMax = max(value, na.rm = TRUE)),
                 .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if (method == "sum") {
    dt <- x$data[, .(hourlySum = sum(volume, na.rm = TRUE)),
                 .(dateTime = paste(as.Date(dateTime), hour(dateTime)))]
  }
  return(dt)
}
