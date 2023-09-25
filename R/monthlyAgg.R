#' @title monthlyAgg
#'
#' @description Aggregates sub monthly time series into a monthly resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return An aggregated dataset taken to the calendar month
#' @export
#'
#' @import data.table
#'
#' @examples
#' monthlyAgg(Buildwas)
monthlyAgg <- function(x, method = "mean", ...) {
  UseMethod("monthlyAgg", x)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    dt <- x[, .(Monthly_Mean = mean(value, na.rm = TRUE)),
            .(dateTime = paste(year(dateTime), month(dateTime)))]
  }
  if (method == "median") {
    dt <- x[, .(Monthly_Median = median(value, na.rm = TRUE)),
            .(dateTime = paste(year(dateTime), month(dateTime)))]
  }
  if (method == "min") {
    dt <- x[, .(Monthly_Min = min(value, na.rm = TRUE)),
            .(dateTime = paste(year(dateTime), month(dateTime)))]
  }
  if (method == "max") {
    dt <- x[, .(Monthly_Max = max(value, na.rm = TRUE)),
            .(dateTime = paste(year(dateTime), month(dateTime)))]
  }
  if (method == "sum") {
    dt <- x[, .(Monthly_Sum = sum(volume, na.rm = TRUE)),
            .(dateTime = paste(year(dateTime), month(dateTime)))]
  }
  return(dt)
}
