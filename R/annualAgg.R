#' @title annualAgg
#'
#' @description Aggregates sub annual time series into an annual resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return A dataset of annually aggregated data
#' @export
#'
#' @import data.table
#'
#' @examples
#' ## Do not run
#' # annualAgg(Buildwas)
annualAgg <- function(x, method = "mean", ...) {
  UseMethod("annualAgg", x)
}

#' @rdname annualAgg
#' @export
annualAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Annual <- x[, .(Annual_Mean = mean(value, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  if (method == "median") {
    Annual <- x[, .(Monthly_Median = median(value, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  if (method == "min") {
    Annual <- x[, .(Annual_Min = min(value, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  if (method == "max") {
    Annual <- x[, .(Annual_Max = max(value, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  if (method == "sum") {
    Annual <- x[, .(Annual_Sum = sum(volume, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  return(Annual)
}
