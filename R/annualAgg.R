#' @title annualAgg
#'
#' @description Aggregates sub annual time series into an annual resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return A dataset of annually aggregated data, data are not stored in a `HydroAggs` container.
#' @export
#'
#' @import data.table
#'
#' @examples
#' data(bewdley)
#'
#' # Calculate the mean annual (calendar year) flows
#' annualAgg(bewdley)
annualAgg <- function(x, method = "mean", ...) {
  UseMethod("annualAgg", x)
}

#' @rdname annualAgg
#' @export
annualAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    dt <- x[, .(annualMean = mean(value, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  if (method == "median") {
    dt <- x[, .(annnualMedian = median(value, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  if (method == "min") {
    dt <- x[, .(annualMin = min(value, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  if (method == "max") {
    dt <- x[, .(annualMax = max(value, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  if (method == "sum") {
    dt <- x[, .(annualSum = sum(volume, na.rm = TRUE)),
                .(dateTime = year(dateTime))]
  }
  return(dt)
}

#' @rdname annualAgg
#' @export
annualAgg.HydroImport <- function(x, method = "mean", ...) {
  if (method == "mean") {
    dt <- x$data[, .(annualMean = mean(value, na.rm = TRUE)),
                 .(dateTime = year(dateTime))]
  }
  if (method == "median") {
    dt <- x$data[, .(annualMedian = median(value, na.rm = TRUE)),
                 .(dateTime = year(dateTime))]
  }
  if (method == "min") {
    dt <- x$data[, .(annualMin = min(value, na.rm = TRUE)),
                 .(dateTime = year(dateTime))]
  }
  if (method == "max") {
    dt <- x$data[, .(annualMax = max(value, na.rm = TRUE)),
                 .(dateTime = year(dateTime))]
  }
  if (method == "sum") {
    dt <- x$data[, .(annualSum = sum(volume, na.rm = TRUE)),
                 .(dateTime = year(dateTime))]
  }
  return(dt)
}
