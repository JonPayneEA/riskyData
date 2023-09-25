#' @title hydroYearAgg
#'
#' @description Aggregates sub annual time series into a hydrological year
#' resolution, aggregations are carried out to the calendar day not a rolling
#' time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return Data are aggregated to the hydrological year
#' @export
#'
#' @import data.table
#'
#' @examples
#' hydroYearAgg(Buildwas)
hydroYearAgg <- function(x, method = "mean", ...) {
  UseMethod("hydroYearAgg", x)
}

#' @rdname hydroYearAgg
#' @export
hydroYearAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    dt <- x[, .(hydroYearMean = mean(value, na.rm = TRUE)),
            hydroYear]
  }
  if (method == "median") {
    dt <- x[, .(hydroYearMedian = median(value, na.rm = TRUE)),
            hydroYear]
  }
  if (method == "min") {
    dt <- x[, .(hydroYearMin = min(value, na.rm = TRUE)),
            hydroYear]
  }
  if (method == "max") {
    dt <- x[, .(hydroYearMax = max(value, na.rm = TRUE)),
            hydroYear]
  }
  if (method == "sum") {
    dt <- x[, .(hydroYearSum = sum(volume, na.rm = TRUE)),
            hydroYear]
  }
  return(dt)
}
