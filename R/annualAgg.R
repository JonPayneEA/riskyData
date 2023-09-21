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
#' annualAgg(Buildwas)
annualAgg <- function(x, method = "mean", ...) {
  UseMethod("annualAgg", x)
}

#' @rdname annualAgg
#' @export
annualAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Annual <- x[, .(Annual_Mean = mean(value, na.rm = TRUE)), .(dateTime = year(dateTime))]
  }
  if (method == "median") {
    Annual <- x[, .(Monthly_Median = median(value, na.rm = TRUE)), .(dateTime = year(dateTime))]
  }
  if (method == "min") {
    Annual <- x[, .(Annual_Min = min(value, na.rm = TRUE)), .(dateTime = year(dateTime))]
  }
  if (method == "max") {
    Annual <- x[, .(Annual_Max = max(value, na.rm = TRUE)), .(dateTime = year(dateTime))]
  }
  if (method == "sum") {
    Annual <- x[, .(Annual_Sum = sum(volume, na.rm = TRUE)), .(dateTime = year(dateTime))]
  }
  return(Annual)
}

#' @rdname annualAgg
#' @export
annualAgg.flowLoad <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Annual <- x$GaugeData[, .(Annual_Mean = mean(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "median") {
    Annual <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "min") {
    Annual <- x$GaugeData[, .(Annual_Min = min(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "max") {
    Annual <- x$GaugeData[, .(Annual_Max = max(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "sum") {
    Annual <- x$GaugeData[, .(Annual_Sum = sum(Volume, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  return(Annual)
}

#' @rdname annualAgg
#' @export
annualAgg.rainLoad <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Annual <- x$GaugeData[, .(Annual_Mean = mean(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "median") {
    Annual <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "min") {
    Annual <- x$GaugeData[, .(Annual_Min = min(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "max") {
    Annual <- x$GaugeData[, .(Annual_Max = max(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "sum") {
    Annual <- x$GaugeData[, .(Annual_Sum = sum(Volume, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  return(Annual)
}

#' @rdname annualAgg
#' @export
annualAgg.stageLoad <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Annual <- x$GaugeData[, .(Annual_Mean = mean(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "median") {
    Annual <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "min") {
    Annual <- x$GaugeData[, .(Annual_Min = min(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "max") {
    Annual <- x$GaugeData[, .(Annual_Max = max(Value, na.rm = TRUE)), .(Calendar_Year = year(DateTime))]
  }
  if (method == "sum") {
    print("Stage data are not suitable for sumation")
  }
  return(Annual)
}

#' @rdname annualAgg
#' @export
annualAgg.rainAll <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Annual <- x[, lapply(.SD, mean, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  if (method == "median") {
    Annual <- x[, lapply(.SD, median, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  if (method == "min") {
    Annual <- x[, lapply(.SD, min, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  if (method == "max") {
    Annual <- x[, lapply(.SD, max, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  if (method == "sum") {
    Annual <- x[, lapply(.SD, sum, na.rm = TRUE), .(Calendar_Year = year(DateTime))]
  }
  class(Annual)[3] <- "rainAllYear"
  return(Annual)
}
