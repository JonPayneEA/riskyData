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
#' dailyAgg(Buildwas)
dailyAgg <- function(x, method = "mean", ...) {
  UseMethod("dailyAgg", x)
}

#' @rdname dailyAgg
#' @export
dailyAgg.data.table <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Daily <- x[, .(Daily_Mean = mean(value, na.rm = TRUE)), .(dateTime = as.Date(dateTime))]
  }
  if (method == "median") {
    Daily <- x[, .(Daily_Median = median(value, na.rm = TRUE)), .(dateTime = as.Date(dateTime))]
  }
  if (method == "min") {
    Daily <- x[, .(Daily_Min = min(value, na.rm = TRUE)), .(dateTime = as.Date(dateTime))]
  }
  if (method == "max") {
    Daily <- x[, .(Daily_Max = max(value, na.rm = TRUE)), .(dateTime = as.Date(dateTime))]
  }
  if (method == "sum") {
    Daily <- x[, .(Daily_Sum = sum(volume, na.rm = TRUE)), .(dateTime = as.Date(dateTime))]
  }
  return(Daily)
}

#' @rdname dailyAgg
#' @export
dailyAgg.flowLoad <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Daily <- x$GaugeData[, .(Daily_Mean = mean(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "median") {
    Daily <- x$GaugeData[, .(Daily_Median = median(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "min") {
    Daily <- x$GaugeData[, .(Daily_Min = min(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "max") {
    Daily <- x$GaugeData[, .(Daily_Max = max(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "sum") {
    Daily <- x$GaugeData[, .(Daily_Sum = sum(Volume, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  return(Daily)
}

#' @rdname dailyAgg
#' @export
dailyAgg.rainLoad <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Daily <- x$GaugeData[, .(Daily_Mean = mean(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "median") {
    Daily <- x$GaugeData[, .(Daily_Median = median(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "min") {
    Daily <- x$GaugeData[, .(Daily_Min = min(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "max") {
    Daily <- x$GaugeData[, .(Daily_Max = max(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "sum") {
    Daily <- x$GaugeData[, .(Daily_Sum = sum(Volume, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  return(Daily)
}

#' @rdname dailyAgg
#' @export
dailyAgg.stageLoad <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Daily <- x$GaugeData[, .(Daily_Mean = mean(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "median") {
    Daily <- x$GaugeData[, .(Daily_Median = median(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "min") {
    Daily <- x$GaugeData[, .(Daily_Min = min(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "max") {
    Daily <- x$GaugeData[, .(Daily_Max = max(Value, na.rm = TRUE)), .(Daily = as.Date(DateTime))]
  }
  if (method == "sum") {
    print("Stage data are not suitable for sumation")
  }
  return(Daily)
}

#' @rdname dailyAgg
#' @export
dailyAgg.rainAll <- function(x, method = "mean", ...) {
  if (method == "mean") {
    Daily <- x[, lapply(.SD, mean, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  if (method == "median") {
    Daily <- x[, lapply(.SD, median, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  if (method == "min") {
    Daily <- x[, lapply(.SD, min, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  if (method == "max") {
    Daily <- x[, lapply(.SD, max, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  if (method == "sum") {
    Daily <- x[, lapply(.SD, sum, na.rm = TRUE), .(DateTime = as.Date(DateTime))]
  }
  class(Daily)[3] <- "rainAllDaily"
  return(Daily)
}
