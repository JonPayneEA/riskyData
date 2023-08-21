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
hourlyAgg <- function(x, method = 'mean', ...) {
  UseMethod('hourlyAgg', x)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.data.table <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x[, .(Hourly_Mean = mean(value, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x[, .(Hourly_Median = median(value, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x[, .(Hourly_Min = min(value, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x[, .(Hourly_Max = max(value, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if(method  == 'sum') {
    Hourly <- x[, .(Hourly_Sum = sum(volume, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  return(Hourly)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.flowLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x$GaugeData[, .(Hourly_Mean = mean(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x$GaugeData[, .(Hourly_Median = median(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x$GaugeData[, .(Hourly_Min = min(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x$GaugeData[, .(Hourly_Max = max(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'sum') {
    Hourly <- x$GaugeData[, .(Hourly_Sum = sum(Volume, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  return(Hourly)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.stageLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x$GaugeData[, .(Hourly_Mean = mean(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x$GaugeData[, .(Hourly_Median = median(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x$GaugeData[, .(Hourly_Min = min(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x$GaugeData[, .(Hourly_Max = max(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'sum') {
    print('Stage data are not suitable for sumation')
  }
  return(Hourly)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.rainLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x$GaugeData[, .(Hourly_Mean = mean(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x$GaugeData[, .(Hourly_Median = median(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x$GaugeData[, .(Hourly_Min = min(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x$GaugeData[, .(Hourly_Max = max(Value, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'sum') {
    Hourly <- x$GaugeData[, .(Hourly_Sum = sum(Volume, na.rm = TRUE)), .(Hourly = paste(as.Date(DateTime), hour(DateTime)))]
  }
  return(Hourly)
}

#' @rdname hourlyAgg
#' @export
hourlyAgg.rainAll <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x[, lapply(.SD, mean, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x[, lapply(.SD, median, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x[, lapply(.SD, min, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x[, lapply(.SD, max, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  if(method  == 'sum') {
    Hourly <- x[, lapply(.SD, sum, na.rm = TRUE), .(DateTime = paste(as.Date(DateTime), hour(DateTime)))]
  }
  class(Hourly)[3] <- 'rainAllHourly'
  return(Hourly)
}
