#' @title monthlyAgg
#'
#' @description Aggregates sub monthly time series into a monthly resolution,
#' aggregations are carried out to the calendar day not a rolling time period.
#'
#' @param x Data generated in the riskyData package
#' @param method 'mean', 'median', 'max', 'min', or 'sum'
#' @param ... Other variables as required
#'
#' @return
#' @export
#'
#' @import data.table
#'
#' @examples
#' monthlyAgg(Buildwas)
monthlyAgg <- function(x, method = 'mean', ...) {
  UseMethod('monthlyAgg', x)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.data.table <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Monthly <- x[, .(Monthly_Mean = mean(value, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x[, .(Monthly_Median = median(value, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x[, .(Monthly_Min = min(value, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x[, .(Monthly_Max = max(value, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  if(method  == 'sum') {
    Monthly <- x[, .(Monthly_Sum = sum(volume, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  return(Monthly)
}


#' @rdname monthlyAgg
#' @export
monthlyAgg.flowLoad <- function(x, method = mean, ...){
  if(method  == 'mean') {
    Monthly <- x$GaugeData[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x$GaugeData[, .(Monthly_Min = min(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x$GaugeData[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'sum') {
    Monthly <- x$GaugeData[, .(Monthly_Sum = sum(Volume, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  return(Monthly)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.rainLoad <- function(x, method = mean, ...){
  if(method  == 'mean') {
    Monthly <- x$GaugeData[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x$GaugeData[, .(Monthly_Min = min(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x$GaugeData[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'sum') {
    Monthly <- x$GaugeData[, .(Monthly_Sum = sum(Volume, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  return(Monthly)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.stageLoad <- function(x, method = mean, ...){
  if(method  == 'mean') {
    Monthly <- x$GaugeData[, .(Monthly_Mean = mean(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x$GaugeData[, .(Monthly_Min = min(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x$GaugeData[, .(Monthly_Max = max(Value, na.rm = TRUE)), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'sum') {
    print('Stage data are not suitable for sumation')
  }
  return(Monthly)
}

#' @rdname monthlyAgg
#' @export
monthlyAgg.rainAll <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Monthly <- x[, lapply(.SD, mean, na.rm = TRUE), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x[, lapply(.SD, median, na.rm = TRUE), .(Year_Month = paste(year(Date), month(DateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x[, lapply(.SD, min, na.rm = TRUE), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x[, lapply(.SD, max, na.rm = TRUE), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  if(method  == 'sum') {
    Monthly <- x[, lapply(.SD, sum, na.rm = TRUE), .(Year_Month = paste(year(DateTime), month(DateTime)))]
  }
  class(Monthly)[3] <- 'rainAllMonthly'
  return(Monthly)
}
