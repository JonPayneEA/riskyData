#' @title hydroYearAgg
#'
#' @description Aggregates sub annual time series into a hydrological year resolution,
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
#' hydroYearAgg(Buildwas)
hydroYearAgg <- function(x, method = 'mean', ...) {
  UseMethod('hydroYearAgg', x)
}

#' @rdname hydroYearAgg
#' @export
hydroYearAgg.data.table <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hydro_year <- x[, .(hydroYearMean = mean(value, na.rm = TRUE)), hydroYear]
  }
  if(method  == 'median') {
    Hydro_year <- x[, .(hydroYearMedian = median(value, na.rm = TRUE)), hydroYear]
  }
  if(method  == 'min') {
    Hydro_year <- x[, .(hydroYearMin = min(value, na.rm = TRUE)), hydroYear]
  }
  if(method  == 'max') {
    Hydro_year <- x[, .(hydroYearMax = max(value, na.rm = TRUE)), hydroYear]
  }
  if(method  == 'sum') {
    Hydro_year <- x[, .(hydroYearSum = sum(volume, na.rm = TRUE)), hydroYear]
  }
  return(Hydro_year)
}

#' @rdname hydroYearAgg
#' @export
hydroYearAgg.flowLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Mean = mean(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'median') {
    Hydro_year <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'min') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Min = min(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'max') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'sum') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Sum = sum(Volume, na.rm = TRUE)), HydrologicalYear]
  }
  return(Hydro_year)
}

#' @rdname hydroYearAgg
#' @export
hydroYearAgg.rainLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Mean = mean(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'median') {
    Hydro_year <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'min') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Min = min(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'max') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'sum') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Sum = sum(Volume, na.rm = TRUE)), HydrologicalYear]
  }
  return(Hydro_year)
}

#' @rdname hydroYearAgg
#' @export
hydroYearAgg.stageLoad <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Mean = mean(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'median') {
    Hydro_year <- x$GaugeData[, .(Monthly_Median = median(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'min') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Min = min(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'max') {
    Hydro_year <- x$GaugeData[, .(Hydro_year_Max = max(Value, na.rm = TRUE)), HydrologicalYear]
  }
  if(method  == 'sum') {
    print('Stage data are not suitable for sumation')
  }
  return(Hydro_year)
}
