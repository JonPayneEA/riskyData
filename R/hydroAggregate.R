#' @title Hydro Aggregate
#'
#' @details Wrapper function for the various aggregation methods employed in
#' riskyData
#'
#' @param dt flowLoad, rainLoad, or stageLoad data
#' @param rolling_aggregations User defined periods of aggregation
#' @param interval Set as 0.25 to represent 15 minute data, for hourly change to 1 etc.
#' @param method 'min', 'max', 'mean', 'median', and 'sum' options available
#'
#' @return Numerous aggregations
#' @export
#'
#' @examples
#' #hydroAggregate(buildwas)
hydroAggregate <- function(dt, interval = 0.25, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), method = 'mean') {
  if(missingArg(dt)){
    stop("Data missing. Please supply data to run this function")
  }
  if("Value" %in% colnames(dt$GaugeData) == FALSE){
    stop("Values (flow) field missing from data.table")
  }
  if("DateTime" %in% colnames(dt$GaugeData) == FALSE){
    stop("DateTime field missing from data.table")
  }
  data_list <- list()
  if(interval<1) {
    cat("====================== Calculating hourly aggregations =====================\n")
    Hourly <- hourlyAgg(dt, method = method)
  } else {
    Hourly <- NA
  }
  data_list[['Hourly']] <- Hourly

  cat("====================== Calculating daily aggregations ======================\n")
  Daily <- dailyAgg(dt, method = method)
  data_list[['Daily']] <- Daily

  cat("====================== Calculating monthly aggregations ====================\n")
  Monthly <- monthlyAgg(dt, method = method)
  data_list[['Monthly']] <- Monthly

  cat("====================== Calculating annual aggregations =====================\n")
  Annual <- annualAgg(dt, method = method)
  data_list[['Annual']] <- Annual

  cat("====================== Calculating Hydro Year aggregations =================\n")
  Hydro_year <- hydroYearAgg(dt, method = method)
  data_list[['Hydro_year']] <- Hydro_year

  if(length(rolling_aggregations) > 0){
    Rolling_Aggregations <- rollingAggs(dt, interval = interval, rolling_aggregations = rolling_aggregations, method = method)
  }
  data_list[['Rolling_Aggregations']] <- Rolling_Aggregations
  class(data_list) <- append(class(data_list), c(paste('HydroAggs', method, sep = ''), 'HydroAggs'))
  return(data_list)
}
