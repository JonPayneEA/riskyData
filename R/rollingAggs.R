#' @title rollingAggs
#'
#' @description Provides a rolling aggregation of a user defined periods.
#'
#' @param dt flowLoad, rainLoad, or stageLoad data
#' @param rolling_aggregations User defined periods of aggregation
#' @param interval Set as 0.25 to represent 15 minute data, for hourly change to 1 etc.
#' @param method 'min', 'max', 'mean', 'median', and 'sum' options available
#'
#' @return A dataset containg numerous aggregations on a roll
#' @export
#'
#' @import data.table
#' @import RcppRoll
#'
#' @examples
#' rollingAggs(Buildwas)
rollingAggs <- function(x, aggregations = NULL, interval = 0.25, method = "mean") {
  UseMethod("rollingAggs", x)
}

#' @rdname rollingAggs
#' @export
rollingAggs.data.table <- function(x, aggregations = NULL, interval = 0.25, method = "mean") {
  if (is.null(aggregations)) {
    aggregations <- c(1, 2, 3, 4, 8, 24, 120)
  }
  roller <- eval(parse(text = paste0("RcppRoll::roll_", method)))
  agg <- length(aggregations)
  if (method == "sum") {
    Rolling_Aggregations <- data.table(dateTime = x$dateTime, hydroYear = x$hydroYear, hydroYearDay = x$hydroYearDay, Raw = x$volume)
  } else {
    # Rolling_Aggregations <- data.table(dateTime = x$dateTime, hydroYear = x$hydroYear, hydroYearDay = x$hydroYearDay, Raw = x$value)
    Rolling_Aggregations <- data.table(dateTime = x$dateTime, Raw = x$value)
  }
  for (i in seq_along(aggregations)) {
    window <- aggregations[i] / interval
    if (aggregations[i] %% interval > 0) {
      cat("Using a rolling aggregation of ", aggregations[i], "is not divisible by 0.25, skipping for next accumulation\n")
      Rolling_Aggregations[, paste("roll", aggregations[i], "Hr", method, sep = "") := rep(NA, length(Rolling_Aggregations$dateTime))]
      next
    } else {
      window <- aggregations[i] / interval
    }

    # cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
    Rolling_Aggregations[, paste("roll", aggregations[i], "Hr", sep = "") := roller(Rolling_Aggregations$Raw, window, fill = NA)]
  }
  dt <- data.table(Rolling_Aggregations)
  return(dt)
}


#' #' @rdname rollingAggs
#' #' @export
#' rollingAggs.flowLoad <- function(x, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean'){
#'   roller <- get(paste0("roll_", method))
#'   agg <- length(rolling_aggregations)
#'   if(method == 'sum'){
#'     Rolling_Aggregations <- data.table(DateTime = x$GaugeData$DateTime, HydroYear = x$GaugeData$HydrologicalYear, HydroDay = x$GaugeData$day_of_hy, Raw = x$GaugeData$Volume)
#'   } else {
#'     Rolling_Aggregations <- data.table(DateTime = x$GaugeData$DateTime, HydroYear = x$GaugeData$HydrologicalYear, HydroDay = x$GaugeData$day_of_hy, Raw = x$GaugeData$Value)
#'   }
#'   for(i in seq_along(rolling_aggregations)){
#'     window <- rolling_aggregations[i]/interval
#'     if(rolling_aggregations[i] %% interval > 0){
#'       cat("Using a rolling aggregation of ", rolling_aggregations[i], "is not divisible by 0.25, skipping for next accumulation\n")
#'       Rolling_Aggregations[, paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "") := rep(NA, length(Rolling_Aggregations$DateTime))]
#'       next
#'     } else {
#'       window <- rolling_aggregations[i]/interval
#'     }
#'
#'     cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
#'     Rolling_Aggregations[,paste("Roll_",rolling_aggregations[i], "hr", sep = ""):= roller(Rolling_Aggregations$Raw, window, fill = NA)]
#'   }
#'   return(Rolling_Aggregations)
#' }
#'
#' #' @rdname rollingAggs
#' #' @export
#' rollingAggs.rainLoad <- function(x, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean'){
#'   roller <- get(paste0("roll_", method))
#'   agg <- length(rolling_aggregations)
#'   if(method == 'sum'){
#'     Rolling_Aggregations <- data.table(DateTime = x$GaugeData$DateTime, HydroYear = x$GaugeData$HydrologicalYear, HydroDay = x$GaugeData$day_of_hy, Raw = x$GaugeData$Volume)
#'   } else {
#'     Rolling_Aggregations <- data.table(DateTime = x$GaugeData$DateTime, HydroYear = x$GaugeData$HydrologicalYear, HydroDay = x$GaugeData$day_of_hy, Raw = x$GaugeData$Value)
#'   }
#'   for(i in seq_along(rolling_aggregations)){
#'     window <- rolling_aggregations[i]/interval
#'     if(rolling_aggregations[i] %% interval > 0){
#'       cat("Using a rolling aggregation of ", rolling_aggregations[i], "is not divisible by 0.25, skipping for next accumulation\n")
#'       Rolling_Aggregations[, paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "") := rep(NA, length(Rolling_Aggregations$DateTime))]
#'       next
#'     } else {
#'       window <- rolling_aggregations[i]/interval
#'     }
#'
#'     cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
#'     Rolling_Aggregations[,paste("Roll_",rolling_aggregations[i], "hr", sep = ""):= roller(Rolling_Aggregations$Raw, window, fill = NA)]
#'   }
#'   return(Rolling_Aggregations)
#' }
#'
#' #' @rdname rollingAggs
#' #' @export
#' rollingAggs.stageLoad <- function(x, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean'){
#'   roller <- get(paste0("roll_", method))
#'   agg <- length(rolling_aggregations)
#'   if(method == 'sum'){
#'     stop('Stage data are not suitable for sumation')
#'   } else {
#'     Rolling_Aggregations <- data.table(DateTime = x$GaugeData$DateTime, HydroYear = x$GaugeData$HydrologicalYear, HydroDay = x$GaugeData$day_of_hy, Raw = x$GaugeData$Value)
#'   }
#'   for(i in seq_along(rolling_aggregations)){
#'     window <- rolling_aggregations[i]/interval
#'     if(rolling_aggregations[i] %% interval > 0){
#'       cat("Using a rolling aggregation of ", rolling_aggregations[i], "is not divisible by 0.25, skipping for next accumulation\n")
#'       Rolling_Aggregations[, paste("Roll_",rolling_aggregations[i], "hr_", method, sep = "") := rep(NA, length(Rolling_Aggregations$DateTime))]
#'       next
#'     } else {
#'       window <- rolling_aggregations[i]/interval
#'     }
#'
#'     cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
#'     Rolling_Aggregations[,paste("Roll_",rolling_aggregations[i], "hr", sep = ""):= roller(Rolling_Aggregations$Raw, window, fill = NA)]
#'   }
#'   return(Rolling_Aggregations)
#' }
