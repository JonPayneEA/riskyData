#' Access the realtime API
#'
#' @param ID Station Reference ID
#' @param measure Type of measurement required
#'
#' @importFrom cli cli_progress_step
#' @import data.table
#' @import jsonlite
#'
#' @return A data.table of data exported from the EAs realtime API
#' @export
#'
#' @examples
#' ## Generate a table of all IDs available
#' stationIDs <- riskyData::loadAPI("flood")
#'
#' ## Find parameters available
#' rtAPI(ID = "2001")

#' ## Download flow data
#' data <- rtAPI(ID = "2001", measure = "flow")
#' plot(data$value, type = 'l')
rtAPI <- function(ID = NULL, measure = NULL){
  if (is.null(ID)){
    stop("Please include a station reference")
  }
  if(!is.character(ID)){
    stop("Please include the Station Reference ID as a character")
  }

  if (is.null(measure)){
    rtStation <- 'https://environment.data.gov.uk/flood-monitoring/id/stations/'
    rtLink <- paste0(rtStation, ID)

    # Download the measures
    rtSeries <- data.table(jsonlite::fromJSON(rtLink))
    rtMeasures <- data.table(rtSeries$V1[3][[1]]$measures)
    colnames(rtMeasures)[1] <- 'measID'
    return(rtMeasures[,.(measID, parameter, unitName, period)])
  }
  rtStation <- 'https://environment.data.gov.uk/flood-monitoring/id/stations/'
  rtLink <- paste0(rtStation, ID)

  # Download the measures
  rtSeries <- data.table(jsonlite::fromJSON(rtLink))
  rtMeasures <- data.table(rtSeries$V1[3][[1]]$measures)
  colnames(rtMeasures)[1] <- 'measID'

  ## Filter to the data used
  rtMeasure <- rtMeasures[period == 900 & parameter == measure, measID,]

  if (identical(rtMeasure, character(0))){
    stop('There is no realtime data available for the measures required. If you
       want flow data you might be able to convert stage data')
  }

  ## Setting a limit of 4 weeks, 2688 recordings
  ##! Longer data series tend to be corrupted
  measureLink <- paste0(rtMeasure, '/readings.json?_sorted&_limit=2688')

  ## Download data
  cli::cli_progress_step("Downloading extended realtime data")
  rtTS <- data.table(jsonlite::fromJSON(measureLink)[3][[1]])
  rtTS <- rtTS[order(dateTime)] #! Helps combine data

  if (measure == 'rainfall') {
    rt <- rtTS[, .(dateTime = as.POSIXct(dateTime,
                                         format = "%Y-%m-%dT%H:%M",
                                         tz = "GMT"),
                   value,
                   valid = NA,
                   invalid = NA,
                   missing = NA,
                   completeness = NA,
                   quality = 'Unchecked')]
  } else {
    rt <- rtTS[, .(dateTime = as.POSIXct(dateTime,
                                         format = "%Y-%m-%dT%H:%M",
                                         tz = "GMT"),
                   value, quality = 'Unchecked', qcode = '<NA>' )]
  }

  return(rt)
}

