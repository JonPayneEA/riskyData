#' Find the amount of events over a given threshold and supply the amount of
#' time steps to which they were in excess.
#'
#' @param dateTime The date and time series
#' @param value The observed data
#' @param threshold The threshold at which to test
#' @param gapWidth This allows you to ignore a set number of time steps beneath
#' a threshold between two or more events
#' @param timeStep The time step used applied in the observed data in seconds
#'
#' @importFrom data.table data.table
#' @importFrom data.table shift
#' @importFrom data.table rleid
#'
#' @return A table of events with start and end times
#' @export
#'
#' @examples
#' data(bewdley)
#' events <- exceed(bewdley$data$dateTime,
#'                  bewdley$data$value,
#'                  threshold = 200,
#'                  gapWidth = 20,
#'                  timeStep = 900)
#' events
exceed <- function(dateTime = NULL,
                   value = NULL,
                   threshold = NULL,
                   gapWidth = 0,
                   timeStep = 900){
  dt <- data.table(dateTime = dateTime, value = value)
  ## Filter NA values out
  dt$value[is.na(dt$value)] <- 0
  ## Use run length encoder to pick NAs out
  consecEx <- rle(dt$value >= threshold)
  ## Find end point positions
  end <- cumsum(consecEx$lengths)
  pos <- which(consecEx$values == TRUE)
  end[pos] <- end[pos]
  ## Find start position using lagged consecEx values
  start <- c(1, data.table::shift(end, type = 'lag')[-1] + 1)
  ## Relate positions to the dates
  ## Pad out the end value by one time step on TRUE events
  #! There will be exceedence in this time period
  end <- dt$dateTime[end] + timeStep
  start <- dt$dateTime[start]

  ## Combine data for next steps
  rawdt <- data.table(event = consecEx$values,
                      start,
                      end,
                      ts = as.numeric(difftime(end,
                                               start,
                                               units = "secs")/timeStep))
  ## Iterate through table to find non-events smaller than the threshold
  #! Converts "event" to TRUE if criteria met
  gap <- gapWidth
  for (i in seq_along(rawdt$event)) {
    if(rawdt$event[i] == FALSE &
       i != 1 &
       rawdt$ts[i] <= gap){
      rawdt$event[i] <- TRUE
    }
  }

  ## Generate group IDs based on rleid function
  rawdt$group <- data.table::rleid(rawdt$event == TRUE)

  export <- rawdt[event == TRUE, .(Start = min(start, na.rm = TRUE),
                                   End = max(end, na.rm = TRUE)),
                  by = group]
  ## Calculate time steps
  export <- export[, .(Start,
                       End = End,
                       timeSteps = as.numeric(difftime(End,
                                                       Start,
                                                       units = "secs")/timeStep))]
  ## Export
  return(export)
}

