#' This identifies where NA values in the time series sit
#'
#' @param dateTime The date and time series
#' @param value The observed data
#' @param timestep The time step used applied in the observed data in seconds
#'
#' @return A data.table containing the start and end points of NA runs
#' @export
#'
#' @examples
#' data(bewdley)
#' nas <- naRun(bewdley$data$dateTime,
#'              bewdley$data$value,
#'              timestep = 900)
#' nas
naRun <- function(dateTime = NULL,
                  value = NULL,
                  timestep = 900){
  dt <- data.table(dateTime = dateTime, value = value)
  ## Use run length encoder to pick NAs out
  consecNA <- rle(is.na(dt$value))
  ## Collate start and end dates of events
  endPos <- cumsum(consecNA$lengths)
  startPos <- c(1, data.table::shift(endPos, type = 'lag')[-1] + 1)
  dt1 <- data.table(id = seq_along(startPos),
                    startPos,
                    endPos,
                    NA_Event = consecNA$values)

  ## Tidy dt
  dt1$startTime <- dt$dateTime[dt1$start]
  dt1$endTime <- dt$dateTime[dt1$end]
  dt1 <- dt1[NA_Event == TRUE , .(id,
                                  startTime,
                                  endTime,
                                  startPos,
                                  endPos),]
  dt1$id <- seq_along(dt1$id)
  dt1$timeSteps <- difftime(dt1$end, dt1$start)/timestep

  return(dt1)
}

