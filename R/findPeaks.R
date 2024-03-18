#' Find peaks in hydrometric data
#'
#' @description The `findPeaks` function detects peaks in hydrometric time
#' series using run length encoding against a subset of threshlds.
#'
#' @param x R6 hydroImport object
#' @param levels The number of thresholds to apply against the time seires.
#' Defaults to 100
#' @param from  Set the location from where the thresholds should start.
#' Defaults to the bottom 10% value.
#' @param to Set the location from where the thresholds should end
#' Defaults to the maximum observed value.
#' @param gaps Used to pad out the run length encoding, prevents wobbly data
#' showing multiple exceedances. Set to 96.
#'
#' @return A data.table containing dateTime and values of observed peaks
#' @export
#'
#' @examples
#' data(bewdley)
#' findPeaks(bewdley)
findPeaks <-  function(x,
                       levels = 100,
                       from = NULL,
                       to = NULL,
                       gaps = 96){
  low <- quantile(x$data$value, probs = 0.1, na.rm = TRUE)
  thresholds <- seq(from = low, to = max(x$data$value, na.rm = TRUE),
                    length.out = levels)
  ##
  times <- list()
  for(i in seq_along(thresholds)){
    times[[i]] <- riskyData::exceed(x$data$dateTime,
                                    x$data$value, gapWidth = gaps,
                                    threshold = thresholds[i])
  }
  times <- data.table::rbindlist(times)

  ## Clone dataset to stop modification in r6 object
  dt <- x$clone()
  dt <- dt$data[, .(dateTime, value)]

  ## Duplicate dateTime so that it's preserved in merge
  dt$timeDate <- dt$dateTime
  setkey(dt, dateTime)
  setkey(times, Start, End)

  ## Carry out a merge on times and the dt
  result <- dt[times,
               .SD[which.max(value)],
               on = .(dateTime >= Start, dateTime <= End),
               by = .EACHI]

  results <- unique(result[, .(dateTime = timeDate,
                               value = value)])
  ## Order results by dateTime
  setorder(results, dateTime)
  return(results)
}
