#' A means to pair up hydrological events by nearest match time
#'
#' @param upstream Peak values and date time of upstream site
#' @param downstream Peak values and date time of downstream site
#' @param timeMin Used to filter the time difference in paired peaks, set as 0
#' @param timeMax Used to filter the time difference in paired peaks, set as 1000
#'
#' @return a data.table of paired events
#' @export
#'
#' @examples
#' bewdS <- loadAPI(ID = '2001', 'level', 900, 'instantaneous',
#'                  datapoints = 'range',
#'                  from = '2008-10-01 09:00')
#'                  bewdS$hydroYearDay()
#'
#' buildS <- loadAPI(ID = '2134', 'level', 900, 'instantaneous',
#'                   datapoints = 'range',
#'                   from = '2008-10-01 09:00')
#' buildS$hydroYearDay()
#'
#' bewdPS <- findPeaks(bewdS, levels = 100)
#' buildPS <- findPeaks(buildS, levels = 100)
#' pairedStage <- linkEvents(upstream = buildPS,
#'                           downstream = bewdPS)
#' pairedStage
#'
#' ## Plot the paired events
#' plot(pairedStage$value ~ pairedStage$`i.value`,
#'      xlab = 'Buildwas Stage (m)',
#'      ylab = "Bewdley Stage (m)")
#' model <- lm(pairedStage$value ~ pairedStage$`i.value`)
#' abline(model, lwd = 2, col = 2)
linkEvents <- function(upstream = NULL,
                       downstream = NULL,
                       timeMin = 0,
                       timeMax = 1000){

  ## Duplicate times so they are preserved in join
  upstream$upstreamTime <- upstream$dateTime
  downstream$downstreamTime <- downstream$dateTime

  data.table::setkey(upstream, dateTime)
  data.table::setkey(downstream, dateTime)

  ## Join the data based on nearest date
  combined <- downstream[upstream, roll = "nearest"]

  ## Calculate the difftime in mins
  combined$diff <- difftime(combined$downstreamTime ,
                            combined$upstreamTime, units = "mins")

  ## Constrain the data to the minimum and maximum time differences
  dt <- combined[diff > 0 & diff < 900,]
  return(dt)
}
