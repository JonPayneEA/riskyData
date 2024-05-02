#' A means to pair up hydrological events by nearest match time
#'
#' @param upstream Peak values and date time of upstream site
#' @param downstream Peak values and date time of downstream site
#' @param timeMin Used to filter the time difference in paired peaks, set as 0
#' @param timeMax Used to filter the time difference in paired peaks, set as 1000
#' @param matrix Set to true, exports a matrix of difftimes in minutes that can
#' be used to highlight the peak matching. If set as FALSE a nearest join is used.
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
                       timeMax = 1000,
                       matrix = TRUE){

  if (matrix == TRUE){
    ## Set diffs function to be used in outer
    diffs <- function(x, y){
      dt <- as.numeric(difftime(x, y, units = "mins"))
      return(dt)
    }

    ## Apply outer
    ## Difftime on every event combination
    matrix <- outer(downstream$dateTime,
                    upstream$dateTime,
                    diffs)

    ## Remove negative values
    # is.na(matrix) <-  matrix <= 0

    ## Filter by min and max times
    is.na(matrix) <- matrix <= timeMin | matrix >= timeMax


    ## Find position of smallest time difference
    matrixMin <- as.matrix(apply(matrix, 1, which.min))[,1]
    matches <- data.table(upstreamPos = matrixMin,
                          downstreamPos = seq_along(matrixMin))
    return(matches)
    ## Using the positions of the matrix extract min values as position
    ## cbind used to coerce to vector
    matches$timeDiff <- matrix[cbind(matches$downstreamPos,
                                     matches$upstreamPos)]

    ## Extract data
    matches <- matches[, .(upstreamPos,
                           upstreamDateTime = upstream$dateTime[upstreamPos],
                           upstreamValue = upstream$value[upstreamPos],
                           downstreamPos,
                           downstreamDateTime = downstream$dateTime[downstreamPos],
                           downstreamValue = downstream$value[downstreamPos],
                           timeDiff)]

    return(list(matrix, matches))
    # plot(y = bewdPS[matches$downstream,]$value,
    #      x = buildPS[matches$upstream,]$value)
  }

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
  cli::cli_text(paste("{.strong Upstream peaks:}", length(upstream$value)))
  cli::cli_text(paste("{.strong Downstream peaks:}", length(downstream$value)))
  cli::cli_text(paste("{.strong Paired peaks:}", length(dt$diff)))
  cli::cli_text(paste("{.strong Omitted peaks:}", length(downstream$value) -
                        length(combined$diff)))

  return(dt)
}
