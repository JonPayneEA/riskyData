#' @title Rainfall event seperation
#'
#' @description The `rainSep()` function uses run length encoding to find
#' consecutive rainfall periods. To avoid brief timesteps of no rain from
#' splitting events, the use of `RcppRoll::roll_sum()` can pad out the event d
#' urations.
#'
#' @param dateTime DateTime variable from time series.
#' @param precip Observed rainfall data.
#' @param threshold Used to tidy averaged and radar data where very low
#' accumulations can be recorded, defaults to 0mm.
#' @param minTotal Used to help tidy the total number of events. Sets a
#' minimum event total rainfall.
#' @param roll Number of time steps so use in the `RcppRoll::roll_sum()`.
#'
#' @return Function returns a data table with event ID, start and end times of
#' the events, and the total accumulation for the events
#' @export
#'
#' @examples
#' ## Do not run
#' ## No accumulation applied, minimum total set to 2mm
#' # start_time <- Sys.time()
#' # rainSep(dateTime = chesterton$data$dateTime,
#' #       precip = chesterton$data$value,
#' #       threshold = 0,
#' #       minTotal = 2,
#' #       roll = 0)
#' # end_time <- Sys.time()
#' # end_time - start_time
#'
#' ## 2 hr rolling accumulation
#' # rainSep(dateTime = chesterton$data$dateTime,
#' #         precip = chesterton$data$value,
#' #         threshold = 0,
#' #         roll = 8)
#'
#' ## Window plots
#' # plot(chesterton$data$dateTime[1:200000], chesterton$data$value[1:200000],
#' #      ylim = rev(range(chesterton$data$value[1:200000], na.rm = TRUE)),
#' #      type = 'h')
#' # dayRain <- rainSep(dateTime = chesterton$data$dateTime[1:200000],
#' #                    precip = chesterton$data$value[1:200000],
#' #                    threshold = 0,
#' #                    minTotal = 4,
#' #                    roll = 20)
#' # for (i in seq_along(dayRain$id)){
#' #   polygon(x = c(dayRain$start[i], dayRain$start[i], dayRain$end[i],
#' #                 dayRain$end[i]),
#' #           y = c(0, 12, 12, 0),
#' #           col = scales::alpha('red', 0.5),
#' #           border = NA)
#' # }
#' # lines(chesterton$data$dateTime[1:200000], chesterton$data$value[1:200000])
rainSep <- function(dateTime = NULL,
                    precip = NULL,
                    threshold = 0,
                    minTotal = 0,
                    roll = 0) {
  precip <- data.table(dateTime = dateTime, value = precip)
  ## Remove NAs
  precip$value <- as.numeric(precip$value)
  precip$value[is.na(precip$value)] <- 0

  if (roll == 0) {
    ## Use run length encoder to pick events out
    consecPrecip <- rle(precip$value > threshold)
  } else {
    rolled <- RcppRoll::roll_sum(
      x = precip$value,
      n = roll,
      fill = 0,
      align = "center"
    )
    ## Use run length encoder to pick events out
    consecPrecip <- rle(rolled > threshold)
  }

  ## Collate start and end dates of events
  end <- cumsum(consecPrecip$lengths)
  start <- c(1, data.table::shift(end, type = "lag")[-1] + 1)
  dt <- data.table(id = seq_along(start),
                   start,
                   end,
                   precip = consecPrecip$values)
  ## Calculate totals
  aggID <- data.table(
    id = rep(dt$id, times = consecPrecip$lengths),
    value = precip$value
  )
  dt$total <- aggID[, .(total = sum(value)), .(id)]$total
  rm(aggID)

  ## Tidy dt
  dt$start <- precip$dateTime[dt$start]
  dt$end <- precip$dateTime[dt$end]
  dt <- dt[precip == TRUE & total >= minTotal, .(id, start, end, total), ]
  dt$id <- seq_along(dt$id)

  return(dt)
}
