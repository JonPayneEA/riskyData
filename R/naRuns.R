naRun <- function(dateTime = NULL,
                  value = NULL,
                  timestep = 900){
  dt <- data.table(dateTime = dateTime, value = value)
  ## Use run length encoder to pick NAs out
  consecNA <- rle(is.na(dt$value))
  print(consecNA)
  ## Collate start and end dates of events
  end <- cumsum(consecNA$lengths)
  start <- c(1, data.table::shift(end, type = 'lag')[-1] + 1)
  dt1 <- data.table(id = seq_along(start), start, end, NA_Event = consecNA$values)
  ## Calculate totals


  ## Tidy dt
  dt1$start <- dt$dateTime[dt1$start]
  dt1$end <- dt$dateTime[dt1$end]
  dt1 <- dt1[NA_Event == TRUE , .(id, start, end),]
  dt1$id <- seq_along(dt1$id)
  dt1$timeSteps <- difftime(dt1$end, dt1$start)/timestep

  return(dt1)
}

naRun(bewdley$data$dateTime, bewdley$data$value)

rle(is.na(bewdley$data$value))
? difftime
