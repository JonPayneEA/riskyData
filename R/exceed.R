library(data.table)
library(openxlsx)
library(riskyData)

files <- list.files("C:/Users/jpayne05/OneDrive - Defra/FFIDP/Validation/Data",
                    full.names = TRUE)

FW_Times <- fread(files[1])
thresholds <- data.table(openxlsx::read.xlsx(files[2]))


## Convert FW times to date times
FW_Times$month <- match(FW_Times$Month, month.name)
FW_Times$times <- gsub('1899-12-30 ', '', FW_Times$time)

FW_Times[, dateTime := as.POSIXct(paste0(Year, '/', month, '/', Day, ' ', times),
                                  tz = 'GMT')]

## Extract useful features
fws <- FW_Times[, .(Centre, `Area Names`, taname, dateTime)]

## Find gauges that 122FWF723 is issued from
deets <- thresholds[TargetAreaID == '122FWF723' &
                      Type == 'RES' &
                      Parameter == 'H.obs',
                    .(Location, Value),]

yorkVik <- loadAPI(ID = deets$Location, measure = 'level',
                   period = 900,
                   type = 'instantaneous',
                   datapoints = 'range',
                   from = '2017/10/01 00:00')

## Adapt NA run to a threshold based one

exceed <- function(dateTime = NULL,
                   value = NULL,
                   threshold = NULL,
                   gapWidth = 0,
                   timestep = 900){
  dt <- data.table(dateTime = dateTime, value = value)
  ## Filter NA values out
  dt[is.na(value)] <- 0
  ## Use run length encoder to pick NAs out
  consecEx <- rle(dt$value >= 2.9)
  ## Find end point positions
  end <- cumsum(consecEx$lengths)
  ## Pad out the end value by one timestep on TRUE events
  #! There will be exceedence in this time period
  pos <- which(r$values == TRUE)
  end[pos] <- end[pos] + 1
  ## Find start position using lagged consecEx values
  start <- c(1, data.table::shift(end, type = 'lag')[-1] + 1)
  ## Relate positions to the dates
  end <- dt$dateTime[end]
  start <- dt$dateTime[start]

  ## Combine data for next steps
  rawdt <- data.table(event = consecEx$values,
                      start,
                      end,
                      ts = as.numeric(difftime(end,
                                               start,
                                               units = "secs")/900))
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

  export <- rawdt[event == TRUE, .(Start = min(start),
                                   End = max(end)),
                  by = group]
  ## Calculate time steps
  export <- export[, .(Start,
                       End,
                       timeSteps = as.numeric(difftime(End,
                                                       Start,
                                                       units = "secs")/900))]
  ## Export
  return(export)
}



exceed(yorkVik$data$dateTime,
       yorkVik$data$value,
       threshold = deets$Value,
       gapWidth = 100)

plot(yorkVik$data$dateTime, yorkVik$data$value,
     # ylim = rev(range(yorkVik$data$value, na.rm = TRUE)),
     type = 'l', ylab = "Stage (m)", xlab = "Date Time")http://127.0.0.1:42479/graphics/plot_zoom_png?width=2687&height=861

events <- exceed(yorkVik$data$dateTime,
                 yorkVik$data$value,
                 threshold = deets$Value,
                 gapWidth = 0)

for (i in seq_along(events$Start)){
  polygon(x = c(events$Start[i], events$Start[i], events$End[i],
                events$End[i]),
          y = c(0, 12, 12, 0),
          col = scales::alpha('red', 0.5),
          border = NA)
}

lines(yorkVik$data$dateTime, yorkVik$data$value])
abline(h = 2.9, lwd = 1.5, lty = 2)
abline(v = FW_Times$dateTime, col = 'black', lwd = 1, lty = 3)





ggplot(yorkVik$data, aes(dateTime, value)) +
  geom_line(linewidth = 0.1) +
  labs(x = NULL, y = 'Stage (m)') +
  scale_x_datetime(labels = scales::date_format("%b")) + # this sets it to
  #only show the month. Lord knows why month ended up at "%b"
  facet_wrap(hydroYear ~., scales = 'free_x') +
  geom_hline(yintercept = 2.9, colour = 'red')

yV <- loadAPI(ID = deets$Location, measure = 'level',
              period = 900,
              type = 'instantaneous',
              datapoints = 'range',
              from = '2018/03/01 00:00',
              to = '2018/05/01 00:00')


plot(yV$data$dateTime, yV$data$value,
     # ylim = rev(range(yorkVik$data$value, na.rm = TRUE)),
     type = 'l', ylab = "Stage (m)", xlab = "Date Time")

events <- exceed(yV$data$dateTime,
                 yV$data$value,
                 threshold = deets$Value,
                 gapWidth = 0)

for (i in seq_along(events$Start)){
  polygon(x = c(events$Start[i], events$Start[i], events$End[i],
                events$End[i]),
          y = c(-5, 12, 12, -5),
          col = scales::alpha('red', 0.5),
          border = NA)
}

lines(yV$data$dateTime, yV$data$value)
abline(h = 2.9, lwd = 1.5, lty = 2)


yV <- loadAPI(ID = deets$Location, measure = 'level',
              period = 900,
              type = 'instantaneous',
              datapoints = 'all')

plot(yV$data$dateTime, yV$data$value,
     # ylim = rev(range(yorkVik$data$value, na.rm = TRUE)),
     type = 'l', ylab = "Stage (m)", xlab = "Date Time")

events <- exceed(yV$data$dateTime,
                 yV$data$value,
                 threshold = deets$Value,
                 gapWidth = 96)

## Error on row 24
events <- events[-24,]

for (i in seq_along(events$Start)){
  polygon(x = c(events$Start[i], events$Start[i], events$End[i],
                events$End[i]),
          y = c(0, 12, 12, 0),
          col = scales::alpha('red', 0.5),
          border = NA)
}

lines(yV$data$dateTime, yV$data$value)
abline(h = 2.9, lwd = 1.5, lty = 2)
