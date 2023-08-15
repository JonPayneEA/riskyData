library(R6)

hydroLoad <- R6Class(
  classname = "hydroLoad",
  public = list(
    ## Set public fields
    data = NULL,
    # Initialisation steps
    initialize = function(data = NA,
                          stationName = NA,
                          riverName = NA,
                          WISKI = NA,
                          RLOID = NA,
                          stationGuide = NA,
                          baseURL = NA,
                          dataURL = NA,
                          measureURL = NA,
                          idNRFA = NA,
                          urlNRFA = NA,
                          easting = NA,
                          northing = NA,
                          latitude = NA,
                          longitude = NA,
                          area = NA,
                          parameter = NA,
                          unitName = NA,
                          unit = NA,
                          datum = NA,
                          boreholeDepth = NA,
                          aquifer = NA,
                          start = NA,
                          end = NA,
                          timeZone = "GMT - UTC±00:00",
                          records = NA) {
      self$data = data
      private$stationName = stationName
      private$riverName = riverName
      private$WISKI = WISKI
      private$RLOID = RLOID
      private$stationGuide = stationGuide
      private$baseURL = baseURL
      private$dataURL = dataURL
      private$measureURL = measureURL
      private$idNRFA = idNRFA
      private$urlNRFA = urlNRFA
      private$easting = easting
      private$northing = northing
      private$latitude = latitude
      private$longitude = longitude
      private$area = area
      private$parameter = parameter
      private$unitName = unitName
      private$unit = unit
      private$datum = datum
      private$boreholeDepth = boreholeDepth
      private$aquifer = aquifer
      # private$start = start
      # private$end = end
      private$timeZone = timeZone
      # private$records = records

    },
    print = function(...) {
      cat(crayon::bgGreen(crayon::bold(" <hydroLoad> \n")))
      cat(crayon::bold("    Private:\n"))
      cat(crayon::bold("\tStation name: "), "\t",private$stationName, "\n", sep = "")
      cat(crayon::bold("\tWISKI ID: "), "\t",private$WISKI, "\n", sep = "")
      cat(crayon::bold("\tData Type: "), "\t",private$parameter, " (", private$unitName, ")", "\n", sep = "")
      cat(crayon::bold("\tData Range: "), "\t",as.character(private$start()), " to ", as.character(private$end()), "\n", sep = "")
      cat(crayon::bold("\tEasting: "), "\t",private$easting, "\n", sep = "")
      cat(crayon::bold("\tNorthing: "), "\t",private$northing, "\n", sep = "")
      cat(crayon::bold("\tLatitude: "), "\t",private$latitude, "\n", sep = "")
      cat(crayon::bold("\tLongitude: "), "\t",private$longitude, "\n", sep = "")
      cat("\n")
      cat(crayon::bold("   Public:\n"))
      print(self$data)
      cat("\n")
      cat(crayon::bold("For more details use the $methods() function, the format should be as `Object_name`$methods()\n"))

    },
    methods = function() {
      cat(crayon::bgGreen(crayon::bold(" <hydroLoad> \n")))
      cat(crayon::bold("    Methods: \n"))
      cat(crayon::bold(" \t$data "), " \t\t\t Returns the raw data imported via the API\n", sep = "")
      cat(crayon::bold(" \t$meta() "), " \t\t Returns the metadata associated with the object\n", sep = "")
      cat(crayon::bold(" \t$asVol() "), " \t\t Calculates the volume of water relative to the time step used, see ?asVol\n", sep = "")
      cat(crayon::bold(" \t$hydroYearDay() "), " \t Calculates the hydrological year and day, see ?hydroYearDay\n", sep = "")
      cat(crayon::bold(" \t$rmVol() "), " \t\t Removes the volume column\n", sep = "")
      cat(crayon::bold(" \t$rmHY() "), " \t\t Removes the hydroYear column\n", sep = "")
      cat(crayon::bold(" \t$rmHYD() "), " \t\t Removes the hydroYearDay column\n", sep = "")
      cat(crayon::bold(" \t$summary() "), " \t\t Provides a quick summary of the raw data\n", sep = "")
      cat(crayon::bold(" \t$start() "), " \t\t Returns the timing of the first time step\n", sep = "")
      cat(crayon::bold(" \t$end() "), " \t\t Returns the timing of the last time step\n", sep = "")
      cat(crayon::bold(" \t$coords()  "), "\t\t Returns coordinates from the metadata\n", sep = "")
      cat(crayon::bold(" \t$nrfa() "), " \t\t Returns the NRFA data from the metadata\n", sep = "")
      cat(crayon::bold(" \t$hourlyAgg() "), " \t\t Aggregates data by the calendar hours, see ?hourlyAgg\n", sep = "")
      cat(crayon::bold(" \t$dailyAgg() "), " \t\t Aggregates data by the calendar days, see ?dailyAgg\n", sep = "")
      cat(crayon::bold(" \t$monthlyAgg() "), " \t\t Aggregates data by the calendar months, see ?monthlyAgg\n", sep = "")
      cat(crayon::bold(" \t$annualAgg() "), " \t\t Aggregates data by the calendar year, see ?annualAgg\n", sep = "")
      cat(crayon::bold(" \t$hydroYearAgg() "), " \t Aggregates data by the hydrological year, see ?hydroYearAgg\n", sep = "")
      cat(crayon::bold(" \t$rollingAggs() "), " \t Uses user specified aggregation timings, see ?rollingAggs\n", sep = "")
    },
    summary = function() {
      cat("hydroLoad: \n")
      cat("\tStation ID: ", private$WISKI, "\n", sep = "")
      cat("\tStart: ", as.character(private$start()), "\n", sep = "")
      cat("\tEnd: ", as.character(private$end()), "\n", sep = "")
      cat("\tTime Zone: ", private$timeZone, "\n", sep = "")
      cat("\tObservations: ", private$records(), "\n", sep = "")
      cat("\tParameter: ", private$parameter, "\n", sep = "")
      cat("\tUnit Type: ", private$unitName, "\n", sep = "")
      cat("\tUnit: ", private$unit, "\n", sep = "")
    },
    asVol = function(){
      asVol(x = self$data)
    },
    hydroYearDay = function(){
      dt <- hydroYearDay(x = self$data)
      self$data <- data.table(self$data, dt)
    },
    rmVol = function(){
      rmVol(x = self$data)
    },
    rmHY = function(){
      rmHY(x = self$data)
    },
    rmHYD = function(){
      rmHYD(x = self$data)
    },
    coords = function() {
      # cat(private$easting, "\n")
      dt <- data.table(Easting = private$easting,
                       Northing = private$northing,
                       Latitude = private$latitude,
                       Longitude = private$longitude
                       )
      return(dt)
    },
    nrfa = function() {
      dt <- data.table(WISKI = private$WISKI,
                       codeNRFA = private$idNRFA,
                       urlNRFA = private$urlNRFA)
      return(dt)
    },
    hourlyAgg = function(method = 'mean') {
      dt <- hourlyAgg(x = self$data, method = method)
      return(dt)
    },
    dailyAgg = function(method = 'mean') {
      dt <- dailyAgg(x = self$data, method = method)
      return(dt)
    },
    monthlyAgg = function(method = 'mean') {
      dt <- monthlyAgg(x = self$data, method = method)
      return(dt)
    },
    annualAgg = function(method = 'mean') {
      dt <- annualAgg(x = self$data, method = method)
      return(dt)
    },
    hydroYearAgg = function(method = 'mean') {
      dt <- hydroYearAgg(x = self$data, method = method)
      return(dt)
    },
    rollingAggs = function(method = 'mean', rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25) {
      dt <- rollingAggs(x = self$data, method = method)
      return(dt)
    },
    meta = function() {
      dt <- data.table(
        stationName = private$stationName,
        riverName = private$riverName,
        WISKI = private$WISKI,
        RLOID = private$RLOID,
        stationGuide = private$stationGuide,
        baseURL = private$baseURL,
        dataURL = private$dataURL,
        measureURL = private$measureURL,
        idNRFA = private$idNRFA,
        urlNRFA = private$urlNRFA,
        easting = private$easting,
        northing = private$northing,
        latitude = private$latitude,
        longitude = private$longitude,
        area = private$area,
        parameter = private$parameter,
        unitName = private$unitName,
        unit = private$unit,
        datum = private$datum,
        boreholeDepth = private$boreholeDepth,
        aquifer = private$aquifer,
        start = private$start(),
        end = private$end(),
        timeZone = private$timeZone,
        records = private$records())
      return(dt)
    }
  ),

  ## Set private fields (hidden from normal view)
  private = list(
    stationName = NULL,
    start = function(){
      return(min(self$data$dateTime))
    },
    end = function(){
      return(max(self$data$dateTime))
    },
    riverName = NULL,
    WISKI = NULL,
    RLOID = NULL,
    stationGuide = NULL,
    baseURL = NULL,
    dataURL = NULL,
    measureURL = NULL,
    idNRFA = NULL,
    urlNRFA = NULL,
    easting = NULL,
    northing = NULL,
    latitude = NULL,
    longitude = NULL,
    area = NULL,
    parameter = NULL,
    unitName = NULL,
    unit = NULL,
    datum = NULL,
    boreholeDepth = NULL,
    aquifer = NULL,
    timeZone = NULL,
    records = function(){
      return(length(self$data$dateTime))
    }
  )
)

gauge <- loadAPI(ID = '2001',
                measure = 'flow',
                period = 900,
                type = 'instantaneous',
                datapoints = 'range',
                from = '2022-07-29',
                to = '2022-08-01')
gauge$methods()
gauge$meta()
print(gauge)
gauge$asVol()
gauge$hydroYearDay()
gauge$rmVol()
gauge$rmHY()
gauge$rmHYD()
gauge$data
gauge$summary()
gauge$start()
gauge$coords()
gauge$nrfa()
gauge$hourlyAgg(method = 'max')
gauge$dailyAgg()
gauge$monthlyAgg()
gauge$annualAgg()
gauge$hydroYearAgg()
gauge$rollingAggs()


#=========================#
#        Functions        #
#=========================#

hourlyAgg <- function(x, method = 'mean', ...) {
  UseMethod('hourlyAgg', x)
}

hourlyAgg.data.table <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hourly <- x[, .(Hourly_Mean = mean(value, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if(method  == 'median') {
    Hourly <- x[, .(Hourly_Median = median(value, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if(method  == 'min') {
    Hourly <- x[, .(Hourly_Min = min(value, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if(method  == 'max') {
    Hourly <- x[, .(Hourly_Max = max(value, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  if(method  == 'sum') {
    Hourly <- x[, .(Hourly_Sum = sum(volume, na.rm = TRUE)), .(Hourly = paste(as.Date(dateTime), hour(dateTime)))]
  }
  return(Hourly)
}

dailyAgg <- function(x, method = 'mean', ...) {
  UseMethod('dailyAgg', x)
}

dailyAgg.data.table <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Daily <- x[, .(Daily_Mean = mean(value, na.rm = TRUE)), .(Daily = as.Date(dateTime))]
  }
  if(method  == 'median') {
    Daily <- x[, .(Daily_Median = median(value, na.rm = TRUE)), .(Daily = as.Date(dateTime))]
  }
  if(method  == 'min') {
    Daily <- x[, .(Daily_Min = min(value, na.rm = TRUE)), .(Daily = as.Date(dateTime))]
  }
  if(method  == 'max') {
    Daily <- x[, .(Daily_Max = max(value, na.rm = TRUE)), .(Daily = as.Date(dateTime))]
  }
  if(method  == 'sum') {
    Daily <- x[, .(Daily_Sum = sum(volume, na.rm = TRUE)), .(Daily = as.Date(dateTime))]
  }
  return(Daily)
}

monthlyAgg <- function(x, method = 'mean', ...) {
  UseMethod('monthlyAgg', x)
}

monthlyAgg.data.table <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Monthly <- x[, .(Monthly_Mean = mean(value, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  if(method  == 'median') {
    Monthly <- x[, .(Monthly_Median = median(value, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  if(method  == 'min') {
    Monthly <- x[, .(Monthly_Min = min(value, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  if(method  == 'max') {
    Monthly <- x[, .(Monthly_Max = max(value, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  if(method  == 'sum') {
    Monthly <- x[, .(Monthly_Sum = sum(volume, na.rm = TRUE)), .(Year_Month = paste(year(dateTime), month(dateTime)))]
  }
  return(Monthly)
}

annualAgg <- function(x, method = 'mean', ...) {
  UseMethod('annualAgg', x)
}

annualAgg.data.table <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Annual <- x[, .(Annual_Mean = mean(value, na.rm = TRUE)), .(Calendar_Year = year(dateTime))]
  }
  if(method  == 'median') {
    Annual <- x[, .(Monthly_Median = median(value, na.rm = TRUE)), .(Calendar_Year = year(dateTime))]
  }
  if(method  == 'min') {
    Annual <- x[, .(Annual_Min = min(value, na.rm = TRUE)), .(Calendar_Year = year(dateTime))]
  }
  if(method  == 'max') {
    Annual <- x[, .(Annual_Max = max(value, na.rm = TRUE)), .(Calendar_Year = year(dateTime))]
  }
  if(method  == 'sum') {
    Annual <- x[, .(Annual_Sum = sum(volume, na.rm = TRUE)), .(Calendar_Year = year(dateTime))]
  }
  return(Annual)
}

hydroYearAgg <- function(x, method = 'mean', ...) {
  UseMethod('hydroYearAgg', x)
}

hydroYearAgg.data.table <- function(x, method = 'mean', ...){
  if(method  == 'mean') {
    Hydro_year <- x[, .(Hydro_year_Mean = mean(value, na.rm = TRUE)), hydroYear]
  }
  if(method  == 'median') {
    Hydro_year <- x[, .(Monthly_Median = median(value, na.rm = TRUE)), hydroYear]
  }
  if(method  == 'min') {
    Hydro_year <- x[, .(Hydro_year_Min = min(value, na.rm = TRUE)), hydroYear]
  }
  if(method  == 'max') {
    Hydro_year <- x[, .(Hydro_year_Max = max(value, na.rm = TRUE)), hydroYear]
  }
  if(method  == 'sum') {
    Hydro_year <- x[, .(Hydro_year_Sum = sum(volume, na.rm = TRUE)), hydroYear]
  }
  return(Hydro_year)
}

rollingAggs <- function(x, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean') {
  UseMethod('rollingAggs', x)
}

rollingAggs.data.table <- function(x, rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25, method = 'mean'){
  # roller <- get(paste0("RcppRoll::roll_", method))
  roller <- eval(parse(text="RcppRoll::roll_mean"))
  agg <- length(rolling_aggregations)
  if(method == 'sum'){
    Rolling_Aggregations <- data.table(dateTime = x$dateTime, hydroYear = x$hydroYear, hydroYearDay = x$hydroYearDay, Raw = x$volume)
  } else {
    Rolling_Aggregations <- data.table(dateTime = x$dateTime, hydroYear = x$hydroYear, hydroYearDay = x$hydroYearDay, Raw = x$value)
  }
  for(i in seq_along(rolling_aggregations)){
    window <- rolling_aggregations[i]/interval
    if(rolling_aggregations[i] %% interval > 0){
      cat("Using a rolling aggregation of ", rolling_aggregations[i], "is not divisible by 0.25, skipping for next accumulation\n")
      Rolling_Aggregations[, paste("roll",rolling_aggregations[i], "Hr", method, sep = "") := rep(NA, length(Rolling_Aggregations$dateTime))]
      next
    } else {
      window <- rolling_aggregations[i]/interval
    }

    # cat(paste("====================== Rolling ",method," of ", rolling_aggregations[i], " hours ===========================\n"))
    Rolling_Aggregations[,paste("roll",rolling_aggregations[i], "Hr", sep = ""):= roller(Rolling_Aggregations$Raw, window, fill = NA)]
  }
  dt <- data.table(Rolling_Aggregations)
  return(dt)
}

hydroYearDay <- function(x, hy_cal = 'oct_us_gb') {
  UseMethod('hydroYearDay', x)
}

hydroYearDay.data.table <-function(x, calendar = 'oct_us_gb'){
  date <-as.Date(x$dateTime)

  m <- as.numeric(month(date)) # extract month
  y <- as.numeric(year(date)) # extract year

  ## Create array for hydrological year
  hydroYear <- y

  if (calendar == 'oct_us_gb'){
    ## USA and Great Britain
    # Hydrological year 2010 starts on Oct 1st 2009 and finishes on Sep 30th 2010
    hydroYear[m >= 10] <- (hydroYear[m >= 10] + 1)
    startHY <- as.Date(paste0(hydroYear - 1,'-10-01'))

  } else if(calendar == 'sep_br'){
    ## Brazil
    # Hydrological year 2010 starts on Sep 1st 2009 and finishes on Aug 31st 2010
    hydroYear[m >= 9] <- (hydroYear[m >= 9] + 1)
    startHY <- as.Date(paste0(hydroYear - 1,'-09-01'))

  } else if(calendar == 'apr_cl'){
    ## Chile
    # Hydrological year 2010 starts on Apr 1st 2010 and finishes on Mar 31st 2011
    hydroYear[m <= 3] <-(hydroYear[m <= 3] - 1)
    startHY <- as.Date(paste0(hydroYear, '-04-01'))

  } else {
    stop(paste0('Unkown hydrological year calendar:', calendar))
  }

  ## days since the beginning of the hydro year
  hydroYearDay <- as.numeric(date - startHY + 1)

  if (any(hydroYearDay <1|hydroYearDay>366)){
    stop('Error when computing day of hydro year')
  }
  return(data.table(hydroYear, hydroYearDay))
}

asVol <- function(x, area_km){
  UseMethod('asVol', x)
}

#' @rdname asVol
#' @export
asVol.data.table <- function(x, area_km = NULL){
  tDiff <- difftime(x$dateTime[2],
                    x$dateTime[1],
                    units = 'mins')
  tDiff <- as.numeric(tDiff)
  x[, volume := value * tDiff * 60]
}

#### Remove data
## hydroYear
rmHY <- function(x){
  UseMethod('rmHY', x)
}

rmHY.data.table <- function(x){
  if ('hydroYear' %in% colnames(x)){
    x[, hydroYear := NULL,]
  } else {
    cat('hydroYear field is not present in the data.table\n')
  }
}

## hydroYearDay
rmHYD <- function(x){
  UseMethod('rmHYD', x)
}

rmHYD.data.table <- function(x){
  if ('hydroYearDay' %in% colnames(x)){
    x[, hydroYearDay := NULL,]
  } else {
    cat('hydroYearDay field is not present in the data.table\n')
  }
}

## Volume
rmVol <- function(x){
  UseMethod('rmVol', x)
}

rmVol.data.table <- function(x){
  if ('volume' %in% colnames(x)){
    x[, volume := NULL,]
  } else {
    cat('Volume field is not present in the data.table\n')
  }
}

