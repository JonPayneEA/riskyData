#' @title R6 object for imported data via the EA API
#'
#' @description
#' This is the base class for all data imported via the API. A hydroLoad contains public raw data and private metadata.
#'
#' @param data Raw data
#' @param stationName Name of gauge
#' @param riverName River name
#' @param WISKI WISKI ID
#' @param RLOID River Levels on the Internet ID
#' @param stationGuide Station Unique IDentifier
#' @param baseURL Base URL used in the API
#' @param dataURL End of URL that downloaded the raw data
#' @param measureURL Primarily used in gaining the metadata
#' @param idNRFA National River Flow Archive station identifier
#' @param urlNRFA National River Flow Archive station URL
#' @param easting Easting coordinate
#' @param northing Northing coordinate
#' @param latitude Latitude coordinate
#' @param longitude Longitude coordinate
#' @param area Catchment area of flow/level gauge
#' @param parameter Details the collected data e.g. flow, level etc.
#' @param unitName Unit name used
#' @param unit Details on the unit of measurement
#' @param datum Datum of the gauge, if available
#' @param boreholeDepth Depth of borehole recording
#' @param aquifer Details on the aquifer type
#' @param start Function calculates the date of the first imported record
#' @param end Function calculates the date of the last imported record
#' @param timeZone Time zone used in data, defaults to GMT
#' @param records Function calculates the number of records
#'
#' @param  hourlyAgg See [hourlyAgg]
#' @param  dailyAgg See [dailyAgg]
#' @param  monthlyAgg See [monthlyAgg]
#' @param  annualAgg See [annualAgg]
#' @param  hydroYearAgg See [hydroYearAgg]
#' @param  rollingAggs See [rollingAggs]
#'
#' @import R6
#' @import cli
#'
#' @name hydroLoad
#' @rdname hydroLoad
#' @return A class containing raw data, metadata and methods
#' @export
hydroLoad <- R6::R6Class(
  classname = "hydroLoad",
  public = list(
    #' @field data Imported data via the API tool. Uses data.table.
    data = NULL,
    #' @description
    #' Initialise the new hydroLoad object
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
      private$timeZone = timeZone

    },
    #' @description
    #' Display the R6 object
    #' @param ... (ignored).
    print = function(...) {
      cli::cli_h1("Class: hydroLoad")
      cli::cli_h2("Private:")
      cli::cli_text(paste("{.strong Station name:}", private$stationName))
      cli::cli_text(paste("{.strong WISKI ID:}", private$WISKI))
      cli::cli_text(paste("{.strong Data Type:}", private$parameter))
      cli::cli_text(paste("{.strong Start:}", private$start()))
      cli::cli_text(paste("{.strong End:}", private$end()))
      cli::cli_text(paste("{.strong Easting:}", private$easting))
      cli::cli_text(paste("{.strong Northing:}", private$northing))
      cli::cli_text(paste("{.strong Longitude:}", private$longitude))
      cli::cli_text(paste("{.strong Latitude:}", private$latitude))
      cat("\n")
      cli::cli_h2("Public:")
      print(self$data)
      cat("\n")
      cli::cli_text(paste("{.strong For more details use the $methods() function, the format should be as `Object_name`$methods()}"))

    },
    #' @description
    #' Display the methods available in the R6 object
    #' @param ... (ignored).
    methods = function(...) {
      ## Collate the methods
      usage <- c('obj$data', 'obj$meta()', 'obj$asVol()', 'obj$hydroYearDay()',
                 'obj$rmVol()', 'obj$rmHY()', 'obj$rmHYD()', 'obj$summary()',
                 'obj$coords()', 'obj$nrfa()', 'obj$hourlyAgg()',
                 'obj$dailyAgg()', 'obj$monthlyAgg()','obj$annualAgg()',
                 'obj$hydroYearAgg()', 'obj$rollingAggs()', 'obj$quality()')

      desc <- c('Returns the raw data imported via the API',
                'Returns the metadata associated with the object',
                'Calculates the volume of water relative to the time step, see ?asVol',
                'Calculates the hydrological year and day, see ?hydroYearDay',
                'Removes the volume column',
                'Removes the hydroYear column',
                'Removes the hydroYearDay column',
                'Provides a quick summary of the raw data',
                'Returns coordinates from the metadata',
                'Returns the NRFA data from the metadata',
                'Aggregates data by the calendar hours, see ?hourlyAgg',
                'Aggregates data by the calendar days, see ?dailyAgg',
                'Aggregates data by the calendar months, see ?monthlyAgg',
                'Aggregates data by the calendar year, see ?annualAgg',
                'Aggregates data by the hydrological year, see ?hydroYearAgg',
                'Uses user specified aggregation timings, see ?rollingAggs',
                'Provides a quick summary table of the data qualiity flags')

      ## Set the box interior up
      fmt <- ansi_columns(
        paste(style_bold(col_red(usage)),"  ", {symbol$arrow_right},"  ", desc),
        width = 90,
        fill = "cols",
        max_cols=2,
        align = "left",
        sep = "    "
      )
      ## Create the box for the table
      boxx(fmt, padding = c(0,1,0,1), header = style_bold(col_blue("Methods")))
    },
    #' @description
    #' Display a summary of the R6 object
    #' @param ... (ignored).
    summary = function(...) {
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
    #' @description
    #' Calculate the volume of water for flow or rainfall data
    #' @param ... (ignored).
    asVol = function(...){
      asVol(x = self$data)
      invisible(self)
    },
    #' @description
    #' Calculate hydrological year and day
    #' @param ... (ignored).
    hydroYearDay = function(...){
      dt <- hydroYearDay(x = self$data)
      self$data <- data.table(self$data, dt)
      invisible(self)
    },
    #' @description
    #' Remove the calculated volume
    #' @param ... (ignored).
    rmVol = function(...){
      rmVol(x = self$data)
      invisible(self)
    },
    #' @description
    #' Remove the calculated hydroYear
    #' @param ... (ignored).
    rmHY = function(...){
      rmHY(x = self$data)
      invisible(self)
    },
    #' @description
    #' Remove the calculated hydrological day
    #' @param ... (ignored).
    rmHYD = function(...){
      rmHYD(x = self$data)
      invisible(self)
    },
    #' @description
    #' Return the coordinates of the gauge
    #' @param ... (ignored).
    coords = function(...) {
      dt <- data.table(Easting = private$easting,
                       Northing = private$northing,
                       Latitude = private$latitude,
                       Longitude = private$longitude
      )
      return(dt)
    },
    #' @description
    #' Return the NRFA details of the gauge
    #' @param ... (ignored).
    nrfa = function(...) {
      dt <- data.table(WISKI = private$WISKI,
                       codeNRFA = private$idNRFA,
                       urlNRFA = private$urlNRFA)
      return(dt)
    },
    #' @description
    #' Return the hourly aggregated data
    #' @param method Choose mean, median, min, max, and sum for volumes
    hourlyAgg = function(method = 'mean') {
      dt <- hourlyAgg(x = self$data, method = method)
      return(dt)
    },
    #' @description
    #' Return the daily aggregated data
    #' @param method Choose mean, median, min, max, and sum for volumes
    dailyAgg = function(method = 'mean') {
      dt <- dailyAgg(x = self$data, method = method)
      return(dt)
    },
    #' @description
    #' Return the monthly aggregated data
    #' @param method Choose mean, median, min, max, and sum for volumes
    monthlyAgg = function(method = 'mean') {
      dt <- monthlyAgg(x = self$data, method = method)
      return(dt)
    },
    #' @description
    #' Return the annually aggregated data
    #' @param method Choose mean, median, min, max, and sum for volumes
    annualAgg = function(method = 'mean') {
      dt <- annualAgg(x = self$data, method = method)
      return(dt)
    },
    #' @description
    #' Return the aggregated data by hydrological year
    #' @param method Choose mean, median, min, max, and sum for volumes
    hydroYearAgg = function(method = 'mean') {
      dt <- hydroYearAgg(x = self$data, method = method)
      return(dt)
    },
    #' @description
    #' Return the different user selecting rolling aggregations
    #' @param method Choose mean, median, min, max, and sum for volumes
    #' @param rolling_aggregations The hourly aggregations selected by the user
    #' @param interval Corrects for the time step of the data, set to 0.25 hours
    rollingAggs = function(method = 'mean', rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25) {
      dt <- rollingAggs(x = self$data, method = method)
      return(dt)
    },
    #' @description
    #' Returns the metadata as a data.table
    #' @param ... (ignored).
    meta = function(...) {
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
    },
    #' @description
    #' Displays the number of observations under each quality flag
    #' @param ... (ignored).
    quality = function(...) {
      dt <- self$data[, .(count = .N), by = quality]
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

