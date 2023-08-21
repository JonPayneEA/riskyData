library(R6)
library(cli)

# gauge <- loadAPI(ID = '2001',
#                  measure = 'flow',
#                  period = 900,
#                  type = 'instantaneous',
#                  datapoints = 'range',
#                  from = '2020-07-29',
#                  to = '2022-08-01')
#
# gauge <- loadAPI(ID = '2001',
#                  measure = 'flow',
#                  period = 900,
#                  type = 'instantaneous',
#                  datapoints = 'all')
#
# gauge$hydroYearDay()$hydroYearAgg()
#
# gauge$data
# gauge$quality()
# gauge$methods()
# gauge$meta()
# print(gauge)
# gauge$asVol()
# gauge$hydroYearDay()
# gauge$rmVol()
# gauge$rmHY()
# gauge$rmHYD()
# gauge$summary()
# gauge$start()
# gauge$coords()
# gauge$nrfa()
# gauge$hourlyAgg()
# gauge$dailyAgg()
# gauge$monthlyAgg()
# gauge$annualAgg()
# gauge$hydroYearAgg(method = 'max')
# gauge$rollingAggs()


#' R6 Class representing a hydroLoad import from the API
#'
#' A hydrooLoad contains public raw data and private metadata.
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
      private$timeZone = timeZone

    },
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
    methods = function() {
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
      invisible(self)
    },
    hydroYearDay = function(){
      dt <- hydroYearDay(x = self$data)
      self$data <- data.table(self$data, dt)
      invisible(self)
    },
    rmVol = function(){
      rmVol(x = self$data)
      invisible(self)
    },
    rmHY = function(){
      rmHY(x = self$data)
      invisible(self)
    },
    rmHYD = function(){
      rmHYD(x = self$data)
      invisible(self)
    },
    coords = function() {
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
    },
    quality = function() {
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



