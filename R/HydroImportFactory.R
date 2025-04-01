#' @title R6 object for imported data via the EA API
#'
#' @description
#' This is the base class for all data imported via the API. A HydroImport
#' contains public raw data and private metadata.
#'
#' @param data Raw data
#' @param rating Set to blank, if data requires a rating add to here
#' @param peaks Set to blank, if you calculate peaks they can be stored here
#' @param catchProp Set to blank, proportion of catchment coverage for rain
#' gauges can be stored here
#' @param dataType Details the type of data in this environment
#' @param modifications Details modifications made to the data
#' @param stationName Name of gauge
#' @param riverName River name
#' @param WISKI WISKI ID
#' @param RLOID River Levels on the Internet ID
#' @param stationGuide Station Unique Identifier
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
#' @param timeStep Function that calculates the timestep in seconds
#' @param timeZone Time zone used in data, defaults to GMT
#' @param records Function calculates the number of records
#'
#' @param  dataAgg See [dataAgg]
#' @param  rollingAggs See [rollingAggs]
#'
#' @import R6
#' @import cli
#' @import ggplot2
#' @import data.table
#'
#' @name HydroImport
#' @rdname HydroImport
#' @return A class containing raw data, metadata and methods
#' @export
HydroImportFactory <- R6::R6Class(
  classname = "HydroImport",
  public = list(
    #' @field data Imported data via the API tool. Uses data.table.
    data = NULL,
    #' @field rating Rating parameters. Uses data.table. At the moment only 1 rating can be applied.
    rating = NULL,
    #' @field peaks Calculated peaks, initialised by "$findPeaks".
    peaks = NULL,
    #' @field catchProp Proportional catchment area, used for rain gauge averaging.
    catchProp = NULL,
    #' @description
    #' Initialise the new HydroImport object
    initialize = function(data = NA,
                          rating = NULL,
                          peaks = NULL,
                          catchProp = NULL,
                          dataType = "Raw Import",
                          modifications = NA,
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
                          timeStep = NA,
                          timeZone = NA,
                          records = NA) {
      self$data <- data
      self$rating <- rating
      self$peaks <-  peaks
      self$catchProp <- catchProp
      private$dataType <- dataType
      private$modifications <- modifications
      private$stationName <- stationName
      private$riverName <- riverName
      private$WISKI <- WISKI
      private$RLOID <- RLOID
      private$stationGuide <- stationGuide
      private$baseURL <- baseURL
      private$dataURL <- dataURL
      private$measureURL <- measureURL
      private$idNRFA <- idNRFA
      private$urlNRFA <- urlNRFA
      private$easting <- easting
      private$northing <- northing
      private$latitude <- latitude
      private$longitude <- longitude
      private$area <- area
      private$parameter <- parameter
      private$unitName <- unitName
      private$unit <- unit
      private$datum <- datum
      private$boreholeDepth <- boreholeDepth
      private$aquifer <- aquifer
    },
    #' @description
    #' Display the R6 object
    #' @param . (ignored).
    print = function(.) {
      cli::cli_h1("Class: HydroImport")
      cli::cli_h2("Metadata:")
      cli::cli_text(paste("{.strong Data Type:}", private$dataType))
      cli::cli_text(paste("{.strong Station name:}", private$stationName))
      cli::cli_text(paste("{.strong WISKI ID:}", private$WISKI))
      cli::cli_text(paste("{.strong Parameter Type:}", private$parameter))
      cli::cli_text(paste("{.strong Modifications:}", private$modifications))
      cli::cli_text(paste("{.strong Start:}", private$start()))
      cli::cli_text(paste("{.strong End:}", private$end()))
      cli::cli_text(paste("{.strong Time Step:}", private$timeStep()))
      cli::cli_text(paste("{.strong Observations:}", private$records()))
      cli::cli_text(paste("{.strong Easting:}", private$easting))
      cli::cli_text(paste("{.strong Northing:}", private$northing))
      cli::cli_text(paste("{.strong Longitude:}", private$longitude))
      cli::cli_text(paste("{.strong Latitude:}", private$latitude))
      cat("\n")
      cli::cli_h2("Observed data:")
      print(self$data)
      cat("\n")
      if (!is.null(self$peaks)){
        cli::cli_h2("Peaks data:")
        cat("\n")
        print(self$peaks)
      }
      cat("\n")
      if (!is.null(self$rating)){
        cli::cli_h2("Rating data:")
        cat("\n")
        print(self$rating)
      }
      cat("\n")
      if (!is.null(self$catchProp)){
        cli::cli_h2("Proportion of catchment coverage:")
        cat("\n")
        print(self$catchProp)
      }
      cat("\n")
      cli::cli_text(paste("{.strong For more details use the $methods()
                          function, the format should be as
                          `Object_name`$methods()}"))

    },
    #' @description
    #' Display the methods available in the R6 object
    #' @param . (ignored).
    #' @examples
    #' data(bewdley)
    #' bewdley$methods()
    methods = function(.) {
      ## Collate the methods
      usage <- c(
        "obj$data", "obj$rating", "obj$peaks", "obj$meta()", "obj$asVol()",
        "obj$hydroYearDay()", "obj$rmVol()", "obj$rmHY()", "obj$rmHYD()",
        "obj$summary()", "obj$coords()", "obj$nrfa()", "obj$dataAgg()",
        "obj$rollingAggs()", "obj$dayStats()", "obj$quality()", "obj$missing()",
        "obj$exceed", "obj$plot()", "obj$window()", "obj$rateFlow()",
        "obj$rateStage(), obj$statSummary()"
      )

      desc <- c(
        "Returns the raw data imported via the API",
        "Returns the user imported rating details",
        "Returns the calculated peaks that have been detected",
        "Returns the metadata associated with the object",
        "Calculates the volume of water relative to the time step, see ?asVol",
        "Calculates the hydrological year and day, see ?hydroYearDay",
        "Removes the volume column",
        "Removes the hydroYear column",
        "Removes the hydroYearDay column",
        "Provides a quick summary of the raw data",
        "Returns coordinates from the metadata",
        "Returns the NRFA data from the metadata",
        "Aggregate data by, hour, day, month calendar year and hydroYear",
        "Uses user specified aggregation timings, see ?rollingAggs",
        "Daily statistics of flow, carried out on hydrological or calendar day",
        "Provides a quick summary table of the data quality flags",
        "Quickly finds the positions of missing data points",
        "Show how many times observed data exceed a given threshold",
        "Create a plot of each year of data, by hydrological year",
        "Extracts the subset of data observed between the times start and end.",
        "Converts stage into a rated flow using the specified rating table",
        "Converts flow into a rated stage using the specified rating table",
        "Returns some basic global statistics"
      )

      ## Set the box interior up
      fmt <- ansi_columns(
        paste(
          style_bold(col_red(usage)),
          "  ",
          {
            symbol$arrow_right
          },
          "  ",
          desc
        ),
        width = 90,
        fill = "cols",
        max_cols = 2,
        align = "left",
        sep = "    "
      )
      ## Create the box for the table
      boxx(fmt, padding = c(0, 1, 0, 1),
           header = style_bold(col_blue("Methods")))
    },
    #' @description
    #' Display a summary of the R6 object
    #' @param . (ignored).
    #' @examples
    #' data(bewdley)
    #' bewdley$summary()
    summary = function(.) {
      cat("hydroLoad: \n")
      cat("\tData Type: ", private$dataType, "\n", sep = "")
      cat("\tStation ID: ", private$WISKI, "\n", sep = "")
      cat("\tStart: ", as.character(private$start()), "\n", sep = "")
      cat("\tEnd: ", as.character(private$end()), "\n", sep = "")
      cat("\tTime Zone: ", private$timeZone(), "\n", sep = "")
      cat("\tObservations: ", private$records(), "\n", sep = "")
      cat("\tParameter: ", private$parameter, "\n", sep = "")
      cat("\tUnit Type: ", private$unitName, "\n", sep = "")
      cat("\tUnit: ", private$unit, "\n", sep = "")
    },
    #' @description
    #' Display a small statistics summary of the R6 object
    #' @param . (ignored).
    #' @examples
    #' data(bewdley)
    #' bewdley$statSummary()
    statSummary = function(.) {
      ## Calculate missing data
      missing <-  naRun(dateTime = self$data$dateTime,
                        value = self$data$value,
                        timestep = private$timeStep())
      missing <- sum(missing$timeSteps, na.rm = TRUE)

      ## Calculate basic global stats
      dt <- data.table(
        WISKI_ID = private$WISKI,
        tsStart = private$start(),
        tsEnd = private$end(),
        parameter = private$parameter,
        unit = private$unitName,
        missing = missing,
        min = min(self$data$value,
                  na.rm = TRUE),
        max = max(self$data$value,
                  na.rm = TRUE),
        mean = mean(self$data$value,
                    na.rm = TRUE),
        p10 = quantile(self$data$value,
                       na.rm = TRUE,
                       probs = .10),
        p25 = quantile(self$data$value,
                       na.rm = TRUE,
                       probs = .25),
        p50 = quantile(self$data$value,
                       na.rm = TRUE,
                       probs = .50),
        p75 = quantile(self$data$value,
                       na.rm = TRUE,
                       probs = .75),
        p90 = quantile(self$data$value,
                       na.rm = TRUE,
                       probs = .90),
        p99 = quantile(self$data$value,
                       na.rm = TRUE,
                       probs = .99)
      )
      return(dt)
    },

    #' @description
    #' Calculate the volume of water for flow or rainfall data
    #' @param . (ignored).
    asVol = function(.) {
      cli::cli_progress_step("Calculating volumes")
      asVol(x = self$data)
      invisible(self)
    },
    #' @description
    #' Calculate hydrological year and day
    #' @param . (ignored).
    hydroYearDay = function(.) {
      cli::cli_progress_step("Calculating hydrological year and day")
      dt <- hydroYearDay(x = self$data)
      self$data <- data.table(self$data, dt)
      invisible(self)
    },
    #' @description
    #' Remove the calculated volume
    #' @param . (ignored).
    rmVol = function(.) {
      cli::cli_progress_step("Removing volume column")
      rmVol(x = self$data)
      invisible(self)
    },
    #' @description
    #' Remove the calculated hydroYear
    #' @param . (ignored).
    rmHY = function(.) {
      cli::cli_progress_step("Removing hydroYear column")
      rmHY(x = self$data)
      invisible(self)
    },
    #' @description
    #' Remove the calculated hydrological day
    #' @param . (ignored).
    rmHYD = function(.) {
      cli::cli_progress_step("Removing hydroYearDay column")
      rmHYD(x = self$data)
      invisible(self)
    },
    #' @description
    #' Reorders data by date time, resolving API issues.
    #' @param . (ignored).
    postOrder = function(.){
      self$data <- self$data[order(dateTime)]
      invisible(self)
    },
    #' @description
    #' Detect peaks within the HydroImport or HydroAggs objects
    #' @param levels The number of thresholds to apply against the time series.
    #' Defaults to 100
    #' @param from  Set the location from where the thresholds should start.
    #' Defaults to the bottom 10% value.
    #' @param to Set the location from where the thresholds should end
    #' Defaults to the maximum observed value.
    #' @param gaps Used to pad out the run length encoding, prevents wobbly data
    #' showing multiple exceedances. Set to 96.
    findPeaks = function(levels = NULL, from = NULL, to = NULL, gaps = NULL){
      # if (!is.null(levels)){lev <- levels}
      # if (!is.null(from)){start <- from}
      # if (!is.null(to)){end <- to}
      # if (!is.null(gaps)){gap <- gaps}

      self$peaks <- findPeaks(x = self,
                              levels = 100,
                              from = NULL,
                              to = NULL,
                              gaps = 96)
      invisible(self)

    },
    #' @description
    #' Add a rating table to the HydroImport or HydroAggs objects
    #' @param C parameter
    #' @param A parameter
    #' @param B parameter
    #' @param max parameter
    addRating = function(C = NULL, A = NULL, B = NULL, max = NULL){
      self$rating <- data.table(C, A, B, max)
      self$rating$maxFlow <- C*(max - A)^ B
      invisible(self)
    },
    #' @description
    #' Add a catchment proportion table to the HydroImport or HydroAggs objects
    #' @param total total area of catchment
    #' @param area area of proportion covered by rain gauge
    #' @param B parameter
    #' @param max parameter
    addCatchProp = function(total = NULL, area = NULL){
      self$catchProp <- data.table(total, area)
      invisible(self)
    },
    #' @description
    #' Converts stage to flow using the supplied rating. It uses the following
    #' equation;
    #' Q = C(h - a)^b
    #' @param start defaults to 0, change to set a different start point in the
    #' rating conversion.
    #' @param full If set to FALSE (default) a data table of stage and flows
    #' will be supplied. If set to TRUE a new `HydroImport` object is created.
    rateFlow = function(start = 0, full = FALSE){
      if(is.null(self$rating)){
        stop("Please supply a rating table")
      }
      if(private$parameter != "Level"){
        stop("Please ensure the supplied data is stage")
      }
      if(class(self)[1] != "HydroImport"){
        stop("Please use the raw data import, only class HydroImport can have a
             rating applied")
      }

      ## Set and identifier for each row
      ratingTbl <- data.frame(row = seq_along(self$rating$C),
                             # limb = paste0('Limb_', seq_along(ratings[,1])),
                             self$rating)
      ## Cut the stage data so it can identify a limb
      cuts <- cut(self$data$value,
                  breaks = c(start, ratingTbl$max),
                  labels = ratingTbl$row)
      ## Max number limb used
      total <- max(ratingTbl$row)
      ##! Sites beyond the max limb are NA
      ## Extend the maximum limb replacing NAs with the max row from ratingTbl
      limbs <- replace(cuts, is.na(cuts), total)

      ## Calculate flow
      flow <- c()
      for(i in seq_along(self$data$value)){
        A <- ratingTbl$A[limbs[i]]
        B <- ratingTbl$B[limbs[i]]
        C <- ratingTbl$C[limbs[i]]
        # print(A * (data[i] - B)^C)
        est <- C * (self$data$value[i] - A)^B
        # print(est)
        flow[i] <- est
      }
      dt <- self$data
      dt$value <-  flow
      if(full == TRUE){
        # cli::cli_progress_step("Exporting to HydroImport container")
        hI <- HydroImportFactory$new(
          data = self$data,
          dataType = "Rated flow based on stage/discharge rating",
          modifications = ifelse(is.na(private$modifications),
                                 paste("Rated flow calculated"),
                                 append(
                                   private$modifications,
                                   paste("Rated flow calculated")
                                 )
          ),
          rating = self$rating,
          peaks = self$peaks,
          catchProp = self$catchProp,
          timeStep = type,
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
          parameter = "Flow",
          unitName = "m3/s",
          unit = "http://qudt.org/1.1/vocab/unit#CubicMeterPerSecond",
          datum = private$datum,
          boreholeDepth = private$boreholeDepth,
          aquifer = private$aquifer,
          timeZone = private$timeZone
        )
        # cli::cli_progress_step()
        return(hI)
      } else {
      dfAll <- data.table(stage = self$data$value, flow = flow, limb = limbs)
      return(dfAll)
      }
    },
    #' @description
    #' Converts flow to stage using the supplied rating. It uses the following
    #' equation;
    #' h = ((Q/a)^(1/C))- b
    #' @param start defaults to 0, change to set a different start point in the
    #' rating conversion.
    #' @param full If set to FALSE (default) a data table of stage and flows
    #' will be supplied. If set to TRUE a new `HydroImport` object is created.
    rateStage = function(start = 0, full = FALSE){
      if(is.null(self$rating)){
        stop("Please supply a rating table")
      }
      if(private$parameter != "Flow"){
        stop("Please ensure the supplied data is flow")
      }
      if(class(self)[1] != "HydroImport"){
        stop("Please use the raw data import, only class HydroImport can have a
             rating applied")
      }

      ## Set and identifier for each row
      ratingTbl <- data.frame(row = seq_along(self$rating$C),
                              # limb = paste0('Limb_', seq_along(ratings[,1])),
                              self$rating)
      ## Cut the stage data so it can identify a limb
      cuts <- cut(self$data$value,
                  breaks = c(start, ratingTbl$maxFlow),
                  labels = ratingTbl$row)
      ## Max number limb used
      total <- max(ratingTbl$row)
      ##! Sites beyond the max limb are NA
      ## Extend the maximum limb replacing NAs with the max row from ratingTbl
      limbs <- replace(cuts, is.na(cuts), total)

      ## Calculate flow
      stage <- c()
      for(i in seq_along(self$data$value)){
        A <- ratingTbl$A[limbs[i]]
        B <- ratingTbl$B[limbs[i]]
        C <- ratingTbl$C[limbs[i]]
        # print(A * (data[i] - B)^C)
        est <- ((self$data$value[i] / C) ^ (1 / B)) +A
        # print(est)
        stage[i] <- est
      }
      dt <- self$data
      dt$value <-  stage
      if(full == TRUE){
        # cli::cli_progress_step("Exporting to HydroImport container")
        hI <- HydroImportFactory$new(
          data = self$data,
          dataType = "Rated stage based on stage/discharge rating",
          modifications = ifelse(is.na(private$modifications),
                                 paste("Rated stage calculated"),
                                 append(
                                   private$modifications,
                                   paste("Rated stage calculated")
                                 )
          ),
          rating = self$rating,
          peaks = self$peaks,
          catchProp = self$catchProp,
          timeStep = type,
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
          parameter = "Level",
          unitName = "m",
          unit = "http://qudt.org/1.1/vocab/unit#Meter",
          datum = private$datum,
          boreholeDepth = private$boreholeDepth,
          aquifer = private$aquifer,
          timeZone = private$timeZone
        )
        # cli::cli_progress_step()
        return(hI)
      } else {
        dfAll <- data.table(stage = stage, flow = self$data$value, limb = limbs)
        return(dfAll)
      }
    },
    #' @description
    #' Extract windows of data from object using start and end date/times
    #' @param from Start date/time
    #' @param to end date/time, if kept blank will default to last time step
    #' @param export Set as "dt", this exports a data.table of public data. If
    #' set to "snip" then the R6 object is modified.
    window = function(start = NULL, end = NULL, export = "dt"){
      if (is.null(start)){
        stop("Please include a start date for the from argument")
      }

      #! R assumes that dates are BST during the summer months
      #! This causes an hour shift when running queries

      ## Constrain dates to GMT
      start <- as.POSIXct(start,
                          format = "%Y-%m-%d %H:%M",
                          tz = "GMT"
      )

      # If the to argument is null the last record is assumed
      if (is.null(end)){
        end <- as.POSIXct(tail(self$data$dateTime, 1),
                          format = "%Y-%m-%d %H:%M",
                          tz = "GMT"
        )
      } else {
        end <- as.POSIXct(end,
                          format = "%Y-%m-%d %H:%M",
                          tz = "GMT"
        )
      }
      ## Snipping the data
      dt <- self$data[dateTime >= start & dateTime <= end,,]
      if(export == 'dt'){
        return(dt)
      } else {
        self$data <- dt
        mod <- paste0("Data window of ", start, " - ", end)
        if (is.na(private$modifications)){
          private$modifications <- mod
        } else {
          private$modifications <- paste0(private$modifications, ", ", mod)
        }
        invisible(self)
      }
    },
    #' @description
    #' Return the coordinates of the gauge
    #' @param . (ignored).
    #' @examples
    #' data(bewdley)
    #' bewdley$coords()
    coords = function(.) {
      dt <- data.table(
        stationName = private$stationName,
        WISKI = private$WISKI,
        Easting = private$easting,
        Northing = private$northing,
        Latitude = private$latitude,
        Longitude = private$longitude
      )
      return(dt)
    },
    #' @description
    #' Return the NRFA details of the gauge
    #' @param . (ignored).
    #' @examples
    #' data(bewdley)
    #' bewdley$nrfa()
    nrfa = function(.) {
      dt <- data.table(
        WISKI = private$WISKI,
        codeNRFA = private$idNRFA,
        urlNRFA = private$urlNRFA
      )
      return(dt)
    },
    #' @description
    #' Return aggregated data
    #' @param type Set the aggrgegation level to either hourly', 'daily',
    #' 'monthly', 'annual', or 'hydroYear'
    #' @param method Available aggregation methods include 'min', 'max',
    #' 'mean', 'median', and 'sum'
    dataAgg = function(type = NULL, method = NULL) {
      dt <- dataAgg(
        x = self$data,
        type = type,
        method = method
      )
      cli::cli_progress_step("Exporting to HydroAggs container")
      return(HydroAggsFactory$new(
        data = dt,
        dataType = paste("Aggregated",
          paste(type, method),
          sep = " - "
        ),
        modifications = ifelse(is.na(private$modifications),
          paste(type, method),
          append(
            private$modifications,
            paste(type, method)
          )
        ),
        rating = self$rating,
        peaks = self$peaks,
        catchProp= self$catchProp,
        timeStep = type,
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
      ))
      cli::cli_progress_step()
    },
    #' @description
    #' Return the different user selecting rolling aggregations
    #' @param method Choose mean, median, min, max, and sum for volumes
    #' @param rolls The hourly aggregations selected by the user
    #' @param interval Corrects for the time step of the data, set to 0.25 hours
    rollingAggs = function(method = "mean",
                           rolls = c(1, 2, 3, 4, 8, 24, 120),
                           interval = 0.25) {
      dt <- rollingAggs(x = self$data, method = method)
      return(dt)
    },
    #' @description
    #' Returns the metadata as a data.table
    #' @param transform Set to FALSE. If TRUE the data.table is transformed
    #' which increases readability in the console.
    #' @examples
    #' data(bewdley)
    #' bewdley$meta()
    meta = function(transform = FALSE) {
      dt <- data.table(
        dataType = private$dataType,
        modifications = private$modifications,
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
        timeStep = private$timeStep(),
        timeZone = private$timeZone(),
        records = private$records()
      )
      if (transform == FALSE){
        return(dt)
      } else {
        return(t(dt))
      }
    },
    #' @description
    #' Daily statistics for imported data
    #' @param plot Set to TRUE, will produce a plot of the statistics
    #' @param year Set to NULL, overlays the mean daily flow for a chosen
    #' hydrological year
    dayStats = function(plot = TRUE, year = NULL) {
      yearOI <- year
      if ("hydroYearDay" %in% colnames(self$data)) {
        dt <- dayStats(x = self$data, plot = plot, year = yearOI)
        return(dt)
      } else {
        stop("Please run the `obj$hydroYearDay()` function to generate the
             hydrological day of the year")
      }
    },
    #' @description
    #' Plot the data by years
    #' @param wrap Set to 3, use this to specify how many of columns of graphs
    #' there should be
    #' @param cumul Used to provide cumulative rainfall totals on rainfall
    #' @param title Used to add a plot title and subtitle
    #' @param theme Used to decide on EA colour them
    #' @param dir Use this to save the plots
    #' @examples
    #' data(bewdley)
    #' bewdley$plot()
    plot = function(wrap = TRUE, cumul = FALSE, title = TRUE, theme = NULL,
                    dir = NULL){
      # Function runs on hydrological year
      ## Assert whether hydroYear has been calculated
      if (!"hydroYear" %in% colnames(self$data)) {
        stop("hydroYear field is not present in the data.table\n")
      }

      # Find the years present in the data
      # years <- unique(self$data$hydroYear)

      # Construct the y axis label using the private metadata
      yAxis <- paste0(private$parameter, " (", private$unitName, ")")

      if (length(unique(self$data$hydroYear)) < 3){
        cols <- length(unique(self$data$hydroYear))
      } else {
        cols <- 3
      }

      if(private$parameter == 'Rainfall'){
        if(wrap == TRUE){
          if(cumul == TRUE) {
            cumul <- self$data[, .(dateTime,
                                   value = cumsumNA.numeric(value)),
                               by = hydroYear]
            plot <- ggplot(cumul, aes(dateTime, value)) +
              geom_line() +
              labs(x = NULL, y = yAxis) +
              scale_x_datetime(labels = scales::date_format("%b")) + # this sets
              # it to only show the month. Lord knows why month ended up at "%b"
              facet_wrap(~ hydroYear, scales = 'free_x', ncol = cols)
          } else {
            plot <- ggplot(self$data, aes(dateTime, value)) +
              geom_col() +
              labs(x = NULL, y = yAxis) +
              scale_x_datetime(labels = scales::date_format("%b")) + # this sets
              # it to only show the month. Lord knows why month ended up at "%b"
              facet_wrap(~ self$data$hydroYear, scales = 'free_x', ncol = cols)
          }
        } else {
          if (cumul == TRUE) {
            cumul <- self$data[, .(dateTime,
                                   value = cumsumNA.numeric(value))]
            plot <- ggplot(cumul, aes(dateTime, value)) +
              geom_line() +
              labs(x = NULL, y = yAxis)
          } else {
            plot <- ggplot(self$data, aes(dateTime, value)) +
              geom_col() +
              labs(x = 'Date', y = yAxis)
          }
        }
      } else {
        if (wrap == TRUE){
          plot <- ggplot(self$data, aes(dateTime, value)) +
            geom_line(size = 0.1)  +
            labs(x = NULL, y = yAxis) +
            scale_x_datetime(labels = scales::date_format("%b")) + # this sets
            # it to only show the month. Lord knows why month ended up at "%b"
            facet_wrap(~ self$data$hydroYear, scales = 'free_x', ncol = cols)
        } else {
          plot <- ggplot(self$data, aes(dateTime, value)) +
            geom_line(size = 0.1)  +
            labs(x = 'Date', y = yAxis)
        }
      }
      if (title == TRUE){
        plot <- plot +
          ggplot2::ggtitle(label = private$stationName,
                           subtitle = paste(private$parameter,
                                            "data from", private$start(),
                                            "to", private$end())) +
          ggplot2::theme(plot.title=element_text(family = "",
                                                 face = "bold",
                                                 colour = "#00A33B",
                                                 size = 26),
                         panel.spacing = unit(1, "lines"))
      }
      if (!is.null(dir)){
        if(cols == 1){
          rowsN <-1
        } else {
        rowsN <- ceiling((max(self$data$hydroYear, na.rm = TRUE) -
                           min(self$data$hydroYear, na.rm = TRUE)) / 3)
      }
        ggplot2::ggsave(plot = plot,
                        filename = paste0(dir, "/", private$stationName, "_",
                                          private$parameter, ".png"),
                        width = 210,
                        height = 297 *(1 + 5 * rowsN) / 26,
                        units = "mm",
                        dpi = 300,
                        bg = "white")
      }
      return(plot)
    },
    #' @description
    #' Flow duration curve of data
    #' @param perc Determine flow percentiles. Set to c(5, 25, 75, 95)
    flowDuration = function(perc = c(5, 25, 75, 95)) {
      cli::cli_progress_step("Calculating quartiles")
      dataClean <- na.omit(self$data, cols = "value")
      quant <- quantile(dataClean$value, 1 - (perc / 100))
      dt <- data.table(percentile = perc, value = quant)
      cli::cli_progress_step("Generating plot")
      plot(seq(from = 0, to = 100, length.out = length(dataClean$value)),
        dataClean[order(value, decreasing = TRUE)]$value,
        type = "l",
        xlab = "Percentile",
        ylab = "Flow",
        lwd = 2
      )
      segments(
        x0 = perc,
        x1 = perc,
        y0 = -100,
        y1 = quant,
        col = "red",
        lty = 2,
        lwd = 2
      )
      return(dt)
    },
    #' @description
    #' Displays the number of observations under each quality flag
    #' @param . (ignored).
    #' @examples
    #' data(bewdley)
    #' bewdley$quality()
    quality = function(.) {
      dt <- self$data[, .(count = .N), by = quality]
      return(dt)
    },

  #' @description
  #' Finds missing values in the data series
  #' @param . (ignored).
  #' @examples
  #' data(bewdley)
  #' bewdley$missing()
  missing = function(.) {
    dt <- naRun(dateTime = self$data$dateTime,
                value = self$data$value,
                timestep = private$timeStep())
    return(dt)
  },
  #' @description
  #' Shows threshold exceedance statistics
  #' @param threshold The threshold at which to test against
  #' @param gapWidth This allows you to ignore a set number of time steps
  #' beneath a threshold between two or more events
  #' @param . (ignored).
  #' @examples
  #' data(bewdley)
  #' bewdley$exceed(threshold = 200, gapWidth = 20)
  exceed = function(threshold = 0, gapWidth = 0) {
    dt <- exceed(dateTime = self$data$dateTime,
                 value = self$data$value,
                 timeStep = private$timeStep(),
                 gapWidth = gapWidth,
                 threshold = threshold)
    return(dt)
  }
  ),

  ## Set private fields (hidden from normal view)
  private = list(
    stationName = NULL,
    start = function() {
      if (is.null(dim(self$data))) {
        return(NA)
      }
      return(head(self$data$dateTime, 1))
    },
    end = function() {
      if (is.null(dim(self$data))) {
        return(NA)
      }
      return(tail(self$data$dateTime, 1))
    },
    timeStep = function() {
      if (is.null(dim(self$data))) {
        return(NA)
      }
      if (grepl("hourly", private$dataType)) {
        return("Hourly Unstable")
      }
      if (grepl("daily", private$dataType)) {
        return("Daily Unstable")
      }
      if (grepl("monthly", private$dataType)) {
        return("Monthly Unstable")
      }
      if (grepl("annual", private$dataType)) {
        return("Annual Unstable")
      }
      if (grepl("hydroYear", private$dataType)) {
        return("Hydrological Year Unstable")
      }
      if (private$dataType == "Raw Import") {
        return(as.numeric(difftime(self$data$dateTime[2], self$data$dateTime[1],
                                   units = "secs")))
      }
    },
    riverName = NULL,
    dataType = NULL,
    modifications = NULL,
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
    timeZone = function(){
      if (is.null(dim(self$data))) {
        return(NA)
      }
      return(attr(self$data$dateTime[1],"tzone"))
    },
    records = function() {
      if (is.null(dim(self$data))) {
        return(NA)
      }
      return(length(self$data$dateTime))
    }
  )
)
