#' @title R6 object for imported data via the EA API
#'
#' @description
#' This is the base class for all data imported via the API. A HydroImport contains public raw data and private metadata.
#'
#' @param data Raw data
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
    #' @description
    #' Initialise the new HydroImport object
    initialize = function(data = NA,
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
                          timeZone = "GMT - UTC±00:00",
                          records = NA) {
      self$data <- data
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
      private$timeZone <- timeZone
    },
    #' @description
    #' Display the R6 object
    #' @param . (ignored).
    print = function(.) {
      cli::cli_h1("Class: HydroImport")
      cli::cli_h2("Private:")
      cli::cli_text(paste("{.strong Data Type:}", private$dataType))
      cli::cli_text(paste("{.strong Station name:}", private$stationName))
      cli::cli_text(paste("{.strong WISKI ID:}", private$WISKI))
      cli::cli_text(paste("{.strong Data Type:}", private$parameter))
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
      cli::cli_h2("Public:")
      print(self$data)
      cat("\n")
      cli::cli_text(paste("{.strong For more details use the $methods() function, the format should be as `Object_name`$methods()}"))
    },
    #' @description
    #' Display the methods available in the R6 object
    #' @param . (ignored).
    methods = function(.) {
      ## Collate the methods
      usage <- c(
        "obj$data", "obj$meta()", "obj$asVol()", "obj$hydroYearDay()",
        "obj$rmVol()", "obj$rmHY()", "obj$rmHYD()", "obj$summary()",
        "obj$coords()", "obj$nrfa()", "obj$hourlyAgg()",
        "obj$dailyAgg()", "obj$monthlyAgg()", "obj$annualAgg()",
        "obj$hydroYearAgg()", "obj$rollingAggs()", "obj$dayStats()",
        "obj$quality()"
      )

      desc <- c(
        "Returns the raw data imported via the API",
        "Returns the metadata associated with the object",
        "Calculates the volume of water relative to the time step, see ?asVol",
        "Calculates the hydrological year and day, see ?hydroYearDay",
        "Removes the volume column",
        "Removes the hydroYear column",
        "Removes the hydroYearDay column",
        "Provides a quick summary of the raw data",
        "Returns coordinates from the metadata",
        "Returns the NRFA data from the metadata",
        "Aggregates data by the calendar hours, see ?hourlyAgg",
        "Aggregates data by the calendar days, see ?dailyAgg",
        "Aggregates data by the calendar months, see ?monthlyAgg",
        "Aggregates data by the calendar year, see ?annualAgg",
        "Aggregates data by the hydrological year, see ?hydroYearAgg",
        "Uses user specified aggregation timings, see ?rollingAggs",
        "Produces daily statistics of flow, carried out on hydrological of calendar days",
        "Provides a quick summary table of the data qualiity flags"
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
      boxx(fmt, padding = c(0, 1, 0, 1), header = style_bold(col_blue("Methods")))
    },
    #' @description
    #' Display a summary of the R6 object
    #' @param . (ignored).
    summary = function(.) {
      cat("hydroLoad: \n")
      cat("\tData Type: ", private$dataType, "\n", sep = "")
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
      cli::cli_progress_step("Removing volum column")
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
    #' Return the coordinates of the gauge
    #' @param . (ignored).
    coords = function(.) {
      dt <- data.table(
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
    #' @param type Set the aggrgegation level to either hourly', 'daily', 'monthly', 'annual', or 'hydroYear'
    #' @param method Available aggregation methods include 'min', 'max', 'mean', 'median', and 'sum'
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
        timeZone = private$timeZone
      ))
      cli::cli_progress_step()
    },
    #' @description
    #' Return the different user selecting rolling aggregations
    #' @param method Choose mean, median, min, max, and sum for volumes
    #' @param rolling_aggregations The hourly aggregations selected by the user
    #' @param interval Corrects for the time step of the data, set to 0.25 hours
    rollingAggs = function(method = "mean", rolling_aggregations = c(1, 2, 3, 4, 8, 24, 120), interval = 0.25) {
      dt <- rollingAggs(x = self$data, method = method)
      return(dt)
    },
    #' @description
    #' Returns the metadata as a data.table
    #' @param . (ignored).
    meta = function(.) {
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
        timeZone = private$timeZone,
        records = private$records()
      )
      return(dt)
    },
    #' @description
    #' Daily statistics for imported data
    #' @param plot Set to TRUE, will produce a plot of the statistics
    #' @param year Set to NULL, overlays the mean daily flow for a chosen hydrological year
    dayStats = function(plot = TRUE, year = NULL) {
      yearOI <- year
      if ("hydroYearDay" %in% colnames(self$data)) {
        dt <- dayStats(x = self$data, plot = plot, year = yearOI)
        return(dt)
      } else {
        stop("Please run the `obj$hydroYearDay()` function to generate the hydrological day of the year")
      }
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
      # segments(x0 = -10,
      #          x1 = perc,
      #          y0 = quant,
      #          y1 = quant,
      #          col = 'red',
      #          lty = 2)
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
    quality = function(.) {
      dt <- self$data[, .(count = .N), by = quality]
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
      return(min(self$data$dateTime))
    },
    end = function() {
      if (is.null(dim(self$data))) {
        return(NA)
      }
      return(max(self$data$dateTime))
    },
    # timeStep = function(){
    #   if (is.null(dim(self$data))){
    #     return(NA)
    #   } else {
    #     return(as.numeric(difftime(self$data$dateTime[2],
    #                                self$data$dateTime[1],
    #                                units = 'secs')))
    #   }
    #   # return(as.numeric(difftime(self$data$dateTime[2],
    #   #                            self$data$dateTime[1],
    #   #                            units = 'secs')))
    # },
    timeStep = function() {
      if (is.null(dim(self$data))) {
        return(NA)
      }
      if (grepl("Hourly", private$dataType)) {
        return("Hourly Unstable")
      }
      if (grepl("Daily", private$dataType)) {
        return("Daily Unstable")
      }
      if (grepl("Monthly", private$dataType)) {
        return("Monthly Unstable")
      }
      if (grepl("Annual", private$dataType)) {
        return("Annual Unstable")
      }
      if (grepl("hydroYear", private$dataType)) {
        return("Hydrological Year Unstable")
      }
      if (private$dataType == "Raw Import") {
        return(as.numeric(difftime(self$data$dateTime[2],
          self$data$dateTime[1],
          units = "secs"
        )))
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
    timeZone = NULL,
    records = function() {
      if (is.null(dim(self$data))) {
        return(NA)
      }
      return(length(self$data$dateTime))
    }
  )
)
