#' @title R6 object for imported data via the EA API
#'
#' @description
#' This is the base class for all data imported via the API. A HydroAggs contains public raw data and private metadata.
#'
#' @param data Raw data
#' @param dataType Details the type of data in this environment
#' @param stationName Name of gauge
#' @param riverName River name
#' @param modifications Details modifications made to the data
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
#' @name HydroAggs
#' @rdname HydroAggs
#' @return A class containing raw data, metadata and methods
#' @export
HydroAggsFactory <- R6::R6Class(
  classname = "HydroAggs",
  inherit = HydroImportFactory,
  public = list(
    #' @description
    #' Display the R6 object
    #' @param . (ignored).
    print = function(.) {
      cli::cli_h1("Class: HydroAggs")
      cli::cli_h2("Private:")
      cli::cli_text(paste("{.strong Data Type:}", paste(private$dataType)))
      cli::cli_text(paste("{.strong Station name:}", private$stationName))
      cli::cli_text(paste("{.strong WISKI ID:}", private$WISKI))
      cli::cli_text(paste("{.strong Data Type:}", private$parameter))
      cli::cli_text(paste("{.strong Modifications:}", private$modifications))
      cli::cli_text(paste("{.strong Start:}", private$start()))
      cli::cli_text(paste("{.strong End:}", private$end()))
      cli::cli_text(paste("{.strong Time Step:}", private$timeStep()))
      cli::cli_text(paste("{.strong Easting:}", private$easting))
      cli::cli_text(paste("{.strong Northing:}", private$northing))
      cli::cli_text(paste("{.strong Longitude:}", private$longitude))
      cli::cli_text(paste("{.strong Latitude:}", private$latitude))
      cat("\n")
      cli::cli_h2("Public:")
      print(self$data)
      cat("\n")
      cli::cli_text(paste("{.strong For more details use the $methods() function, the format should be as `Object_name`$methods()}"))

    }
  )
)



# HydroAggsFactory$new(gauge$data)
#
# HydroAggsFactory$new(data = series[,-1:-2],
#                      stationName = metaD$Data[[1]],
#                      riverName = metaD$Data[[2]],
#                      WISKI = metaD$Data[[3]],
#                      RLOID = metaD$Data[[4]],
#                      stationGuide = metaD$Data[[5]],
#                      baseURL = metaD$Data[[6]],
#                      dataURL = metaD$Data[[7]],
#                      measureURL = metaD$Data[[8]],
#                      idNRFA = metaD$Data[[9]],
#                      urlNRFA = metaD$Data[[10]],
#                      easting = metaD$Data[[11]],
#                      northing = metaD$Data[[12]],
#                      latitude = metaD$Data[[13]],
#                      longitude = metaD$Data[[14]],
#                      area = metaD$Data[[15]],
#                      parameter = metaD$Data[[16]],
#                      unitName = metaD$Data[[17]],
#                      unit = metaD$Data[[18]],
#                      datum = metaD$Data[[19]],
#                      boreholeDepth = metaD$Data[[20]],
#                      aquifer = metaD$Data[[21]],
#                      timeZone = metaD$Data[[22]])
