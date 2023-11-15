#' @title Pull data from the Environment Agency's API
#'
#' @description The loadAPI function is used to interogate the EAs API. Data for
#'  all station can be pulled, or when specified to a single site various
#'  datasets can be pulled.
#'
#' @param ID Use to specify a particular WISKI ID or for when downloading all
#' stations 'wiski' or 'nrfa' to filter results. Additionally to access the
#' real time APIs use "flood" for all critical sites and "tidal" for the
#' category A tide gauges.
#' @param measure Use this when exporting observations to select the available
#' parameter. Generally 'flow', 'level', or 'groundwater'
#' @param period  This is so you can select the time steps available, generally
#' 900 (15 min) or 86400 (daily)
#' @param type  Selects the data type, instantaneous, min, max etc.
#' @param datapoints Allows you to decide where in the time series you are
#' pulling the data from
#' @param from First time step of choice
#' @param to Last time step of choice
#' @param lat Latitude coordinate for when searching by geographic area
#' @param long Longitude coordinate for when searching by geographic area
#' @param easting Easting coordinate for when searching by geographic area
#' @param northing Northing coordinate for when searching by geographic area
#' @param dist Distance in km for how far you wish to search
#' @param obsProperty Used to filter the stations when identifying sites.
#' Available metrics;
#' "waterFlow", "waterLevel", "rainfall", "groundwaterLevel", "wind",
#' "temperature", "ammonium", "dissolved-oxygen", "conductivity", "ph",
#' "temperature", "turbidity", "nitrate", "chlorophyll", "salinity",
#' "bga", "fdom"
#' @param meta Set as TRUE, exports metadata for gauge with the data request
#' @param rtExt Set to NULL, if TRUE hydrological data series are extended with
#' the real time API
#'
#' @import data.table
#' @import jsonlite
#' @import R6
#' @import cli
#' @import tidyr
#' @import ggplot2
#'
#' @return Various outputs see vignettes
#' @export
#'
#' @examples
#' #loadAPI()
#'
#' loadAPI(easting = 378235, northing = 276165, dist = 30, ID = "nrfa")
#'
#' loadAPI(ID = "L1207")
#'
#' loadAPI(
#'   ID = "L1207",
#'   measure = "level",
#'   period = 900,
#'   type = "instantaneous",
#'   datapoints = "earliest"
#' )
#'
#' dt <- loadAPI(
#'   ID = "L1207",
#'   measure = "level",
#'   period = 900,
#'   type = "instantaneous",
#'   datapoints = "all"
#' )
#'
#' with(dt$data, plot(value ~ dateTime,
#'   type = "l",
#'   xlab = "Time",
#'   ylab = "Stage (mAoD)"
#' ))
loadAPI <- function(ID = NULL, measure = NULL, period = NULL,
                    type = NULL, datapoints = "standard",
                    from = NULL, to = NULL, lat = NULL, long = NULL,
                    easting = NULL, northing = NULL, dist = NULL,
                    obsProperty = NULL, meta = TRUE, rtExt = FALSE) {
  # Initial start up check ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  obsVars <- c(
    "waterFlow", "waterLevel", "rainfall", "groundwaterLevel", "wind",
    "temperature", "ammonium", "dissolved-oxygen", "conductivity", "ph",
    "temperature", "turbidity", "nitrate", "chlorophyll", "salinity",
    "bga", "fdom"
  )
  if (!is.null(obsProperty) && !obsProperty %in% obsVars) {
    return(warning("Observed property does not match those available"))
  }

  if (!is.null(period) && period > 900 && rtExt == TRUE){
    stop("If you wish to use realtime dat in your exports, it must have
         period = 900")
  }

  # Start up of main function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dataType <- type # Fixes data.table filter issue
  timestep <- period # Fixes data.table filter issue
  # Base link for the EAs API
  baselink <- "http://environment.data.gov.uk/hydrology/id/stations.json"

  #~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~ List all sites ~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~#

  ##! Base hydrology API
  ## Return all available stations from hydrology API ~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ((is.null(ID) || ID %in% c("all", "wiski", "nrfa")) &
      is.null(easting) & is.null(lat) & is.null(long) & is.null(northing) &
      is.null(dist)) {
    ## Limit set to 20,000 (current API is ~8,000) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    datalink <- paste0(baselink, "?_limit=20000")
    dt <- data.table(jsonlite::fromJSON(datalink)$items)

    ## Unnesting observed property is slightly more difficult ~~~~~~~~~~~~~~~~~~
    ## Stored as dataframe list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove URL and convert dataframes to string ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dt_obs <- dt$observedProperty
    lst <- list()
    for (i in seq_along(dt_obs)) {
      lst[[i]] <- basename(dt_obs[[i]]$`@id`)
    }
    dt$observedProperty <- lst
    ## Check if any columns are class list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ("list" %in% sapply(dt, class)) {
      dt <- data.table(tidyr::unnest(dt, c(
        easting, northing, lat, long
      ),
      keep_empty = TRUE
      ))

      # Unnesting again properties that can be variable in length
      properties <- c('wiskiID', 'label', 'riverName', 'observedProperty')
      for(i in properties){
        dt <- data.table(tidyr::unnest(dt, i,keep_empty = TRUE))
        }
    }

    ## Select relevant columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data_level <- dt[, .(
      wiskiID, label, riverName,
      observedProperty, easting, northing, lat,
      long, dateOpened, catchmentArea, nrfaStationID
    ), ]
    # data_level$observedProperty <- unlist(lst)
    if(is.null(ID)){
      return(data_level)
    }
    if(ID == 'all'){
      return(data_level)
    }

    if (ID == "wiski") {
      data_level <- na.omit(data_level, cols = "wiskiID")
    }
    if (ID == "nrfa") {
      data_level <- na.omit(data_level, cols = "nrfaStationID")
    }
    if (is.null(obsProperty)) {
      return(data_level)
    } else {
      return(data_level[observedProperty == obsProperty, , ])
    }

    # if (any(!is.null(ID) & ID == "wiski")) {
    #   data_level <- na.omit(data_level, cols = "wiskiID")
    # }
    # if (any(!is.null(ID) & ID == "nrfa")) {
    #   data_level <- na.omit(data_level, cols = "nrfaStationID")
    # }
    # if (is.null(obsProperty)) {
    #   return(data_level)
    # } else {
    #   return(data_level[observedProperty == obsProperty, , ])
    # }
  }

  ##! Base real time API
  ## Return all sites from realtime API
  if (ID == 'flood' &&
      is.null(easting) & is.null(lat) & is.null(long) & is.null(northing) &
      is.null(dist)){

    ## Limit set to 20,000 (current API is ~5,000) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Change baselink
    baselink <- "http://environment.data.gov.uk/flood-monitoring/id/stations.json"

    datalink <- paste0(baselink, "?_limit=20000")
    dt <- data.table(jsonlite::fromJSON(datalink)$items)

    ## Change `@id` col name, it clashes with measures ~~~~~~~~~~~~~~~~~~~~~~~~~
    colnames(dt)[1] <- 'stationID'

    ## Only the measures column should be a list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dt <- data.table(tidyr::unnest(dt, c(measures), keep_empty = TRUE))
    ## Select relevant columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data_level <- dt[, .(
      stationReference, wiskiID, label, parameter, parameterName, riverName,
      catchmentName, easting, northing, lat, long, gridReference, dateOpened,
      datumOffset
    ), ]

    ## Convert parameter values to match obsProperty criteria
    data_level$parameter <- gsub("flow", "waterFlow", data_level$parameter)
    data_level$parameter <- gsub("level", "waterLevel", data_level$parameter)

    # filter on obsProperty
    if (is.null(obsProperty)) {
      return(data_level)
    } else {
      return(data_level[property == obsProperty, , ])
    }
  }

  ##! Base tidal API
  ## Return all tidal sites from realtime API
  if (ID == 'tidal' &&
      is.null(easting) & is.null(lat) & is.null(long) & is.null(northing) &
      is.null(dist)){

    ## Limit set to 20,000 (current API is ~5,000) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Change baselink
    baselink <- "http://environment.data.gov.uk/flood-monitoring/id/stations.json?type=TideGauge"

    datalink <- paste0(baselink, "&_limit=20000")
    dt <- data.table(jsonlite::fromJSON(datalink)$items)

    ## Change `@id` col name, it clashes with measures ~~~~~~~~~~~~~~~~~~~~~~~~~
    colnames(dt)[1] <- 'stationID'

    ## Only the measures column should be a list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dt <- data.table(tidyr::unnest(dt, c(measures), keep_empty = TRUE))
    ## Select relevant columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data_level <- dt[, .(
      stationReference, label, parameter, parameterName, riverName,
      catchmentName, easting, northing, lat, long, gridReference, dateOpened
    ), ]

    ## Convert parameter values to match obsProperty criteria
    data_level$parameter <- gsub("level", "waterLevel", data_level$parameter)

    # filter on obsProperty
    if (is.null(obsProperty)) {
      return(data_level)
    } else {
      return(data_level[property == obsProperty, , ])
    }
  }

  # Find available stations within a set distance ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if ((is.null(ID) || ID %in% c("all", "wiski", "nrfa")) &
      (!is.null(easting) | !is.null(lat)) &
      (!is.null(northing) | !is.null(long)) &
      !is.null(dist)) {
    ## Checking for duplicate coordinates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ((!is.null(easting) | !is.null(northing)) &
        (!is.null(lat) & !is.null(long))) {
      return(warning("Conflicting coordinate systems, please use  lat-long OR
                     easting-northing"))
    }
    ## Create different links depending of coordinates used ~~~~~~~~~~~~~~~~~~~~
    if (!is.null(easting) & !is.null(northing)) {
      datalink <- paste0(
        baselink, "?easting=", easting,
        "&northing=", northing, "&dist=", dist
      )
    } else if (!is.null(lat) & !is.null(long)) {
      datalink <- paste0(baselink, "?lat=", lat, "&long=", long, "&dist=", dist)
    } else {
      return(warning("Please check your coordinate data"))
    }
    dt <- data.table(jsonlite::fromJSON(datalink)$items)
    ## Unnesting observed property is slightly more difficult ~~~~~~~~~~~~~~~~~~
    ## Stored as dataframe list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove URL and convert dataframes to string ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dt_obs <- dt$observedProperty
    lst <- list()
    for (i in seq_along(dt_obs)) {
      lst[[i]] <- basename(dt_obs[[i]]$`@id`)
    }
    dt$observedProperty <- lst
    ## Check if any columns are class list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ("list" %in% sapply(dt, class)) {
      dt <- data.table(tidyr::unnest(dt, c(
        wiskiID, label,
        riverName, easting, northing, lat,
        long, observedProperty
      ),
      keep_empty = TRUE
      ))
    }
    ## Select relevant columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Catchment area is sometimes a missing field ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Replicated as NAs for consistency ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!"catchmentArea" %in% colnames(dt)) {
      dt$catchmentArea <- rep("NA", times = dim(dt)[1])
    }
    data_level <- dt[, .(
      wiskiID, label, riverName,
      observedProperty, easting, northing, lat,
      long, dateOpened, catchmentArea, nrfaStationID
    ), ]
    ## Additional is.null(ID) level, resolves problems with needless arguments ~
    if (is.null(ID)) {
      return(data_level)
    }
    if (!is.null(ID) && ID == "wiski") {
      data_level <- na.omit(data_level, cols = "wiskiID")
    }
    if (!is.null(ID) && ID == "nrfa") {
      data_level <- na.omit(data_level, cols = "nrfaStationID")
    }
    if (is.null(obsProperty)) {
      return(data_level)
    } else {
      return(data_level[observedProperty == obsProperty, , ])
    }
  }

  # Return options for available data at specified ID site ~~~~~~~~~~~~~~~~~~
  if (!is.null(ID) & is.null(measure) & is.null(timestep)) {
    link <- paste0(baselink, "?wiskiID=", ID)
    data <- jsonlite::fromJSON(link)
    data_level <- jsonlite::fromJSON(as.character(data$items[1]))
    params <- data.table(
      measure = data_level$items$measures[[1]]$parameter,
      period = data_level$items$measures[[1]]$period,
      type = data_level$items$measures[[1]]$valueType
    )
    return(params)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~#
  #~~~~ Main data export ~~~~#
  #~~~~~~~~~~~~~~~~~~~~~~~~~~#

  ## Download data from hydrology API
  if (!is.null(ID) & !is.null(measure) & !is.null(period)) {
    cli::cli_progress_step("Compiling parameters for raw download")
    link <- paste0(baselink, "?wiskiID=", ID)
    data <- jsonlite::fromJSON(link)
    data_level <- jsonlite::fromJSON(as.character(data$items[1]))
    params <- data.table(
      parameter = data_level$items$measures[[1]]$parameter,
      period = data_level$items$measures[[1]]$period,
      type = data_level$items$measures[[1]]$valueType,
      note = data_level$items$measures[[1]]$notation
    )
    ## Insert criteria on what data to extract ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Merges into URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    datalink <- params[
      parameter == measure &
        period == timestep &
        type == dataType,
      note,
    ]
    measImp <- paste0(
      "http://environment.data.gov.uk/hydrology/id/measures/",
      datalink
    )

    ## Additional URL commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (datapoints == "all") {
      datalinkAppend <- paste0(measImp, "/readings.json?_limit=2000000")
    }
    if (datapoints == "earliest") {
      datalinkAppend <- paste0(measImp, "/readings.json?earliest")
    }
    if (datapoints == "latest") {
      datalinkAppend <- paste0(measImp, "/readings.json?latest")
    }
    if (datapoints == "standard") {
      datalinkAppend <- paste0(measImp, "/readings.json")
    }
    if (datapoints == "day") {
      minDate <- as.Date(from)
      datalinkAppend <- paste0(measImp, "/readings.json?_limit=2000000&date=", minDate)
    }
    if (datapoints == "range") {
      minDate <- as.Date(from)
      maxDate <- as.Date(to)
      datalinkAppend <- paste0(
        measImp, "/readings.json?_limit=2000000&mineq-date=",
        minDate, "&max-date=", maxDate
      )
    }
    cli::cli_progress_step("Downloading raw data")
    series <- data.table(jsonlite::fromJSON(datalinkAppend)[[2]])
    ## Drop needless parameters
    series <- series[,-1:-2]
    ## For clarity all dates are coerced to POSIXct ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Currently split between 2 timesteps to future proof ~~~~~~~~~~~~~~~~~~~~~
    if (timestep == 900) {
      series$dateTime <- as.POSIXct(series$dateTime,
                                    format = "%Y-%m-%dT%H:%M",
                                    tz = "GMT"
      )
    }
    if (timestep == 86400) {
      series$dateTime <- as.POSIXct(series$dateTime,
                                    format = "%Y-%m-%dT%H:%M",
                                    tz = "GMT"
      )
    }
    if (rtExt == TRUE){
      ## Download the realtime data

      rtStation <- 'https://environment.data.gov.uk/flood-monitoring/id/stations/'
      rtLink <- paste0(rtStation, ID)

      # Download the measures
      rtSeries <- data.table(jsonlite::fromJSON(rtLink))
      rtMeasures <- data.table(rtSeries$V1[3][[1]]$measures)
      colnames(rtMeasures)[1] <- 'measID'

      ## Filter to the data used in the `loadAPI()` call
      rtMeasure <- rtMeasures[period == period & parameter == measure, measID,]

      if (identical(rtMeasure, character(0))){
        stop('There is no realtime data available for the measures required. If you
       want flow data you might be able to convert stage data')
      }

      ## Setting a limit of 4 weeks, 2688 recordings
      ##! Longer data series tend to be corrupted
      measureLink <- paste0(rtMeasure, '/readings.json?_sorted&_limit=2688')

      ## Download data
      cli::cli_progress_step("Downloading extended realtime data")
      rtTS <- data.table(jsonlite::fromJSON(measureLink)[3][[1]])
      rtTS <- rtTS[order(dateTime)] #! Helps combine data

      ## Convert to consistent date time

      if (measure == 'rainfall') {
        rt <- rtTS[, .(dateTime = as.POSIXct(dateTime,
                                             format = "%Y-%m-%dT%H:%M",
                                             tz = "GMT"),
                       value,
                       valid = NA,
                       invalid = NA,
                       missing = NA,
                       completeness = NA,
                       quality = 'Unchecked')]
      } else {
        rt <- rtTS[, .(dateTime = as.POSIXct(dateTime,
                                             format = "%Y-%m-%dT%H:%M",
                                             tz = "GMT"),
                       value, quality = 'Unchecked', qcode = '<NA>' )]
      }

      ## Filter rt by anything after the latest hydrology API data point
      hydrologyMax <- max(series$dateTime)
      rt <- rt[dateTime > hydrologyMax,,]

      ## Merge 2 time series
      ##! Column presence can vary, particularly the qcode
      ## Find intersecting columns and bind rows
      inter <- intersect(names(series), names(rt))
      series <- rbind(series[,..inter], rt[,..inter])
    }

    ## Download the metadata
    if (meta == TRUE) {
      cli::cli_progress_step("Collating metadata")
      metaD <- getMeta(
        ID = ID,
        mainLink = datalinkAppend,
        measureLink = measImp,
        import = series[, -1:-2]
      )
      cli::cli_progress_step("Exporting data to HydroImport container")
      out <- HydroImportFactory$new(
        data = series,
        stationName = metaD$Data[[1]],
        riverName = metaD$Data[[2]],
        WISKI = metaD$Data[[3]],
        RLOID = metaD$Data[[4]],
        stationGuide = metaD$Data[[5]],
        baseURL = metaD$Data[[6]],
        dataURL = metaD$Data[[7]],
        measureURL = metaD$Data[[8]],
        idNRFA = metaD$Data[[9]],
        urlNRFA = metaD$Data[[10]],
        easting = metaD$Data[[11]],
        northing = metaD$Data[[12]],
        latitude = metaD$Data[[13]],
        longitude = metaD$Data[[14]],
        area = metaD$Data[[15]],
        parameter = metaD$Data[[16]],
        unitName = metaD$Data[[17]],
        unit = metaD$Data[[18]],
        datum = metaD$Data[[19]],
        boreholeDepth = metaD$Data[[20]],
        aquifer = metaD$Data[[21]],
      )
      return(out)
    }
  } else {
    return(series[, -1:-2])
  }
}
