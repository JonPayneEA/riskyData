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
#' @param rtLookup Set as FALSE, this uses an inbuilt dataset, if set to TRUE
#' this will update the table using the most recent data. However, this can
#' dramatically increase the run time.
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
#'
#' loadAPI('wiski')
#'
#' loadAPI(easting = 378235, northing = 276165, dist = 30, ID = "nrfa")
#'
#' loadAPI(ID = "L1207")
#'
#' loadAPI(ID = c("2001", 2002),
#'           measure = c("level", "flow", "level"),
#'           period = 900,
#'           type = "instantaneous",
#'           datapoints = "earliest",
#'           rtLookup = FALSE)
#'
#' loadAPI(ID = "L1207",
#'           measure = "level",
#'           period = 900,
#'           type = "instantaneous",
#'           datapoints = "latest")
loadAPI <- function(ID = NULL, measure = NULL, period = NULL,
                    type = NULL, datapoints = "standard",
                    from = NULL, to = NULL, lat = NULL, long = NULL,
                    easting = NULL, northing = NULL, dist = NULL,
                    obsProperty = NULL, meta = TRUE, rtExt = FALSE,
                    rtLookup = FALSE, assign = TRUE) {
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
  if ((is.null(ID) || ID[1] %in% c("all", "wiski", "nrfa")) &
      is.null(easting) & is.null(lat) & is.null(long) & is.null(northing) &
      is.null(dist)) {
    ## Limit set to 20,000 (current API is ~8,000) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    datalink <- paste0(baselink, "?_limit=20000")
    dt <- data.table::data.table(jsonlite::fromJSON(datalink)$items)

    ## Unnesting observed property is slightly more difficult ~~~~~~~~~~~~~~~~~~
    ## Stored as dataframe list ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Remove URL and convert dataframes to string ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dt_obs <- dt$observedProperty
    lst <- list()
    for (i in seq_along(dt_obs)) {
      ## Some null values in ID field cause errors
      lst[[i]] <- tryCatch({basename(dt_obs[[i]]$`@id`)},
                           error = function(e){'NA'})
    }
    dt$observedProperty <- lst
    ## Check if any columns are class list and unnest ~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ("list" %in% sapply(dt, class)) {
      dt <- data.table::data.table(tidyr::unnest(dt, c(
        easting, northing, lat, long
      ),
      keep_empty = TRUE
      ))

      # Unnesting again properties that can be variable in length
      properties <- c('wiskiID', 'label', 'riverName', 'observedProperty')
      for(i in properties){
        dt <- data.table::data.table(tidyr::unnest(dt, i,keep_empty = TRUE))
      }
    }

    ## Select relevant columns ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    data_level <- dt[, .(
      wiskiID, label, riverName,
      observedProperty, easting, northing, lat,
      long, dateOpened, dateClosed, catchmentArea, nrfaStationID
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
  if (ID[1] == "flood" &&
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

    ## Check if any columns are class list and unnest ~~~~~~~~~~~~~~~~~~~~~~~~~~
    if ("list" %in% sapply(dt, class)) {
      dt <- data.table(tidyr::unnest(dt, c(
        easting, northing, lat, long
      ),
      keep_empty = TRUE
      ))

      # Unnesting again properties that can be variable in length
      properties <- c('wiskiID', 'label', 'dateOpened')
      for(i in properties){
        dt <- data.table(tidyr::unnest(dt, i,keep_empty = TRUE))
      }
    }

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
  if (ID[1] == "tidal" &&
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
  if ((is.null(ID) || ID[1] %in% c("all", "wiski", "nrfa")) &
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
      ## Some null values in ID field cause errors
      lst[[i]] <- tryCatch({basename(dt_obs[[i]]$`@id`)},
                           error = function(e){'NA'})
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
    refs <- data.table::data.table(ID,
                                   measure,
                                   period,
                                   type,
                                   datapoints,
                                   from,
                                   to)

    if (length(refs$ID) > 1) {
      cli_alert_info(paste0(" File names are in the format of measure_ID format"))
      cli::cli_h3(" Data referennces used in download")
      print(refs)
      ## Iterate through IDs
      ## Where data are not downloaded - error messages will  flag
      errors <- list()
      for(i in seq_along(refs$ID)){
        name <- paste(refs$measure[i], refs$ID[i], sep = "_")
        cli::cli_h3(paste0("Data import: ", name))
        temp <- tryCatch(
          {
            hdeAPI(ID = refs$ID[i],
                   measure = refs$measure[i],
                   period = refs$period[i],
                   type = refs$type[i],
                   datapoints = refs$datapoints[i],
                   from = refs$from[i],
                   to = refs$to[i],
                   rtExt = rtExt,
                   meta = meta,
                   rtLookup = rtLookup)

          },
          error = function(msg){
            errorMes <- paste0("{.strong Failed to download data for ",
                               name, "}")
            cli::cli_alert_danger(errorMes)
            return(NULL)
          }
        )
        ## Export data
        if (!is.null(temp)){
          cli_alert_info(paste0(" Storing data in environment as {.pkg ",
                                name,
                                "}"))
          assign(name, temp, envir = .GlobalEnv)
        } else {
          ## Error message if temp file is empty due to error
          cli::cli_div(theme = list(span.emph = list(color = "orange")))
          cli::cli_alert_info(" Try using the {.emph loadAPI()} function instead")
          cli::cli_end()
          ## export errors
          errors[[i]] <- refs$ID[i]
        }
        rm(temp)
      }
      ## Unlist errors
      errors <- unlist(errors)
      if (!is.null(errors)){
        cli::cli_h3(paste0(" Errors detected at the following sites:"))
        cli::cli_text("{.pkg {errors}}")
      }
    } else {
      ## To improve functionality the code was embedded into `hdeAPI()` function
      r6 <- hdeAPI(ID = ID,
                   measure = measure,
                   period = period,
                   type = type,
                   datapoints = datapoints,
                   from = from,
                   to = to,
                   rtExt = rtExt,
                   meta = meta,
                   rtLookup = rtLookup)
      return(r6)
    }
  }
}
