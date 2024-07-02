#' @title Download hydrometric data from the Hydrology Data Explorer
#'
#' @description Used in the `loadAPI()` function
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
#' @param meta Set as TRUE, exports metadata for gauge with the data request
#' @param rtExt Set to NULL, if TRUE hydrological data series are extended with
#' the real time API
#' @param rtLookup Set as FALSE, this uses an inbuilt dataset, if set to TRUE
#' this will update the table using the most recent data. However, this can
#' dramatically increase the run time.
#'
#' @return r6 HydroImport container
#' @export
#'
#' @examples
#' #hdeAPI(ID = '2001', measure = 'flow', period = 900)
hdeAPI <- function(ID = NULL,
                   measure = NULL,
                   period = NULL,
                   type = NULL,
                   datapoints = "standard",
                   from = NULL,
                   to = NULL,
                   rtExt = FALSE,
                   meta = TRUE,
                   rtLookup = FALSE) {
  dataType <- type # Fixes data.table filter issue
  timestep <- period # Fixes data.table filter issue
  # Base link for the EAs API
  baselink <- "http://environment.data.gov.uk/hydrology/id/stations.json"

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
  ## Insert criteria on what data to extract ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Merges into URL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

  ## Additional URL commands ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    datalinkAppend <- paste0(measImp, "/readings.json?_limit=2000000&date=",
                             minDate)
  }
  if (datapoints == "range") {
    minDate <- as.Date(from)
    maxDate <- as.Date(to) + 1
    datalinkAppend <- paste0(
      measImp, "/readings.json?_limit=2000000&mineq-date=",
      minDate, "&max-date=", maxDate
    )
  }
  cli::cli_progress_step("Downloading raw data")
  series <- data.table(jsonlite::fromJSON(datalinkAppend)[[2]])
  ## Drop needless parameters
  series <- series[,-1:-2]
  ## For clarity all dates are coerced to POSIXct ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Currently split between 2 timesteps to future proof ~~~~~~~~~~~~~~~~~~~~~~~
  if (timestep == 900) {
    series$dateTime <- as.POSIXct(series$dateTime,
                                  format = "%Y-%m-%dT%H:%M",
                                  tz = "GMT"
    )
    if (datapoints == "range"){
      ## API only pulls data by calendar day
      ## Windowing time series by input data
      series <- snipRange(x = series, start = from, end = to)
    }
  }

  if (timestep == 86400) {
    series$dateTime <- as.POSIXct(series$dateTime,
                                  format = "%Y-%m-%dT%H:%M",
                                  tz = "GMT"
    )
    if (datapoints == "range"){
      lastLine <- length(series$dateTime)
      series <- series[-lastLine,]
    }
  }
  if (rtExt == TRUE){
    ## Download the realtime data
    rtStation <- 'https://environment.data.gov.uk/flood-monitoring/id/measures?stationReference='

    ## Lookup measure to parameters required
    measures <- c('flow', 'level', 'rainfall')
    params <- c('waterFlow', 'waterLevel', 'rainfall')
    names(params) <- measures
    param <- params[measure]

    ## Implement realtime extension linking to the realtiome API
    if(rtLookup == FALSE) {
      data("realtimeIDs")
    } else {
      realtimeIDs <- riskyData::loadAPI(ID = 'flood')
      realtimeIDs <- unique(realtimeIDs[, .(stationReference,
                                            wiskiID,
                                            label,
                                            parameter,
                                            parameterName
      )])
    }
    rloid <- realtimeIDs[wiskiID == ID &
                           parameter == param,
                         .(stationReference)]
    rtLink <- paste0(rtStation, rloid[1])
    # Download the measures
    rtSeries <- jsonlite::read_json(rtLink)
    rtMeasures <- data.table::rbindlist(rtSeries$items, fill = TRUE)
    ## The fill causes duplicates, easier to just remove
    rtMeasures <- unique(rtMeasures[, .(`@id`,
                                        label,
                                        parameter,
                                        parameterName,
                                        period,
                                        valueType)])
    ## Rename the `@id` column
    colnames(rtMeasures)[1] <- 'measID'

    ## Filter to the data used in the `loadAPI()` call
    rtMeasure <- rtMeasures[period == period &
                              parameter == measure &
                              valueType == type,
                            measID]
    ## IF realtime data can't be paired up
    if (identical(rtMeasure, character(0))){
      stop('There is no realtime data available for the measures required. If
      you want flow data you might be able to convert stage data')
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
      peaks = NULL,
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
}
