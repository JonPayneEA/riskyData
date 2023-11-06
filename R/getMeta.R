#' Import metadata for a gauge
#'
#' @param ID Use to specify a particular WISKI ID
#' @param mainLink URL to the main data download
#' @param measureLink URL to the measures element of the site
#' @param import The main raw data import file, used to collate dates etc.
#'
#' @return A data.table of gauge metadata, for adding to the R6 `hydroLoad`
#' @export
#'
#' @examples
#' # getMeta(2001)
getMeta <- function(ID = NULL,
                    mainLink = NULL,
                    measureLink = NULL,
                    import = NULL) {
  ## Create URL for metadata
  metaBase <- "https://environment.data.gov.uk/hydrology/id/stations?wiskiID="
  metaLink <- paste0(metaBase, ID)
  # Import data
  metaRaw <- jsonlite::fromJSON(metaLink)
  subMeta <- jsonlite::fromJSON(measureLink)

  ## Empty data function to tidy code
  emptyField <- function(x = NULL) {
    field <- ifelse(is.null(x), NA, x)
    return(field)
  }

  ## Extract what's needed
  data <- data.table(
    "Parameter" = c(
      "Station Name",
      "River Name",
      "Station WISKI",
      "Station RLOID",
      "Station Guide",
      "Base URL",
      "Data URL",
      "Measure URL",
      "NRFA ID",
      "NRFA URL",
      "Easting",
      "Northing",
      "Latitude",
      "Longtiude",
      "Catchment Area (km)",
      "Parameter",
      "Unit name",
      "Unit",
      "Datum",
      "Borehole Depth",
      "Aquifer",
      "Start",
      "End",
      "Time Zone",
      "Datapoints"
    ),
    "Data" = c(
      emptyField(metaRaw$items$label),
      emptyField(metaRaw$items$riverName),
      emptyField(metaRaw$items$wiskiID),
      emptyField(metaRaw$items$RLOIid),
      emptyField(subMeta$items$stationGuid),
      "http://environment.data.gov.uk/hydrology/id/measures/",
      sub("http://environment.data.gov.uk/hydrology/id/measures/", "",
          mainLink),
      sub("http://environment.data.gov.uk/hydrology/id/measures/", "",
          measureLink),
      emptyField(metaRaw$items$nrfaStationID),
      emptyField(metaRaw$items$nrfaStationURL),
      emptyField(metaRaw$items$easting),
      emptyField(metaRaw$items$northing),
      emptyField(metaRaw$items$lat),
      emptyField(metaRaw$items$long),
      emptyField(metaRaw$items$catchmentArea),
      emptyField(subMeta$items$parameterName),
      emptyField(subMeta$items$unitName),
      emptyField(subMeta$items$unit),
      emptyField(metaRaw$items$datum),
      emptyField(metaRaw$items$boreholeDepth),
      emptyField(metaRaw$items$aquifer),
      NA,
      NA,
      "GMT - UTC±00:00",
      length(import$dateTime)
    )
  )
  return(data)
}
