#' @title Export data in a format suitable for PDM for PCs
#'
#' @param x File, currently only supports the class `catchAvg` for rainfall
#' @param path Optional path for file export. Defaults to working directory.
#' @param type Export type that supports csv, XML, and data.table (for internal use)
#' @param name Catchment name used in XML exports
#'
#' @importFrom data.table data.table
#' @importFrom data.table year
#' @importFrom data.table month
#' @importFrom data.table mday
#' @importFrom data.table hour
#' @importFrom data.table minute
#' @importFrom data.table second
#' @importFrom XML xmlParse
#' @importFrom XML saveXML
#' @importFrom cli cli_alert_success
#'
#' @return csv, XML or data.tables in the format suitable for PDM for PCs
#' @export
#'
#' @examples
#' boot <- loadAPI(ID = "590310",
#' measure = 'rainfall',
#' period = 900,
#' type = 'total',
#' datapoints = 'range',
#' from = '2023-10-01 09:00',
#' to = '2024-10-01 08:45')
#'## Import some data
#' whit <- loadAPI(ID = "197036",
#'                 measure = 'rainfall',
#'                 period = 900,
#'                 type = 'total',
#'                 datapoints = 'range',
#'                 from = '2023-10-01 09:00',
#'                 to = '2024-10-01 08:45')
#'
#' dogd <- loadAPI(ID = "U06",
#'                 measure = 'rainfall',
#'                 period = 900,
#'                 type = 'total',
#'                 datapoints = 'range',
#'                 from = '2023-10-01 09:00',
#'                 to = '2024-10-01 08:45')
#' ## Carry oout a catchment average
#' rainfall <- catchAvg(boot, whit, dogd, areas = c(45, 25, 15))
#'
#' exportToPDM(x = rainfall, type = "data.table")
exportToPDM <- function(x = NULL, path = NULL, type = "csv", name = "PDM_Input"){
  if (is.null(x)) {cli::cli_abort("No data have been supplied for export!")}
  fileTypes <- c("csv", "XML", "data.table")
  if (!type %in% fileTypes) {cli::cli_abort("The export type, {.emph {type}}, is not supported. Please use either of {.emph {paste(fileTypes, collapse = ', ')}}.", wrap = TRUE)}
  root <- path
  if (is.null(root)){root <- tools::file_path_as_absolute("./")}
  if (any(class(x) == "catchAvg")) {
    dt <- data.table::data.table(year = data.table::year(x$dateTime),
                                 month = data.table::month(x$dateTime),
                                 day = data.table::mday(x$dateTime),
                                 hour = data.table::hour(x$dateTime),
                                 minute = data.table::minute(x$dateTime),
                                 second = data.table::second(x$dateTime),
                                 rainfall = x$value)
    if (type == "csv"){
      fullPath <- file.path(root, "RAIN.csv")
      fwrite(dt, file = fullPath)
      cli::cli_alert_success("RAIN.csv exported to {.path {root}}")
    }
    if (type == "data.table") {
      return(dt)
    }
    if (type == "XML"){
      startDate <- as.Date(x$dateTime[1])
      startTime <- format(x$dateTime[1], "%H:%M:%S")
      endDate <- as.Date(tail(x$dateTime, 1))
      endTime <- format(tail(x$dateTime, 1), "%H:%M:%S")
      longName <- name

      ## Establish header string
      headerString <- paste0('
<TimeSeries version="1.2">
  <!-- Created by R script written by Jac v1 05th Nov 2025 -->
  <series>
    <header>
      <type>accumulative</type>
      <locationId>rainfall_station_1</locationId>
      <parameterId>rainfall_1</parameterId>
      <timeStep unit="minute" multiplier="15"/>
      <startDate date="', startDate,'" time="', startTime, '"/>
      <endDate date="', endDate,'" time="', endTime, '"/>
      <missVal>NaN</missVal>
      <longName>"',longName,'"</longName>
      <stationName>rainfall_1</stationName>
      <units>mm/hr</units>
    </header>
')
      ## Create date time as strings
      dateStrs <- sprintf("%04d-%02d-%02d", dt$year, dt$month, dt$day)
      timeStrs <- sprintf("%02d:%02d:%02d", dt$hour, dt$minute, dt$second)
      valueStrs <- dt$rainfall

      ## Create pseudo events nodes
      eventLines <- sprintf(
        '<event date="%s" time="%s" value="%.4f" flag="%s"/>',
        dateStrs, timeStrs, valueStrs, 0
      )
      eventsString <- paste(eventLines, collapse = "\n")

      ## Establish footer
      footerString <- '
  </series>
</TimeSeries>
'
      ## Tye it together
      fullXMLString <- paste0(headerString, eventsString, footerString)
      ## Parse it to XML
      doc <- XML::xmlParse(fullXMLString, asText = TRUE)

      fullPath <- file.path(root, "RAIN.XML")
      XML::saveXML(doc, fullPath)
      cli::cli_alert_success("RAIN.XML exported to {.path {root}}")    }
  }
}



