link <- 'C:/Users/jpayne05/OneDrive - Defra/Desktop/Datasets/Test_Data/Buildwas.csv'
buildwas <- loadWISKI(link)


buildwas[[1]]$Parameter


dt <- riskyData::loadAPI()
link <- 'https://environment.data.gov.uk/hydrology/id/stations?wiskiID=2001'


getMeta <- function(ID = NULL, mainLink = NULL, measureLink = NULL, import = NULL){
  ## Create URL for metadata
  metaBase <- 'https://environment.data.gov.uk/hydrology/id/stations?wiskiID='
  metaLink <- paste0(metaBase, ID)
  # Import data
  metaRaw <- jsonlite::fromJSON(metaLink)
  subMeta <- jsonlite::fromJSON(measureLink)

  ## Empty data function to tidy code
  emptyField <- function(x){
    stuff <- ifelse(is.null(x), NA, x)
  }

  ## Extract what's needed
  data <- data.table('Parameter' = c("Station Name",
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
                                     "Datapoints"),
                     'Data' = c(emptyField(metaRaw$items$label),
                                emptyField(metaRaw$items$riverName),
                                emptyField(metaRaw$items$wiskiID),
                                emptyField(metaRaw$items$RLOIid),
                                emptyField(subMeta$items$stationGuid),
                                'http://environment.data.gov.uk/hydrology/id/measures/',
                                sub('http://environment.data.gov.uk/hydrology/id/measures/', '', mainLink),
                                sub('http://environment.data.gov.uk/hydrology/id/measures/', '', measureLink),
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
                                as.character(min(import$dateTime)),
                                as.character(max(import$dateTime)),
                                "GMT - UTC±00:00",
                                length(import$dateTime)))
  return(data)
}


getMeta(2001)

subMeta <- jsonlite::fromJSON('https://environment.data.gov.uk/hydrology/id/measures/ee3702af-ac65-4191-8b72-7fb7c84c670c-rainfall-t-86400-mm-qualified')


metad <- jsonlite::fromJSON(link)

metad$items


rain <- loadAPI(ID = '051729',
        measure = 'rainfall',
        period = 900,
        type = 'total',
        datapoints = 'range',
        from = '2022-07-29',
        to = '2022-08-01')

'https://environment.data.gov.uk/hydrology/id/stations?wiskiID=F2802'

loadAPI(ID = '028502',
        measure = 'rainfall',
        period = 86400,
        type = 'total',
        datapoints = 'latest')

loadAPI(ID = '2001',
        measure = 'level',
        period = 900,
        type = 'instantaneous',
        datapoints = 'latest')

return(measImp)

loadAPI('3_016',
        measure = 'level',
        period = 86400,
        type = 'instantaneous',
        datapoints = 'latest')

# Groundwater
'https://environment.data.gov.uk/hydrology/id/stations?wiskiID=3_016'

# Flow not on NRFA
'https://environment.data.gov.uk/hydrology/id/stations?wiskiID=037052'

# Stage not on NRFA
'https://environment.data.gov.uk/hydrology/id/stations?wiskiID=680324'

loadAPI('680324',
        measure = 'level',
        period = 86400,
        type = 'max',
        datapoints = 'latest')


setClass("apiEA", slots=list(Station_Name="character",
                             River_Name="character",
                             Station_WISKI="character",
                             Station_RLOID="character",
                             Station_Guide="character",
                             Data="data.table"))

s <- new("apiEA",Station_Name=as.character(rain$Metadata$Data[1]),
         River_Name=as.character(rain$Metadata$Data[2]),
         Station_WISKI=as.character(rain$Metadata$Data[3]),
         Station_RLOID=as.character(rain$Metadata$Data[4]),
         Station_Guide=as.character(rain$Metadata$Data[5]),
         Data=rain$GaugeData)



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
