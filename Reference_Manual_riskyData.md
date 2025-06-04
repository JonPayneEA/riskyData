---
output:
  pdf_document: default
  html_document: default
---
# DESCRIPTION

```
Package: riskyData
Title: Hydrometric data import and maniuplation with riskyData  
Version: 0.3.8
Authors@R: c(
    person("Jonathan", "Payne", , "jon.payne@environment-agency.gov.uk", role = c("aut", "cre"),
           comment = c(ORCID = "0000-0003-2551-5353")),
    person("Oliver", "Dyer", , "oliver.dyer@environment-agency.gov.uk", role = c("aut"),
           comment = c(ORCID = "0000-0003-2773-6731"))
           )
Description: The package riskyData has been developed for the Evidence and Risk colleagues to cleanly and reliably import data from the Environment Agencies API.
License: GPL (>= 3)
Encoding: UTF-8
Language: es
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.1
Imports: 
    data.table,
    jsonlite,
    R6,
    cli,
    tidyr,
    ggplot2,
    cowplot,
    RcppRoll,
    dplyr
URL: https://jonpayneea.github.io/riskyData/, https://github.com/JonPayneEA/riskyData
Depends: 
    R (>= 2.10)
LazyData: true
Suggests: 
    knitr,
    rmarkdown
VignetteBuilder: knitr
```

# `annualAgg`: annualAgg

## Description

Aggregates sub annual time series into an annual resolution,
aggregations are carried out to the calendar day not a rolling time period.

## Usage

```r
annualAgg(x, method = "mean", ...)

# S3 method for data.table
annualAgg(x, method = "mean", ...)

# S3 method for HydroImport
annualAgg(x, method = "mean", ...)
```

## Arguments

* `x`: Data generated in the riskyData package
* `method`: 'mean', 'median', 'max', 'min', or 'sum'
* `...`: Other variables as required

## Value

A dataset of annually aggregated data, data are not stored in a `HydroAggs` container.

## Examples

```r
data(bewdley)

# Calculate the mean annual (calendar year) flows
annualAgg(bewdley)
```

# `asVol`: Add convert flow or rainfall data to a volume

## Description

Add convert flow or rainfall data to a volume

## Usage

```r
asVol(x, area)

# S3 method for data.table
asVol(x, area = NULL)

# S3 method for HydroImport
asVol(x, area = NULL)

# S3 method for HydroAggs
asVol(x, area = NULL)
```

## Arguments

* `x`: Data set of classes `HydroImport` or ¬`HydroAggs`
* `area_km`: Area of coverage for rain gauges

## Value

Calculated volume of flow or rainfall

## Examples

```r
# Load bewdley dataset
data(bewdley)

# Calculate volumes
asVol(bewdley)

# Print the bewdley dataset
bewdley
```

# `catchAvg`: Calculate catchment average rainfall

## Description

Calculate catchment average rainfall

## Usage

```r
catchAvg(..., list = NULL, areas = NULL)
```

## Arguments

* `...`: hydroImport containers
* `list`: set to Null, however lists of hydroImports can be placed here if required
* `areas`: user specified area sizes contributed by each rain gauge

## Value

Catchment averaged rainfall in data.table format

## Examples

```r
## Do not run
```

# `cumsumNA`: cumsumNA

## Description

Provides a cumulative sum of rain gauge data while excluding NA
values.

## Usage

```r
cumsumNA(x, ...)

# S3 method for data.table
cumsumNA(x)

# S3 method for HydroImport
cumsumNA(x)

# S3 method for HydroAggs
cumsumNA(x)

# S3 method for numeric
cumsumNA(x)
```

## Arguments

* `x`: Raw rain gauge data or merged dataset
* `...`: Additional parameters as required

## Value

A cumulative sum of rain gauge data

## Examples

```r
data(chesterton)

# Apply cumulative sum
dt <- cumsumNA(chesterton)

plot(dt$dateTime,
     dt$cumSum,
     main = "Cumulative rainfall plot for Chesterton Gauge (WISKI: 164163)",
     xla = "Date Time",
     ylab = "Cumulative rainfall (mm)",
     type = "l")
```

# `dailyAgg`: dailyAgg

## Description

Aggregates sub daily time series into a daily resolution,
aggregations are carried out to the calendar day not a rolling time period.

## Usage

```r
dailyAgg(x, method = "mean", ...)

# S3 method for data.table
dailyAgg(x, method = "mean", ...)

# S3 method for HydroImport
dailyAgg(x, method = "mean", ...)
```

## Arguments

* `x`: Data generated in the riskyData package
* `method`: 'mean', 'median', 'max', 'min', or 'sum'
* `...`: Other variables as required

## Value

An aggregated dataset taken to the daily resolution, data are not stored in a `HydroAggs` container.

## Examples

```r
data(bewdley)

# Daily maximum flows
dailyAgg(bewdley, method = "max")
```

# `dataAgg`: Data aggregation for `HydroImport` objects

## Description

This acts as a wrapper function for all the aggregation
functions that sit within riskyData

## Usage

```r
dataAgg(x = NULL, type = NULL, method = NULL)
```

## Arguments

* `x`: A data.table that sits within an R6 `HydroImport` object
* `type`: Set the aggregation level to either hourly', 'daily', 'monthly',
'annual', or 'hydroYear'
* `method`: Available aggregation methods include 'min', 'max', 'mean',
'median', and 'sum'

## Seealso

[hourlyAgg](hourlyAgg)[dailyAgg](dailyAgg)[monthlyAgg](monthlyAgg)[annualAgg](annualAgg)[hydroYearAgg](hydroYearAgg)

## Value

Aggregated dataset determined from the input `type` and `method`. Data are placed within a `HydroAggs` type container

## Examples

```r
# Load bewdley dataset
data(bewdley)

# Calculate mean hourly flows
bewdley$dataAgg(type = "hourly", method = "mean")

# Calculate median daily flows
bewdley$dataAgg(type = "daily", method = "median")

# Calculate min
bewdley$dataAgg(type = "monthly", method = "min")

# Calculate annual (calendar year) max
bewdley$dataAgg(type = "annual", method = "max")

# Calculate hydroYear max
# Hydrological years need to be calculated for this. The `hydroYearDay()` function can be piped into the operation.
bewdley$hydroYearDay()$dataAgg(type = "hydroYear", method = "max")

# Calculate total hourly flow
# Flow has to be converted to volume. The `asVol()` function can be piped into the operation.
bewdley$asVol()$dataAgg(type = "hourly", method = "sum")
```

# `dayStats`: dayStats

## Description

Daily statistics for imported data

## Usage

```r
dayStats(
  x,
  methods = c("mean", "median", "min", "max"),
  percentiles = c(5, 25, 75, 95),
  hydroDay = TRUE,
  plot = TRUE,
  ...
)

# S3 method for data.table
dayStats(
  x = NULL,
  methods = c("mean", "median", "min", "max"),
  percentiles = c(5, 25, 75, 95),
  hydroDay = TRUE,
  plot = TRUE,
  year = NULL,
  ...
)
```

## Arguments

* `x`: Data generated in the riskyData package
* `methods`: 'mean', 'median', 'max', 'min', or 'sum'
* `percentiles`: Set to 5, 25, 75, and 95
* `plot`: Set to TRUE, will produce a plot of the statistics
* `...`: Other variables as required
* `hyrdroDay`: Set to TRUE, use hydrological day

## Value

Data are aggregated to a daily resolution

## Examples

```r
# Load data
data(bewdley)

# Calculate hydrological year
bewdley$hydroYearDay()

# Calculate daily statistics
bewdley$dayStats(plot = TRUE)
```

# `exceed`: Find the amount of events over a given threshold and supply the amount of
time steps to which they were in excess.

## Description

Find the amount of events over a given threshold and supply the amount of
time steps to which they were in excess.

## Usage

```r
exceed(
  dateTime = NULL,
  value = NULL,
  threshold = NULL,
  gapWidth = 0,
  timeStep = 900
)
```

## Arguments

* `dateTime`: The date and time series
* `value`: The observed data
* `threshold`: The threshold at which to test
* `gapWidth`: This allows you to ignore a set number of time steps beneath
a threshold between two or more events
* `timeStep`: The time step used applied in the observed data in seconds

## Value

A table of events with start and end times

## Examples

```r
data(bewdley)
events <- exceed(bewdley$data$dateTime,
                 bewdley$data$value,
                 threshold = 200,
                 gapWidth = 20,
                 timeStep = 900)
events
```

# `findPeaks`: Find peaks in hydrometric data

## Description

The `findPeaks` function detects peaks in hydrometric time
series using run length encoding against a subset of threshlds.

## Usage

```r
findPeaks(x, levels = 100, from = NULL, to = NULL, gaps = 96)
```

## Arguments

* `x`: R6 hydroImport object
* `levels`: The number of thresholds to apply against the time seires.
Defaults to 100
* `from`: Set the location from where the thresholds should start.
Defaults to the bottom 10% value.
* `to`: Set the location from where the thresholds should end
Defaults to the maximum observed value.
* `gaps`: Used to pad out the run length encoding, prevents wobbly data
showing multiple exceedances. Set to 96.

## Value

A data.table containing dateTime and values of observed peaks

## Examples

```r
data(bewdley)
bewdley$hydroYearDay()
peaksB <- riskyData::findPeaks(bewdley)
print(peaksB)
peaksB <- data.table::data.table(peaksB,
                                 riskyData::hydroYearDay.POSIXct(peaksB$dateTime))

plot <- bewdley$plot(wrap = FALSE)
a <- plot + geom_point(data = peaksB, inherit.aes = FALSE,
                       aes(x = dateTime, y = value), colour = 'red') +
  facet_wrap(~hydroYear, scales = 'free_x')
```

# `getGaugeSAAR`: Calculate the observed SAAR values from rain gauge data

## Description

Calculate the observed SAAR values from rain gauge data

## Usage

```r
getGaugeSAAR(gaugeIDs)
```

## Arguments

* `gaugeIDs`: A vector of rain gauge WISKI IDs that can be fed into
`riskyData::loadAPI()`

## Value

A list containing the annual rainfalls, summary statistics and plots

## Examples

```r
rainGaugeIDs <- c('015347','021228', '013045', 'NE034')
getGaugeSAAR(rainGaugeIDs)
```

# `getMeta`: Import metadata for a gauge

## Description

Import metadata for a gauge

## Usage

```r
getMeta(ID = NULL, mainLink = NULL, measureLink = NULL, import = NULL)
```

## Arguments

* `ID`: Use to specify a particular WISKI ID
* `mainLink`: URL to the main data download
* `measureLink`: URL to the measures element of the site
* `import`: The main raw data import file, used to collate dates etc.

## Value

A data.table of gauge metadata, for adding to the R6 `hydroLoad`

## Examples

```r
# getMeta(2001)
```

# `hdeAPI`: Download hydrometric data from the Hydrology Data Explorer

## Description

Used in the `loadAPI()` function

## Usage

```r
hdeAPI(
  ID = NULL,
  measure = NULL,
  period = NULL,
  type = NULL,
  datapoints = "standard",
  from = NULL,
  to = NULL,
  rtExt = FALSE,
  meta = TRUE,
  rtLookup = FALSE
)
```

## Arguments

* `ID`: Use to specify a particular WISKI ID or for when downloading all
stations 'wiski' or 'nrfa' to filter results. Additionally to access the
real time APIs use "flood" for all critical sites and "tidal" for the
category A tide gauges.
* `measure`: Use this when exporting observations to select the available
parameter. Generally 'flow', 'level', or 'groundwater'
* `period`: This is so you can select the time steps available, generally
900 (15 min) or 86400 (daily)
* `type`: Selects the data type, instantaneous, min, max etc.
* `datapoints`: Allows you to decide where in the time series you are
pulling the data from
* `from`: First time step of choice
* `to`: Last time step of choice
* `rtExt`: Set to NULL, if TRUE hydrological data series are extended with
the real time API
* `meta`: Set as TRUE, exports metadata for gauge with the data request
* `rtLookup`: Set as FALSE, this uses an inbuilt dataset, if set to TRUE
this will update the table using the most recent data. However, this can
dramatically increase the run time.

## Value

r6 HydroImport container

## Examples

```r
#hdeAPI(ID = '2001', measure = 'flow', period = 900)
```

# `hourlyAgg`: hourlyAgg

## Description

Aggregates sub hourly time series into an hourly resolution,
aggregations are carried out to the calendar day not a rolling time period.

## Usage

```r
hourlyAgg(x, method = "mean", ...)

# S3 method for data.table
hourlyAgg(x, method = "mean", ...)

# S3 method for HydroImport
hourlyAgg(x, method = "mean", ...)
```

## Arguments

* `x`: Data generated in the riskyData package
* `method`: 'mean', 'median', 'max', 'min', or 'sum'
* `...`: Other variables as required

## Value

A dataset of hourly aggregated, data are not stored in a `HydroAggs` container.

## Examples

```r
data(bewdley)

## Calculate median flow
hourlyAgg(bewdley, method = "median")
```

# `HydroAggs`: R6 object for imported data via the EA API

## Description

This is the base class for all data imported via the API. A HydroAggs
contains public raw data and private metadata.

## Super class

`[riskyData::HydroImport](https://rdrr.io/pkg/riskyData/man/HydroImport.html)` -> `HydroAggs`

## Methods

### Public methods

* [`HydroAggsFactory$print()`](#method-HydroAggs-print)
* [`HydroAggsFactory$clone()`](#method-HydroAggs-clone)

### Method `print()`

Display the R6 object

#### Usage

```
HydroAggsFactory$print(.)
```

#### Arguments

* `.`: (ignored).

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

```
HydroAggsFactory$clone(deep = FALSE)
```

#### Arguments

* `deep`: Whether to make a deep clone.

## Value

A class containing raw data, metadata and methods

# `HydroImport`: R6 object for imported data via the EA API

## Description

This is the base class for all data imported via the API. A HydroImport
contains public raw data and private metadata.

## Public fields

* `data`: Imported data via the API tool. Uses data.table.
* `rating`: Rating parameters. Uses data.table. At the moment only 1 rating can be applied.
* `peaks`: Calculated peaks, initialised by "$findPeaks".
* `catchProp`: Proportional catchment area, used for rain gauge averaging.

## Methods

### Public methods

* [`HydroImportFactory$new()`](#method-HydroImport-new)
* [`HydroImportFactory$print()`](#method-HydroImport-print)
* [`HydroImportFactory$methods()`](#method-HydroImport-methods)
* [`HydroImportFactory$summary()`](#method-HydroImport-summary)
* [`HydroImportFactory$statSummary()`](#method-HydroImport-statSummary)
* [`HydroImportFactory$asVol()`](#method-HydroImport-asVol)
* [`HydroImportFactory$hydroYearDay()`](#method-HydroImport-hydroYearDay)
* [`HydroImportFactory$rmVol()`](#method-HydroImport-rmVol)
* [`HydroImportFactory$rmHY()`](#method-HydroImport-rmHY)
* [`HydroImportFactory$rmHYD()`](#method-HydroImport-rmHYD)
* [`HydroImportFactory$postOrder()`](#method-HydroImport-postOrder)
* [`HydroImportFactory$findPeaks()`](#method-HydroImport-findPeaks)
* [`HydroImportFactory$addRating()`](#method-HydroImport-addRating)
* [`HydroImportFactory$addCatchProp()`](#method-HydroImport-addCatchProp)
* [`HydroImportFactory$rateFlow()`](#method-HydroImport-rateFlow)
* [`HydroImportFactory$rateStage()`](#method-HydroImport-rateStage)
* [`HydroImportFactory$window()`](#method-HydroImport-window)
* [`HydroImportFactory$coords()`](#method-HydroImport-coords)
* [`HydroImportFactory$nrfa()`](#method-HydroImport-nrfa)
* [`HydroImportFactory$dataAgg()`](#method-HydroImport-dataAgg)
* [`HydroImportFactory$rollingAggs()`](#method-HydroImport-rollingAggs)
* [`HydroImportFactory$meta()`](#method-HydroImport-meta)
* [`HydroImportFactory$dayStats()`](#method-HydroImport-dayStats)
* [`HydroImportFactory$plot()`](#method-HydroImport-plot)
* [`HydroImportFactory$flowDuration()`](#method-HydroImport-flowDuration)
* [`HydroImportFactory$quality()`](#method-HydroImport-quality)
* [`HydroImportFactory$missing()`](#method-HydroImport-missing)
* [`HydroImportFactory$exceed()`](#method-HydroImport-exceed)
* [`HydroImportFactory$clone()`](#method-HydroImport-clone)

### Method `new()`

Initialise the new HydroImport object

#### Usage

```
HydroImportFactory$new(
  data = NA,
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
  records = NA
)
```

#### Arguments

* `data`: Raw data
* `rating`: Set to blank, if data requires a rating add to here
* `peaks`: Set to blank, if you calculate peaks they can be stored here
* `catchProp`: Set to blank, proportion of catchment coverage for rain
gauges can be stored here
* `dataType`: Details the type of data in this environment
* `modifications`: Details modifications made to the data
* `stationName`: Name of gauge
* `riverName`: River name
* `WISKI`: WISKI ID
* `RLOID`: River Levels on the Internet ID
* `stationGuide`: Station Unique Identifier
* `baseURL`: Base URL used in the API
* `dataURL`: End of URL that downloaded the raw data
* `measureURL`: Primarily used in gaining the metadata
* `idNRFA`: National River Flow Archive station identifier
* `urlNRFA`: National River Flow Archive station URL
* `easting`: Easting coordinate
* `northing`: Northing coordinate
* `latitude`: Latitude coordinate
* `longitude`: Longitude coordinate
* `area`: Catchment area of flow/level gauge
* `parameter`: Details the collected data e.g. flow, level etc.
* `unitName`: Unit name used
* `unit`: Details on the unit of measurement
* `datum`: Datum of the gauge, if available
* `boreholeDepth`: Depth of borehole recording
* `aquifer`: Details on the aquifer type
* `start`: Function calculates the date of the first imported record
* `end`: Function calculates the date of the last imported record
* `timeStep`: Function that calculates the timestep in seconds
* `timeZone`: Time zone used in data, defaults to GMT
* `records`: Function calculates the number of records

### Method `print()`

Display the R6 object

#### Usage

```
HydroImportFactory$print(.)
```

#### Arguments

* `.`: (ignored).

### Method `methods()`

Display the methods available in the R6 object

#### Usage

```
HydroImportFactory$methods(.)
```

#### Arguments

* `.`: (ignored).

#### Examples

```
data(bewdley)
bewdley$methods()
```

### Method `summary()`

Display a summary of the R6 object

#### Usage

```
HydroImportFactory$summary(.)
```

#### Arguments

* `.`: (ignored).

#### Examples

```
data(bewdley)
bewdley$summary()
```

### Method `statSummary()`

Display a small statistics summary of the R6 object

#### Usage

```
HydroImportFactory$statSummary(.)
```

#### Arguments

* `.`: (ignored).

#### Examples

```
data(bewdley)
bewdley$statSummary()
```

### Method `asVol()`

Calculate the volume of water for flow or rainfall data

#### Usage

```
HydroImportFactory$asVol(.)
```

#### Arguments

* `.`: (ignored).

### Method `hydroYearDay()`

Calculate hydrological year and day

#### Usage

```
HydroImportFactory$hydroYearDay(.)
```

#### Arguments

* `.`: (ignored).

### Method `rmVol()`

Remove the calculated volume

#### Usage

```
HydroImportFactory$rmVol(.)
```

#### Arguments

* `.`: (ignored).

### Method `rmHY()`

Remove the calculated hydroYear

#### Usage

```
HydroImportFactory$rmHY(.)
```

#### Arguments

* `.`: (ignored).

### Method `rmHYD()`

Remove the calculated hydrological day

#### Usage

```
HydroImportFactory$rmHYD(.)
```

#### Arguments

* `.`: (ignored).

### Method `postOrder()`

Reorders data by date time, resolving API issues.

#### Usage

```
HydroImportFactory$postOrder(.)
```

#### Arguments

* `.`: (ignored).

### Method `findPeaks()`

Detect peaks within the HydroImport or HydroAggs objects

#### Usage

```
HydroImportFactory$findPeaks(
  levels = NULL,
  from = NULL,
  to = NULL,
  gaps = NULL
)
```

#### Arguments

* `levels`: The number of thresholds to apply against the time series.
Defaults to 100
* `from`: Set the location from where the thresholds should start.
Defaults to the bottom 10% value.
* `to`: Set the location from where the thresholds should end
Defaults to the maximum observed value.
* `gaps`: Used to pad out the run length encoding, prevents wobbly data
showing multiple exceedances. Set to 96.

### Method `addRating()`

Add a rating table to the HydroImport or HydroAggs objects

#### Usage

```
HydroImportFactory$addRating(C = NULL, A = NULL, B = NULL, max = NULL)
```

#### Arguments

* `C`: parameter
* `A`: parameter
* `B`: parameter
* `max`: parameter

### Method `addCatchProp()`

Add a catchment proportion table to the HydroImport or HydroAggs objects

#### Usage

```
HydroImportFactory$addCatchProp(total = NULL, area = NULL)
```

#### Arguments

* `total`: total area of catchment
* `area`: area of proportion covered by rain gauge
* `B`: parameter
* `max`: parameter

### Method `rateFlow()`

Converts stage to flow using the supplied rating. It uses the following
equation;
Q = C(h - a)^b

#### Usage

```
HydroImportFactory$rateFlow(start = 0, full = FALSE)
```

#### Arguments

* `start`: defaults to 0, change to set a different start point in the
rating conversion.
* `full`: If set to FALSE (default) a data table of stage and flows
will be supplied. If set to TRUE a new `HydroImport` object is created.

### Method `rateStage()`

Converts flow to stage using the supplied rating. It uses the following
equation;
h = ((Q/a)^(1/C))- b

#### Usage

```
HydroImportFactory$rateStage(start = 0, full = FALSE)
```

#### Arguments

* `start`: defaults to 0, change to set a different start point in the
rating conversion.
* `full`: If set to FALSE (default) a data table of stage and flows
will be supplied. If set to TRUE a new `HydroImport` object is created.

### Method `window()`

Extract windows of data from object using start and end date/times

#### Usage

```
HydroImportFactory$window(start = NULL, end = NULL, export = "dt")
```

#### Arguments

* `start`: Function calculates the date of the first imported record
* `end`: Function calculates the date of the last imported record
* `export`: Set as "dt", this exports a data.table of public data. If
set to "snip" then the R6 object is modified.
* `from`: Start date/time
* `to`: end date/time, if kept blank will default to last time step

### Method `coords()`

Return the coordinates of the gauge

#### Usage

```
HydroImportFactory$coords(.)
```

#### Arguments

* `.`: (ignored).

#### Examples

```
data(bewdley)
bewdley$coords()
```

### Method `nrfa()`

Return the NRFA details of the gauge

#### Usage

```
HydroImportFactory$nrfa(.)
```

#### Arguments

* `.`: (ignored).

#### Examples

```
data(bewdley)
bewdley$nrfa()
```

### Method `dataAgg()`

Return aggregated data

#### Usage

```
HydroImportFactory$dataAgg(type = NULL, method = NULL)
```

#### Arguments

* `type`: Set the aggrgegation level to either hourly', 'daily',
'monthly', 'annual', or 'hydroYear'
* `method`: Available aggregation methods include 'min', 'max',
'mean', 'median', and 'sum'

### Method `rollingAggs()`

Return the different user selecting rolling aggregations

#### Usage

```
HydroImportFactory$rollingAggs(
  method = "mean",
  rolls = c(1, 2, 3, 4, 8, 24, 120),
  interval = 0.25
)
```

#### Arguments

* `method`: Choose mean, median, min, max, and sum for volumes
* `rolls`: The hourly aggregations selected by the user
* `interval`: Corrects for the time step of the data, set to 0.25 hours

### Method `meta()`

Returns the metadata as a data.table

#### Usage

```
HydroImportFactory$meta(transform = FALSE)
```

#### Arguments

* `transform`: Set to FALSE. If TRUE the data.table is transformed
which increases readability in the console.

#### Examples

```
data(bewdley)
bewdley$meta()
```

### Method `dayStats()`

Daily statistics for imported data

#### Usage

```
HydroImportFactory$dayStats(plot = TRUE, year = NULL)
```

#### Arguments

* `plot`: Set to TRUE, will produce a plot of the statistics
* `year`: Set to NULL, overlays the mean daily flow for a chosen
hydrological year

### Method `plot()`

Plot the data by years

#### Usage

```
HydroImportFactory$plot(
  wrap = TRUE,
  cumul = FALSE,
  title = TRUE,
  theme = NULL,
  dir = NULL
)
```

#### Arguments

* `wrap`: Set to 3, use this to specify how many of columns of graphs
there should be
* `cumul`: Used to provide cumulative rainfall totals on rainfall
* `title`: Used to add a plot title and subtitle
* `theme`: Used to decide on EA colour them
* `dir`: Use this to save the plots

#### Examples

```
data(bewdley)
bewdley$plot()
```

### Method `flowDuration()`

Flow duration curve of data

#### Usage

```
HydroImportFactory$flowDuration(perc = c(5, 25, 75, 95))
```

#### Arguments

* `perc`: Determine flow percentiles. Set to c(5, 25, 75, 95)

### Method `quality()`

Displays the number of observations under each quality flag

#### Usage

```
HydroImportFactory$quality(.)
```

#### Arguments

* `.`: (ignored).

#### Examples

```
data(bewdley)
bewdley$quality()
```

### Method `missing()`

Finds missing values in the data series

#### Usage

```
HydroImportFactory$missing(.)
```

#### Arguments

* `.`: (ignored).

#### Examples

```
data(bewdley)
bewdley$missing()
```

### Method `exceed()`

Shows threshold exceedance statistics

#### Usage

```
HydroImportFactory$exceed(threshold = 0, gapWidth = 0)
```

#### Arguments

* `threshold`: The threshold at which to test against
* `gapWidth`: This allows you to ignore a set number of time steps
beneath a threshold between two or more events
* `.`: (ignored).

#### Examples

```
data(bewdley)
bewdley$exceed(threshold = 200, gapWidth = 20)
```

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

```
HydroImportFactory$clone(deep = FALSE)
```

#### Arguments

* `deep`: Whether to make a deep clone.

## Value

A class containing raw data, metadata and methods

## Examples

```r
## ------------------------------------------------
## Method `HydroImportFactory$methods`
## ------------------------------------------------

data(bewdley)
bewdley$methods()

## ------------------------------------------------
## Method `HydroImportFactory$summary`
## ------------------------------------------------

data(bewdley)
bewdley$summary()

## ------------------------------------------------
## Method `HydroImportFactory$statSummary`
## ------------------------------------------------

data(bewdley)
bewdley$statSummary()

## ------------------------------------------------
## Method `HydroImportFactory$coords`
## ------------------------------------------------

data(bewdley)
bewdley$coords()

## ------------------------------------------------
## Method `HydroImportFactory$nrfa`
## ------------------------------------------------

data(bewdley)
bewdley$nrfa()

## ------------------------------------------------
## Method `HydroImportFactory$meta`
## ------------------------------------------------

data(bewdley)
bewdley$meta()

## ------------------------------------------------
## Method `HydroImportFactory$plot`
## ------------------------------------------------

data(bewdley)
bewdley$plot()

## ------------------------------------------------
## Method `HydroImportFactory$quality`
## ------------------------------------------------

data(bewdley)
bewdley$quality()

## ------------------------------------------------
## Method `HydroImportFactory$missing`
## ------------------------------------------------

data(bewdley)
bewdley$missing()

## ------------------------------------------------
## Method `HydroImportFactory$exceed`
## ------------------------------------------------

data(bewdley)
bewdley$exceed(threshold = 200, gapWidth = 20)
```

# `hydroYearAgg`: hydroYearAgg

## Description

Aggregates sub annual time series into a hydrological year
resolution, aggregations are carried out to the calendar day not a rolling
time period.

## Usage

```r
hydroYearAgg(x, method = "mean", ...)

# S3 method for data.table
hydroYearAgg(x, method = "mean", ...)

# S3 method for HydroImport
hydroYearAgg(x, method = "mean", ...)
```

## Arguments

* `x`: Data generated in the riskyData package
* `method`: 'mean', 'median', 'max', 'min', or 'sum'
* `...`: Other variables as required

## Value

Data are aggregated to the hydrological year, data are not stored in a `HydroAggs` container.

## Examples

```r
data(bewdley)

# Calculate the hydrological year
bewdley$hydroYearDay()

# Calculate the annual maximum flows for Bewdley (hydrological year)
hydroYearAgg(bewdley, method = "max")
```

# `hydroYearDay`: HydroYearDay

## Description

Creates fields for hydrological year and hydrological day.
Speed is slow, needs conversion to C++ in future iterations.

## Usage

```r
hydroYearDay(x, calendar = "oct_us_gb", ...)

# S3 method for data.table
hydroYearDay(x, calendar = "oct_us_gb")

# S3 method for POSIXct
hydroYearDay(x, calendar = "oct_us_gb")
```

## Arguments

* `x`: Array of dates of class Date
* `calendar`: hydrological year calendar. Set to 'oct_us_gb' (USA and UK),
but can also be 'sep_br' (Brazil), 'apr_cl' (Chille).

## Value

Creates a hydrological year and hydrological day column

## Examples

```r
## Load bewdley dataset
data(bewdley)

## Inspect the data
bewdley$data

## Add hydrological year and hydrological day columns
bewdley$hydroYearDay()

## Print the bewdley dataset
bewdley
```

# `linkEvents`: A means to pair up hydrological events by nearest match time

## Description

A means to pair up hydrological events by nearest match time

## Usage

```r
linkEvents(
  upstream = NULL,
  downstream = NULL,
  timeMin = 0,
  timeMax = 1000,
  matrix = TRUE
)
```

## Arguments

* `upstream`: Peak values and date time of upstream site
* `downstream`: Peak values and date time of downstream site
* `timeMin`: Used to filter the time difference in paired peaks, set as 0
* `timeMax`: Used to filter the time difference in paired peaks, set as 1000
* `matrix`: Set to true, exports a matrix of difftimes in minutes that can
be used to highlight the peak matching. If set as FALSE a nearest join is used.

## Value

a data.table of paired events

## Examples

```r
bewdS <- loadAPI(ID = '2001', 'level', 900, 'instantaneous',
                 datapoints = 'range',
                 from = '2008-10-01 09:00')
                 bewdS$hydroYearDay()

buildS <- loadAPI(ID = '2134', 'level', 900, 'instantaneous',
                  datapoints = 'range',
                  from = '2008-10-01 09:00')
buildS$hydroYearDay()

bewdPS <- findPeaks(bewdS, levels = 100)
buildPS <- findPeaks(buildS, levels = 100)
pairedStage <- linkEvents(upstream = buildPS,
                          downstream = bewdPS)
pairedStage

## Plot the paired events
plot(pairedStage$value ~ pairedStage$`i.value`,
     xlab = 'Buildwas Stage (m)',
     ylab = "Bewdley Stage (m)")
model <- lm(pairedStage$value ~ pairedStage$`i.value`)
abline(model, lwd = 2, col = 2)
```

# `loadAPI`: Pull data from the Environment Agency's API

## Description

The loadAPI function is used to interogate the EAs API. Data for
all station can be pulled, or when specified to a single site various
datasets can be pulled.

## Usage

```r
loadAPI(
  ID = NULL,
  measure = NULL,
  period = NULL,
  type = NULL,
  datapoints = "standard",
  from = NULL,
  to = NULL,
  lat = NULL,
  long = NULL,
  easting = NULL,
  northing = NULL,
  dist = NULL,
  obsProperty = NULL,
  meta = TRUE,
  rtExt = FALSE,
  rtLookup = FALSE,
  assign = TRUE
)
```

## Arguments

* `ID`: Use to specify a particular WISKI ID or for when downloading all
stations 'wiski' or 'nrfa' to filter results. Additionally to access the
real time APIs use "flood" for all critical sites and "tidal" for the
category A tide gauges.
* `measure`: Use this when exporting observations to select the available
parameter. Generally 'flow', 'level', or 'groundwater'
* `period`: This is so you can select the time steps available, generally
900 (15 min) or 86400 (daily)
* `type`: Selects the data type, instantaneous, min, max etc.
* `datapoints`: Allows you to decide where in the time series you are
pulling the data from
* `from`: First time step of choice
* `to`: Last time step of choice
* `lat`: Latitude coordinate for when searching by geographic area
* `long`: Longitude coordinate for when searching by geographic area
* `easting`: Easting coordinate for when searching by geographic area
* `northing`: Northing coordinate for when searching by geographic area
* `dist`: Distance in km for how far you wish to search
* `obsProperty`: Used to filter the stations when identifying sites.
Available metrics;
"waterFlow", "waterLevel", "rainfall", "groundwaterLevel", "wind",
"temperature", "ammonium", "dissolved-oxygen", "conductivity", "ph",
"temperature", "turbidity", "nitrate", "chlorophyll", "salinity",
"bga", "fdom"
* `meta`: Set as TRUE, exports metadata for gauge with the data request
* `rtExt`: Set to NULL, if TRUE hydrological data series are extended with
the real time API
* `rtLookup`: Set as FALSE, this uses an inbuilt dataset, if set to TRUE
this will update the table using the most recent data. However, this can
dramatically increase the run time.

## Value

Various outputs see vignettes

## Examples

```r
#loadAPI()

loadAPI(easting = 378235, northing = 276165, dist = 30, ID = "nrfa")

loadAPI(ID = "L1207")

loadAPI(
  ID = "L1207",
  measure = "level",
  period = 900,
  type = "instantaneous",
  datapoints = "earliest"
)

dt <- loadAPI(
  ID = "L1207",
  measure = "level",
  period = 900,
  type = "instantaneous",
  datapoints = "all"
)

with(dt$data, plot(value ~ dateTime,
  type = "l",
  xlab = "Time",
  ylab = "Stage (mAoD)"
))

loadAPI('wiski')

loadAPI(easting = 378235, northing = 276165, dist = 30, ID = "nrfa")

loadAPI(ID = "L1207")

loadAPI(ID = c("2001", 2002),
          measure = c("level", "flow", "level"),
          period = 900,
          type = "instantaneous",
          datapoints = "earliest",
          rtLookup = FALSE)

loadAPI(ID = "L1207",
          measure = "level",
          period = 900,
          type = "instantaneous",
          datapoints = "latest")
```

# `loadAPIdata`: Pull data from the Environment Agency's API

## Description

The loadAPI function is used to interogate the EAs API. Data for
all station can be pulled, or when specified to a single site various
datasets can be pulled.

## Usage

```r
loadAPIdata(
  ID = NULL,
  measure = NULL,
  period = NULL,
  type = NULL,
  datapoints = "standard",
  from = NULL,
  to = NULL,
  lat = NULL,
  long = NULL,
  easting = NULL,
  northing = NULL,
  dist = NULL,
  obsProperty = NULL,
  meta = TRUE,
  rtExt = FALSE,
  rtLookup = FALSE,
  assign = TRUE
)
```

## Arguments

* `ID`: Use to specify a particular WISKI ID or for when downloading all
stations 'wiski' or 'nrfa' to filter results. Additionally to access the
real time APIs use "flood" for all critical sites and "tidal" for the
category A tide gauges.
* `measure`: Use this when exporting observations to select the available
parameter. Generally 'flow', 'level', or 'groundwater'
* `period`: This is so you can select the time steps available, generally
900 (15 min) or 86400 (daily)
* `type`: Selects the data type, instantaneous, min, max etc.
* `datapoints`: Allows you to decide where in the time series you are
pulling the data from
* `from`: First time step of choice
* `to`: Last time step of choice
* `lat`: Latitude coordinate for when searching by geographic area
* `long`: Longitude coordinate for when searching by geographic area
* `easting`: Easting coordinate for when searching by geographic area
* `northing`: Northing coordinate for when searching by geographic area
* `dist`: Distance in km for how far you wish to search
* `obsProperty`: Used to filter the stations when identifying sites.
Available metrics;
"waterFlow", "waterLevel", "rainfall", "groundwaterLevel", "wind",
"temperature", "ammonium", "dissolved-oxygen", "conductivity", "ph",
"temperature", "turbidity", "nitrate", "chlorophyll", "salinity",
"bga", "fdom"
* `meta`: Set as TRUE, exports metadata for gauge with the data request
* `rtExt`: Set to NULL, if TRUE hydrological data series are extended with
the real time API
* `rtLookup`: Set as FALSE, this uses an inbuilt dataset, if set to TRUE
this will update the table using the most recent data. However, this can
dramatically increase the run time.

## Value

Various outputs see vignettes

## Examples

```r
#loadAPI()

loadAPI(easting = 378235, northing = 276165, dist = 30, ID = "nrfa")

loadAPI(ID = "L1207")

loadAPI(
  ID = "L1207",
  measure = "level",
  period = 900,
  type = "instantaneous",
  datapoints = "earliest"
)

dt <- loadAPI(
  ID = "L1207",
  measure = "level",
  period = 900,
  type = "instantaneous",
  datapoints = "all"
)

with(dt$data, plot(value ~ dateTime,
  type = "l",
  xlab = "Time",
  ylab = "Stage (mAoD)"
))

loadAPI('wiski')

loadAPI(easting = 378235, northing = 276165, dist = 30, ID = "nrfa")

loadAPI(ID = "L1207")

loadAPI(ID = c("2001", 2002),
          measure = c("level", "flow", "level"),
          period = 900,
          type = "instantaneous",
          datapoints = "earliest",
          rtLookup = FALSE)

loadAPI(ID = "L1207",
          measure = "level",
          period = 900,
          type = "instantaneous",
          datapoints = "latest")
```

# `loadAPIDep`: Depreciated version Pull data from the Environment Agency's API

## Description

The loadAPI function is used to interogate the EAs API. Data for
all station can be pulled, or when specified to a single site various
datasets can be pulled.

## Usage

```r
loadAPIDep(
  ID = NULL,
  measure = NULL,
  period = NULL,
  type = NULL,
  datapoints = "standard",
  from = NULL,
  to = NULL,
  lat = NULL,
  long = NULL,
  easting = NULL,
  northing = NULL,
  dist = NULL,
  obsProperty = NULL,
  meta = TRUE,
  rtExt = FALSE,
  rtLookup = FALSE
)
```

## Arguments

* `ID`: Use to specify a particular WISKI ID or for when downloading all
stations 'wiski' or 'nrfa' to filter results. Additionally to access the
real time APIs use "flood" for all critical sites and "tidal" for the
category A tide gauges.
* `measure`: Use this when exporting observations to select the available
parameter. Generally 'flow', 'level', or 'groundwater'
* `period`: This is so you can select the time steps available, generally
900 (15 min) or 86400 (daily)
* `type`: Selects the data type, instantaneous, min, max etc.
* `datapoints`: Allows you to decide where in the time series you are
pulling the data from
* `from`: First time step of choice
* `to`: Last time step of choice
* `lat`: Latitude coordinate for when searching by geographic area
* `long`: Longitude coordinate for when searching by geographic area
* `easting`: Easting coordinate for when searching by geographic area
* `northing`: Northing coordinate for when searching by geographic area
* `dist`: Distance in km for how far you wish to search
* `obsProperty`: Used to filter the stations when identifying sites.
Available metrics;
"waterFlow", "waterLevel", "rainfall", "groundwaterLevel", "wind",
"temperature", "ammonium", "dissolved-oxygen", "conductivity", "ph",
"temperature", "turbidity", "nitrate", "chlorophyll", "salinity",
"bga", "fdom"
* `meta`: Set as TRUE, exports metadata for gauge with the data request
* `rtExt`: Set to NULL, if TRUE hydrological data series are extended with
the real time API
* `rtLookup`: Set as FALSE, this uses an inbuilt dataset, if set to TRUE
this will update the table using the most recent data. However, this can
dramatically increase the run time.

## Value

Various outputs see vignettes

## Examples

```r
#loadAPI()

loadAPI(easting = 378235, northing = 276165, dist = 30, ID = "nrfa")

loadAPI(ID = "L1207")

loadAPI(
  ID = "L1207",
  measure = "level",
  period = 900,
  type = "instantaneous",
  datapoints = "earliest"
)

dt <- loadAPI(
  ID = "L1207",
  measure = "level",
  period = 900,
  type = "instantaneous",
  datapoints = "all"
)

with(dt$data, plot(value ~ dateTime,
  type = "l",
  xlab = "Time",
  ylab = "Stage (mAoD)"
))
```

# `loadMulti`: Depreciated Load multiple `HydroImport` containers

## Description

The `loadMulti()` function iterates through combinations of
parameters supplied to the arguments. These can be added as vectors, this
reduces the amount of calls to `loadAPI()` in working scripts. Objects or
`HydroImport` are automatically assigned to the environment using a name
convention of *paramater*_*WISKI-ID*.

## Usage

```r
loadMulti(
  API = "EA",
  ID = NULL,
  measure = NULL,
  period = NULL,
  type = NULL,
  datapoints = NULL,
  from = NULL,
  to = NULL,
  ...
)
```

## Arguments

* `API`: Currently only supports the Environment Agency API set as "EA".
* `ID`: Use to specify a particular WISKI ID or for when downloading all
stations 'wiski' or 'nrfa' to filter results. Additionally to access the real
time APIs use "flood" for all critical sites and "tidal" for the category A
tide gauges. Vectors are supported.
* `measure`: Use this when exporting observations to select the available
parameter. Generally 'flow', 'level', or 'groundwater'
* `period`: This is so you can select the time steps available, generally
900 (15 min) or 86400 (daily).
* `type`: Selects the data type, instantaneous, min, max etc.
* `datapoints`: Allows you to decide where in the time series you are
pulling the data from.
* `from`: First time step of choice.
* `to`: Last time step of choice.
* `...`: Additional `loadAPI()` arguments can be called.

## Value

Multiple `HydroImport` containers

## Examples

```r
loadMulti(ID = c('2001', '2002'),
          measure = c('flow', 'level'),
          period = 900,
          type = 'instantaneous',
          datapoints = 'earliest')
```

# `loadPE`: Load potential evaporation data

## Description

Load potential evaporation data

## Usage

```r
loadPE(link, skip = 0, metaRows = 10, impute = TRUE)
```

## Arguments

* `link`: Link to the specified file for import
* `skip`: Stet to zero, denotes the number off rows you wish to skip
* `metaRows`: Set to 10, determines the amount of rows set as metadata
* `impute`: Set to TRUE, this fills in missing data using the
'forecast::na.interp()' function

## Details

Loads PE data obtained from HYRAD. Missing data are imputed using
the 'na.interp()' function in the forecast package. For the seasonal series,
a robust STL decomposition is first computed. Then a linear interpolation is
applied to the seasonally adjusted data, and the seasonal component is added
back.

## Value

A tidied dataset of PE data

# `mergeData`: Merge HydroImport and HydroAggs R6 objects

## Description

The `mergeData()` is used to combine multiple R6 encapsulations.
The data have to be in `HydroImport` or `HydroAggs` classes. Depending on the
metadata argument, metadata can be exported with the merged dataset as a
list. The merging process is carried out on looped full joins, this allows
datasets f varying lengths to be collated. Missing information is padded
with NAs.

## Usage

```r
mergeData(..., list = NULL, metadata = FALSE)
```

## Arguments

* `...`: Datasets downloaded from the EAs API set inside an R6 container
* `list`: set to Null, however lists of hydroImports can be placed here if required
* `metadata`: Set to FALSE, if TRUE collated metadata are exported with the
merged dataset

## Value

A merged dataset as a data,table, if metadata is TRUE then a list of
2 data.tables

## Examples

```r
a <- loadAPI(ID = 2001,
             measure = 'flow',
             period = 900,
             type = 'instantaneous',
             datapoints = 'range',
             from = '2023-06-12 09:00',
             to = '2023-06-13 09:00')

b <- loadAPI(ID = 2002,
             measure = 'flow',
             period = 900,
             type = 'instantaneous',
             datapoints = 'range',
             from = '2023-06-12 09:00',
             to = '2023-07-12 09:00')

c <- loadAPI(ID = 2004,
             measure = 'flow',
             period = 900,
             type = 'instantaneous',
             datapoints = 'range',
             from = '2023-06-13 09:00',
             to = '2023-07-12 09:00')

d <- loadAPI(ID = 2001,
             measure = 'level',
             period = 900,
             type = 'instantaneous',
             datapoints = 'range',
             from = '2023-06-12 09:00',
             to = '2023-07-12 09:00')

z <- mergeData(a, b, c, d, metadata = FALSE)
z
```

# `monthlyAgg`: monthlyAgg

## Description

Aggregates sub monthly time series into a monthly resolution,
aggregations are carried out to the calendar day not a rolling time period.

## Usage

```r
monthlyAgg(x, method = "mean", ...)

# S3 method for data.table
monthlyAgg(x, method = "mean", ...)

# S3 method for HydroImport
monthlyAgg(x, method = "mean", ...)
```

## Arguments

* `x`: Data generated in the riskyData package
* `method`: 'mean', 'median', 'max', 'min', or 'sum'
* `...`: Other variables as required

## Value

An aggregated dataset taken to the calendar month, data are not stored in a `HydroAggs` container.

## Examples

```r
data(bewdley)

## Monthly minimum flow
monthlyAgg(bewdley, method = "min")
```

# `naRun`: This identifies where NA values in the time series sit

## Description

This identifies where NA values in the time series sit

## Usage

```r
naRun(dateTime = NULL, value = NULL, timestep = 900)
```

## Arguments

* `dateTime`: The date and time series
* `value`: The observed data
* `timestep`: The time step used applied in the observed data in seconds

## Value

A data.table containing the start and end points of NA runs

## Examples

```r
data(bewdley)
nas <- naRun(bewdley$data$dateTime,
             bewdley$data$value,
             timestep = 900)
nas
```

# `rainSep`: Rainfall event separation

## Description

The `rainSep()` function uses run length encoding to find
consecutive rainfall periods. To avoid brief timesteps of no rain from
splitting events, the use of `RcppRoll::roll_sum()` can pad out the event d
urations.

## Usage

```r
rainSep(dateTime = NULL, precip = NULL, threshold = 0, minTotal = 0, roll = 0)
```

## Arguments

* `dateTime`: DateTime variable from time series.
* `precip`: Observed rainfall data.
* `threshold`: Used to tidy averaged and radar data where very low
accumulations can be recorded, defaults to 0mm.
* `minTotal`: Used to help tidy the total number of events. Sets a
minimum event total rainfall.
* `roll`: Number of time steps so use in the `RcppRoll::roll_sum()`.

## Value

Function returns a data table with event ID, start and end times of
the events, and the total accumulation for the events

## Examples

```r
## No accumulation applied, minimum total set to 2mm
rainSep(dateTime = chesterton$data$dateTime,
      precip = chesterton$data$value,
      threshold = 0,
      minTotal = 2,
      roll = 0)

## 2 hr rolling accumulation
rainSep(dateTime = chesterton$data$dateTime,
        precip = chesterton$data$value,
        threshold = 0,
        roll = 8)

## Window plots
plot(chesterton$data$dateTime[1:200000], chesterton$data$value[1:200000],
     ylim = rev(range(chesterton$data$value[1:200000], na.rm = TRUE)),
     type = 'h', ylab = "Rainfall (mm)", xlab = "Date Time")
dayRain <- rainSep(dateTime = chesterton$data$dateTime[1:200000],
                   precip = chesterton$data$value[1:200000],
                   threshold = 0,
                   minTotal = 4,
                   roll = 20)
for (i in seq_along(dayRain$id)){
  polygon(x = c(dayRain$start[i], dayRain$start[i], dayRain$end[i],
                dayRain$end[i]),
          y = c(0, 12, 12, 0),
          col = scales::alpha('red', 0.5),
          border = NA)
}
lines(chesterton$data$dateTime[1:200000], chesterton$data$value[1:200000])
```

# `rmHY`: Remove hydrological year from HydroImport and HydroAggs classes

## Description

Remove hydrological year from HydroImport and HydroAggs classes

## Usage

```r
rmHY(x)

# S3 method for data.table
rmHY(x)

# S3 method for HydroImport
rmHY(x)

# S3 method for HydroAggs
rmHY(x)
```

## Arguments

* `x`: Raw data in data.table format in `HydroImport` or `HydroAggs` classes

## Value

The hydroYear column removed from the `HydroImport` or `HydroAggs` containers public fields `obj$data`

## Examples

```r
## Do not run
# obj$rmHY()
```

# `rmHYD`: Remove hydrological day of year from hydroYear class

## Description

Remove hydrological day of year from hydroYear class

## Usage

```r
rmHYD(x)

# S3 method for data.table
rmHYD(x)

# S3 method for HydroImport
rmHYD(x)

# S3 method for HydroAggs
rmHYD(x)
```

## Arguments

* `x`: Raw data in data.table format in hydroLoad class

## Value

The hydroYearDay column removed from the `HydroImport` or `HydroAggs` containers public fields `obj$data`

## Examples

```r
## Do not run
# obj$rmHYD()
```

# `rmVol`: Remove calculated volume from hydroYear class

## Description

Remove calculated volume from hydroYear class

## Usage

```r
rmVol(x)

# S3 method for data.table
rmVol(x)

# S3 method for HydroImport
rmVol(x)

# S3 method for HydroAggs
rmVol(x)
```

## Arguments

* `x`: Raw data in data.table format in hydroLoad class

## Value

The volume column removed from the `HydroImport` or `HydroAggs` containers public fields `obj$data`

## Examples

```r
## Do not run
# obj$rmVol()
```

# `rollingAggs`: rollingAggs

## Description

Provides a rolling aggregation of a user defined periods.

## Usage

```r
rollingAggs(x, rolls = NULL, interval = 0.25, method = "mean")

# S3 method for data.table
rollingAggs(x, rolls = NULL, interval = 0.25, method = "mean")
```

## Arguments

* `rolls`: User defined periods of aggregation
* `interval`: Set as 0.25 to represent 15 minute data, for hourly change
to 1 etc.
* `method`: 'min', 'max', 'mean', 'median', and 'sum' options available
* `dt`: Input dat in R6 object

## Value

A dataset containing numerous aggregations on a roll

## Examples

```r
## Do not run
# rollingAggs(Buildwas)
```

# `rtAPI`: Access the realtime API

## Description

Access the realtime API

## Usage

```r
rtAPI(ID = NULL, measure = NULL)
```

## Arguments

* `ID`: Station Reference ID
* `measure`: Type of measurement required

## Value

A data.table of data exported from the EAs realtime API

## Examples

```r
## Generate a table of all IDs available
stationIDs <- riskyData::loadAPI("flood")

## Find parameters available
rtAPI(ID = "2001")
## Download flow data
data <- rtAPI(ID = "2001", measure = "flow")
plot(data$value, type = 'l')
```

# `snipRange`: snipRange used in the loadAPI function to correct "range" datapoints

## Description

Extract windows of data from data.table using start and end date/times

## Usage

```r
snipRange(x = NULL, start = NULL, end = NULL)
```

## Arguments

* `x`: A data.table used in the loadAPI function
* `start`: Start date/time
* `end`: End date/time, if kept blank will default to last time step

## Value

A data.table of outputs

## Examples

```r
## CODE NOT RUN
```

