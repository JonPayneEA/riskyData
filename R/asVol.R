#' @title Add convert flow or rainfall data to a volume
#'
#' @param x Data set of 'flowLoad' or 'rainLoad' class
#' @param area_km Area of coverage for rain gauges
#'
#' @return Calculated volume of flow or rainfall
#' @export
#'
#' @examples
#' # oundle <- loadWISKI(files[4])
#' # oundle
#' # asVol.rainLoad(oundle, area = 40)
#' # oundle
#' #
#' #
#' # link <- 'C:/Users/jpayne05/Desktop/Buildwas_15min_Flow.csv'
#' # Buildwas <- loadWISKI(link)
#' # Buildwas
#' # asVol.flowLoad(Buildwas)
#' # Buildwas
asVol <- function(x, area) {
  UseMethod("asVol", x)
}

#' @rdname asVol
#' @export
asVol.data.table <- function(x, area = NULL) {
  tDiff <- difftime(x$dateTime[2],
    x$dateTime[1],
    units = "mins"
  )
  tDiff <- as.numeric(tDiff)
  x[, volume := value * tDiff * 60]
}

#' @rdname asVol
#' @export
asVol.flowLoad <- function(x, area = NULL) {
  tDiff <- difftime(x$GaugeData$DateTime[2],
    x$GaugeData$DateTime[1],
    units = "mins"
  )
  tDiff <- as.numeric(tDiff)
  x$GaugeData[, Volume := Value * tDiff * 60]
  return(x)
}

#' @rdname asVol
#' @export
asVol.rainLoad <- function(x, area = NULL) {
  if (is.null(area)) {
    stop("Please specify the catchment area")
  }
  area <- area * 1000000
  tDiff <- difftime(x$GaugeData$DateTime[2],
    x$GaugeData$DateTime[1],
    units = "mins"
  )
  tDiff <- as.numeric(tDiff)
  x$GaugeData[, Volume := (Value / 1000) * area]
  return(x)
}
