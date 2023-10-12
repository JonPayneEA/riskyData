#' @title Add convert flow or rainfall data to a volume
#'
#' @param x Data set of 'flowLoad' or 'rainLoad' class
#' @param area_km Area of coverage for rain gauges
#'
#' @return Calculated volume of flow or rainfall
#' @export
#'
#' @examples
#' ## Do not run
#' # asVol(obj)
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
  x$GaugeData[, volume := value * tDiff * 60]
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
  x$GaugeData[, volume := (value / 1000) * area]
  return(x)
}
