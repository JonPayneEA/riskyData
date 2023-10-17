#' @title Add convert flow or rainfall data to a volume
#'
#' @param x Data set of 'flowLoad' or 'rainLoad' class
#' @param area_km Area of coverage for rain gauges
#'
#' @return Calculated volume of flow or rainfall
#' @export
#'
#' @examples
#' # Load bewdley dataset
#' data(bewdley)
#'
#' # Calculate volumes
#' asVol(bewdley)
#'
#' # Print the bewdley dataset
#' bewdley
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
asVol.HydroImport <- function(x, area = NULL) {
  tDiff <- difftime(x$data$dateTime[2],
                    x$data$dateTime[1],
                    units = "mins"
  )
  tDiff <- as.numeric(tDiff)
  x$data[, volume := value * tDiff * 60]
}

#' @rdname asVol
#' @export
asVol.HydroAggs <- function(x, area = NULL) {
  tDiff <- difftime(x$data$dateTime[2],
                    x$data$dateTime[1],
                    units = "mins"
  )
  tDiff <- as.numeric(tDiff)
  x$data[, volume := value * tDiff * 60]
}
