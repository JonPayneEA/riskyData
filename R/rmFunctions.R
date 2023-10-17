#' Remove hydrological year from HydroImport and HydroAggs classes
#'
#' @param x Raw data in data.table format in `HydroImport` or `HydroAggs` classes
#'
#' @return The hydroYear column removed from the `HydroImport` or `HydroAggs` containers public fields `obj$data`
#' @export
#'
#' @examples
#' ## Do not run
#' # obj$rmHY()
rmHY <- function(x) {
  UseMethod("rmHY", x)
}

#' @rdname rmHY
#' @export
rmHY.data.table <- function(x) {
  if ("hydroYear" %in% colnames(x)) {
    x[, hydroYear := NULL, ]
  } else {
    cat("hydroYear field is not present in the data.table\n")
  }
}

#' @rdname rmHY
#' @export
rmHY.HydroImport <- function(x) {
  if ("hydroYear" %in% colnames(x$data)) {
    x$data[, hydroYear := NULL, ]
  } else {
    cat("hydroYear field is not present in the data.table\n")
  }
}

#' @rdname rmHY
#' @export
rmHY.HydroAggs <- function(x) {
  if ("hydroYear" %in% colnames(x$data)) {
    x$data[, hydroYear := NULL, ]
  } else {
    cat("hydroYear field is not present in the data.table\n")
  }
}

#' Remove hydrological day of year from hydroYear class
#'
#' @param x Raw data in data.table format in hydroLoad class
#'
#' @return The hydroYearDay column removed from the `HydroImport` or `HydroAggs` containers public fields `obj$data`
#' @export
#'
#' @examples
#' ## Do not run
#' # obj$rmHYD()
rmHYD <- function(x) {
  UseMethod("rmHYD", x)
}

#' @rdname rmHYD
#' @export
rmHYD.data.table <- function(x) {
  if ("hydroYearDay" %in% colnames(x)) {
    x[, hydroYearDay := NULL, ]
  } else {
    cat("hydroYearDay field is not present in the data.table\n")
  }
}


#' @rdname rmHYD
#' @export
rmHYD.HydroImport <- function(x) {
  if ("hydroYearDay" %in% colnames(x$data)) {
    x$data[, hydroYearDay := NULL, ]
  } else {
    cat("hydroYearDay field is not present in the data.table\n")
  }
}

#' @rdname rmHYD
#' @export
rmHYD.HydroAggs <- function(x) {
  if ("hydroYearDay" %in% colnames(x$data)) {
    x$data[, hydroYearDay := NULL, ]
  } else {
    cat("hydroYearDay field is not present in the data.table\n")
  }
}


#' Remove calculated volume from hydroYear class
#'
#' @param x Raw data in data.table format in hydroLoad class
#'
#' @return The volume column removed from the `HydroImport` or `HydroAggs` containers public fields `obj$data`
#' @export
#'
#' @examples
#' ## Do not run
#' # obj$rmVol()
rmVol <- function(x) {
  UseMethod("rmVol", x)
}

#' @rdname rmVol
#' @export
rmVol.data.table <- function(x) {
  if ("volume" %in% colnames(x)) {
    x[, volume := NULL, ]
  } else {
    cat("Volume field is not present in the data.table\n")
  }
}

#' @rdname rmVol
#' @export
rmVol.HydroImport <- function(x) {
  if ("volume" %in% colnames(x$data)) {
    x$data[, volume := NULL, ]
  } else {
    cat("Volume field is not present in the data.table\n")
  }
}

#' @rdname rmVol
#' @export
rmVol.HydroAggs <- function(x) {
  if ("volume" %in% colnames(x$data)) {
    x$data[, volume := NULL, ]
  } else {
    cat("Volume field is not present in the data.table\n")
  }
}
