#' @title cumsumNA
#'
#' @description Provides a cumulative sum of rain gauge data while excluding NA
#' values.
#'
#' @param x Raw rain gauge data or merged dataset
#' @param ... Additional parameters as required
#'
#' @return A cumulative sum of rain gauge data
#' @export
#'
#' @examples
#' data(chesterton)
#'
#' # Apply cumulative sum
#' dt <- cumsumNA(chesterton)
#'
#' plot(dt$dateTime,
#'      dt$cumSum,
#'      main = "Cumulative rainfall plot for Chesterton Gauge (WISKI: 164163)",
#'      xla = "Date Time",
#'      ylab = "Cumulative rainfall (mm)",
#'      type = "l")
cumsumNA <- function(x, ...) {
  UseMethod("cumsumNA", x)
}

#' @rdname cumsumNA
#' @export
cumsumNA.default <- function(x) {
  dt <- data.table(
    dateTime = x$dateTime,
    cumSum = cumsum(ifelse(is.na(x$value), 0, x$value)) + x$value * 0
  )
  return(dt)
}

#' @rdname cumsumNA
#' @export
cumsumNA.HydroImport <- function(x) {
  dt <- data.table(
    dateTime = x$data$dateTime,
    cumSum = cumsum(ifelse(is.na(x$data$value), 0, x$data$value)) + x$data$value * 0
  )
  return(dt)
}

#' @rdname cumsumNA
#' @export
cumsumNA.HydroAggs <- function(x) {
  dt <- data.table(
    dateTime = x$data$dateTime,
    cumSum = cumsum(ifelse(is.na(x$data$value), 0, x$data$value)) + x$data$value * 0
  )
  return(dt)
}
