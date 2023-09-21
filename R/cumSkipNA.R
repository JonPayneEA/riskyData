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
#' cumsumNA(rain)
cumsumNA <- function(x, ...) {
  UseMethod("cumsumNA", x)
}

#' @rdname cumsumNA
#' @export
cumsumNA <- function(x) {
  dt <- data.table(
    dateTime = x$dateTime,
    cumSum = cumsum(ifelse(is.na(x$value), 0, x$value)) + x$value * 0
  )
  return(dt)
}
