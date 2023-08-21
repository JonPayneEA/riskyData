#' Remove hydrological year from hydroYear class
#'
#' @param x Raw data in data.table format in hydroLoad class
#'
#' @return
#' @export
#'
#' @examples
#' # obj$rmHY()
rmHY <- function(x){
  UseMethod('rmHY', x)
}

#' @rdname rmHY
#' @export
rmHY.data.table <- function(x){
  if ('hydroYear' %in% colnames(x)){
    x[, hydroYear := NULL,]
  } else {
    cat('hydroYear field is not present in the data.table\n')
  }
}

#' Remove hydrological day of year from hydroYear class
#'
#' @param x Raw data in data.table format in hydroLoad class
#'
#' @return
#' @export
#'
#' @examples
#'  # obj$rmHYD()
rmHYD <- function(x){
  UseMethod('rmHYD', x)
}

#' @rdname rmHYD
#' @export
rmHYD.data.table <- function(x){
  if ('hydroYearDay' %in% colnames(x)){
    x[, hydroYearDay := NULL,]
  } else {
    cat('hydroYearDay field is not present in the data.table\n')
  }
}

#' Remove calculated volume from hydroYear class
#'
#' @param x Raw data in data.table format in hydroLoad class
#'
#' @return
#' @export
#'
#' @examples
#' # obj$rmVol()
rmVol <- function(x){
  UseMethod('rmVol', x)
}

rmVol.data.table <- function(x){
  if ('volume' %in% colnames(x)){
    x[, volume := NULL,]
  } else {
    cat('Volume field is not present in the data.table\n')
  }
}
