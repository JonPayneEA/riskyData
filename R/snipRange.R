#' @title snipRange used in the loadAPI function to correct "range" datapoints
#'
#' @description
#' Extract windows of data from data.table using start and end date/times
#'
#' @param x A data.table used in the loadAPI function
#' @param start Start date/time
#' @param end End date/time, if kept blank will default to last time step
#'
#' @return A data.table of outputs
#' @export
#'
#' @examples
#' ## CODE NOT RUN
snipRange <- function(x = NULL, start = NULL, end = NULL){
  if (is.null(start)){
    stop("Please include a start date for the from argument")
  }
  if (is.null(x)){
    stop("Please include data.table of interest")
  }


  #! R assumes that dates are BST during the summer months
  #! This causes an hour shift when running queries

  ## Constrain dates to GMT
  start <- as.POSIXct(start,
                      format = "%Y-%m-%d %H:%M",
                      tz = "UTC"
  )

  # If the to argument is null the last record is assumed
  if (is.null(end)){
    end <- as.POSIXct(tail(x$dateTime, 1),
                      format = "%Y-%m-%dT%H:%M",
                      tz = "UTC"
    )
  } else {
    end <- as.POSIXct(end,
                      format = "%Y-%m-%d %H:%M",
                      tz = "UTC"
    )
  }
  ## Snipping the data
  dt <- x[dateTime >= start & dateTime <= end,,]

  return(dt)
}

