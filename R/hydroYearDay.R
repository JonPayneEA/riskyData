#' @title HydroYearDay
#'
#' @description Creates fields for hydrological year and hydrological day.
#'
#' @description Speed is slow, needs conversion to C++ in future iterations.
#'
#' @param d Array of dates of class Date
#' @param hy_cal hydrological year calendar. Set to 'oct_us_gb' (USA and UK),
#' but can also be 'sep_br' (Brazil), 'apr_cl' (Chille).
#'
#' @return Creates a hydrological year and hydrological day column
#' @export
#'
#' @examples
#' # Load bewdley dataset
#' data(bewdley)
#'
#' # Inspect the data
#' bewdley$data
#'
#' # Add hydrological year and hydrological day columns
#' bewdley$hydroYearDay()
#'
#' # Print the bewdley dataset
#' bewdley
hydroYearDay <- function(x, calendar = "oct_us_gb", ...) {
  UseMethod("hydroYearDay", x)
}

#' @rdname hydroYearDay
#' @export
hydroYearDay.data.table <- function(x, calendar = "oct_us_gb") {
  date <- as.Date(x$dateTime)

  m <- as.numeric(month(date)) # extract month
  y <- as.numeric(year(date)) # extract year

  ## Create array for hydrological year
  hydroYear <- y

  if (calendar == "oct_us_gb") {
    ## USA and Great Britain
    # Hydrological year 2010 starts on Oct 1 2009 and finishes on Sep 30 2010
    hydroYear[m >= 10] <- (hydroYear[m >= 10] + 1)
    startHY <- as.Date(paste0(hydroYear - 1, "-10-01"))
  } else if (calendar == "sep_br") {
    ## Brazil
    # Hydrological year 2010 starts on Sep 1 2009 and finishes on Aug 31 2010
    hydroYear[m >= 9] <- (hydroYear[m >= 9] + 1)
    startHY <- as.Date(paste0(hydroYear - 1, "-09-01"))
  } else if (calendar == "apr_cl") {
    ## Chile
    # Hydrological year 2010 starts on Apr 1 2010 and finishes on Mar 31 2011
    hydroYear[m <= 3] <- (hydroYear[m <= 3] - 1)
    startHY <- as.Date(paste0(hydroYear, "-04-01"))
  } else {
    stop(paste0("Unkown hydrological year calendar:", calendar))
  }

  ## days since the beginning of the hydro year
  hydroYearDay <- as.numeric(date - startHY + 1)

  if (any(hydroYearDay < 1 | hydroYearDay > 366)) {
    stop("Error when computing day of hydro year")
  }
  return(data.table(hydroYear, hydroYearDay))
}
