#' @title HydroYearDay
#'
#' @description Creates fields for hydrological year and hydrological day.
#'
#' @description Speed is slow, needs conversion to C++ in future iterrations.
#'
#' @param d Array of dates of class Date
#' @param hy_cal hydrological year calendar. Set to 'oct_us_gb' (USA and UK), but can also be 'sep_br'
#' (Brazil), 'apr_cl' (Chille).
#'
#' @return Creates a hydrological year and hydrological day column
#' @export
#'
#' @examples
#' hydroYearDay(Buildwas$GaugeData$DateTime, hy_cal = 'oct_us_gb')
hydroYearDay <- function(x, calendar = 'oct_us_gb', ...) {
  UseMethod('hydroYearDay', x)
}

#' @rdname hydroYearDay
#' @export
hydroYearDay.data.table <-function(x, calendar = 'oct_us_gb'){
  date <-as.Date(x$dateTime)

  m <- as.numeric(month(date)) # extract month
  y <- as.numeric(year(date)) # extract year

  ## Create array for hydrological year
  hydroYear <- y

  if (calendar == 'oct_us_gb'){
    ## USA and Great Britain
    # Hydrological year 2010 starts on Oct 1st 2009 and finishes on Sep 30th 2010
    hydroYear[m >= 10] <- (hydroYear[m >= 10] + 1)
    startHY <- as.Date(paste0(hydroYear - 1,'-10-01'))

  } else if(calendar == 'sep_br'){
    ## Brazil
    # Hydrological year 2010 starts on Sep 1st 2009 and finishes on Aug 31st 2010
    hydroYear[m >= 9] <- (hydroYear[m >= 9] + 1)
    startHY <- as.Date(paste0(hydroYear - 1,'-09-01'))

  } else if(calendar == 'apr_cl'){
    ## Chile
    # Hydrological year 2010 starts on Apr 1st 2010 and finishes on Mar 31st 2011
    hydroYear[m <= 3] <-(hydroYear[m <= 3] - 1)
    startHY <- as.Date(paste0(hydroYear, '-04-01'))

  } else {
    stop(paste0('Unkown hydrological year calendar:', calendar))
  }

  ## days since the beginning of the hydro year
  hydroYearDay <- as.numeric(date - startHY + 1)

  if (any(hydroYearDay <1|hydroYearDay>366)){
    stop('Error when computing day of hydro year')
  }
  return(data.table(hydroYear, hydroYearDay))
}

#' @rdname hydroYearDay
#' @export
hydroYearDay.default <-function(x, hy_cal = 'oct_us_gb'){

  if(class(x)!='Date'){stop('x should be of class Date - use as.Date')}

  m <- as.numeric(month(x)) # extract month
  y <- as.numeric(year(x)) # extract year

  ## Create array for hydrological year
  HydrologicalYear <- y

  if (hy_cal == 'oct_us_gb'){
    ## USA and Great Britain
    # Hydrological year 2010 starts on Oct 1st 2009 and finishes on Sep 30th 2010
    HydrologicalYear[m >= 10] <- (HydrologicalYear[m >= 10] + 1)
    start_hy <- as.Date(paste0(HydrologicalYear - 1,'-10-01'))

  } else if(hy_cal=='sep_br'){
    ## Brazil
    # Hydrological year 2010 starts on Sep 1st 2009 and finishes on Aug 31st 2010
    HydrologicalYear[m >= 9] <- (HydrologicalYear[m >= 9] + 1)
    start_hy <- as.Date(paste0(HydrologicalYear - 1,'-09-01'))

  } else if(hy_cal=='apr_cl'){
    ## Chile
    # Hydrological year 2010 starts on Apr 1st 2010 and finishes on Mar 31st 2011
    HydrologicalYear[m <= 3] <-(HydrologicalYear[m <= 3] - 1)
    start_hy <- as.Date(paste0(HydrologicalYear, '-04-01'))

  } else {
    stop(paste0('Unkown hydrological year calendar:', hy_cal))
  }

  ## days since the beginning of the hydro year
  day_of_hy <- as.numeric(x - start_hy+1)

  if (any(day_of_hy <1|day_of_hy>366)){
    stop('Error when computing day of hydro year')
  }
  return(data.table(HydrologicalYear, day_of_hy))
}

