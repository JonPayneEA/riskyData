#' @title dayStats
#'
#' @description Daily statistics for imported data
#'
#' @param x Data generated in the riskyData package
#' @param methods 'mean', 'median', 'max', 'min', or 'sum'
#' @param percentiles Set to 5, 25, 75, and 95
#' @param hyrdroDay Set to TRUE, use hydrological day
#' @param plot Set to TRUE, will produce a plot of the statistics
#' @param ... Other variables as required
#'
#' @return Data are aggregated to a daily resolution
#' @export
#'
#' @import data.table
#'
#' @examples
#' #dayStats(Buildwas)
dayStats <- function(x,
                     methods = c('mean', 'median', 'min', 'max'),
                     percentiles =  c(5,25,75,95),
                     hydroDay = TRUE,
                     plot = TRUE,
                     ...) {
  UseMethod('dayStats', x)
}

#' @rdname dayStats
#' @export
dayStats.data.table <- function(x,
                                methods = c('mean', 'median', 'min', 'max'),
                                percentiles =  c(5,25,75,95),
                                hydroDay = TRUE,
                                plot = TRUE,
                                ...){
  dt <- x
  if(hydroDay == TRUE){
    dt$dayYear <- dt$hydroYearDay
  } else {
    dt$dayYear <- yday(gauge$data$dateTime)
  }

  if(any(methods %in% 'mean')) {
    dt1 <- dt[, .(Mean = mean(value, na.rm = TRUE),
                  Median = median(value, na.rm = TRUE),
                  Min = min(value, na.rm = TRUE),
                  Max = max(value, na.rm = TRUE)), by = 'dayYear']

  }

  for (i in percentiles) {
    ptile <- dt[, .(Percentile = quantile(value, i/100, na.rm = TRUE)),
                by = 'dayYear']
    names(ptile)[names(ptile) == 'Percentile'] <- paste0('Perc', i)
    dt1 <- merge(dt1, ptile, by = 'dayYear')
  }
  if(plot == TRUE){
    a <- dt1[-366,,]

    plot(a$Mean, type = 'l',
         xlim = c(0, 365),
         ylim = c(min(a$Min)*0.9, max(a$Max)*1.1),
         ylab = 'Flow (cumecs)',
         xlab = 'Day of year')
    polygon(c(a$dayYear, rev(a$dayYear)), c(a$Max, rev(a$Min)), col = 'royalblue3')
    polygon(c(a$dayYear, rev(a$dayYear)), c(a$Perc5, rev(a$Perc95)), col = 'royalblue1')
    polygon(c(a$dayYear, rev(a$dayYear)), c(a$Perc25, rev(a$Perc75)), col = 'lightblue')
    lines(a$Perc25 ~ a$dayYear, lwd = 2, col = 'lightblue')
    lines(a$Perc75 ~ a$dayYear, lwd = 2, col = 'lightblue')
    lines(a$Perc95 ~ a$dayYear, lwd = 2, col = 'royalblue1')
    lines(a$Perc5 ~ a$dayYear, lwd = 2, col = 'royalblue1')
    lines(a$Min ~ a$dayYear, lwd = 2, col = 'royalblue3')
    lines(a$Max ~ a$dayYear, lwd = 2, col = 'royalblue3')
    lines(a$Median ~ a$dayYear, lwd = 2, lty = 3)
    lines(a$Mean ~ a$dayYear, lwd = 2, col = 'darkblue')
    return(a)
  }else{
  return(dt1[-366,,])
  }
}
