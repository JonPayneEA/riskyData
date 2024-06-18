#' @title Calculate the observed SAAR values from rain gauge data
#'
#' @param gaugeIDs A vector of rain gauge WISKI IDs that can be fed into
#' `riskyData::loadAPI()`
#'
#' @return A list containing the annual rainfalls, summary statistics and plots
#' @export
#'
#' @examples
#' rainGaugeIDs <- c('015347','021228', '013045', 'NE034')
#' getGaugeSAAR(rainGaugeIDs)
getGaugeSAAR <- function(gaugeIDs){
  ## Loop through sites and export to list
  yrTotals <- list()
  for(s in gaugeIDs){
    ## Read daily data from API
    siteR1 <- riskyData::loadAPI(ID = s,
                                 measure = 'rainfall',
                                 period = 86400,
                                 type = 'total',
                                 datapoints = 'all')

    ## Add hydro year info to the data
    siteR1$hydroYearDay()

    ## Get range of hydrological years
    startYr <- range(siteR1$data$hydroYear)[1]
    endYr <- range(siteR1$data$hydroYear)[2]
    ## Account for leap years in final year - does not account for years
    ## divisible by 100 or 400 ( not required).
    ## i.e., y %% 4 == 0 & (y %% 100 != 0 | y %% 00 == 0)
    endDay <- tail(siteR1$data$hydroYearDay, n = 1)
    if (endDay < 365 || (endYr %% 4) == 0 & endDay < 366) endYr <- endYr - 1

    # Calculate annual rainfall totals
    site <- siteR1$meta()$stationName
    yrTotals[[s]] <- siteR1$data[hydroYear >= startYr & hydroYear <= endYr,
                                 .(totals = sum(value, na.rm = TRUE),
                                   Site = site,
                                   WISKI = s),
                                 by = 'hydroYear']
  }

  ## Bind list to data.table
  yrTotals <- data.table::rbindlist(yrTotals)

  ## Return both the SAAR values and their error estimates
  ## Export to list
  gaugeSAAR <- list()
  gaugeSAAR[["Raw"]] <- yrTotals
  ## Standard error function
  std <- function(x) sd(x) / sqrt(length(x))

  ## Calculate summary stats
  gaugeSAAR[["Summary"]] <- yrTotals[, .(GaugeSAAR = mean(totals),
                                         Error_1sigma = std(totals)),
                                     by = 'Site']
  ## Generate plotting
  gaugeSAAR[["Plot"]] <-  ggplot2::ggplot(yrTotals, ggplot2::aes(x = hydroYear,
                                                                 y = totals)) +
    ggplot2::geom_bar(stat = "identity", colour = "black", fill = "#00A33B") +
    ggplot2::facet_wrap(.~Site) +
    ggplot2::ylab("Rainfall (mm)") +
    ggplot2::xlab("Hydrological Year") +
    ggplot2::ggtitle(label = "Total Rainfall by Hydrological Year",
                     subtitle = paste("Rainfall data from selected rain gauges between",
                                      min(gaugeSAAR[["Raw"]]$hydroYear),
                                      "and",
                                      max(gaugeSAAR[["Raw"]]$hydroYear))) +
    ggplot2::theme(plot.title = element_text(family = "",
                                             face = "bold",
                                             colour = "#00A33B",
                                             size = 26),
                   panel.spacing = unit(1, "lines"))

  return(gaugeSAAR)
}
