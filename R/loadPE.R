#' @title Load potential evaporation data
#'
#' @details Loads PE data obtained from HYRAD. Missing data are imputed using
#' the 'na.interp()' function in the forecast package. For the seasonal series,
#' a robust STL decomposition is first computed. Then a linear interpolation is
#' applied to the seasonally adjusted data, and the seasonal component is added
#' back.
#'
#' @param link Link to the specified file for import
#' @param skip Stet to zero, denotes the number off rows you wish to skip
#' @param metaRows Set to 10, determines the amount of rows set as metadata
#' @param impute Set to TRUE, this fills in missing data using the
#' 'forecast::na.interp()' function
#'
#' @return A tidied dataset of PE data
#' @export
#'
#'
loadPE <- function(link,
                   skip = 0,
                   metaRows = 10,
                   impute = TRUE) {
  type <- fread(link,
    nrows = 2,
    skip = skip,
    sep = ",",
    header = FALSE,
    fill = TRUE
  )[, 1:2]
  if (type[2, 2] == "Single per row") {
    cat("Importing meta data\n")
    meta <- fread(link,
      nrows = metaRows,
      skip = 3,
      sep = ",",
      header = FALSE,
      na.strings = c("#N/A", "NA"),
      fill = TRUE
    )[, 1:3]
    colnames(meta) <- c("Parameter", "Details", "Details2")
    main <- fread(link,
      skip = 18,
      sep = ",",
      header = TRUE,
      col.names = c(
        "ID", " Year", "Month", "Day", "Minutes",
        "Date", "Time", "PE", "Coverage"
      ),
      na.strings = c("#N/A", "NA")
    )
    if (impute == TRUE) {
      main$PE <- forecast::na.interp(main$PE)
    }
    dateTime <- paste(main$Date, main$Time)
    dateTime <- gsub(" GMT", "", dateTime)
    main$DateTime <- as.POSIXct(dateTime,
                                format = "%d-%b-%y %H:%M",
                                tz = "GMT")
    data <- list()
    data[["Metadata"]] <- meta
    data[["RemoteData"]] <- main

    class(data) <- append(class(data), "PELoad")
    return(data)
  } else {
    stop("PE data is not supported in this function when of the type; ",
         type[2, 2])
  }
}
