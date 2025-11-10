#' @title Import Hyrad data
#'
#' @param path File path to desired file
#' @param expand Argument to control whether to expand to 15min series or not
#'
#' @return Returns a data.table in the PDM for PCs format
#' @export
#'
#' @examples
#' importHYRAD(files[1], expand = TRUE)
#' importHYRAD(files[1], expand = FALSE)
importHYRAD <- function(path = NULL, expand = FALSE){
  if (is.null(path)) {cli::cli_abort("No file has been supplied for import!")}
  ## Find import the metadata
  meta <- fread(path,
                nrows = 13,
                header = FALSE,
                col.name = c("col1", "col2"))
  ## Remover leading and trailing whitespace
  meta$col1 <- trimws(meta$col1)
  meta <- dcast(melt(meta, id.vars = "col1"), variable ~ col1)[,-1]

  if (meta$VERSION != "Hyrad Display Client Version: 2.9.001"){
    cli::cli_abort("This function has been developed using 2.9.001, the imported metadata derives from {.emph {meta$VERSION}} has been applied. Results may differ. ")
  }

  if (meta$FORMAT == "PDM format") {
    if (meta$`Data Source` == "MOSES hourly"){
      dt <- fread(path, skip = 13, header = TRUE)
      colnames(dt) <- c("year", "month", "day", "hour", "minute", "second", "pe")
      ## Replace -9999 with NA
      dt[dt$pe == -9999, pe := NA]
      if (expand == TRUE){
        timeFrom <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                                       dt$year[1],
                                       dt$month[1],
                                       dt$day[1],
                                       dt$hour[1],
                                       dt$minute[1],
                                       dt$second[1]), tz = "UTC")
        pos <- as.numeric(meta$`Number Images`)

        timeTo <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                                     dt$year[pos],
                                     dt$month[pos],
                                     dt$day[pos],
                                     dt$hour[pos],
                                     dt$minute[pos],
                                     dt$second[pos]), tz = "UTC")
        ## Correct to allow for repetition of dates
        timeTo <- timeTo + 900 * 4 * 23
        ## Create time sequence - easier to add 15min data
        timeSeq <- seq(from = timeFrom, to = timeTo, by = "15 min")
        dt <- data.table::data.table(year = data.table::year(timeSeq),
                                     month = data.table::month(timeSeq),
                                     day = data.table::mday(timeSeq),
                                     hour = data.table::hour(timeSeq),
                                     minute = data.table::minute(timeSeq),
                                     second = data.table::second(timeSeq),
                                     pe = rep(dt$pe, 24)/24)
      }
    }
    if (meta$`Data Source` == "MOSES daily"){
      dt <- fread(path, skip = 13, header = TRUE)
      colnames(dt) <- c("year", "month", "day", "hour", "minute", "second", "pe")
      ## Replace -9999 with NA
      dt[dt$pe == -9999, pe := NA]
      if (expand == TRUE){
        timeFrom <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                                       dt$year[1],
                                       dt$month[1],
                                       dt$day[1],
                                       dt$hour[1],
                                       dt$minute[1],
                                       dt$second[1]), tz = "UTC")
        pos <- as.numeric(meta$`Number Images`)

        timeTo <- as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d",
                                     dt$year[pos],
                                     dt$month[pos],
                                     dt$day[pos],
                                     dt$hour[pos],
                                     dt$minute[pos],
                                     dt$second[pos]), tz = "UTC")
        ## Correct to allow for repetition of dates
        timeTo <- timeTo + 900 * 95
        ## Create time sequence - easier to add 15min data
        timeSeq <- seq(from = timeFrom, to = timeTo, by = "15 min")
        dt <- data.table::data.table(year = data.table::year(timeSeq),
                                     month = data.table::month(timeSeq),
                                     day = data.table::mday(timeSeq),
                                     hour = data.table::hour(timeSeq),
                                     minute = data.table::minute(timeSeq),
                                     second = data.table::second(timeSeq),
                                     pe = rep(dt$pe, 96)/96)
      }
    }
    class(dt) <- append(class(dt),"mosesPE")
    return(dt)
  }
}



