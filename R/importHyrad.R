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
  ## Define expoort type
  type <- "PDM"
  if (is.null(path)) {cli::cli_abort("No file has been supplied for import!")}
  init <- data.table::fread(path,
                            nrows = 2,
                            header = FALSE,
                            fill = TRUE)
  if (ncol(init) > 2) {
    cli::cli_alert_info("Please apply the export for PDM argument in HYRAD in future")
    if (init$V4[1] != "2.9.001"){
      cli::cli_abort("This function has been developed using 2.9.001, the imported metadata derives from {.emph {init$V4[1]}} has been applied. Results may differ. ")
    }
    ## Update type
    type <- "PDM Other"
    ## Load first 2 lines and make similar format to PDM output
    m1 <- init[, 1:2]
    m1[1,2] <- paste0(init$V2[1], " ", init$V3[1], ": ", init$V4[1])
    m1 <- rbind(m1, list("Exported at", paste0(init$V6[1], " ",init$V7[1], " ", init$V8[1])))

    ## Load the rest of the metadata
    m2 <- data.table::fread(path,
                            skip = 2,
                            nrows = 10,
                            header = FALSE,
                            fill = TRUE)
    m2$V1[1] <- paste(m2$V1[1], strsplit(m2$V2[1], ",")[[1]][1])
    m2$V2[1] <- paste(strsplit(m2$V2[1], ",")[[1]][2], m2$V3[1])
    m2$V1[2] <- paste(m2$V1[2], strsplit(m2$V2[2], ",")[[1]][1])
    m2$V2[2] <- paste(strsplit(m2$V2[2], ",")[[1]][2], m2$V3[2], m2$V4[2], m2$V5[2])
    m2$V1[3] <- paste(m2$V1[3], strsplit(m2$V2[3], ",")[[1]][1])
    m2$V2[3] <- paste(strsplit(m2$V2[3], ",")[[1]][2], m2$V3[3])
    m2$V2[4] <- strsplit(m2$V1[4], ",")[[1]][2]
    m2$V1[4] <- strsplit(m2$V1[4], ",")[[1]][1]
    m2$V2[5] <- paste(strsplit(m2$V1[5], ",")[[1]][2], m2$V2[5], m2$V3[5])
    m2$V1[5] <- strsplit(m2$V1[5], ",")[[1]][1]
    m2$V1[6] <- paste(m2$V1[6], strsplit(m2$V2[6], ",")[[1]][1])
    m2$V2[6] <- strsplit(m2$V2[6], ",")[[1]][2]
    m2$V1[7] <- paste(m2$V1[7], strsplit(m2$V2[7], ",")[[1]][1])
    m2$V2[7] <- strsplit(m2$V2[7], ",")[[1]][2]
    m2$V1[8] <- paste(m2$V1[8], strsplit(m2$V2[8], ",")[[1]][1])
    m2$V2[8] <- paste(strsplit(m2$V2[8], ",")[[1]][2], m2$V3[8], m2$V4[8], m2$V5[8], m2$V6[8], m2$V7[8], m2$V8[8], m2$V9[8], m2$V10[8])
    m2$V1[9] <- paste(m2$V1[9], strsplit(m2$V2[9], ",")[[1]][1])
    m2$V2[9] <- paste(strsplit(m2$V2[9], ",")[[1]][2], m2$V3[9])
    m2$V1[10] <- paste(m2$V1[10], strsplit(m2$V3[10], ",")[[1]][1])
    m2$V2[10] <- strsplit(m2$V3[10], ",")[[1]][2]
    m2 <- m2[, 1:2]

    meta <- rbind(m1, m2)
    colnames(meta) <- c("col1", "col2")

    meta <- data.table::dcast(data.table::melt(meta, id.vars = "col1"), variable ~ col1)[,-1]

    } else {
    ## Find import the metadata
      meta <- data.table::fread(path,
                                nrows = 13,
                                header = FALSE,
                                col.name = c("col1", "col2"))
    ## Remover leading and trailing whitespace
    meta$col1 <- trimws(meta$col1)
    meta <- data.table::dcast(data.table::melt(meta, id.vars = "col1"), variable ~ col1)[,-1]

    if (meta$VERSION != "Hyrad Display Client Version: 2.9.001"){
      cli::cli_abort("This function has been developed using 2.9.001, the imported metadata derives from {.emph {meta$VERSION}} has been applied. Results may differ. ")
    }
    }
  ##Import the data
  if (type == "PDM") {
    dt <- data.table::fread(path, skip = 13, header = TRUE)

  } else {
    dt <- data.table::fread(path, skip = 17, header = FALSE)
    dt$V4 <- dt$V4/60
    dt$V5 <- 0L
    dt$V6 <- 0L
    dt[, (1:6) := lapply(.SD, as.integer), .SDcols = 1:6]
    dt[, 8:9] <- NULL
  }


  if (meta$`Data Source` == "MOSES hourly"){
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
  class(dt) <- append(class(dt), "mosesPE")
  return(dt)
}


# pathEX <-  c("C:/Users/jp000056/Downloads/MOSES_PE_WL_01Oct08-01Jul25.csv")
# init <- fread(pathEX,
#       nrows = 2,
#       header = FALSE,
#       fill = TRUE)

#
# importHYRADs(pathEX, expand = F)
# importHYRADs(files[1], expand = F)
