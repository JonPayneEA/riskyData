#' @title Calculate catchment average rainfall
#'
#' @param ... hydroImport containers
#' @param areas user specified area sizes contributed by each rain gauge
#'
#' @return Catchment averaged rainfall in data.table format
#' @export
#'
#' @examples
#' ## Do not run
catchAvg <- function(..., areas = NULL){
  # Coerce inputs to list
  hydroImportList <- list(...)

  # Use only HydroImport, HydroAggs, and R6
  # Collate classes
  classes <- unlist(lapply(hydroImportList, class))
  # Check that they all match the expected
  if (any(!classes %in% c('R6', 'HydroImport', 'HydroAggs'))){
    incompat <- data.table::data.table(incompatable = classes)
    incompat <- unique(incompat[!incompatable %in%
                                  c('R6', 'HydroImport', 'HydroAggs')])
    cli::cli_abort(
      c(
        "x" = "Incompatable data classes",
        "The class{?es}: {.cls {incompat}} {?is/are} not yet compatable
        with this function",
        "i" = "Please ensure that all data submitted is in either
        {.cls HydroImport} or {.cls HydroAggs} formats"
      ))

    # stop('Incorrect data classes supplied, please use "R6", "HydroImport", or "HydroAggs"')
  }

  # Collate the name of the inputed objects
  names <- sapply(substitute(...()), deparse)

  # Compile metadata for checks
  meta <- list()
  for (i in seq_along(hydroImportList)){
    meta[[i]] <- hydroImportList[[i]]$meta()

    ifelse(is.null(hydroImportList[[i]]$catchProp$area),
           meta[[i]]$catchProp <- NA,
           meta[[i]]$catchProp <- hydroImportList[[i]]$catchProp$area)

    # meta[[i]][, catchProp := hydroImportList[[i]]$catchProp[1]]
  }
  meta <- data.table::rbindlist(meta)

  if (is.null(areas)){
    catchAreas <- meta$catchProp
    if (any(is.na(catchAreas))){
      n <- paste(meta[is.na(catchProp),]$stationName, sep = ", ")
      cli::cli_abort(
        c(
          "x" = "Unable to calculate catchment averaged rainfall",
          "The site{?s}: {.var {n}} {?does/ do} not contain catchment area
          metadata",
          "i" = "Use 'object${.fn addCatchProp}' to correct for this or apply
          the {.arg areas} argument"
        ))
    }
  } else {
    catchAreas <- areas
  }


  # catchProps <- list()
  # for (i in seq_along(hydroImportList)){
  #   catchProps[[i]] <- hydroImportList[[i]]$catchProp
  #   catchpro
  # }
  # catchProps <- data.table::rbindlist(catchProps)
  #
  # Check for equal time steps
  # if(length(unique(meta$timeStep)) > 1){
  #   stop('Data of differing time steps have been submitted')
  # }
  #
  if (any(!meta$parameter %in% c("Rainfall"))){
    stop("Parameters outside of rainfall have been supplied")
  }

  merged <- riskyData::mergeData(...)
  # return(merged)

  # Multiply rain gauges by area
  df <- data.frame(mapply(`*`, merged[, -1], catchAreas, SIMPLIFY = FALSE))
  ## rowSums is much faster than grouping in data.table
  row <- rowSums(df)


  # Calculate areas
  # Find NAs
  dfNA <- !is.na(df)
  dfNA <- as.data.table(dfNA)
  # convert columns tonumeric
  dt <- dfNA[, lapply(.SD, as.numeric)]
  # Multiply booleans by RG areas
  df1 <- data.frame(mapply(`*`, dt, catchAreas, SIMPLIFY = FALSE))
  areaRow <- rowSums(df1)

  ## Divide row totals by area
  averaged <- row / areaRow

  dt <- data.table(dateTime = merged$dateTime,
                   value = averaged)
  return(dt)
}

# catchAvg(dog, whit)
#
#
#
# catchAvg(dog, whittlesey)
#
# catchAvg(boot, cosford, dog, whittlesey)
#
# catchAvg(boot, cosford, dog, whittlesey, areas = c(50, 40, 25, 35))
