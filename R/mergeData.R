#' @title Merge HydroImport and HydroAggs R6 objects
#'
#' @description The `mergeData()` is used to combine multiple R6 encapsulations.
#' The data have to be in `HydroImport` or `HydroAggs` classes. Depending on the
#' metadata argument, metadata can be exported with the merged dataset as a
#' list. The merging process is carried out on looped full joins, this allows
#' datasets f varying lengths to be collated. Missing information is padded
#' with NAs.
#'
#' @param ... Datasets downloaded from the EAs API set inside an R6 container
#' @param metadata Set to FALSE, if TRUE collated metadata are exported with the
#'  merged dataset
#'
#' @return A merged dataset as a data,table, if metadata is TRUE then a list of
#' 2 data.tables
#' @export
#'
#' @examples
#' a <- loadAPI(ID = 2001,
#'              measure = 'flow',
#'              period = 900,
#'              type = 'instantaneous',
#'              datapoints = 'range',
#'              from = '2023-06-12 09:00',
#'              to = '2023-06-13 09:00')
#'
#' b <- loadAPI(ID = 2002,
#'              measure = 'flow',
#'              period = 900,
#'              type = 'instantaneous',
#'              datapoints = 'range',
#'              from = '2023-06-12 09:00',
#'              to = '2023-07-12 09:00')
#'
#' c <- loadAPI(ID = 2004,
#'              measure = 'flow',
#'              period = 900,
#'              type = 'instantaneous',
#'              datapoints = 'range',
#'              from = '2023-06-13 09:00',
#'              to = '2023-07-12 09:00')
#'
#' d <- loadAPI(ID = 2001,
#'              measure = 'level',
#'              period = 900,
#'              type = 'instantaneous',
#'              datapoints = 'range',
#'              from = '2023-06-12 09:00',
#'              to = '2023-07-12 09:00')
#'
#' z <- mergeData(a, b, c, metadata = FALSE)
#' z
mergeData <- function(..., metadata = FALSE){
  # Compile list of the objects inputted
  dtlst <- list(...)

  # Use only HydroImport, HydroAggs , and R6
  # Collate classes
  classes <- unlist(lapply(dtlst, class))
  # Check that they all match the expected
  if (any(!classes %in% c('R6', 'HydroImport', 'HydroAggs'))){
    stop('Incorrect data classes supplied, please use "R6", "HydroImport", or "HydroAggs"')
  }

  # Collate the name of the inputted objects
  names <- sapply(substitute(...()), deparse)

  # Compile metadata for checks
  meta <- list()
  for (i in seq_along(dtlst)){
    meta[[i]] <- dtlst[[i]]$meta()
  }
  meta <- data.table::rbindlist(meta)

  # Check for equal time steps
  if(length(unique(meta$timeStep)) > 1){
    stop('Data of differing time steps have been submitted')
  }

  # Check for equal data types
  if(length(unique(meta$parameter)) > 1){
    warning('Different parametrs will be collated, this could lead to problems in other analyses')
  }

  # Clone the datasets to stop inheritance issues
  # Overwrites existing objects with the clones
  for (i in seq_along(dtlst)){
    dtlst[[i]] <- dtlst[[i]]$clone()
  }

  # Remove private data fields by extracting just the public data
  for (i in seq_along(dtlst)){
    dtlst[[i]] <- data.table(dateTime = dtlst[[i]]$data$dateTime,
                             value = dtlst[[i]]$data$value)
  }

  # Replace value field as the object denoted with "_value"
  for(i in seq_along(dtlst)){
    data.table::setnames(dtlst[[i]], 2, paste0(names[i], '_value'))
  }

  # Merge initial 2 dt's
  fullDT <- merge(dtlst[[1]][,1:2],
                  dtlst[[2]][,1:2],
                  by = 'dateTime',
                  all = TRUE)

  # If more than 2 dt's loop through merge
  if(length(dtlst) > 2) {
    for(i in tail(seq_along(dtlst), -2)){
      fullDT <- merge(fullDT,
                      dtlst[[i]][,1:2],
                      by = 'dateTime',
                      all = TRUE)
    }
  }
  if(metadata == FALSE){
    return(fullDT)
  } else {
    return(list(meta, fullDT))
  }
}

