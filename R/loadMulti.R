#' Load multiple `HydroImport` containers
#'
#' @description The `loadMulti()` function iterates through combinations of
#' parameters supplied to the arguments. These can be added as vectors, this
#' reduces the amount of calls to `loadAPI()` in working scripts. Objects or
#' `HydroImport` are automatically assigned to the environment using a name
#' convention of *paramater*_*WISKI-ID*.
#'
#' @param API Currently only supports the Environment Agency API set as "EA".
#' @param ID Use to specify a particular WISKI ID or for when downloading all
#' stations 'wiski' or 'nrfa' to filter results. Additionally to access the real
#'  time APIs use "flood" for all critical sites and "tidal" for the category A
#'  tide gauges. Vectors are supported.
#' @param measure Use this when exporting observations to select the available
#' parameter. Generally 'flow', 'level', or 'groundwater'
#' @param period 	This is so you can select the time steps available, generally
#' 900 (15 min) or 86400 (daily).
#' @param type Selects the data type, instantaneous, min, max etc.
#' @param datapoints Allows you to decide where in the time series you are
#' pulling the data from.
#' @param from First time step of choice.
#' @param to Last time step of choice.
#' @param ... Additional `loadAPI()` arguments can be called.
#'
#' @return Multiple `HydroImport` containers
#' @export
#'
#' @examples
#' loadMulti(ID = c('2001', '2002'),
#'           measure = c('flow', 'level'),
#'           period = 900,
#'           type = 'instantaneous',
#'           datapoints = 'earliest')
loadMulti <- function(API = "EA",
                      ID = NULL,
                      measure = NULL,
                      period = NULL,
                      type = NULL,
                      datapoints = NULL,
                      from = NULL,
                      to = NULL,
                      ...) {
  ## Data checks
  if (is.null(ID)) stop("Please include the ID parameter")
  if (is.null(measure)) stop("Please include the measure parameter")
  if (is.null(period)) stop("Please include the period parameter")
  if (is.null(type)) stop("Please include the type parameter")
  ## Currently only available for EA API
  if (API == "EA"){
    dt <- data.table::data.table(ID,
                                 measure,
                                 period,
                                 type,
                                 datapoints,
                                 from,
                                 to)
    for(i in seq_along(dt$ID)){
      name <- paste(dt$measure[i], dt$ID[i], sep = "_")
      temp <- tryCatch(
        {
          loadAPI(ID = dt$ID[i],
                  measure = dt$measure[i],
                  period = dt$period[i],
                  type = dt$type[i],
                  datapoints = dt$datapoints[i],
                  from = dt$from[i],
                  to = dt$to[i])
        },
        error = function(msg){
          errorMes <- paste0("{.strong Failed to download data for ",
                             name, "}")
          cli::cli_alert_danger(errorMes)
          return(NULL)
        }
      )
      ## Export data
      if (!is.null(temp)){
        cli_alert_info(paste0(" Storing data in environment as {.pkg ",
                              name,
                              "}"))
        assign(name, temp, envir = .GlobalEnv)
      } else {
        ## Error message if temp file is empty due to error
        cli::cli_div(theme = list(span.emph = list(color = "orange")))
        cli::cli_alert_info(" Try using the {.emph loadAPI()} function instead")
        cli::cli_end()
      }
      rm(temp)
    }

  } else {
    stop("Currently only the EAs API is supported")
  }
}
