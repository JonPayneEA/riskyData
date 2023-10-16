#' @title Data aggregation for `HydroImport` objects
#'
#' @description This acts as a wrapper function for all the aggregation
#' functions that sit within riskyData
#'
#' @param x A data.table that sits within an R6 `HydroImport` object
#' @param type Set the aggregation level to either hourly', 'daily', 'monthly',
#' 'annual', or 'hydroYear'
#' @param method Available aggregation methods include 'min', 'max', 'mean',
#' 'median', and 'sum'
#'
#' @seealso [hourlyAgg]
#' @seealso [dailyAgg]
#' @seealso [monthlyAgg]
#' @seealso [annualAgg]
#' @seealso [hydroYearAgg]
#'
#' @return Aggregated dataset determined from the input `type` and `method`. Data are placed within a `HydroAggs` type container
#' @export
#'
#' @examples
#' # Load bewdley dataset
#' data(bewdley)
#'
#' # Calculate mean hourly flows
#' bewdley$dataAgg(type = "hourly", method = "mean")
#'
#' # Calculate median daily flows
#' bewdley$dataAgg(type = "daily", method = "median")
#'
#' # Calculate min
#' bewdley$dataAgg(type = "monthly", method = "min")
#'
#' # Calculate annual (calendar year) max
#' bewdley$dataAgg(type = "annual", method = "max")
#'
#' # Calculate hydroYear max
#' # Hydrological years need to be calculated for this. The `hydroYearDay()` function can be piped into the operation.
#' bewdley$hydroYearDay()$dataAgg(type = "hydroYear", method = "max")
#'
#' # Calculate total hourly flow
#' # Flow has to be converted to volume. The `asVol()` function can be piped into the operation.
#' bewdley$asVol()$dataAgg(type = "hourly", method = "sum")
dataAgg <- function(x = NULL, type = NULL, method = NULL) {
  ## Available types and methods
  types <- c("hourly", "daily", "monthly", "annual", "hydroYear")
  methods <- c("min", "max", "mean", "median", "sum")

  ## Data checks
  if (is.null(x)) {
    stop("Please check the input data")
  }
  if (is.null(type) || !type %in% types) {
    stop("Please check the aggregation type")
  }
  if (is.null(method) || !method %in% methods) {
    stop("Please check the aggregation method")
  }

  ## Start notifications
  cli::cli_progress_step(paste0(
    "Allocating function {.strong ",
    paste0(
      "riskyData::",
      type,
      "Agg"
    ),
    "}"
  ))
  ## Parse function name to
  func <- eval(parse(text = paste0("riskyData::", type, "Agg")))

  ## Notification to indicate function application
  cli::cli_progress_step(paste("Computing", type, method))
  # Apply function
  dt <- func(x, method = method)

  return(dt)
}
