#' @title rollingAggs
#'
#' @description Provides a rolling aggregation of a user defined periods.
#'
#' @param dt Input dat in R6 object
#' @param rolls User defined periods of aggregation
#' @param interval Set as 0.25 to represent 15 minute data, for hourly change
#' to 1 etc.
#' @param method 'min', 'max', 'mean', 'median', and 'sum' options available
#'
#' @return A dataset containing numerous aggregations on a roll
#' @export
#'
#' @import data.table
#' @import RcppRoll
#'
#' @examples
#' ## Do not run
#' # rollingAggs(Buildwas)
rollingAggs <- function(x,
                        rolls = NULL,
                        interval = 0.25,
                        method = "mean") {
  UseMethod("rollingAggs", x)
}

#' @rdname rollingAggs
#' @export
rollingAggs.data.table <- function(x,
                                   rolls = NULL,
                                   interval = 0.25,
                                   method = "mean") {
  if (is.null(rolls)) {
    rolls <- c(1, 2, 3, 4, 8, 24, 120)
  }
  roller <- eval(parse(text = paste0("RcppRoll::roll_", method)))
  if (method == "sum") {
    rollingAggregations <- data.table(dateTime = x$dateTime,
                                      hydroYear = x$hydroYear,
                                      hydroYearDay = x$hydroYearDay,
                                      Raw = x$volume)
  } else {
    rollingAggregations <- data.table(dateTime = x$dateTime,
                                      Raw = x$value)
  }
  for (i in seq_along(rolls)) {
    window <- rolls[i] / interval
    if (rolls[i] %% interval > 0) {
      cat("Using a rolling aggregation of ",
          rolls[i],
          "is not divisible by 0.25, skipping for next accumulation\n")
      rollingAggregations[, paste("roll",
                                  rolls[i],
                                  "Hr",
                                  method,
                                  sep = "") :=
                            rep(NA, length(rollingAggregations$dateTime))]
      next
    } else {
      window <- rolls[i] / interval
    }

    rollingAggregations[, paste("roll", rolls[i], "Hr", sep = "") :=
                          roller(rollingAggregations$Raw, window, fill = NA)]
  }
  dt <- data.table(rollingAggregations)
  return(dt)
}
