#' @title Function to perform the DMCA analysis
#'
#' @param Rdata Rdata is cumulative rainfall data. A vector, no time data.
#' @param Qdata Qdata is cumulative flow or level data. A vector, no time data.
#' @param Lmin mallest time window. Must be an ODD integer.
#' @param Lmax Largest time window.
#' @param dL Increment between windows. Must be an EVEN integer.
#' @param fracLperDt Set the time increment as a fraction of window size.
#' @param minWindowsPerL Minimum number of windows in the data. Replaces Lmax if needed.
#'
#' @return Minima representing the estimated Tp
#' @export
#' @author Oliver Dyer
#'
#' @examples
#' ## Do not run
getDMCArho <- function(Rdata,               # Rdata is cumulative rainfall data. A vector, no time data.
                       Qdata,               # Qdata is cumulative flow or level data. A vector, no time data.
                       Lmin = 1,            # Smallest time window. Must be an ODD integer.
                       Lmax = 400,          # Largest time window.
                       dL = 2,              # Increment between windows. Must be an EVEN integer.
                       fracLperDt = 1,      # Set the time increment as a fraction of window size.
                       minWindowsPerL = 5)  # Minimum number of windows in the data. Replaces Lmax if needed.
{
  # Check for compatible data
  if(length(Rdata) != length(Qdata))
  {stop("Incompatible lengths of Rdata and Qdata.")}
  # Update Lmax based on the available data
  Lmax <- min(Lmax, floor(length(Rdata) / minWindowsPerL) )
  # Get the number of time windows to loop over
  NL <- floor((Lmax - Lmin)/dL) + 1
  # Create a vector of window sizes
  Lvalues <- rep(Lmin, NL)
  for(i in 2:NL)
  {Lvalues[i] <- Lvalues[i-1] + dL}
  rhoDMCA <- data.frame(L = Lvalues,
                        rho = rep(0, NL) )
  # loop over time windows
  for(l in 1:NL)
  {
    L <- Lvalues[l]
    halfL <- 0.5*(L-1)
    invL <- 1.0/L
    # Reduced number of time steps
    dt <- max(1, floor(halfL*2*fracLperDt) )
    nt <- floor((Nt-1-2*halfL)/dt)
    meanRt <- rep(0, nt)
    meanQt <- rep(0, nt)
    ## Calc moving averages
    t <- 1 + halfL
    # loop over t
    for(i in 1:nt)
    {
      # loop over the time window
      for(j in (t - halfL):(t + halfL) )
      {
        meanRt[i] <- meanRt[i] + Rdata[j]
        meanQt[i] <- meanQt[i] + Qdata[j]
      }
      t <- t + dt
    }
    # Normalise averages
    meanRt <- invL * meanRt
    meanQt <- invL * meanQt
    ## Calc correlation functions
    FR2 <- 0
    FQ2 <- 0
    FRQ <- 0
    # loop over t
    t <- 1 + halfL
    for(i in 1:nt)
    {
      RsubMean <- Rdata[t] - meanRt[i]
      QsubMean <- Qdata[t] - meanQt[i]
      FR2 <- FR2 + (RsubMean * RsubMean)
      FQ2 <- FQ2 + (QsubMean * QsubMean)
      FRQ <- FRQ + (RsubMean * QsubMean)
      t <- t + dt
    }
    # no need to normalise correlation functions as that factor will cancel in rho
    # calc rhoDMCA
    rhoDMCA$rho[l] <- FRQ / sqrt(FR2 * FQ2)
  }
  return(rhoDMCA)
}
