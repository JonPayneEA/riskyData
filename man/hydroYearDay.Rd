% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hydroYearDay.R
\name{hydroYearDay}
\alias{hydroYearDay}
\alias{hydroYearDay.data.table}
\alias{hydroYearDay.POSIXct}
\title{HydroYearDay}
\usage{
hydroYearDay(x, calendar = "oct_us_gb", ...)

\method{hydroYearDay}{data.table}(x, calendar = "oct_us_gb")

\method{hydroYearDay}{POSIXct}(x, calendar = "oct_us_gb")
}
\arguments{
\item{x}{Array of dates of class Date}

\item{calendar}{hydrological year calendar. Set to 'oct_us_gb' (USA and UK),
but can also be 'sep_br' (Brazil), 'apr_cl' (Chille).}
}
\value{
Creates a hydrological year and hydrological day column
}
\description{
Creates fields for hydrological year and hydrological day.

Speed is slow, needs conversion to C++ in future iterations.
}
\examples{
## Load bewdley dataset
data(bewdley)

## Inspect the data
bewdley$data

## Add hydrological year and hydrological day columns
bewdley$hydroYearDay()

## Print the bewdley dataset
bewdley
}
