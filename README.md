
<!-- README.md is generated from README.Rmd. Please edit that file -->

# riskyData <img src="logo.png" align="right" width="120"/>

<!-- badges: start -->

[![License: GNU General Public
License](https://img.shields.io/badge/license-GNU%20General%20Public%20License-blue.svg)](https://cran.r-project.org/web/licenses/GNU%20General%20Public%20License)
[![](https://img.shields.io/github/languages/code-size/JonPayneEA/HydroEnR.svg)](https://github.com/JonPayneEA/HydroEnR)

<!-- badges: end -->

# Welcome to riskyData!

This package is part of the fleet of `flode` tools designed for members
of Evidence and Risk. This package, `riskyData`, is used to interact
with the EAs API. Hydrometric data can be pulled using in built
functions. Data are stored in containers that implement strict quality
controls. Data can be interrogated using a range of inbuilt tools.

## Installation

You can install the development version of riskyData from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JonPayne88/riskyData")
```

## Example 1 - Loading data

``` r
library(riskyData)
## basic example code
data(bewdley)

bewdley$hydroYearDay()$dayStats(plot = TRUE)
#> ℹ Calculating hydrological year and day✔ Calculating hydrological year and day [6.5s]
```

<img src="man/figures/README-example-1.png" width="100%" />

    #>      dayYear     Mean Median  Min Max   Perc5 Perc25  Perc75 Perc95
    #>   1:       1 39.17188  19.30 9.68 210 10.4000 12.200  35.600 192.00
    #>   2:       2 47.99227  25.80 9.64 225 10.2000 12.400  62.500 215.00
    #>   3:       3 53.14900  34.55 9.26 255  9.9715 11.600  67.725 228.00
    #>   4:       4 59.77555  40.65 9.39 252  9.9400 10.900  87.425 240.00
    #>   5:       5 63.06871  34.70 9.76 228 10.2000 11.700 106.000 189.85
    #>  ---                                                               
    #> 361:     361 33.29305  15.05 9.26 222 10.1000 10.975  35.925 163.85
    #> 362:     362 36.36758  15.85 8.77 254  9.8800 11.300  28.500 245.00
    #> 363:     363 38.64064  16.10 9.53 254  9.8115 11.000  26.100 250.00
    #> 364:     364 42.81338  16.20 9.35 247  9.8445 12.175  38.125 214.85
    #> 365:     365 40.54762  15.70 9.65 196 10.8000 12.500  31.550 172.00
