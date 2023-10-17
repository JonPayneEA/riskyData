
<!-- README.md is generated from README.Rmd. Please edit that file -->

# riskyData <img src="logo.png" align="right" width="120" />

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
#> ℹ Calculating hydrological year and day✔ Calculating hydrological year and day [1.4s]
#> Warning in `[.data.table`(dt1, -366, , ): Item 1 of i is -366 but there are
#> only 365 rows. Ignoring this and 0 more like it out of 1.
```

<img src="man/figures/README-example-1.png" width="100%" />

    #>      dayYear     Mean Median   Min   Max   Perc5 Perc25  Perc75  Perc95
    #>   1:       1 24.10660  25.70 11.10  37.7 11.4350 13.275  32.425  36.895
    #>   2:       2 35.88368  30.60 10.20  75.1 10.7350 13.200  55.400  74.100
    #>   3:       3 44.31712  58.60  9.26  83.2  9.3535 10.800  64.250  71.600
    #>   4:       4 67.23299  85.70  9.65 140.0  9.9440 10.800 103.000 123.650
    #>   5:       5 90.21354 120.00 10.30 157.0 10.4000 11.700 135.000 150.000
    #>  ---                                                                   
    #> 361:     361 22.47500  22.45 10.10  37.0 10.4000 11.500  31.900  36.300
    #> 362:     362 20.31781  20.35  9.64  30.4 10.4000 13.100  27.700  29.945
    #> 363:     363 17.46755  16.95  9.75  26.4  9.9775 10.750  24.325  26.000
    #> 364:     364 16.98958  16.95 10.30  23.7 10.7000 13.775  20.900  22.590
    #> 365:     365 16.72240  16.90 10.00  24.4 10.2550 12.800  20.400  23.890
