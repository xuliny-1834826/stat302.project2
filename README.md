  <!-- badges: start -->
  [![Travis build status](https://travis-ci.com/xuliny-1834826/stat302.project2.svg?branch=master)](https://travis-ci.com/xuliny-1834826/stat302.project2)
  <!-- badges: end -->

# stat302.project2 <img src="docs/logo.png" align="right" width="165px"/>

A Tutorial for Stat 302 AU 20 Final Project

`stat302.project2` is an `R` package for gathering all the main functions written this quarter to test their performance and put into a collection for more convenient use.


## Installation

To download the `stat302.project2` package, use the code below.

``` r
# install.packages("devtools")
devtools::install_github("xuliny-1834826/stat302.project2")
library(stat302.project2)
```

## Use

The vignette demonstrates example usage of all main functions. You can see the vignette by using the following code (note that this requires a TeX installation to view properly):


``` r
# install.packages("devtools")
devtools::install_github("xuliny-1834826/stat302.project2", build_vignette = TRUE, build_opts = c())
library(stat302.project2)
# Use this to view the vignette in the stat302.project2 HTML help
help(package = "stat302.project2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "stat302.project2")
```
