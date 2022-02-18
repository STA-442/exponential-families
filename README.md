
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Installation

You can either download the application to your local computer by
cloning the repository, making sure you have the required packages
installed, and running the app (found in: `/exponential-families/app.R`
),

The packages below are required. You can run the code below that will
install the packages if they are not installed already.

``` r
# Package names
library(shiny)
library(bslib)
library(MASS)
library(statmod)
library(invgamma)
library(dplyr)
library(plotly)
library(gt)
packages <- c("shiny", "bslib", "MASS",
              "statmod", "invgamma", "plotly",
              "gt", "knitr", "tidyr",
              "glue")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
```

## Exponential Family of Distributions Shiny app

This simple application allows you to simulate data from several
exponential family distributions. At the moment, you can simulate from:

-   “Beta”
-   “Binomial”
-   “Exponential”
-   “Gamma”
-   “Inverse Gamma”
-   “Normal”
-   “Negative Binomial”
-   “Poisson”

After selecting a family of interest, select a sample size and pick
parameters to simulate random data. A density plot and histogram will
appear in the mail panel along with a table of summary statistics.
