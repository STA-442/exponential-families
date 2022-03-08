library(shiny)
library(bslib)
library(MASS)
library(statmod)
library(invgamma)
library(dplyr)
library(plotly)
library(gt)

port <- Sys.getenv('PORT')

shiny::runApp(
  appDir = getwd(),
  host = '0.0.0.0',
  port = as.numeric(port)
)