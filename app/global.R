# global.R

# Load UI and Web packages
library(shiny)
library(bslib)
library(plotly)
library(DT)
library(shinyjs)

# Load data processing packages
library(readxl)
library(readr)
library(dplyr)
library(data.table)
library(openxlsx)
library(tibble)

# Load math, modeling, and plotting Packages
library(ggplot2)
library(drc)
library(caTools)
library(minpack.lm)
library(DEoptim)

# Global options
options(shiny.maxRequestSize = 50 * 1024^2)

# Source external functions
source("R/CES_functions.R")