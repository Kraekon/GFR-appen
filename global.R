# --- Load Libraries ---
library(shiny)
library(shinyjs)
library(bslib)
library(htmltools)
library(ggplot2)
library(dplyr)
library(DT)
library(ggrepel)

# --- Source Calculation and Utility Scripts ---
# This single file sources all necessary components from the R/ directory.
source("R/utils.R")
source("R/bsa_calculations.R")
source("R/egfr_calculations.R")
source("R/iohexol_calculations.R")