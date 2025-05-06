################################################################################
## File:    Master script
## Purpose: Source analysis 
################################################################################

# Setup
rm(list=ls(all=T)); gc()

# Set your user directory to this repository!
setwd()

# Packages
library(MASS) # be careful here select/dplyr masked
library(tidyverse)
library(sf)
library(lubridate)
library(stargazer)
library(plm)
library(readxl)
library(showtext)
library(lmtest)
library(caret)
library(pROC)
library(doParallel)

# Desired coordinate reference system
crs_number <- 4326

##################################################################
### Input switch to run key data processing steps from scratch ###
##################################################################

SWITCH_NEW_LOAD <- F

##################################################################

# Relevant file paths
path_spatial <- './data/Spatial Repository/'
path_plots <- './plots/'

# Colors
color_orange    <- '#FB8604'
color_green     <- '#66B032'
color_blue      <- '#347B98'
color_lightblue <- '#b3d9ff'
color_purple    <- '#722B87'
color_red       <- '#7B1920'
color_bu        <- "#5AB4AC"
color_br        <- "#D8B365"

# Palettes
color_scheme <- c(color_orange, color_green, color_blue, color_purple)
main_scheme  <- c("#A6611A", "#DFC27D", "#80CDC1", "#018571")
tranche_scheme <- c('#ffcc00','#ff6600', '#ff0000', '#8c0099')

# Options
sf_use_s2(FALSE)
options(dplyr.summarise.inform=F)

# Plotting parameters
font_add_google(name='Open Sans', family='Open Sans')
plot_text <- 'Open Sans'

# Plotting theme
theme_matplotlib <- function () { 
  theme_bw(base_size=30, base_family = plot_text) %+replace% 
    theme(
      legend.background = element_rect(fill="transparent", colour=NA),
      legend.key        = element_rect(fill="transparent", colour=NA),
      legend.text       = element_text(size=36),
      legend.title      = element_text(size=36),
      panel.grid.major  = element_line(color='gray80', linetype = 'dashed', 
                                       linewidth=0.25), 
      panel.grid.major.x = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.ticks.length = unit(0.1, 'cm'),
      axis.text         = element_text(size=30),
      axis.title        = element_text(size=40),
      strip.background  = element_rect(
        color="black", fill="gray99", linewidth=1.5, linetype="solid"
      ),
      strip.text        = element_text(size=36, 
                                       margin = ggplot2::margin(0.25,0.25,0.25,0.25, "cm"))
    )
}

showtext_auto()

# Source miscellaneous functions and definitions, for regression formatting mostly
source('./code/1 misc_functions.R')

# Initiate spatial boundary objects
# only needs to be run once
if (SWITCH_NEW_LOAD) {
  source('./code/1 iniate_spatial_boundaries.R')
}


# Generate dataset --------------------------------------------------------

# Get conductor covariates like conductor age
#source('./code/2a load_conductor_covariates.R')

# Load hardening data
#source('./code/2b load_hardening_data.R')

# Load weather/climate data
#source('./code/2c load_weather_covariates.R')

# Load tree canopy data
#source('./code/2d load_vegetation.R')

# Load and process ignition data
#source('./code/2e load_weather_covariates.R')

# Load PSPS and fast-trip data
#source('./code/2f load_PSPS.R')

# Stitch together the various data inputs & covariates
#source('./code/2g compile_dataset.R')

# Modeling ----------------------------------------------------------------

# Train and fit the ignition risk model
#source('./code/3a risk_score.R')

# Predict days when fast-trip settings enabled
#source('./code/3b high_risk_days.R')

# Match circuits and run regressions
#source('./code/3c matching.R')

# Structure risk ----------------------------------------------------------

# Intersect structure locations with wildfire perimeters
#source('./code/4a intersect_structures')

# Estimate structures burned at each circuit
#source('./code/4b estimate_structures.R')

# Analyze results and generate figures ------------------------------------

# Estimate cost-effectiveness and sensitivity analysis
#source('./code/5a cost_effectiveness.R')

# Run the monte carlo analysis and generate the risk reduction figures
#source('./code/5b underground_curve.R')

# Plot some of the key figures in the analysis
#source('./code/5c main_figures.R')

# Plot some additional figures and tables
#source('./code/5d other_plots.R')
