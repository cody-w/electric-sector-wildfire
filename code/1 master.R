################################################################################
##
## File:    Master script
## Purpose: Source analysis 
##
################################################################################

# Setup
rm(list=ls(all=T)); gc()

# Set directory
setwd("C:/Users/codyw/OneDrive/Documents/UC Berkeley/Analysis/Ignitions/For GitHub/")

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

# Desired projection CRS
crs_number <- 4326

##################################################################
### Input switch to run key data processing steps from scratch ###
##################################################################

SWITCH_NEW_LOAD <- F

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
      #panel.background  = element_blank(),
      #plot.background   = element_rect(fill=NA, colour=NA), 
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
#source('./code/2 load_conductor_covariates.R')

# Load hardening data
#source('./code/2 load_hardening_data.R')
