# electric-sector-wildfire
Code, source data, and workpapers associated with "Dynamic Grid Management Technologies Reduce Electric-Power Sector Wildfire Adaptation Costs"

Detailed documentation of analysis code is under construction.

## Overview

This repository contains the analysis code and data inputs to replicate cost-effectiveness estimates of wildfire adadptation measures in the electric-power sector. The code is organized into the following sections:

1. Setup
2. Load and clean data
3. Estimate risk models
4. Estimate structures burned
5. Analyze cost-effectiveness

## Setup

The analysis is run using RStudio statistical software. 

### Master file

The first script to run is [1 master](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/1%20master.R). This script loads required packages, specifies relevant file paths, color schemes, and the main plotting theme. Further below, a list of the sequence of scripts is commented out. It is recommended to open each of these scripts individually and run them individually. Some scripts can take a signficant amount of time to run.

This file also contains a switch labeled `R SWITCH_NEW_LOAD`. When set to `TRUE` some processes that need only be run once will be run. By default this value is set to `FALSE` so the user does not have to re-run some of the initial steps that take a significant amount of time to run.

### Miscellaneous functions

The next script is [1 misc_functions](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/1%20misc_functions.R). This script defines miscellaneous functions for regression formatting and confidence intervals. It also provides a crosswalk for CALFIRE regions to counties. 

### Initiate spatial data

The next script [1 iniate_spatial_boundaries](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/1%20iniate_spatial_boundaries.R) imports the base GIS spatial files, such as county, utility, and state boundaries. This file also imports the GIS data on distribution circuits. This file needs only to be initialized once. 

## Load and clean data

This next section of code imports datasets on ignitions, vegetation, weather, distribution circuit characteristics, and circuit-level mitigation measures. The final script in this section compiles these different data sources inton one dataset for regression and cost-effectiveness analysis. 

