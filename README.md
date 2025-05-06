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

### Conductor covariates

This script [2a load_conductor_covariates](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/2a%20load_conductor_covariates.R) imports data about the age, the length, and the wind speed rating of distribution circuits. 

### Hardening data

This script [2b load_hardening_data](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/2b%20load_hardening_data.R) imports circuit-year data on vegetation management and system hardening. To convert the circuit-year data to the circuit-day level, weekly project progress reports provided by the utility are used. For one year, vegetation management is provided in different units. The utility reports trees worked instead of miles of vegetation management completed for this year. A crosswalk is created using data from prior years when both miles of vegetation management and trees worked are provided at the circuit level.

### Weather data
This script [2c load_weather_covariates](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/2c%20load_weather_covariates.R) imports gridded weather data from GridMET. Initially, the script creates a crosswalk that intersects the locations of distribution circuits with the grid cells that the raster weather data is provided in. This step only needs to be run once, or alternatively it can be pre-loaded when `SWITCH_NEW_LOAD` is set to `FALSE`. If running from scratch (`SWITCH_NEW_LOAD` set to `TRUE`), the process will rely on parallel processing to improve processing time. This step will automatically detect the total number of cores available on the user's machine and utilize all the user's available cores: 
```R
  # Cores for parallel processing
  no_cores <- detectCores()
  registerDoParallel(no_cores)
```
This approach to parallel processing is used several times throughout the analysis. The user should be aware that these steps will utilize all available CPU on the user's machine. To avoid this, the user can set `no_cores` to a value less than the user's machine's total cores, but this will come at the cost of slower computation time. 

The next section of this script will loop through each weather variable (e.g., vapor pressure deficit, wind speed) and each year of weather data to calculate an average daily value across each grid cell that a given distribution intersects with.



## Estimate risk models

## Estimate structures burned

## Analyze cost-effectiveness

