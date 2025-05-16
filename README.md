# electric-sector-wildfire
Code, source data, and workpapers associated with "Dynamic Grid Management Technologies Reduce Electric-Power Sector Wildfire Adaptation Costs"

Detailed documentation of analysis code is being updated below.

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

### Load vegetation

This script [2d load_vegetation](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/2d%20load_vegetation.R) imports data from USGS LANDFIRE on tree canopy height. The tree canopy data is provided in a raster format. This script calculates average and max canopy height for each distribution circuit. 

### Load ignitions

This script [2e load_ignitions](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/2e%20load_ignitions.R) imports data sources from the California Public Utilities Commission and data produced from wildfire mitigaiton plans on powerline-caused ignitions. Over time, the annual files containing ignition data have changed formats, so data cleaning is required when compiling the different sources together. 

Some years contain the name of the distribution circuit associated with the ignition in addition to the latitude and longitude of the ignition. In other cases, only the lat/long information is provided as a means of identifying the location. For the cases where only lat/long information is provided, this script finds the nearest distribution circuit to the point location. If no distribution circuit is in close proximity, the ignition is discarded. 

Another step of this script is to identify which ignitions occurred when fast-trip settings were enabled. Lists of these ignitions are provided for 2021, 2022, and 2023 based on documents filed in the wildfire mitigation plan proceedings. 

### Load PSPS and fast-trip 

The next script [2f load_PSPS.R](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/2f%20load_PSPS.R) imports the workbook that contains data on PSPS events. The names of circuits that experienced PSPS events appear to manually entered into the source data, so some data cleaning is needed to match the names of circuits that were de-energized with the GIS data on distribution circuits. 

Data on fast-trip events is also loaded in from monthly reports on fast-trip events provided to the CPUC. Some fast-trip data is also sourced from WMP filings.

Because the format of the PSPS and fast-trip data includes a start time and a restoration time, the script pro-rates the outage duration to different circuit-days if the outage extends past midnight to a second (or more) day.

PSPS events can occur at a more granular spatial and temporal resolution than our unit of analysis, which is a circuit-day. In some cases, we observe ignitions on circuit-days when PSPS events occur. This script attempts to identify ignitions that occur outside of PSPS windows by using the timestamp of the ignition event and the PSPS event. 

### Compile dataset

The last script [2g compile_dataset.R](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/2g%20compile_dataset.R) in this section compiles all the various datasets described above into a single dataset for analysis. 

As is the case with the PSPS event data, some cleaning of circuit names is required when merging the ignition data with the GIS data. This is needed because it appears some circuit names are manually entered into the source data, creating some typos and some inconsistencies in circuit name conventions. 

The script also transforms the hardening and vegetation management data to a cumulative basis anchored to 2018. Therefore if a circuit received 10 miles of undergrounding in 2019 and 5 miles in 2020, then its ending value in 2020 will reflect 15 miles.

The units of some treatment variables are transformed in this script. For example, the length of the circuit is measured in hundreds of miles rather than miles.

Two versions of the main analysis dataset are exported in this step. One of them, "regression_dataset_full.RData", includes additional columns that are not critical to the remaining analysis. The other one, "regression_dataset_clean_full.RData", drops some of the unneeded columns for faster loading and computation. 

## Estimate risk models

This section of the analysis primarily addresses the statistical models used to predict baseline ignition probability and to estimate the mitigation effectiveness coefficients.

### Ignition risk model

The first script here, [3a risk_score.R](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/3a%20risk_score.R), trains and tests the random forest model that predicts ignitions at the circuit-day level.

The first part of the function cleans up the main analysis dataset and prepares it for training and testing. The user has the option to focus on equipment-caused on ignitions if interested, but the primary workflow of this analysis focuses only on vegetation-caused ignitions (see discussion in paper). 

Various hyperparameters are tuned, and the user can specify alternate inputs if desired. Down-sampling is performed to create better balance between positive and negative ignition events. The default approach uses 3-repeat 10-fold cross-validation, and the random forest model uses 3, 6, or 9 features at each split. It ultimately selects the hyperparameter that results in the best model performance based on AUC value.

The user has the option to re-run the random forest model, or load the existing model object given computation time can be long here.

Performance statistics are generated after testing the model on the testing data. The model is then evaluated across all circuit-days. Various plots are generated, including the confusion matrix and feature importance.

### Fast-trip enablement

The next script [3b high_risk_days.R](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/3b%20high_risk_days.R) determines which circuit-days fast-trip settings are enabled on. It uses a publicly-available data on the utility's fire potential index (FPI) during ignition events. The script trains a model using a subset of those ignition events, along with detailed fire weather covariates, to predict whether or not the utility's FPI was at level three (R3) or greater. R3 or greater is the criteria to enable fast-trip settings, though in some cases the utility may enable fast-trip settings at a lower level if certain conditions are met. 

The model is then evaluated on all circuit-days, even prior to 2021 when fast-trip settings were not deployed yet. The regression model described next will use these historical days when the fire potential index was R3 or greater to measure the effectiveness of fast-trip settings. 

### Matching and logistic regression

The next script [3c high_risk_days.R](https://github.com/cody-w/electric-sector-wildfire/blob/main/code/3c%20matching.R) implements the matching procedure and fits the logistic regression model to estimate mitigation effectiveness.

The first step identifies circuits that received high or moderate levels of enhanced vegetation management (the language in the code calls these "doses" in the style of Callaway and Sant'anna 2022 differences-in-differences model). The next step prepares the regression dataset in the same fashion before it is used in the ignition probability prediction model. Other data preparation steps include merging in the FPI data and predicted baseline ignition probability from the random forest model. Then, indicators are constructed to reflect when fast-trip settings are enabled. Recall from the paper that some circuits were initially piloted with fast-trip settings in July 2021, and the remaining HFTD circuits had fast-trip settings enabled in 2022.

Various treatment variables are constructed too. This includes indicator variables for whether the circuit is in the high vegetation management tranche or the moderate vegetation management tranche. 

Next, a logistic regression model is specified prior to any matching technique. This regression result is shown in the first column of the primary regression table in the main text of the paper, and it used to illustrate potential bias from differences in baseline risk prior to matching. 

The following step implements the matching technique in which circuits are matched to their nearest neighbors on the basis of average predicted ignition probability. The user has the option here to pick the number of matches (the default is `n = 1`). Robustness results are shown in the supplementary regression tables where `n = 2`. In other words, each treated circuit is matched to its two nearest neighbors in terms of average predicted ignition probability. The user also has the option to change the caliper size of the match (the default is `std = 0.1`). This parameter reflects that matches are only successful if the nearest neighbor's average ignition probability is within 10% of the sample's standard deviation ignition probability. The matching process uses replacment, so a control circuit can be matched multiple times to a different treated circuit.

Once the control circuits have been matched to the treated circuits, the logistic regression model is run through several iterations. This includes a version where only circuits in the high vegetation management (and their matched control counterparts) are considered and a version where only the moderate vegetation management group is considered. The results for the high vegetation management group are shown in the second column of the regression table in the main text. The next set of regression models subset the sample to high-risk days (FPI of R3 or greater). These results are shown in the third column of the regression table in the main text.

All of the regression models are stored and saved in the `rscore_models.RData` object and the regression dataset and matched groups are saved in the `reg_matched_data.RData` object.  

## Estimate structures burned

## Analyze cost-effectiveness

