################################################################################
## File:    Load weather covariates
## Purpose: Match gridded weather data to circuits
################################################################################


# Create a mapping of grid cells (with weather data) to circuits ----------

# Different projection
crs_raster <- 3488

# Load California outline
load(file=paste0('./data/Spatial Repository/CA_State.RData'))
poly_ca    <- st_transform(poly_ca, crs=st_crs(crs_number))

if (SWITCH_NEW_LOAD) {
  
  # Load feeders
  load(file='./intermediate/PGE_circuits.RData')
  lines_OH <- st_transform(lines_OH, st_crs(crs_raster))
  feeder_names <- sort(unique(lines_OH$circuit))
  
  # Load gridded data
  nc_data <- raster::brick('./data/GridMET/erc_2022.nc')
  nc_data <- raster::crop(nc_data, raster::extent(poly_ca))
  nc_data <- raster::as.data.frame(nc_data, xy=T)
  nc_data <- st_as_sf(nc_data, coords = c('x', 'y'))
  nc_data <- st_set_crs(nc_data, st_crs(crs_number))
  
  # Create an ID For each raster point
  nc_data$id <- 1:nrow(nc_data)
  nc_data <- select(nc_data, id, geometry)
  
  # Reproject
  nc_data <- st_transform(nc_data, st_crs(crs_raster))
  
  # Parallel process through each feeder to map to gridmet cells
  # this process can take awhile- high performance computing recommended
  
  # Cores for parallel processing
  no_cores <- detectCores()
  registerDoParallel(no_cores)
  
  # Loop through circuits and find grid ID
  gridmet_func <- function(i) {
    
    # Loop
    foreach(i=1:length(feeder_names), .combine = rbind, 
            .export=c('lines_OH', 'feeder_names', 'nc_data'),
            .packages=c('dplyr', 'sf')) %dopar% {
              
              # Subset to circuit
              tmp_lines <- lines_OH %>% 
                filter(circuit==feeder_names[i]) %>% 
                st_union()
              
              # Create buffer around circuit
              tmp_buffer <- st_buffer(tmp_lines, 2500)
              
              # Find points within buffer
              tmp_nc <- st_intersection(nc_data, tmp_buffer)
              
              # Find grid ids within circuit bounding box
              out_ids <- tmp_nc %>% 
                as.data.frame() %>% 
                select(id) %>% 
                mutate(circuit.name=feeder_names[i])
              
              # If no match
              if(nrow(out_ids)==0) {
                out_ids<-data.frame(id=NA, circuit.name=feeder_names[i])
              } else {
                out_ids<-out_ids
              }
            }
  }
  
  
  system.time(map_gridmet_cells<-gridmet_func())
  stopImplicitCluster()
  save(map_gridmet_cells, file='./intermediate/full_gridmet_mapping.RData')
  
}



# Load mapping of grid cells to circuits ----------------------------------

load('./intermediate/full_gridmet_mapping.RData')

#############################################################
# Write function to summarize weather covariates by circuit #
#############################################################

# Function to calculate average covariate for feeder
calculateFeederCovariates <- function(input_grid, input_mapping) {
  
  feeders<-unique(input_mapping$circuit.name)
  
  # Container
  out<-data.frame()
  
  # Loop
  for (i in 1:length(feeders)) {
    # Find ids for given circuit
    tmp_mapping <- input_mapping %>% 
      filter(circuit.name==feeders[i])
    
    # Collapse and take average for circuit
    tmp_grid <- input_grid %>% 
      filter(id %in% tmp_mapping$id) %>% 
      select(-id) %>% 
      summarise_all(mean) %>% 
      gather(key='key', value='value') %>% 
      select(-key) %>% 
      mutate(circuit.name=feeders[i])
    
    # Add day
    tmp_grid$day <- 1:nrow(tmp_grid)
    
    out<-bind_rows(out, tmp_grid)
  }
  
  # Export
  return(out)
  
}

# Prepping function
prepData <- function(file_name) {
  
  # Prep
  nc_data <- raster::brick(file_name)
  nc_data <- raster::crop(nc_data, raster::extent(poly_ca))
  nc_data <- raster::as.data.frame(nc_data, xy=T)
  nc_data <- st_as_sf(nc_data, coords = c('x', 'y'))
  nc_data <- st_set_crs(nc_data, st_crs(crs_raster))
  nc_data$id <- 1:nrow(nc_data)
  nc_data <- nc_data %>% 
    as.data.frame() %>% 
    select(-geometry)
  return(nc_data)
}

##################
# Identify years #
##################
in_years<-2014:2023

##########################
# Vapor Pressure Deficit #
##########################

in_files <- list.files(path='./data/GRIDMet/', pattern='vpd_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data) 
  print(g)
  
}

out_results <- out_results %>% 
  rename(vpd=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_vpd.RData')


##########################
# Min Relative Humidity  #
##########################

in_files <- list.files(path='./data/GRIDMet/', pattern='rmin_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data) 
  print(g)
  
}

out_results <- out_results %>% 
  rename(rmin=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_rmin.RData')

###############
# 100hr fuels #
###############

in_files <- list.files(path='./data/GRIDMet/', pattern='fm100_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data) 
  print(g)
  
}

out_results <- out_results %>% 
  rename(fm100=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_fm100.RData')


###################
# Max temperature #
###################

in_files <- list.files(path='./data/GRIDMet/', pattern='tmmx_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data) 
  print(g)
  
}

out_results <- out_results %>% 
  rename(tmmx=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_tmmx.RData')


###################
# Precipitation   #
###################

in_files <- list.files(path='./data/GRIDMet/', pattern='pr_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data) 
  print(g)
  
}

out_results <- out_results %>% 
  rename(pr=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_pr.RData')

################
# Wind speed   #
################

in_files <- list.files(path='./data/GRIDMet/', pattern='vs')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data) 
  print(g)
  
}

out_results <- out_results %>% 
  rename(vs=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_vs.RData')

###############################
# Energy release component   #
###############################

in_files <- list.files(path='./data/GRIDMet/', pattern='erc_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data)
  print(g)
  
}

out_results <- out_results %>%
  rename(erc=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_erc.RData')

######################################
# Evapotranspiration (mm) alfalfa?   #
######################################

in_files <- list.files(path='./data/GRIDMet/', pattern='etr_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data) 
  print(g)
  
}

out_results <- out_results %>% 
  rename(etr=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_etr.RData')

###################################################
# Potential evapotranspiration (mm)  short grass? #
###################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='pet_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data)
  print(g)
  
}

out_results <- out_results %>%
  rename(pet=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_pet.RData')

###############################
# Max relative humidity (%)   #
###############################

in_files <- list.files(path='./data/GRIDMet/', pattern='rmax_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data) 
  print(g)
  
}

out_results <- out_results %>% 
  rename(rmax=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_rmax.RData')

#####################################
# Short specific humidity (kg/kg)   #
#####################################

in_files <- list.files(path='./data/GRIDMet/', pattern='sph_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data)
  print(g)
  
}

out_results <- out_results %>%
  rename(sph=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_sph.RData')


###########################################################
# Short surface downwelling shortwave flux in air (W/m2)  #
###########################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='srad_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data)
  print(g)
  
}

out_results <- out_results %>%
  rename(srad=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_srad.RData')

##################################################
# Wind direction (degrees from clockwise north)  #
##################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='th_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data)
  print(g)
  
}

out_results <- out_results %>%
  rename(th=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_th.RData')


##################################################
# Fuel moisture 1,000 hour  #
##################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='fm1000_')

# Container
out_results<-data.frame()

for (g in 1:length(in_years)) {
  
  system.time({
    
    # Prep data
    in_data <- prepData(paste0('./data/GRIDMet/', in_files[g]))
    
    # Run function
    out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
    
    # Keep track of year
    out_data$year <- in_years[g]
  })
  
  out_results<-bind_rows(out_results, out_data)
  print(g)
  
}

out_results <- out_results %>%
  rename(fm1000=value)
save(out_results, file='./intermediate/Intermediate GridMET/compiled_fm1000.RData')



##################################################
# Elevation data  #
##################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='elevation')
in_data <- prepData(paste0('./data/GRIDMet/', in_files))

# Run function
out_data <- calculateFeederCovariates(input_grid = in_data, input_mapping = map_gridmet_cells)
out_data <- out_data %>% 
  rename(elevation=value) %>% 
  select(-day)
save(out_data, file='./intermediate/Intermediate GridMET/compiled_elevation.RData')