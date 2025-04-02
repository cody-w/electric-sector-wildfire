################################################################################
##
## Project: Ignitions
## Date:    Feb 23 2022
## File:    Load risk scores and covariates
## Purpose: Bring in Molly's risk score as well as other covariates
##
################################################################################

# # # Load risk scores
# # library(arrow)
# # df <- read_feather(file='./data/Risk model/PGE_2015to2018_Riskscore.ftr')
# # feeder_ids <- unique(df$FeederID)
# 
crs_raster <- 4326
# 
# # Load treatment data
# df <- read_excel(path='./data/PGE/2021 WMP/Data Requests/CalAdvocates_035 (2021WMP-01)/WildfireMitigationPlans_DR_CalAdvocates_035-Q04-Atch01.xlsx',
#                  sheet = '4_Distribution', skip=2)
# 
# # Filter to HFTD circuits
# df <- df %>% 
#   filter(`Tier 2 OH Miles` > 0 | `Tier 3 OH Miles` > 0)
# feeder_names <- unique(df$`Circuit Name`)
# rm(df); gc()
# 
# # Load feeders
# load(file='./intermediate/PGE_circuits.RData')
# lines_OH <- lines_OH %>% 
#   filter(CIRCUITNAME %in% feeder_names)
# lines_OH <- st_transform(lines_OH, st_crs(crs_raster))
# feeder_names <- feeder_names[feeder_names%in%unique(lines_OH$CIRCUITNAME)]
# 
# Load California outline
load(file=paste0(path_repository, '/CA_State.RData'))
poly_ca    <- st_transform(poly_ca, crs=st_crs(crs_raster))
# 
# # Load gridded data
# nc_data <- raster::brick('./data/GRIDMet/vpd_2022.nc')
# nc_data <- raster::crop(nc_data, raster::extent(poly_ca))
# nc_data <- raster::as.data.frame(nc_data, xy=T)
# nc_data <- st_as_sf(nc_data, coords = c('x', 'y'))
# nc_data <- st_set_crs(nc_data, st_crs(crs_raster))
# 
# # Create an ID For each raster point
# nc_data$id <- 1:nrow(nc_data)
# nc_data <- select(nc_data, id, geometry)
# 
# # Loop through circuits and find grid ID
# 
# # Subset to circuit of interest
# results<-data.frame()
# for (i in 1:length(feeder_names)) {
#   print(i)
#   
#   tmp_lines <- lines_OH %>% 
#     filter(CIRCUITNAME==feeder_names[i]) %>% 
#     st_transform(.,st_crs(crs_number)) %>% 
#     st_union()
#   
#   tmp_buffer <- st_buffer(tmp_lines, 2500)
#   
#   tmp_nc <- st_intersection(tmp_buffer, nc_data %>% 
#                               st_transform(.,st_crs(crs_number)))
#   
#   ggplot() +
#     geom_sf(data=tmp_lines, color='black') +
#     geom_sf(data=tmp_buffer, fill=color_blue, alpha=0.5) +
#     geom_sf(data=tmp_nc, color=color_purple)# +
#     #geom_sf(data=poly_ca, fill=NA, color='black')
#   
#   
#   # Find grid ids within circuit bounding box
#   tmp_nc  <- st_crop(nc_data, st_bbox(tmp_lines))
#   out_ids <- tmp_nc %>% 
#     as.data.frame() %>% 
#     select(id) %>% 
#     mutate(circuit.name=feeder_names[i])
#   
#   # Save to results
#   results <- bind_rows(results, out_ids)
# 
# }
# 
# # Save
# map_gridmet_cells <- results
# save(map_gridmet_cells, file='./intermediate/gridmet_mapping.RData')
# load(file='./intermediate/gridmet_mapping.RData')
# x<-map_gridmet_cells
# load(file='./intermediate/gridmet_mapping_all.RData')
# 
# 
# tmp_lines <- lines_OH
# 
# # Clean up space 
# rm(lines_OH, nc_data, tmp_lines)

##########################
# LOAD MAPPING           #
##########################

load('./intermediate/full_gridmet_mapping.RData')
map_gridmet_cells<-map_gridmet_cells_ALL

#####################################################
# Write function to summarize covariates by circuit #
#####################################################

# Function to calculate average covariate for feeder
calculateFeederCovariates <- function(input_grid, input_mapping) {
  
  # input_grid    <- nc_data %>% 
  #   as.data.frame() %>% 
  #   select(-geometry)
  # input_mapping <- map_gridmet_cells
  
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
  
  #file_name <- './data/GRIDMet/metdata_elevationdata.nc'
  
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

# Identify years
in_years<-2023

##########################
# Vapor Pressure Deficit #
##########################

in_files <- list.files(path='./data/GRIDMet/', pattern='vpd_2023')

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
save(out_results, file='./intermediate_24/compiled_vpd_2023.RData')


##########################
# Min Relative Humidity  #
##########################

in_files <- list.files(path='./data/GRIDMet/', pattern='rmin_2023')

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
save(out_results, file='./intermediate_24/compiled_rmin_2023.RData')

###############
# 100hr fuels #
###############

in_files <- list.files(path='./data/GRIDMet/', pattern='fm100_2023')

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
save(out_results, file='./intermediate_24/compiled_fm100_2023.RData')


###################
# Max temperature #
###################

in_files <- list.files(path='./data/GRIDMet/', pattern='tmmx_2023')

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
save(out_results, file='./intermediate_24/compiled_tmmx_2023.RData')


###################
# Precipitation   #
###################

in_files <- list.files(path='./data/GRIDMet/', pattern='pr_2023')

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
save(out_results, file='./intermediate_24/compiled_pr_2023.RData')

################
# Wind speed   #
################

in_files <- list.files(path='./data/GRIDMet/', pattern='vs_2023')

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
save(out_results, file='./intermediate_24/compiled_vs_2023.RData')

###############################
# Energy release component   #
###############################

in_files <- list.files(path='./data/GRIDMet/', pattern='erc_2023')

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
save(out_results, file='./intermediate_24/compiled_erc_2023.RData')

######################################
# Evapotranspiration (mm) alfalfa?   #
######################################

in_files <- list.files(path='./data/GRIDMet/', pattern='etr_2023')

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
save(out_results, file='./intermediate_24/compiled_etr_2023.RData')

###################################################
# Potential evapotranspiration (mm)  short grass? #
###################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='pet_2023')

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
save(out_results, file='./intermediate_24/compiled_pet_2023.RData')

###############################
# Max relative humidity (%)   #
###############################

in_files <- list.files(path='./data/GRIDMet/', pattern='rmax_2023')

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
save(out_results, file='./intermediate_24/compiled_rmax_2023.RData')

#####################################
# Short specific humidity (kg/kg)   #
#####################################

in_files <- list.files(path='./data/GRIDMet/', pattern='sph_2023')

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
save(out_results, file='./intermediate_24/compiled_sph_2023.RData')


###########################################################
# Short surface downwelling shortwave flux in air (W/m2)  #
###########################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='srad_2023')

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
save(out_results, file='./intermediate_24/compiled_srad_2023.RData')

##################################################
# Wind direction (degrees from clockwise north)  #
##################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='th_2023')

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
save(out_results, file='./intermediate_24/compiled_th_2023.RData')


##################################################
# Fuel moisture 1,000 hour  #
##################################################

in_files <- list.files(path='./data/GRIDMet/', pattern='fm1000_2023')

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
save(out_results, file='./intermediate_24/compiled_fm1000_2023.RData')



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
save(out_data, file='./intermediate/compiled_elevation.RData')









load(file='./intermediate/compiled_erc_2021.RData')

x<-out_results %>% 
  filter(is.na(fm1000))
table(x$circuit.name)

x<-out_results %>% 
  filter(circuit.name=='SILVERADO 2102' |
           circuit.name=='APPLE HILL 2102' | 
           circuit.name=='18TH STREET 0402') %>% 
  group_by(year, circuit.name) %>% 
  summarise(erc=mean(erc))


ggplot(data=x,aes(x=year,y=erc,color=circuit.name)) + 
  geom_line()






x<-out_results %>% 
  group_by(day,year) %>% 
  summarise(rmin=mean(rmin,na.rm=T)) %>% 
  arrange(year,day)
plot(x$rmin)


x<-out_results %>% 
  group_by(circuit.name, year) %>% 
  summarise(tmean=mean(tmmx, na.rm=T)) %>% 
  arrange(tmean)






tmp_lines<-lines_OH %>% 
  filter(CIRCUITNAME==x$circuit.name[1])
ggplot()+geom_sf(data=tmp_lines) +theme(text=element_text(size=7))


# Load gridded data
nc_data <- raster::brick('./data/GRIDMet/tmmx_2020.nc')
nc_data <- raster::crop(nc_data, raster::extent(poly_ca))
nc_data <- raster::as.data.frame(nc_data, xy=T)
nc_data <- st_as_sf(nc_data, coords = c('x', 'y'))
nc_data <- st_set_crs(nc_data, st_crs(crs_raster))

# Create an ID For each raster point
nc_data$id <- 1:nrow(nc_data)
nc_data <- select(nc_data, id, geometry)
keep<-nc_data

# Load gridded data
nc_data <- raster::brick('./data/GRIDMet/rmin_2014.nc')
nc_data <- raster::crop(nc_data, raster::extent(poly_ca))
nc_data <- raster::as.data.frame(nc_data, xy=T)
nc_data <- st_as_sf(nc_data, coords = c('x', 'y'))
nc_data <- st_set_crs(nc_data, st_crs(crs_raster))

# Create an ID For each raster point
nc_data$id <- 1:nrow(nc_data)
nc_data <- select(nc_data, id, geometry)

x<-5000

ggplot() +
  geom_sf(data=nc_data %>% filter(id==x), color=color_blue, size=10) +
  geom_sf(data=keep %>% filter(id==x), color=color_orange, size=5, alpha=0.5)

nc_data <- raster::brick('./data/GRIDMet/tmmx_2014.nc')

nc_data <- ncdf4::nc_open('./data/GRIDMet/pet_2020.nc')
nc_data
