################################################################################
## Project: Ignitions
## Purpose: Intersect circuits with potential structure losses 
################################################################################

#####################
### Load circuits ###
#####################

# Load circuits
load('./intermediate/PGE_Circuits.RData')

# Combine circuit segments
x <- lines_OH %>% 
  filter(is.na(circuit)) %>% 
  filter(!is.na(CONVCIRCUITID))
y <- lines_OH %>% 
  filter(CONVCIRCUITID%in%x$CONVCIRCUITID) %>% 
  as.data.frame() %>% 
  select(circuit, CONVCIRCUITID) %>% 
  unique()%>% 
  filter(!is.na(circuit)) %>% 
  rename(circuit.missing=circuit) %>% 
  group_by(CONVCIRCUITID) %>% 
  mutate(count=n()) %>% 
  filter(count==1)
lines_OH <- left_join(lines_OH, y)
lines_OH <- lines_OH %>% 
  mutate(circuit = ifelse(is.na(circuit), circuit.missing, circuit)) %>% 
  select(-circuit.missing, -count)
lines_OH <- lines_OH %>% 
  filter(!is.na(circuit)) %>% 
  group_by(circuit) %>% 
  summarise() 

# Ensure circuits have some HFTD overlap -- require > 10 miles HFTD
load(file='./intermediate/Regression Dataset/regression_dataset_clean_full.RData')

df <- df %>% 
  filter(date==as.Date('2020-12-31')) %>% 
  filter(!is.na(tier.2.oh.miles)) %>% 
  filter((tier.2.oh.miles + tier.3.oh.miles > 0.1))

lines_hftd <- lines_OH %>% 
  filter(circuit%in%df$circuit.name)

####################################
### Load housing impact data set ###
####################################

# Load raster file
df_building <- raster::raster("./data/Pyrologix/RDS_California/CA/BuildingCount_CA.tif")
df_crps <- raster::raster("./data/Pyrologix/RDS_California_Fire/CRPS_CA.tif")

# Load projection
tmp_proj <- raster::crs(df_building)

###############################################
### Create ignition points for each circuit ###
###############################################

# Use same CRS as raster object
lines_hftd <-st_transform(lines_hftd, tmp_proj)

#########################################################
# Write function to output simulated ignition locations #
#########################################################
simulateIgnitions <- function(c_name, circuits, 
                              i_density=1/5, n_density=-99,
                              r_seed=42) {
  
  circuits <- circuits %>% 
    filter(circuit==c_name)
  
  # Randomly sample coordinates - be careful here-- seed should be reproducible
  # for same sampled ignition locations
  set.seed(r_seed)
  
  # Determine number of ignitions based off length (1 per 5 mile)
  if(n_density<0) {
    N_ignitions<-round(as.numeric(sum(st_length(circuits)))/1000 * 0.621 * i_density)
    
    # Ensure at least 3 ignitions per circuit
    N_ignitions<-max(c(N_ignitions, 3))
    
    # No more than 10 ignitions per circuit
    N_ignitions<-min(c(N_ignitions, 10))
    
  } else {
    N_ignitions<-n_density
  }
  
  x_coords <- as.data.frame(st_coordinates(circuits$SHAPE))
  x_ignitions <- sample_n(x_coords, N_ignitions, replace=F)
  x_ignitions <- st_as_sf(x_ignitions, coords = c('X', 'Y'))
  x_ignitions <-st_set_crs(x_ignitions, st_crs(df_building))
  
  # Make into polygon buffer (1 acre)
  x_ignitions <- st_buffer(x_ignitions, dist=36)
  st_area(x_ignitions)
  
  # Loop through and save each ignition
  for (i in 1:nrow(x_ignitions)) {
    
    # File name
    fname <- gsub(' ', '', c_name)
    fname <- paste0('./intermediate/Missoula Fire Lab/Sampled Ignitions/', 
                    fname, '_ignitions_',
                    i, '.shp')
    
    st_write(x_ignitions[i,],
             fname,
             append = F)
  }
  
  # Add id to each ignition
  x_ignitions <- x_ignitions %>% 
    mutate(id=1:nrow(x_ignitions),
           circuit.name=c_name)
  print(N_ignitions)
  return(x_ignitions)
}

if (SWITCH_NEW_LOAD) {

  # Simulate circuit ignitions and save to file
  for (i in 1:5) {
    out <- simulateIgnitions(lines_hftd$circuit[i], lines_hftd,
                             n_density=5)
    if(i==1) {
      df_ignitions <- out
    } else if(i>1) {
      df_ignitions <- bind_rows(df_ignitions, out)
      
    }
  }
  
  # Save sampled ignitions file
  save(df_ignitions, file = './intermediate/Missoula Fire Lab/ignitions_pyrologix.RData')
}

# Load previously run file
load(file = './intermediate/Missoula Fire Lab/ignitions_pyrologix.RData')
 
################################################################################
# Write function to intersect ignition points with housing impact
################################################################################

### Function ###
intersectHousing <- function(c_name, ignitions, lines,
                             raster_building,
                             raster_crps) {
  
  # Acres/pixel conversion
  pixel_acres <- 900/4046
  
  # # Input
  # c_name <- df_ignitions$circuit.name[1]
  # c_name <- 'SILVERADO 2104'
  # ignitions <- df_ignitions
  # lines <- lines_hftd
  
  # Subset to relevant ignitions & circuit
  ignitions <- ignitions %>% 
    filter(circuit.name==c_name)
  lines <- lines %>% 
    filter(circuit==c_name)
  
  # Acre buckets
  acre_buckets <- c(1, 150, 2000, 20000)
  adj_buckets   <- c(NA, 405, 1570, 5041)
  counter <- 1
  results <- data.frame()
  
  # Loop through acres
  for (a in acre_buckets) {
    
    # one-acre --- no need to change size of fire because ignition=1 acre
    if (a==1) {
      fires <- ignitions
      counter <- counter + 1 
      
      # Crop Building file to circuit bounding box
      housing <- raster::crop(raster_building, lines)
      housing<-raster::as.data.frame(housing, xy=T)
      housing<-st_as_sf(housing, coords = c('x', 'y'))
      housing<-st_set_crs(housing, st_crs(lines))
      
      # Crop CPRS file to circuit bounding box
      crps <- raster::crop(raster_crps, lines)
      crps <- raster::as.data.frame(crps, xy=T)
      crps <- st_as_sf(crps, coords = c('x', 'y'))
      crps <- st_set_crs(crps, st_crs(lines))
      
    }
    
    # larger acres -- need to expand size
    if (a>1) {
      
      # Create fire of given ignition size using buffer
      fires <- st_buffer(ignitions, dist = adj_buckets[counter])
      counter <- counter + 1
      
      # Crop Building file to circuit bounding box
      housing <- raster::crop(df_building, st_bbox(fires))
      housing<-raster::as.data.frame(housing, xy=T)
      housing<-st_as_sf(housing, coords = c('x', 'y'))
      housing<-st_set_crs(housing, st_crs(lines))
      
      # Crop CPRS file to circuit bounding box
      crps <- raster::crop(df_crps, st_bbox(fires))
      crps <- raster::as.data.frame(crps, xy=T)
      crps <- st_as_sf(crps, coords = c('x', 'y'))
      crps <- st_set_crs(crps, st_crs(lines))
      
    }
    
    # Intersect fires with pixels
    fires_buildings <- st_intersection(fires, housing)
    fires_crps      <- st_intersection(fires, crps)
    out_data        <- full_join(data.frame(fires_buildings),
                                 data.frame(fires_crps),
                                 by=c('L1', 'L2', 'id',
                                      'circuit.name', 'geometry')) %>% 
      select(-geometry) %>% 
      mutate(bldg_acre = (BuildingCount_CA * CRPS_CA/100 / pixel_acres),
             fire_size = a)
    
    # Get summary stats of damage
    out_summary <- out_data %>% 
      group_by(circuit.name) %>% 
      summarise(mean_bldg  =mean(BuildingCount_CA, na.rm=T)/pixel_acres,
                median_bldg=median(BuildingCount_CA, na.rm=T)/pixel_acres,
                sd_bldg    =sd(BuildingCount_CA, na.rm=T)/pixel_acres,
                mean_crps  =mean(CRPS_CA/100, na.rm=T),
                median_crps=median(CRPS_CA/100, na.rm=T),
                sd_crps    =sd(CRPS_CA/100, na.rm=T),
                mean_bldg_acre   = mean(bldg_acre, na.rm=T),
                median_bldg_acre = median(bldg_acre, na.rm=T),
                sd_bldg_acre     = sd(bldg_acre, na.rm=T)) %>% 
      mutate(fire_size = a)
    
    # Store results
    results <- bind_rows(results, out_summary)
    print(paste0(c_name, ' -- ', a, ' acres'))
    
  }
  
  # Exit
  return(results)
}



# Run function to get potential structure losses --------------------------

list_circuits <- unique(lines_hftd$circuit)

# Run in chunks due to length of run time 

# Chunk A

out_damages <- data.frame()
system.time({
  for (i in 1:100) {
    tmp <- intersectHousing(c_name    = list_circuits[i],
                            ignitions = df_ignitions,
                            lines     = lines_hftd,
                            raster_building = df_building,
                            raster_crps = df_crps)
    out_damages <- bind_rows(out_damages, tmp)
    print(round(i/100, 2))
  }
})

save(out_damages, file='./intermediate/Missoula Fire Lab/damages_1_100.RData')

# Chunk B

out_damages <- data.frame()
system.time({
  for (i in 101:200) {
    tmp <- intersectHousing(c_name    = list_circuits[i],
                            ignitions = df_ignitions,
                            lines     = lines_hftd,
                            raster_building = df_building,
                            raster_crps = df_crps)
    out_damages <- bind_rows(out_damages, tmp)
    print(round(i/100, 2))
  }
})

save(out_damages, file='./intermediate/Missoula Fire Lab/damages_101_200.RData')

# Chunk C

out_damages <- data.frame()
system.time({
  for (i in 201:300) {
    tmp <- intersectHousing(c_name    = list_circuits[i],
                            ignitions = df_ignitions,
                            lines     = lines_hftd,
                            raster_building = df_building,
                            raster_crps = df_crps)
    out_damages <- bind_rows(out_damages, tmp)
    print(round(i/100, 2))
  }
})

save(out_damages, file='./intermediate/Missoula Fire Lab/damages_201_300.RData')

# Chunk D

out_damages <- data.frame()
system.time({
  for (i in 301:length(list_circuits)) {
    tmp <- intersectHousing(c_name    = list_circuits[i],
                            ignitions = df_ignitions,
                            lines     = lines_hftd,
                            raster_building = df_building,
                            raster_crps = df_crps)
    out_damages <- bind_rows(out_damages, tmp)
    print(round(i/100, 2))
  }
})

save(out_damages, file='./intermediate/Missoula Fire Lab/damages_301_400.RData')