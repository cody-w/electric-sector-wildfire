################################################################################
##
## Project: Ignitions
## Date:    Feb 23 2022
## File:    Load vegetation height data
## Purpose: Bring in LANDFIRE vegetation height data
##
################################################################################


########
# 2022 #
########

# Box 1: 42.02409, 38.33632 -- -124.70010, -119.11676
# Box 2: 38.33632, 34.00000 -- -123.70010, -118.00000

# Load raster file
#df_veg <- raster::raster("./data/LANDFIRE/landfire 2022/LF2022_CH_220_CONUS/Tif/LC22_CH_220.tif")
load("./data/LANDFIRE/landfire 2022/LF2022_CH_220_CONUS/Tif/lf2022_ch.RData")

# Load projection
tmp_proj <- raster::crs(df_veg)

# Load circuits
load('./intermediate/PGE_circuits.RData')

# Use same CRS as raster object
lines_OH <-st_transform(lines_OH, tmp_proj)

# Crop to raster extent 
# Be careful of edge observations
lines_OH <- st_crop(lines_OH, 
                    st_bbox(c(xmin=raster::bbox(df_veg)[1],
                              xmax=raster::bbox(df_veg)[3],
                              ymax=raster::bbox(df_veg)[4],
                              ymin=raster::bbox(df_veg)[2])))


# Grab all HFTD circuits
load('./intermediate/Intermediate GridMET/compiled_vpd.RData')
hftd_circuits<-unique(out_results$circuit.name)
rm(out_results);gc()

# Grab 2022 crosswalk
xwalk_22 <- read.csv('./data/LANDFIRE/landfire 2022/LF2022_CH_220_CONUS/CSV_Data/LF20_CH_220.csv') %>% 
  select(VALUE, Meters)

# Loop through 2022 HFTD circuits to get vegetation data
results<-data.frame()
for(i in 1:length(hftd_circuits)) {
  
  tmp<-lines_OH %>% 
    filter(circuit==hftd_circuits[i])
  
  # Crop to circuit of interest
  out<-raster::crop(df_veg, tmp)
  out<-raster::as.data.frame(out, xy=T)
  out<-st_as_sf(out, coords = c('x', 'y'))
  out<-st_set_crs(out, st_crs(tmp))
  
  # Create buffer
  tmp_buffer <- st_buffer(tmp, dist=100)
  
  # Find grid cells that intersect circuit of interest
  st_agr(out)='constant'
  st_agr(tmp_buffer)='constant'
  out<-st_intersection((out), 
                       (tmp_buffer))
  
  # Categorize meter height
  out <- left_join(out, xwalk_22,
                   by=c('MetersX10_Meters'='Meters')) %>% 
    mutate(height=ifelse(VALUE<=0, 0,
                         VALUE/10))
  
  # Grab max and mean
  out <- out %>% 
    as.data.frame() %>% 
    summarise(max_veg_height=max(height,na.rm=T),
              mean_veg_height=mean(height,na.rm=T))
  out$circuit.name<-tmp$circuit[1]
  
  # Bring into results container
  results<-bind_rows(results,out)
  print(i)
  
}

results <- results %>% 
  mutate(max_veg_height=ifelse(max_veg_height==-Inf, NA, max_veg_height))
save(results, file='./intermediate/Intermediate Vegetation/vegetation_height_2022.RData')

########
# 2019 #
########

# Load raster file
#df_veg <- raster::raster("./data/LANDFIRE/landfire_2019/LF2019_CH_200_CONUS/Tif/LC19_CH_200.tif")
load(file='./data/LANDFIRE/landfire_2019/LF2019_CH_200_CONUS/Tif/LC19_CH_200.RData')
xwalk_19 <- read.csv('./data/LANDFIRE/landfire_2019/LF2019_CH_200_CONUS/CSV_Data/LF16_CH_200.csv') %>% 
  select(VALUE, Meters)

# Loop through 2020 HFTD circuits to get vegetation data
results<-data.frame()
for(i in 1:length(hftd_circuits)) {
  
  tmp<-lines_OH %>% 
    filter(circuit==hftd_circuits[i])
  
  # Crop to circuit of interest
  out<-raster::crop(df_veg, tmp)
  out<-raster::as.data.frame(out, xy=T)
  out<-st_as_sf(out, coords = c('x', 'y'))
  out<-st_set_crs(out, st_crs(tmp))
  
  # Create buffer
  tmp_buffer <- st_buffer(tmp, dist=100)
  
  # Find grid cells that intersect circuit of interest
  st_agr(out)='constant'
  st_agr(tmp_buffer)='constant'
  out<-st_intersection((out), 
                       (tmp_buffer))
  
  # Categorize meter height
  out <- left_join(out, xwalk_19,
                   by=c('MetersX10_Meters'='Meters')) %>% 
    mutate(height=ifelse(VALUE<=0, 0,
                         VALUE/10))
  
  # Grab max and mean
  out <- out %>% 
    as.data.frame() %>% 
    summarise(max_veg_height=max(height,na.rm=T),
              mean_veg_height=mean(height,na.rm=T))
  out$circuit.name<-tmp$circuit[1]
  
  # Bring into results container
  results<-bind_rows(results,out)
  print(i)
  
}

results <- results %>% 
  mutate(max_veg_height=ifelse(max_veg_height==-Inf, NA, max_veg_height))
save(results, file='./intermediate/Intermediate Vegetation/vegetation_height_2019.RData')

########
# 2020 #
########


# Load raster file
#df_veg <- raster::raster("./data/LANDFIRE/landfire_2020/LF2020_CH_200_CONUS/Tif/LC20_CH_200.tif")
load(file='./data/LANDFIRE/landfire_2020/LF2020_CH_200_CONUS/Tif/LC20_CH_200.RData')
xwalk_20 <- read.csv('./data/LANDFIRE/landfire_2020/LF2020_CH_200_CONUS/CSV_Data/LF16_CH_200.csv') %>% 
  select(VALUE, Meters)

# Loop through 2020 HFTD circuits to get vegetation data
results<-data.frame()
for(i in 1:length(hftd_circuits)) {
  
  tmp<-lines_OH %>% 
    filter(circuit==hftd_circuits[i])
  
  # Crop to circuit of interest
  out<-raster::crop(df_veg, tmp)
  out<-raster::as.data.frame(out, xy=T)
  out<-st_as_sf(out, coords = c('x', 'y'))
  out<-st_set_crs(out, st_crs(tmp))
  
  # Create buffer
  tmp_buffer <- st_buffer(tmp, dist=100)
  
  # Find grid cells that intersect circuit of interest
  st_agr(out)='constant'
  st_agr(tmp_buffer)='constant'
  out<-st_intersection((out), 
                       (tmp_buffer))
  
  # Categorize meter height
  out <- left_join(out, xwalk_20,
                   by=c('MetersX10_Meters'='Meters')) %>% 
    mutate(height=ifelse(VALUE<=0, 0,
                         VALUE/10))
  
  # Grab max and mean
  out <- out %>% 
    as.data.frame() %>% 
    summarise(max_veg_height=max(height,na.rm=T),
              mean_veg_height=mean(height,na.rm=T))
  out$circuit.name<-tmp$circuit[1]
  
  # Bring into results container
  results<-bind_rows(results,out)
  print(i)
  
}

results <- results %>% 
  mutate(max_veg_height=ifelse(max_veg_height==-Inf, NA, max_veg_height))
save(results, file='./intermediate/Intermediate Vegetation/vegetation_height_2020.RData')


########
# 2014 #
########

# Load raster file
#df_veg <- raster::raster("./data/LANDFIRE/landfire_2014/US_140_CH/Tif/us_140ch.tif")
load(file='./data/LANDFIRE/landfire_2014/US_140_CH/Tif/us_140ch.RData')
xwalk_14 <- read.csv('./data/LANDFIRE/landfire_2014/US_140_CH/CSV_Data/us_140ch.csv') %>% 
  select(VALUE, CH_M_X_10)

# Loop through 2020 HFTD circuits to get vegetation data
results<-data.frame()
for(i in 1:length(hftd_circuits)) {
  
  tmp<-lines_OH %>% 
    filter(circuit==hftd_circuits[i])
  
  # Crop to circuit of interest
  out<-raster::crop(df_veg, tmp)
  out<-raster::as.data.frame(out, xy=T)
  out<-st_as_sf(out, coords = c('x', 'y'))
  out<-st_set_crs(out, st_crs(tmp))
  
  # Create buffer
  tmp_buffer <- st_buffer(tmp, dist=100)
  
  # Find grid cells that intersect circuit of interest
  st_agr(out)='constant'
  st_agr(tmp_buffer)='constant'
  out<-st_intersection((out), 
                       (tmp_buffer))
  
  # Categorize meter height
  out <- left_join(out, xwalk_14,
                   by=c('VALUE_1_CH_M_X_10'='CH_M_X_10')) %>% 
    mutate(height=ifelse(VALUE<=0, 0,
                         VALUE/10))
  
  # Grab max and mean
  out <- out %>% 
    as.data.frame() %>% 
    summarise(max_veg_height=max(height,na.rm=T),
              mean_veg_height=mean(height,na.rm=T))
  out$circuit.name<-tmp$circuit[1]
  
  # Bring into results container
  results<-bind_rows(results,out)
  print(i)
  
}

results <- results %>% 
  mutate(max_veg_height=ifelse(max_veg_height==-Inf, NA, max_veg_height))
save(results, file='./intermediate/Intermediate Vegetation/vegetation_height_2014.RData')


################################################################################
# Compile vegetation together 
################################################################################

# 2022
load(file='./intermediate/Intermediate Vegetation/vegetation_height_2022.RData')
veg_2022 <- results %>% 
  mutate(year=2022)

# 2020
load(file='./intermediate/Intermediate Vegetation/vegetation_height_2020.RData')
veg_2020 <- results %>% 
  mutate(year=2020)

# 2019
load(file='./intermediate/Intermediate Vegetation/vegetation_height_2019.RData')
veg_2019 <- results %>% 
  mutate(year=2019)

load(file='./intermediate/Intermediate Vegetation/vegetation_height_2014.RData')
veg_2014 <- results %>% 
  mutate(year=2014)

# Combine
df_veg <- bind_rows(veg_2014, veg_2019) %>%
  bind_rows(veg_2020) %>% 
  bind_rows(veg_2022)


# Interpolate
projectVegHeight <- function(df_veg, circuit) {
  
  df_veg <- df_veg %>% 
    filter(circuit.name==circuit)
  
  # Interpolate 2014-2020
  tmp <- df_veg %>% 
    filter(year==2014|year==2019)
  tmp_mean <- approx(x=tmp$year, y=tmp$mean_veg_height, 
                     xout=2014:2019)$y
  tmp_max <- approx(x=tmp$year, y=tmp$max_veg_height, 
                    xout=2014:2019)$y
  out <- data.frame(circuit.name=circuit,
                    year=2014:2019)
  out <- left_join(out, data.frame(year=2014:2019,
                                   mean_veg_height=tmp_mean,
                                   max_veg_height=tmp_max))
  
  # Grab 2020-22
  tmp <- df_veg %>% 
    filter(year==2020|year==2022)
  tmp_mean <- approx(x=tmp$year, y=tmp$mean_veg_height, 
                     xout=2020:2022)$y
  tmp_max <- approx(x=tmp$year, y=tmp$max_veg_height, 
                    xout=2020:2022)$y
  tmp<-data.frame(year=2020:2022,
                  mean_veg_height=tmp_mean,
                  max_veg_height=tmp_max,
                  circuit.name=circuit)
  
  # Combine
  out<-bind_rows(out,tmp)
  return(out)
}

results<-data.frame()
hftd_circuits <- unique(df_veg$circuit.name)
for(i in 1:length(hftd_circuits)) {
  tmp<-projectVegHeight(df_veg, hftd_circuits[i])
  results<-bind_rows(results,tmp)
  print(i)
}

# Export
save(results, file='./intermediate/Intermediate Vegetation/vegetation_height_2014_2022.RData')

# Use 2022 as approximation for 2023
load(file='./intermediate/Intermediate Vegetation/vegetation_height_2014_2022.RData')
tmp <- results %>% 
  filter(year==2022) %>% 
  mutate(year=2023)
results <- bind_rows(results, tmp) %>% 
  arrange(circuit.name, year)
save(results, file='./intermediate/Intermediate Vegetation/vegetation_height_2014_2023.RData')
