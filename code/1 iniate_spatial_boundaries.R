################################################################################
###                                                                            #                                            #  
### File:    Initiate spatial data                                             #
### Purpose: To source outlines of states, counties, etc.                      #                                                  #
###                                                                            #   
################################################################################


# US Counties -------------------------------------------------------------

# Load U.S. counties and store
poly_county <- st_read(paste0(path_spatial, '/US Counties'))

# Transform
poly_county <- st_transform(poly_county, crs = st_crs(crs_number))

# Export all
save(poly_county, file=paste0(path_spatial, '/US_Counties.RData'))

# Crop to California
poly_county <- poly_county %>% 
  filter(STATEFP=='06')

# Export CA counties
save(poly_county, file=paste0(path_spatial, '/CA_Counties.RData'))

# ~~ Utility service territories ~~ #

# Load and crop
poly_utility <- st_read(paste0(path_spatial, '/Utility Service Territory'))
poly_utility <- poly_utility %>% 
  filter(NAME %in% c('PACIFIC GAS & ELECTRIC CO.', 'SAN DIEGO GAS & ELECTRIC CO',
                     'SOUTHERN CALIFORNIA EDISON CO')) 
poly_utility$utility <- c('PG&E', 'SCE', 'SDG&E')

# Transform
poly_utility <- st_transform(poly_utility, crs=st_crs(crs_number))

# Export
save(poly_utility, file=paste0(path_spatial, '/CA_Utility.RData'))

# -- California state boundary -- #

# Load
poly_ca <- st_read(paste0(path_spatial, '/CA State'))

# Transform
poly_ca <- st_transform(poly_ca, crs = st_crs(crs_number))

# Export
save(poly_ca, file=paste0(path_spatial, '/CA_State.RData'))

# -- California high fire threat districts -- #

# Tier 2
poly_hftd2 <- st_read(paste0(path_spatial, '/CPUC_Fire_Threat_Tier2_Elevated_WGS84'))

# Transform 
poly_hftd2 <- st_transform(poly_hftd2, crs=st_crs(crs_number))

# Tier 3
poly_hftd3 <- st_read(paste0(path_spatial, '/CPUC_Fire_Threat_Tier3_Elevated_WGS84'))

# Transform 
poly_hftd3 <- st_transform(poly_hftd3, crs=st_crs(crs_number))

# Combine
poly_hftd <- bind_rows(poly_hftd2, poly_hftd3) %>% 
  mutate(HFTD = ifelse(HazZone==2, 'Tier 2', 'Tier 3'))

# Export
save(poly_hftd, file=paste0(path_spatial, '/HFTD.RData'))



# Load PG&E Circuits ------------------------------------------------------

# Load circuits
tmp_layers  <- st_layers("./data/PGE/2020 WMP/EDGIS2-12.gdb")
lines_OH   <- st_read("./data/PGE/2020 WMP/EDGIS2-12.gdb",
                       layer=tmp_layers$name[10])

# Transform
lines_OH <- st_transform(lines_OH, crs = 3488)

# Check geometry types
table(st_geometry_type(lines_OH$SHAPE))
sum(!st_geometry_type(lines_OH)%in%c('MULTILINESTRING', 'LINESTRING'))
tmp <- !st_geometry_type(lines_OH)%in%c('MULTILINESTRING', 'LINESTRING')
lines_OH <- lines_OH[!tmp,]
tmp <- lines_OH[tmp,]
tmp <- st_cast(tmp, 'MULTILINESTRING')
lines_OH <- bind_rows(lines_OH, tmp)

# Names
lines_OH <- lines_OH %>% 
  rename(circuit = CIRCUITNAME, voltage = NOMINALVOLTAGE) %>% 
  select(circuit, voltage, INSTALLJOBYEAR, CIRCUITID:CONVCIRCUITID2,
         COUNTY, ZIP, WINDSPEEDCODE, WINDSPEEDRERATEYEAR)

# Save
save(lines_OH, file='./intermediate/PGE_Circuits.RData')