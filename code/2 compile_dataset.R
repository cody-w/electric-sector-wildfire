################################################################################
##
## Project: Ignitions
## Purpose: Stitch together ignitions, treatment data, and covariates
##
################################################################################

# Load treatment data
load(file='./intermediate_24/compiled_treatment_2014_2023.RData')

# #------ drop out sectionalization devices
# df_treatment <- df_treatment %>% 
#   select(-`sectionalization_devices_hftd-2`:-`sectionalization_devices_non-hftd`)

#------ change names
df_treatment <- df_treatment %>% 
  mutate(circuit.name=gsub('NEWARK 12KV', 'NEWARK', circuit.name),
         circuit.name=gsub('NEWARK 21KV', 'NEWARK', circuit.name))

#############################
# Bring in circuit controls #
#############################

load(file='./intermediate/circuit_covariates.RData')
df_treatment<-left_join(df_treatment, circuit_data)

# Replace missing circuits age with mean
x<-mean(df_treatment$conductor_year, na.rm=T)
df_treatment <- df_treatment %>% 
  mutate(conductor_year=ifelse(is.na(conductor_year), x, conductor_year))

# Transform to age based off of 2019 year
df_treatment$conductor_age<- 2019-df_treatment$conductor_year

rm(circuit_data, tmp2); gc()

#############################
# Bring in PSPS information #
#############################

load('./intermediate_24/psps_compiled_PGE.RData')

# Merge with main dataset
df_treatment <- left_join(df_treatment, psps_results)

# Set to zero if NA for PSPS
df_treatment <- df_treatment %>% 
  mutate(across(psps_customer_hours:is_psps_ignition, ~ifelse(is.na(.), 0, .)))

# Create 'sectionalized' version of psps
df_treatment <- df_treatment %>%
  mutate(is_psps_section = ifelse(psps_hours>0, 1, 0),
         is_psps_section = ifelse(psps_ignition_highwind=='N' &
                                    psps_ignition_redflag=='N' & year<2021, 0, is_psps_section),
         is_psps_section = ifelse(psps_ignition_landuse=='Urban' & year<2021, 0, is_psps_section),
         is_psps_section = ifelse(psps_ignition_HFTD=='Non-HFTD' & year<2021, 0, is_psps_section),
         is_psps = ifelse(psps_hours>0, 1, 0),
         is_psps_section = ifelse(is.na(is_psps_section) & is_psps==1, 1, is_psps_section),
         is_psps_section = ifelse(is.na(is_psps_section), 0, is_psps_section))

rm(psps_results, tmp); gc()


#!! memory management
df_treatment <- df_treatment %>% 
  select(-psps_customers, -psps_hours,
         -psps_ignition_cause, -psps_ignition_landuse,
         -psps_ignition_highwind, -psps_ignition_redflag,
         -psps_ignition_HFTD); gc()

################################################################################
# Bring in EPSS information ####################################################
################################################################################

# Load
load(file='./intermediate_24/epss_compiled_PGE_2023.RData')

# Consolidate
epss_results <- epss_results %>% 
  group_by(circuit.name, date) %>% 
  summarise(across(contains('epss'), sum)) %>% 
  mutate(across(contains('epss'), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(across(contains('epss'), ~./60)) 
names(epss_results) <- gsub('minutes', 'hours', names(epss_results))

# Merge
df_treatment <- left_join(df_treatment, epss_results)

# Edit
df_treatment <- df_treatment %>% 
  mutate(across(contains('epss'), ~ifelse(is.na(.), 0, .))) %>%
  mutate(is_epss = ifelse(epss.customer.hours>0, 1, 0))

rm(epss_results)

##############################################
# Drop 2014 now
##############################################

df_treatment <- df_treatment %>%
  filter(year>2014)
gc()

#######################
# Bring in covariates #
#######################

# Weather vars to compile/merge
weather_vars <- c('vpd', 'rmin', 'fm100', 'fm1000',
                  'tmmx', 'pr', 'vs', 'th',
                  'erc', 'etr', 'srad', 'sph', 'pet')

# Write function
loadWeatherVars <- function(w_var) {
  
  # Load
  load(paste0('./intermediate_24/compiled_', w_var, '_2023.RData'))
  tmp <- out_results
  load(paste0('./intermediate/compiled_', w_var, '_2021.RData'))
  out_results <- bind_rows(out_results,tmp)
  
  save(out_results, 
       file=paste0('./Clone/electric-sector-wildfire/intermediate/Intermediate GridMET/compiled_',
                   w_var, '.RData'))
  
}

# Run function
for(i in 1:length(weather_vars)) {
  print(i)
  print(weather_vars[i])
  
  # Execute
  loadWeatherVars(weather_vars[i]) 
  gc()
}

#######################
# Bring in covariates #
#######################

# Weather vars to compile/merge
weather_vars <- c('vpd', 'rmin', 'fm100', 'fm1000',
                  'tmmx', 'pr', 'vs', 'th',
                  'erc', 'etr', 'srad', 'sph', 'pet')

# Write function
loadWeatherVars <- function(df, w_var, dt_start, dt_end) {
  
  # Load
  load(paste0('./intermediate_24/compiled_', w_var, '_2023.RData'))
  tmp <- out_results
  load(paste0('./intermediate/compiled_', w_var, '_2021.RData'))
  out_results <- bind_rows(out_results,tmp)
  
  # Create date range
  dt_range <- seq(dt_start, dt_end, 1)
  dt_range <- rep(dt_range, length(unique(out_results$circuit.name)))
  
  # Arrange and merge
  out_results <- out_results %>% 
    arrange(circuit.name, year, day) %>% 
    filter(year>2014)
  out_results$date <- dt_range
  out_results$day  <- NULL
  
  # Merge
  df <- left_join(df, out_results)
  return(df)
  
}

# Run function
for(i in 1:length(weather_vars)) {
  print(i)
  print(weather_vars[i])
  
  # Execute
  df_treatment <- loadWeatherVars(df_treatment, weather_vars[i],
                                  dt_start=ymd('2015-01-01'),
                                  dt_end=ymd('2023-12-31')) 
  gc()
}


# ~~~~~~~~~~~~~~~ELEVATION
load('./intermediate/compiled_elevation.RData')

# Merge
df_treatment <- left_join(df_treatment, out_data)
rm(out_data, out_results); gc()

# ~~~~~~~~~~~~~~~Vegetation height
load('./intermediate_24/vegetation_height_2014_2023.RData')

# Merge
df_treatment <- left_join(df_treatment, results)
rm(results);gc()

##################################
# Bring in REVISED ignition data #
##################################

load('./intermediate_24/revised_ignition_tracker.RData')

# Edit circuit names that don't match 
out <- out %>% 
  mutate(circuit.name=ifelse(circuit=='ANYTA 1106', 'ANITA 1106',
                             circuit),
         circuit.name=ifelse(circuit.name=='ASHLAN AVE 1116 / ASHLAN AVE 1108',
                             'ASHLAN AVE 1116', circuit.name),
         circuit.name=ifelse(circuit.name=='ASHLAN AVENUE 2119',
                             'ASHLAN AVE 2119', circuit.name),
         circuit.name=ifelse(circuit.name=='CARMEL 0405',
                             'VIEJO 2203', circuit.name),
         circuit.name=ifelse(circuit.name=='BRENTWOOD SUB 2112',
                             'BRENTWOOD 2112', circuit.name),
         circuit.name=ifelse(circuit.name=='CAL CEMENT 1101',
                             'CALAVERAS CEMENT 1101', circuit.name),
         circuit.name=ifelse(circuit.name=='CHICO STATION A 1101',
                             'CHICO A 1101', circuit.name),
         circuit.name=ifelse(circuit.name=='CHICO STATION C 0401',
                             'CHICO C 0401', circuit.name),
         circuit.name=ifelse(circuit.name=='COALINGA NO. 2 1106?',
                             'COALINGA NO 2 1106', circuit.name),
         circuit.name=ifelse(circuit.name=='COARSEGOLD SUB 2103',
                             'COARSEGOLD 2103', circuit.name),
         circuit.name=ifelse(circuit.name=='COARSEGOLD SUB 2104',
                             'COARSEGOLD 2104', circuit.name),
         circuit.name=ifelse(circuit.name=='COLUMBIA 1101',
                             'COLUMBIA HILL 1101', circuit.name),
         circuit.name=ifelse(circuit.name=='CONTRA COSTA 2203',
                             'CONTRA COSTA 2103', circuit.name),
         circuit.name=ifelse(circuit.name=='CONTRA COSTA 2204',
                             'CONTRA COSTA 2104', circuit.name),
         circuit.name=ifelse(circuit.name=='COTTONWOOD 1101 OR 1102',
                             'COTTONWOOD 1101', circuit.name),
         circuit.name=gsub('DELMAR', 'DEL MAR', circuit.name),
         circuit.name=ifelse(circuit.name=='DUMBARTON SUB 1102',
                             'DUMBARTON 1102', circuit.name),
         circuit.name=ifelse(circuit.name=='EEL RIVER 1101',
                             'EEL RIVER 1102', circuit.name),
         circuit.name=ifelse(circuit.name=='EL DORADO P H 2101',
                             'EL DORADO PH 2101', circuit.name),
         circuit.name=ifelse(circuit.name=='FIGARDEN SUB. 2107',
                             'FIGARDEN 2107', circuit.name),
         circuit.name=ifelse(circuit.name=='FORT BRAGG STA A 1101',
                             'FORT BRAGG A 1101', circuit.name),
         circuit.name=ifelse(circuit.name=='FORT BRAGG STA A 1102',
                             'FORT BRAGG A 1102', circuit.name),
         circuit.name=ifelse(circuit.name=='FORT ORD 1107',
                             'FORT ORD 2107', circuit.name),
         circuit.name=ifelse(circuit.name=='FORD ORD 2106',
                             'FORT ORD 2106', circuit.name),
         circuit.name=ifelse(circuit.name=='GOLDTREE1105',
                             'GOLDTREE 1105', circuit.name),
         circuit.name=ifelse(circuit.name=='JACOBS CORNER SUB 1101',
                             'JACOBS CORNER 1101', circuit.name),
         circuit.name=ifelse(circuit.name=='LERDO 1107',
                             'LERDO 1116', circuit.name),
         circuit.name=ifelse(circuit.name=='LIVINGTON 1104',
                             'LIVINGSTON 1104', circuit.name),
         circuit.name=ifelse(circuit.name=='LOCKEFORD SUB 2102',
                             'LOCKEFORD 2102', circuit.name),
         circuit.name=ifelse(circuit.name=='LOGAN CREEK 1102',
                             'LOGAN CREEK 2102', circuit.name),
         circuit.name=ifelse(circuit.name=='MC MULLIN SUB 1106',
                             'MC MULLIN 1106', circuit.name),
         circuit.name=ifelse(circuit.name=='MCMULLIN 1104',
                             'MC MULLIN 1104', circuit.name),
         circuit.name=gsub('MCKEE', 'MC KEE', circuit.name),
         circuit.name=ifelse(circuit.name=='MISSION 1116',
                             'MISSION (X) 1116', circuit.name),
         circuit.name=ifelse(circuit.name=='MIWUK SUB 1701',
                             'MIWUK 1701', circuit.name),
         circuit.name=ifelse(circuit.name=='MT. EDEN 1101',
                             'MT EDEN 1101', circuit.name),
         circuit.name=ifelse(circuit.name=='NORTH TOWER 2201',
                             'NORTH TOWER 2101', circuit.name),
         circuit.name=ifelse(circuit.name=='OAKLAND X 401',
                             'OAKLAND X 0401', circuit.name),
         circuit.name=ifelse(circuit.name=='ORLAND STATION B 1103',
                             'ORLAND B 1103', circuit.name),
         circuit.name=ifelse(circuit.name=='PEASE 1102?',
                             'PEASE 1102', circuit.name),
         circuit.name=gsub('PEORIA FLAT ', 'PEORIA ', circuit.name),
         circuit.name=gsub('PIT NO[.]', 'PIT NO', circuit.name),
         circuit.name=ifelse(circuit.name=='POSO MOUNTAIN 2101/KERN OIL 1104',
                             'POSO MOUNTAIN 2101', circuit.name),
         circuit.name=ifelse(circuit.name=='R1126',
                             'RICHMOND R 1126', circuit.name),
         circuit.name=ifelse(circuit.name=='K1102',
                             'OAKLAND K 1102', circuit.name),
         circuit.name=ifelse(circuit.name=='K 1104',
                             'OAKLAND K 1104', circuit.name),
         circuit.name=ifelse(circuit.name=='RACETRACK SUB 1704',
                             'RACETRACK 1704', circuit.name),
         circuit.name=ifelse(circuit.name=='RICHMOND 1127',
                             'RICHMOND R 1127', circuit.name),
         circuit.name=ifelse(circuit.name=='RICHMOND 1129',
                             'RICHMOND R 1129', circuit.name),
         circuit.name=ifelse(circuit.name=='SAN JOAQUIN PH #3 1103',
                             'SAN JOAQUIN #3 1103', circuit.name),
         circuit.name=ifelse(circuit.name=='SAN JOAQUIN POWER HOUSE NO 2 1103',
                             'SAN JOAQUIN #2 1103', circuit.name),
         circuit.name=ifelse(circuit.name=='SAN JOSE SUB B 1112',
                             'SAN JOSE B 1112', circuit.name),
         circuit.name=ifelse(circuit.name=='SAN LEANDRO SUB',
                             'SAN LEANDRO U 1108', circuit.name),
         circuit.name=ifelse(circuit.name=='SANTA ROSA 1102',
                             'SANTA ROSA A 1102', circuit.name),
         circuit.name=ifelse(circuit.name=='SMYRNA 1103',
                             'SMYRNA 1106', circuit.name),
         circuit.name=ifelse(circuit.name=='SPENCE 1122',
                             'SPENCE 1102', circuit.name),
         circuit.name=ifelse(circuit.name=='SPENCE 1123',
                             'SPENCE 1103', circuit.name),
         circuit.name=ifelse(circuit.name=='STATION E EUREKA 1104',
                             'EUREKA E 1104', circuit.name),
         circuit.name=ifelse(circuit.name=='STILLWATER STATION 1101',
                             'STILLWATER 1101', circuit.name),
         circuit.name=ifelse(circuit.name=='VALLEJO STATION B 0413',
                             'VALLEJO B 0413', circuit.name),
         circuit.name=ifelse(circuit.name=='VALLEJO STATION B 1101',
                             'VALLEJO B 1101', circuit.name),
         circuit.name=ifelse(circuit.name=='VALLEY HOME 1701',
                             'VALLEY HOME 1703', circuit.name))

# Collapse for multiple ignitions
out<-out %>% 
  group_by(circuit.name,date,fpi) %>% 
  summarise(is_ignition=sum(is_ignition),
            is_epss_ignition=sum(is_epss_ignition),
            acreage=sum(acreage),
            is_veg_ignition=sum(is_veg_ignition),
            is_equip_ignition=sum(is_equip_ignition),
            is_hftd_ignition=sum(is_hftd_ignition))

unique(out$circuit.name)[!(unique(out$circuit.name) %in%
                             unique(df_treatment$circuit.name))]

# Merge in
df_treatment <- left_join(df_treatment, out, by=c('circuit.name'='circuit.name',
                                                  'date'='date'))

# Edit NAs
df_treatment <- df_treatment %>%
    mutate(is_ignition=ifelse(is.na(is_ignition), 0, is_ignition),
           is_veg_ignition=ifelse(is.na(is_veg_ignition), 0, is_veg_ignition),
           is_equip_ignition=ifelse(is.na(is_equip_ignition), 0, is_equip_ignition),
           is_hftd_ignition=ifelse(is.na(is_hftd_ignition), 0, is_hftd_ignition))

#######################################
# Adjust treatment data to cumulative #
#######################################

# # Drop some vars for memory management
# df_treatment <- df_treatment %>% 
#   select(-(`Transformer Climate Zone_R`:`Transformer Climate Zone_X`),
#          -(`Support Structure Material_ALM`:`Support Structure Material_WOST`),
#          -(`PriOH Conductor Construction Type_1.0`:`OH Conductor Material Proportion_Null`),
#          -is_dup, -count)
# gc()

# Write function
adjustCumulativeValues <- function(input_data, input_var) {
  
  # Variables to grab
  grab_vars <- c('circuit.name', 'year', input_var)
  
  # Find ending values in 2018-2021 to adjust starting values 2019-2022
  tmp <- input_data %>% 
    select(-conductor_year) %>% 
    # select(-conductor_year,
    #        -evm_progress, -evm_progress_intp) %>% 
    select(matches(paste(c(grab_vars, 'date'), collapse='|'))) %>% 
    filter(year==2018 | year==2019 | year==2020 | year==2021 | year==2022) %>% 
    filter(month(date)==12 & day(date)==31) %>% 
    arrange(circuit.name, year) %>% 
    group_by(circuit.name) %>% 
    mutate(across(input_var, ~ cumsum(.))) %>% 
    select(matches(paste(grab_vars, collapse='|'))) %>% 
    mutate(year=year+1)
  names(tmp)[3] <- 'adj'
  
  input_data <- input_data %>% 
    select(-conductor_year) %>% 
    select(matches(paste(c(grab_vars, 'date'), collapse='|'))) %>% 
    left_join(tmp) %>% 
    mutate(adj=ifelse(is.na(adj), 0, adj))
  
  # Adjust
  input_data[,input_var] <- input_data[,input_var] + input_data$adj
  
  input_data <- input_data %>% 
    select(matches(paste(c('circuit.name', 'date', input_var), collapse='|')))
  
  # Export
  return(input_data)
  
}

# Loop throgh each var and adjust
pos1 <- which(names(df_treatment)=='evm_hftd-2')
pos2 <- which(names(df_treatment)=='underground_non-hftd')
adj_vars <- names(df_treatment)[pos1:pos2]

for(i in 1:length(adj_vars)) {
  if(i==1) {
    out<-adjustCumulativeValues(df_treatment, adj_vars[i])
  } else {
    tmp<-adjustCumulativeValues(df_treatment, adj_vars[i])
    out<-left_join(out, tmp)
    print(i)
  }
}

# Incorporate back into dataset
df_treatment <- df_treatment[,-(pos1:pos2)]

# Drop some vars
df_treatment <- df_treatment %>% 
  select(-evm_progress, -evm_progress_intp,
         -hard_progress, -hard_progress_intp); gc()

df_treatment <- left_join(df_treatment, out)
rm(out, tmp); gc()

# Calculate totals
df_treatment <- df_treatment %>% 
  mutate(evm_hftd         = `evm_hftd-2` + `evm_hftd-3`,
         evm_units        = evm_hftd + `evm_non-hftd`,
         covered_hftd     = `covered_conductor_hftd-2` + `covered_conductor_hftd-3`,
         covered_units    = covered_hftd + `covered_conductor_non-hftd`,
         poles_hftd       = `poles_replaced_hftd-2` + `poles_replaced_hftd-3`,
         poles_units      = poles_hftd + `poles_replaced_non-hftd`,
         underground_hftd = `underground_hftd-2` + `underground_hftd-3`,
         underground_units = underground_hftd + `underground_non-hftd`,
         # Sectionalizers
         sectionalizers_hftd  = `sectionalization_devices_hftd-2` + `sectionalization_devices_hftd-3`,
         sectionalizers_units = sectionalizers_hftd + `sectionalization_devices_non-hftd`)

# Calculate as shares
df_treatment <- df_treatment %>% 
  mutate(evm_hftd_share         =evm_hftd,
         covered_hftd_share     =covered_hftd,
         poles_hftd_share       = poles_hftd,
         underground_hftd_share = underground_hftd) %>% 
  mutate(across(c('evm_hftd_share', 'covered_hftd_share',
                  'poles_hftd_share', 'underground_hftd_share'),
                ~ . / (tier.2.oh.miles + tier.3.oh.miles))) %>% 
  mutate(evm_share         =evm_units,
         covered_share     =covered_units,
         poles_share       = poles_units,
         underground_share = underground_units) %>% 
  mutate(across(c('evm_share', 'covered_share',
                  'poles_share', 'underground_share'),
                ~ . / (total.ug.miles + total.oh.miles)))


# Export
df_treatment <- df_treatment %>% 
  select(-(`overhead_inspections_hftd-2`:`overhead_inspections_non-hftd`)) %>% 
  select(-(`evm_other-hftd`:`sectionalization_devices_other-hftd`))
gc()
#save(df_treatment, file='./intermediate_24/regression_dataset_full_082024.RData')


################################################################################
# Clean up dataset 
# 
################################################################################

load(file='./intermediate_24/regression_dataset_full_082024.RData')

# Ensure treatment activity doesn't start in Jan 2018, but later in the fall
df_treatment <- df_treatment %>%
  mutate(across(names(df_treatment)[grepl('evm', names(df_treatment))],
                ~ ifelse(date<'2018-09-17', 0, .))) %>%
  mutate(across(names(df_treatment)[grepl('covered', names(df_treatment))],
                ~ ifelse(date<'2018-12-14', 0, .))) %>%
  mutate(across(names(df_treatment)[grepl('poles', names(df_treatment))],
                ~ ifelse(date<'2018-12-14', 0, .))) %>%
  mutate(across(names(df_treatment)[grepl('underground', names(df_treatment))],
                ~ ifelse(date<'2018-12-14', 0, .)))

# Update
df <- df_treatment
rm(df_treatment); gc()


###############################################
# Change raw units to more manageable numbers #
###############################################

df<-df %>%
  mutate(evm_units=evm_units/100,
         covered_units=covered_units/100,
         poles_units=poles_units/1000,
         underground_units=underground_units/100,
         
         evm_hftd=evm_hftd/100,
         covered_hftd=covered_hftd/100,
         poles_hftd=poles_hftd/1000,
         underground_hftd=underground_hftd/100,
         
         total.ug.miles=total.ug.miles/100,
         total.oh.miles=total.oh.miles/100,
         non.hftd.oh.miles=non.hftd.oh.miles/100,
         tier.2.oh.miles=tier.2.oh.miles/100,
         tier.3.oh.miles=tier.3.oh.miles/100,
         conductor_age=conductor_age/10,
         elevation=elevation/1000)

###########################$
# Additional data cleaning #
############################

# # Exclude PSPS problem children
# df <- df %>%
#   filter(!(is_ignition>=1 & is_psps==1)) 

# Ensure EPSS ignitions are accounted for
df <- df %>% 
  mutate(is_epss_ignition = ifelse(is.na(is_epss_ignition), 0, is_epss_ignition),
         is_epss = ifelse(is_epss_ignition==1, 1, is_epss))

# Exclude EPSS when its known ignition was not caused during EPSS
df <- df %>% 
  mutate(is_epss = ifelse(is_ignition>=1 & is_epss_ignition==0 & is_epss==1,
                          0, is_epss))

# HFTD portion
df <- df %>%
  mutate(hftd_share = (1-non.hftd.oh.miles/total.oh.miles))

# Calculate ignitions per mile
df <- df %>%
  # Ignitions per mi
  mutate(is_ignition_100mi = is_ignition/((total.oh.miles+total.ug.miles)))

# Add veg + equip ignition
df$is_vegequip_ignition <- ifelse(df$is_veg_ignition==1 | df$is_equip_ignition==1,
                                  1, 0)
# Grab calfire regions
# load('./intermediate/regression_dataset_clean_full_090123.Rdata')
# x <- df %>%
#   filter(date=='2020-12-30') %>%
#   filter(hftd_share>0.01) %>%
#   select(circuit.name, region)
# save(x, file='./intermediate_24/int_circuit_CALFIRE.RData')
load(file='./intermediate_24/int_circuit_CALFIRE.RData')
df<-left_join(df, x)


#### Edit wind direction
df <- df %>%
  mutate(is_wind_north = ifelse((th>=315 | th<45), 1, 0),
         is_wind_east  = ifelse((th>=45 & th<135), 1, 0),
         is_wind_south = ifelse((th>=135 & th <225), 1, 0))

# Save
save(df, file='./intermediate_24/regression_dataset_clean_full_082024.RData')
