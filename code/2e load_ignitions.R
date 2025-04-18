################################################################################
## File: Load ignitions
## Purpose: Bring in ignition data 
################################################################################

# Load ignition tracker from 2023 data request
x <- read_excel(path="./data/PGE/2023 WMP/Data Requests/WMP-Discovery2023_DR_SPD_004-Q001Atch01.xlsx",
                skip = 1)

# Clean up
df <- x
names(df) <- tolower(make.names(names(df)))
names(df)[1] <- 'index'
names(df)[3] <- 'date'
names(df)[4] <- 'time'

# Drop some columns
df <- df %>% 
  select(-was.there.an.outage, -date...21, -time...22,
         -...2)

# Distribution of tx vs distr ignitions
tmp <- df %>% 
  mutate(tx=ifelse(voltage>=60E3, 'Tx', 'Dist.')) %>% 
  group_by(tx) %>% 
  summarise(count=n())

# Filter to distribution
df <- df %>% 
  filter(voltage<60E3)

# Edit dates and times
df$date <- ymd(df$date)
df$time <- ymd_hms(df$time)
df$time <- hms(paste(hour(df$time), minute(df$time), 
                     second(df$time), sep=':'))
df$year<-year(df$date)
df$month<-month(df$date)
df$day<-day(df$date)

# Numeric index
df$index <- gsub('SAP', '', df$index)
df$index <- gsub('MIT', '', df$index)
df$index <- gsub('MIA', '', df$index)
df$index <- gsub('FAS', '', df$index)
df$index <- gsub('EIR', '', df$index)
df$index <- gsub('N', '', df$index)
df$index <- as.numeric(df$index)

################################################################################
# Load 2023 ignitions from CPUC
x <- read_excel('./data/Reportable ignitions/2023_PGE Fire Incident Data Collection Report.xlsx',
                       skip=1)
ign_2023 <- x

# Names
names(ign_2023) <- tolower(make.names(names(ign_2023)))
names(ign_2023)[1] <- 'index'
names(ign_2023)[3] <- 'date'
names(ign_2023)[4] <- 'time'

# Drop some columns
ign_2023 <- ign_2023 %>% 
  select(-was.there.an.outage, -date...19, -time...20,
         -...2, -material.at.origin...comments, -notes)

# Distribution of tx vs distr ignitions
tmp2 <- ign_2023 %>% 
  mutate(tx=ifelse(voltage>=60E3, 'Tx', 'Dist.')) %>% 
  group_by(tx) %>% 
  summarise(count2=n())

# Total distr vs. tx
tx <- left_join(tmp, tmp2) %>% 
  mutate(total=count+count2)

# Filter to distribution
ign_2023 <- ign_2023 %>% 
  filter(voltage<60E3)

# Edit dates and times
ign_2023$date <- ymd(ign_2023$date)
ign_2023$time <- ymd_hms(ign_2023$time)
ign_2023$time <- hms(paste(hour(ign_2023$time), minute(ign_2023$time), 
                     second(ign_2023$time), sep=':'))
ign_2023$year<-year(ign_2023$date)
ign_2023$month<-month(ign_2023$date)
ign_2023$day<-day(ign_2023$date)

# Numeric index
ign_2023$index <- gsub('N', '', ign_2023$index)
ign_2023$index <- as.numeric(ign_2023$index)

# Combine with other data
df <- bind_rows(df, ign_2023)

#############################################################
# Bring in other ignition tracker that has circuit names
# 2014-2020
#############################################################
y <- read_xlsx(path="./data/PGE/2021 WMP/2394.02 Recur13_2021_Ignition_Tracker_20210601_Redacted.xlsm",
               sheet = '2014-2020 CPUC Reportable')
# Date and time
y$date <- ymd(y$`Create Date`)
y$time <- ymd_hms(y$`Create Time`)
y$time <- hms(paste(hour(y$time), minute(y$time), 
                     second(y$time), sep=':'))
y$year<-year(y$date)
y$month<-month(y$date)
y$day<-day(y$date)

# Drop non-PG&E
table(y$`Ignition Source`)
y<-y %>% filter(`Ignition Source`!='Not PG&E')

# Filter to distribution
y<-y %>% filter(`Asset Family - Cause`=='D')
table(y$`Nominal Voltage`)

# Drop missing circuits
y<-y %>% filter(!(is.na(Circuit)))

# Grab relevant vars for match
y <- y %>% 
  select(`Index No#`, Circuit, date, year)

# Clean up index
y$index <- gsub('MIA', '', y$`Index No#`)
y$index <- gsub('EIR', '', y$index)
y$index <- as.numeric(y$index)
y <- y %>% 
  select(-`Index No#`)

# Clean up circuit name
y <- y %>% 
  mutate(circuit.name=gsub('-', ' ', Circuit)) %>% 
  select(-Circuit) %>% 
  unique()

# Merge in 
df <- left_join(df, y)

# Arrange
df <- df %>% 
  arrange(date)

# Create numeric fire size variable from categorical -- use midpoint
df <- df %>% 
  mutate(fire.size_numeric = ifelse(size=='Less Than .25 Acres', 0.25, NA),
         fire.size_numeric = ifelse(size=='Less than three(3) meters of linear travel', 0.01, fire.size_numeric),
         fire.size_numeric = ifelse(size=='Less than three {3} meters of linear travel', 0.01, fire.size_numeric),
         fire.size_numeric = ifelse(size=='Structure Only', 0.01, fire.size_numeric),
         fire.size_numeric = ifelse(size=='Greater than 5,000 Acres', 5000, fire.size_numeric),
         fire.size_numeric = ifelse(size=='0.26-9.99 Acres', 5.125, fire.size_numeric),
         fire.size_numeric = ifelse(size=='.26 - 9.99 Acres', 5.125, fire.size_numeric),
         fire.size_numeric = ifelse(size=='1,000-4,999 Acres', 2999.5, fire.size_numeric),
         fire.size_numeric = ifelse(size=='10-99 Acres', 54.5, fire.size_numeric),
         fire.size_numeric = ifelse(size=='10 - 99 Acres', 54.5, fire.size_numeric),
         fire.size_numeric = ifelse(size=='100-299 Acres', 199.5, fire.size_numeric),
         fire.size_numeric = ifelse(size=='100 - 299 Acres', 199.5, fire.size_numeric),
         fire.size_numeric = ifelse(size=='1000 - 4999 Acres', 2999.5, fire.size_numeric),
         fire.size_numeric = ifelse(size=='300-999 Acres', 649.5, fire.size_numeric))

# Add implied acreage if missing
df <- df %>% 
  mutate(acreage=ifelse(is.na(acreage), fire.size_numeric, acreage))

################################################################################
# Try matching on facility ID
################################################################################

# Load spatial data on circuits
load(file='./intermediate/PGE_circuits.RData')
lines_OH <- lines_OH %>% 
  as.data.frame() %>% 
  select(circuit, CIRCUITID, CONVCIRCUITID)

# Facility ID
df$facility <- as.numeric(df$facility.identification)

#------------------------------------------------------
# Write function to find matching circuit based on ID
findCircuit <- function(ign, lines_OH) {
  
  # Find matches
  tmp_lines <- lines_OH %>% 
    filter(CIRCUITID%in%ign$facility)
  tmp_lines <- tmp_lines %>% 
    filter(!is.na(circuit)) %>% 
    unique()
  
  if(nrow(tmp_lines)>0) {
    tmp<-crossing(CIRCUITID=unique(tmp_lines$CIRCUITID),
                  circuit.facility=unique(tmp_lines$circuit),
                  facility=ign$facility[[1]])
    return(tmp)
  }
  
}

# Only circuits with a facility ID
in_circ <- df %>% 
  filter(!is.na(facility))

# Loop through
results<-data.frame()
for(i in 1:nrow(in_circ)) {
  tmp<-findCircuit(in_circ[i,], lines_OH)
  tmp$i <- i
  results<-bind_rows(results,tmp)
  rm(tmp)
}

# Save condensed version
results <- results %>% 
  select(circuit.facility, facility) %>% 
  unique() %>% 
  na.omit() %>% 
  arrange(circuit.facility)

# bring in facility matched circuits
df <- left_join(df, results)

################################################################################
# Try to match on coordinates (lat/long)
################################################################################

# Create spatial data
df_spatial <- df %>%
  st_as_sf(coords = c('longitude', 'latitude'),
           crs=st_crs(4326))

# Create id
df_spatial$id<-1:nrow(df_spatial)

# Run from scratch
if (SWITCH_NEW_LOAD) {
  
  # Load spatial data
  load(file='./intermediate/PGE_circuits.RData')  
    
  # Update projection
  df_spatial <- st_transform(df_spatial, st_crs(lines_OH))
  
  # Write function for lat/long matching
  matchLatLong <- function(check, lines) {
    
    # Create a buffer around point (1 square kilometer)
    tmp_buffer <- st_buffer(check, 5.7E2)
    
    # Subset to circuits within buffer
    tmp_lines <- st_crop(lines, tmp_buffer)
    
    # Calculate distance
    tmp_lines$dist <- as.numeric(st_distance(tmp_lines,check))
    
    # Find nearest
    tmp_lines <- tmp_lines %>%
      as.data.frame() %>%
      filter(dist==min(dist)) %>%
      select(circuit, CIRCUITID, CONVCIRCUITID, dist)
    out<-tmp_lines
    try(out$id<-check$id, silent = T) # Skip and catch error if no matches
    return(out)
  }
  
  # Write parallel wrapper function
  ParallelLatLong <- function(i) {
    
    # Loop
    foreach(i=1:nrow(df_spatial), .combine = rbind,
            .export=c('df_spatial', 'lines_OH', 'matchLatLong'),
            .packages=c('dplyr', 'sf')) %dopar% {
              
              matched <- matchLatLong(check=df_spatial[i,], lines=lines_OH)
              
            }
    
  }
  
  # Cores for parallel processing
  no_cores <- detectCores()
  registerDoParallel(5) # pick your number of cores here for parallel processing
  
  # Run parallel processing
  system.time(df_match<-ParallelLatLong())
  stopImplicitCluster()
  
  save(df_match, file='./intermediate/Intermediate Ignitions/matched_ignitions.RData')
} else {
  load('./intermediate/Intermediate Ignitions/matched_ignitions.RData')
}

# Merge in with data
df_match <- df_match %>% 
  rename(circuit.spatial = circuit) %>% 
  select(circuit.spatial, dist, id)
tmp<-df_spatial %>% 
  as.data.frame() %>% 
  select(index,date,time,id)
tmp<-left_join(tmp,unique(df_match))
df <- left_join(df, tmp)

# Fix two that had circuit segment with NA circuit name
df <- df %>% 
  mutate(circuit.spatial=ifelse(id==3119 & facility==103971980,
                                'PARADISE 1105', circuit.spatial),
         circuit.spatial=ifelse(id==3576 & facility==108120519,
                                'PARADISE 2107', circuit.spatial))

# Note-- one ignition is not near any PG&E distribution lines

# Drop missing
df <- df %>% 
  filter(!(is.na(circuit.name) & is.na(circuit.facility) & 
             is.na(circuit.spatial)))


################################################################################
# Identify 2022 ignitions that occurred during EPSS
################################################################################
tmp <- data.frame(index=c(20220642,
                       20220654,
                       20220657,
                       20220659,
                       20220815,
                       20220816,
                       20220848,
                       20220918,
                       20220980,
                       20220981,
                       20220982,
                       20220991,
                       20221014,
                       20221070,
                       20221071,
                       20221096,
                       20221163,
                       20221335,
                       20221347,
                       20221377,
                       20221403,
                       20221462,
                       20221514,
                       20221555,
                       20221577,
                       20221590,
                       20221614,
                       20221767,
                       20221826,
                       20221842,
                       20221862),
                  create_data=c('20-May',
                                '23-May',
                                '23-May',
                                '23-May',
                                '10-Jun',
                                '10-Jun',
                                '14-Jun',
                                '21-Jun',
                                '24-Jun',
                                '25-Jun',
                                '25-Jun',
                                '26-Jun',
                                '28-Jun',
                                '05-Jul',
                                '05-Jul',
                                '07-Jul',
                                '15-Jul',
                                '05-Aug',
                                '09-Aug',
                                '14-Aug',
                                '17-Aug',
                                '26-Aug',
                                '04-Sep',
                                '07-Sep',
                                '09-Sep',
                                '10-Sep',
                                '12-Sep',
                                '04-Oct',
                                '17-Oct',
                                '20-Oct',
                                '26-Oct'),
                  circuit.epss=c('PIKE CITY 1101',
                                 'NORTH DUBLIN 2103',
                                 'SILVERADO 2102',
                                 'SANTA YNEZ 1104',
                                 'HOPLAND 1101',
                                 'MORGAN HILL 2105',
                                 'PAUL SWEET 2106',
                                 'EMERALD LAKE 0401',
                                 'PHILO 1101',
                                 'LOS GATOS 1106',
                                 'CAMP EVERS 2106',
                                 'VACAVILLE 1111',
                                 'ATASCADERO 1103',
                                 'ROB ROY 2104',
                                 'KONOCTI 1102',
                                 'PHILO 1101',
                                 'CHALLENGE 1101',
                                 'TEMPLETON 2110',
                                 'DEL MONTE 2104',
                                 'AUBERRY 1102',
                                 'ROB ROY 2105',
                                 'HOOPA 1101',
                                 'EL DORADO PH 2101',
                                 'SARATOGA 1106',
                                 'HIGGINS 1104',
                                 'MARIPOSA 2101',
                                 'WEIMAR 1101',
                                 'MOUNTAIN QUARRIES 2101',
                                 'RIO DELL 1102',
                                 'MENLO 1103',
                                 'ANNAPOLIS 1101'))



# Merge in 2022 EPSS ignitions
df <- left_join(df, tmp)

################################################################################
################################################################################
# Identify 2021 ignitions that occurred during EPSS
################################################################################

# Identify 2021 EPSS ignitions
tmp <- data.frame(index=paste0(2021, c(1762,
                                       1648,
                                       1309,
                                       1258,
                                       1220,
                                       1190,
                                       1154)),
                  date=paste0('2021-', c('10-11',
                                           '9-30',
                                           '8-27',
                                           '8-10',
                                           '8-8',
                                           '8-5',
                                           '7-31')))
tmp$date <- ymd(tmp$date)
tmp$epss_2021 <- 1
tmp$index <- as.numeric(tmp$index)

# Merge in 2021
df <- left_join(df, tmp)

################################################################################
# Identify 2023 ignitions that occurred during EPSS
################################################################################

# Load file with ignitions & circuit names
tmp <- read_excel(path='./data/PGE/Fast-trip proceeding/PGE - EPSS Monthly Report - 20240116 xls.xlsx',
                  sheet='YTD Ignition Data')

# Select relevant vars
tmp <- tmp %>% 
  select(circuit, fixed_index_no, ignition_start_create_timestamp) %>% 
  filter(circuit!='-') %>% 
  mutate(index=gsub('N', '', fixed_index_no)) %>% 
  select(-fixed_index_no) %>% 
  mutate(month=month(ignition_start_create_timestamp),
         year=year(ignition_start_create_timestamp),
         day=day(ignition_start_create_timestamp)) %>% 
  select(-ignition_start_create_timestamp) %>% 
  mutate(index=as.numeric(index),
         is_epss_2023 = 1)

# Merge in 2023
df <- left_join(df, tmp)

# Identify all 2021 and 2022 EPSS ignitions
df <- df %>% 
  mutate(is_epss_ignition = ifelse(!is.na(circuit.epss) | !is.na(epss_2021) |
                                     !is.na(is_epss_2023),
                                   1, 0))

################################################################################
# Classify Ignitions
################################################################################

save(df, file='./intermediate/Intermediate Ignitions/ignition_tracker_2014_2023.RData')

# All ignitions
out <- df %>% 
  mutate(circuit=ifelse(is.na(circuit.name), circuit.facility, circuit.name),
         circuit=ifelse(is.na(circuit), circuit.spatial, circuit),
         circuit=ifelse(is.na(circuit), circuit.spatial2, circuit)) %>% 
  group_by(circuit, date) %>% 
  summarise(is_ignition=n(),
            is_epss_ignition=sum(is_epss_ignition),
            acreage=sum(acreage))

# Vegetation ignitions
veg <- df %>% 
  mutate(circuit=ifelse(is.na(circuit.name), circuit.facility, circuit.name),
         circuit=ifelse(is.na(circuit), circuit.spatial, circuit),
         circuit=ifelse(is.na(circuit), circuit.spatial2, circuit)) %>% 
  mutate(is_veg_ignition=ifelse(contact.from...object=='Vegetation', 1, 0)) %>% 
  group_by(circuit, date) %>% 
  summarise(is_veg_ignition=sum(is_veg_ignition))

out <- left_join(out, veg)

# Equipment ignitions
equip <- df %>% 
  mutate(circuit=ifelse(is.na(circuit.name), circuit.facility, circuit.name),
         circuit=ifelse(is.na(circuit), circuit.spatial, circuit),
         circuit=ifelse(is.na(circuit), circuit.spatial2, circuit)) %>% 
  mutate(is_equip_ignition=ifelse(suspected.initiating.event=='Equipment/Facility Failure',
                                  1, 0)) %>% 
  group_by(circuit, date) %>% 
  summarise(is_equip_ignition=sum(is_equip_ignition))

out <- left_join(out, equip)

# HFTD ignitions
hftd <- df %>% 
  mutate(circuit=ifelse(is.na(circuit.name), circuit.facility, circuit.name),
         circuit=ifelse(is.na(circuit), circuit.spatial, circuit),
         circuit=ifelse(is.na(circuit), circuit.spatial2, circuit))  %>% 
  mutate(is_hftd_ignition = ifelse(hftd!='Non-HFTD', 1, 0)) %>% 
  group_by(circuit, date) %>% 
  summarise(is_hftd_ignition=sum(is_hftd_ignition)) %>% 
  mutate(is_hftd_ignition = ifelse(year(date)==2023, NA, is_hftd_ignition))

out <- left_join(out, hftd)

# Bring in fire potential index
fpi <- df %>% 
  filter(!is.na(fpi)) %>% 
  mutate(circuit=ifelse(is.na(circuit.name), circuit.facility, circuit.name),
         circuit=ifelse(is.na(circuit), circuit.spatial, circuit)) %>% 
  select(circuit, date, fpi) %>% 
  unique()

out <- left_join(out, fpi)

### Save for export
save(out, file='./intermediate/ignition_tracker.RData')
