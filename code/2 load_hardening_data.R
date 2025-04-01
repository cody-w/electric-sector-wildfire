################################################################################
## File:    Load grid hardening, veg data
## Purpose: Bring in hardening data 
################################################################################


# Load data on grid hardening
df <- read_excel(path='./data/PGE/2021 WMP/Data Requests/CalAdvocates_035 (2021WMP-01)/WildfireMitigationPlans_DR_CalAdvocates_035-Q04-Atch01.xlsx',
                 sheet = '4_Distribution', skip=2)


# Column names
names(df) <- tolower(make.names(names(df)))

#############################################
# Edit data entry error in PG&E spreadsheet #
#############################################

df <- df %>% 
  # Total veg mgmt miles
  mutate(total_veg_mgmt=miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020 +
           miles.of.evm.work.in.hftd.tier.2.in.2020..+
           miles.of.evm.work.in.hftd.tier.3.in.2020.,
         
         # CHeck if total exceeds circuit miles
         greater_than_length = ifelse(total_veg_mgmt>total.circuit.miles, 1, 0),
         
         # Check which HFTD segments equal total circuit length
         is_nonhftd_evm = total.circuit.miles==miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020,
         is_hftd2_evm   = total.circuit.miles==miles.of.evm.work.in.hftd.tier.2.in.2020..,
         is_hftd3_evm   = total.circuit.miles==miles.of.evm.work.in.hftd.tier.3.in.2020.
         )

df <- df %>% 
  mutate(miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020 = 
           ifelse(greater_than_length==1 & is_nonhftd_evm, 0, miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020),
         miles.of.evm.work.in.hftd.tier.2.in.2020..=ifelse(greater_than_length==1 & is_hftd2_evm, 0, 
                                                           miles.of.evm.work.in.hftd.tier.2.in.2020..),
         miles.of.evm.work.in.hftd.tier.3.in.2020.=ifelse(greater_than_length==1 & is_hftd3_evm, 0,
                                                          miles.of.evm.work.in.hftd.tier.3.in.2020.)) %>% 
  select(-(total_veg_mgmt:is_hftd3_evm))

##########################
# Reshape to long format #
##########################

# Reshape
df <- df %>% 
  gather(key='label', value='quantity', 
         -(circuit.name:circuit.maifi..momentary.average.interruption.frequency.index.)) %>% 
  rename(circuit.saidi = circuit.saidi..system.average.interruption.duration.index.,
         circuit.saifi = circuit.saifi..system.average.interruption.frequency.index.,
         circuit.maifi = circuit.maifi..momentary.average.interruption.frequency.index.) %>% 
  mutate(year=ifelse(grepl('2018', label), 2018, 
                     ifelse(grepl('2019', label), 2019,
                            ifelse(grepl('2020', label), 2020, NA)))) %>% 
  mutate(HFTD = ifelse(grepl('non.hftd', label), 'Non-HFTD', 
                       ifelse(grepl('hftd.tier.2', label), 'HFTD-2',
                              ifelse(grepl('hftd.tier.3', label), 'HFTD-3', 
                                     ifelse(grepl('non.high.fire', label), 'Non-HFTD', NA))))) %>% 
  mutate(prevention_type = ifelse(grepl('evm', label), 'EVM',
                                  ifelse(grepl('routine.vegetation', label), 'routine_veg',
                                         ifelse(grepl('covered.conductor', label), 'covered_conductor',
                                                ifelse(grepl('poles.replaced', label), 'poles_replaced',
                                                       ifelse(grepl('underground', label), 'underground',
                                                              # multiple types of lidar be warned
                                                              ifelse(grepl('lidar', label), 'lidar',
                                                                     ifelse(grepl('detailed.overhead', label), 'overhead_inspections',
                                                                            ifelse(grepl('sectionalization', label), 'sectionalization_devices', 
                                                                                   NA))))))))) %>% 
  mutate(units = ifelse(grepl('miles', label), 'miles',
                        ifelse(grepl('number', label), 'number', NA)))

# Look into risk level 
df <- df %>% 
  mutate(wildfire.risk.level=as.numeric(wildfire.risk.level),
         wildfire.risk.level=ifelse(wildfire.risk.level<0, 0, wildfire.risk.level),
         wildfire.risk.level=ifelse(is.na(wildfire.risk.level), 0, wildfire.risk.level))

# Sort data
df <- df %>% 
  arrange(circuit.name, prevention_type, year, HFTD)

# Convert to zero if missing
df <- df %>% 
  mutate(quantity=ifelse(is.na(quantity), 0, quantity),
         tier.2.oh.miles=ifelse(is.na(tier.2.oh.miles), 0, tier.2.oh.miles),
         tier.3.oh.miles=ifelse(is.na(tier.3.oh.miles), 0, tier.3.oh.miles))

#######################################################
# Load 2019 EVM that is not included in other dataset #
#######################################################

# Spatially allocating to HFTD can take awhile to compute
if (SWITCH_NEW_LOAD) {
  # 2019 EVM
  evm_2019 <- st_read('./data/PGE/2020 WMP/Supplemental Data Request/Final Sent/SDR Attachments/3.4 Table 7 Veg')
  
  # Transform
  evm_2019 <- st_transform(evm_2019, crs=st_crs(crs_number))
  
  # Check lines miles treated - PG&E documents indicate 2,455 miles treated 2019
  sum(st_length(evm_2019))*.621/1E3
  
  # Load HFTD areas
  load('./data/Spatial Repository/HFTD.RData')
  poly_hftd<-st_transform(poly_hftd, crs=st_crs(crs_number))
  
  # Break out EVM treatments into HFTD district
  # ** this process takes awhile to run
  int <- st_intersection(evm_2019, poly_hftd)
  tmp <- int %>% 
    mutate(segment_length=st_length(geometry)) %>% 
    as.data.frame() %>% 
    select(-geometry) %>% 
    group_by(CIRCUITNAM, HFTD) %>% 
    summarise(segment_length=sum(segment_length))
  
  # Convert to miles and reshape
  evm_2019 <- tmp %>% 
    mutate(quantity=as.numeric(segment_length)*0.621371/1E3,
           prevention_type='EVM',
           units='miles',
           year=2019,
           HFTD=ifelse(HFTD=='Tier 2', 'HFTD-2', 
                       ifelse(HFTD=='Tier 3', 'HFTD-3', NA))) %>% 
    rename(circuit.name=CIRCUITNAM) %>% 
    select(-segment_length)
  
  # Stitch together in main dataframe
  shell <- df %>% 
    select(circuit.name:circuit.maifi) %>% 
    unique()
  evm_2019<-left_join(evm_2019, shell)
  df<-bind_rows(df, evm_2019)
  
  # Save
  save(df, file='./intermediate/Intermediate Hardening Data/circuit_treatment_data_2019_2020.RData')
  
  # 2018 EVM data
  
  # Allocate missing EVM data to 2018
  evm_2018 <- evm_2019 %>% 
    mutate(year=2018)
  
  # Total EVM treatment miles in 2018 from PG&E documents
  total_2018 <- 761
  
  # Calculate proportion to scale down 
  scale_2018 <- total_2018/sum(evm_2019$quantity)
  evm_2018$quantity <- evm_2018$quantity*scale_2018
  
  # Combine
  df <- bind_rows(df, evm_2018)
  
  save(df, file='./intermediate/Intermediate Hardening Data/circuit_treatment_data_2018_2020.RData')
  
# Or load from previous run
} else {load(file='./intermediate/Intermediate Hardening Data/circuit_treatment_data_2018_2020.RData')}


###################################################################
# Expand treatment data to daily level, applying the distribution
# of hardening/veg management over the course of the year 
###################################################################

# Create daily dataset
dt_start <- ymd('2014-01-01')
dt_end   <- ymd('2020-12-31')
dt_range <- seq(dt_start,dt_end, 1)
df_treatment <- crossing(unique(df$circuit.name), dt_range)

# Bring in treatment data
df_treatment<-df_treatment %>% 
  rename(circuit.name='unique(df$circuit.name)',
         date='dt_range') %>% 
  mutate(year=year(date))

# Bring in 
hftd <- unique(df$HFTD)
ptypes <- unique(df$prevention_type)

# Loop through and create panel data
for(j in c(2,1,5,8,4,7)) {
  
  for(i in 1:length(hftd)) {
    
    cname <- paste0(ptypes[j], '_', hftd[i])
    
    # Reorganize
    tmp<-df %>% 
      select(-label) %>% 
      filter(prevention_type==ptypes[j]) %>% 
      filter(HFTD==hftd[i]) %>% 
      select(circuit.name, year, quantity)
    names(tmp)[3] <- cname
    
    # Merge 
    df_treatment<-left_join(df_treatment, tmp)
    
  }
  
}

# Set to NA if zero
names(df_treatment) <- tolower(names(df_treatment))
df_treatment <- df_treatment %>% 
  mutate_at(vars(`evm_hftd-2`:`sectionalization_devices_non-hftd`),
            function(x){ifelse(is.na(x), 0, x)})

# Bring in distribution of veg management projects weekly - 2020
df_dist2020 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                      sheet='2020', skip=6) %>% 
  select(week_ending...1, evm_miles_cum)

# Calculate shares for how EVM progresses throughout 2020
df_dist2020 <- df_dist2020 %>% 
  mutate(total=max(evm_miles_cum),
         evm_progress=evm_miles_cum/total,
         date=ymd(week_ending...1)) %>% 
  select(-week_ending...1, -evm_miles_cum, -total) 

# Repeat for 2019

# Bring in distribution of veg management projects weekly - 2019
df_dist2019 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                      sheet='2019', skip=6) %>% 
  select(week_ending...1, evm_miles_cum) %>% 
  filter(!(is.na(week_ending...1) | is.na(evm_miles_cum)))

# Calculate shares for how EVM progresses throughout 2019
df_dist2019 <- df_dist2019 %>% 
  mutate(total=max(evm_miles_cum),
         evm_progress=evm_miles_cum/total,
         date=ymd(week_ending...1)) %>% 
  select(-week_ending...1, -evm_miles_cum, -total)

# Repeat for 2018

# Bring in distribution of veg management projects weekly - 2018
df_dist2018 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                          sheet='2018', skip=7) %>% 
  select(week_ending...1, evm_miles_cum) %>% 
  filter(!(is.na(week_ending...1) | is.na(evm_miles_cum)))

# Calculate shares for how EVM progresses throughout 2019
df_dist2018 <- df_dist2018 %>% 
  mutate(total=max(evm_miles_cum),
         evm_progress=evm_miles_cum/total,
         date=ymd(week_ending...1)) %>% 
  select(-week_ending...1, -evm_miles_cum, -total)

df_dist<-bind_rows(df_dist2020,df_dist2019) %>% 
  bind_rows(df_dist2018)

# Merge in 
df_treatment <- left_join(df_treatment, df_dist)

###########################################################
# Write a function to interpolate values 
###########################################################
interpolate_progress <- function(input_data, switch_hardening=F) {
  
  # rename if running for sys hardening and not EVM
  if(switch_hardening==T){
    input_data<-input_data %>% 
      rename(evm_progress=hard_progress)
  }
  
  pvalues <- input_data %>% 
    filter(!is.na(evm_progress))
  pvalues<-pvalues$evm_progress
  
  # Container
  results <- c()
  
  # Loop through and interpolate
  for (i in 1:(length(pvalues)-1)) {
   
    pos1 <- which(input_data$evm_progress==pvalues[i])
    pos2 <- which(input_data$evm_progress==pvalues[i+1])
    
    # Subset to data to interpolate
    x <- input_data[pos1:pos2,]
    n_steps <- nrow(x)
    
    # Interpolate
    out <- approx(x=c(pvalues[i], pvalues[i+1]), y=NULL,
                  method='linear', n=n_steps)
    out <- out$y
    
    
    # Drop to not duplicate
    if (i>1){
      out<-out[-1]
    }
    
    results <- append(results,out)
    
  }
  
  # Add rows to fill out year
  add_rows <- nrow(input_data)-length(results)
  add_rows <- rep(1, add_rows)
  results  <- append(results, add_rows)
  
  return(results)
  
}

# Run the function to interpolate seasonal distribution of veg mgmt.
# activities 

# 2020
pdata <- df_treatment %>% 
  filter(year==2020) %>% 
  select(date, year, evm_progress) %>% 
  unique()
x_2020 <- interpolate_progress(pdata)

# 2019
pdata <- df_treatment %>% 
  filter(year==2019) %>% 
  select(date, year, evm_progress) %>% 
  unique()
x_2019 <- interpolate_progress(pdata)

# 2018
pdata <- df_treatment %>% 
  filter(year==2018) %>% 
  select(date, year, evm_progress) %>% 
  unique()
x_2018 <- interpolate_progress(pdata)

# Stitch together
pdata <- df_treatment %>% 
  filter(year%in%c(2018,2019,2020)) %>% 
  select(date,year) %>% 
  unique()
pdata$evm_progress_intp <- c(x_2018, x_2019, x_2020)

# Bring in EVM progress distribution to treatment data
df_treatment <- left_join(df_treatment, pdata)


###########################################################
# Repeat process for interpolating system hardening
###########################################################

# Bring in distribution of sys hardening projects weekly - 2020
df_dist2020 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                          sheet='2020', skip=6) %>% 
  select(week_ending...5, sys_hardening_cum) %>% 
  filter(!(is.na(week_ending...5) | is.na(sys_hardening_cum)))

# Calculate shares for how sys hardening progresses throughout 2020
df_dist2020 <- df_dist2020 %>% 
  mutate(total=max(sys_hardening_cum),
         hard_progress=sys_hardening_cum/total,
         date=ymd(week_ending...5)) %>% 
  select(-week_ending...5, -sys_hardening_cum, -total) 

# Repeat for 2019

# Bring in distribution of veg management projects weekly - 2019
df_dist2019 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                          sheet='2019', skip=6) %>% 
  select(week_ending...6, sys_hardening_cum) %>% 
  filter(!(is.na(week_ending...6) | is.na(sys_hardening_cum)))

# Calculate shares for how EVM progresses throughout 2019
df_dist2019 <- df_dist2019 %>% 
  mutate(total=max(sys_hardening_cum),
         hard_progress=sys_hardening_cum/total,
         date=ymd(week_ending...6)) %>% 
  select(-week_ending...6, -sys_hardening_cum, -total)

# Repeat for 2018

# Bring in distribution of veg management projects weekly - 2018
df_dist2018 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                          sheet='2018', skip=7) %>% 
  select(week_ending...5, sys_hardening_cum) %>% 
  filter(!(is.na(week_ending...5) | is.na(sys_hardening_cum)))

# Calculate shares for how EVM progresses throughout 2018
df_dist2018 <- df_dist2018 %>% 
  mutate(total=max(sys_hardening_cum),
         hard_progress=sys_hardening_cum/total,
         date=ymd(week_ending...5)) %>% 
  select(-week_ending...5, -sys_hardening_cum, -total)

df_dist<-bind_rows(df_dist2020,df_dist2019) %>% 
  bind_rows(df_dist2018)

# Merge in 
df_treatment <- left_join(df_treatment, df_dist)

# 2020
pdata <- df_treatment %>% 
  filter(year==2020) %>% 
  select(date, year, hard_progress) %>% 
  unique()
x_2020 <- interpolate_progress(pdata, switch_hardening = T)

# 2019
pdata <- df_treatment %>% 
  filter(year==2019) %>% 
  select(date, year, hard_progress) %>% 
  unique()
x_2019 <- interpolate_progress(pdata, switch_hardening = T)

# 2018
pdata <- df_treatment %>% 
  filter(year==2018) %>% 
  select(date, year, hard_progress) %>% 
  unique()
x_2018 <- interpolate_progress(pdata, switch_hardening = T)

# Stitch together
pdata <- df_treatment %>% 
  filter(year%in%c(2018,2019,2020)) %>% 
  select(date,year) %>% 
  unique()
pdata$hard_progress_intp <- c(x_2018, x_2019, x_2020)

# Bring in hardening progress distribution to treatment data
df_treatment <- left_join(df_treatment, pdata)

# Apply distribution of annual progress

# Set to 0 for prior 2018
df_treatment <- df_treatment %>% 
  mutate(evm_progress_intp=ifelse(year<2018, 0, evm_progress_intp),
         hard_progress_intp=ifelse(year<2018, 0, hard_progress_intp))

# Apply
df_treatment <- df_treatment %>% 
  mutate(across(`evm_hftd-2`:`evm_non-hftd`, ~.x * evm_progress_intp)) %>% 
  mutate(across(`covered_conductor_hftd-2`:`sectionalization_devices_non-hftd`, ~.x * hard_progress_intp))

# Export
save(df_treatment, file='./intermediate/Intermediate Hardening Data/compiled_treatment_data.RData')

################################################################################
#
# 2021 and 2022 data processing ################################################
#
################################################################################

# Load 2021 file
df_2021 <- read_excel(path='./data/PGE/2022 WMP/Data Requests/WMP-Discovery2022_DR_MGRA_002-Q16Atch01.xlsx',
                      sheet = '1_Distribution', skip=4)

# Column names
names(df_2021) <- tolower(make.names(names(df_2021)))

######################################
####### Write a function to reformat #
######################################

reshapeGridData <- function(df) {
  
  # Reshape
  df <- df %>% 
    gather(key='label', value='quantity', 
           -(circuit.name:circuit.maifi..momentary.average.interruption.frequency.index.)) %>% 
    rename(circuit.saidi = circuit.saidi..system.average.interruption.duration.index.,
           circuit.saifi = circuit.saifi..system.average.interruption.frequency.index.,
           circuit.maifi = circuit.maifi..momentary.average.interruption.frequency.index.) %>% 
    mutate(year=ifelse(grepl('2018', label), 2018, 
                       ifelse(grepl('2019', label), 2019,
                              ifelse(grepl('2020', label), 2020,
                                     ifelse(grepl('2021', label), 2021, NA))))) %>% 
    mutate(HFTD = ifelse(grepl('non.hftd', label), 'Non-HFTD', 
                         ifelse(grepl('tier.2', label), 'HFTD-2',
                                ifelse(grepl('tier.3', label), 'HFTD-3', 
                                       ifelse(grepl('non.high.fire', label), 'Non-HFTD',
                                              ifelse(grepl('other.hftd', label), 'Other-HFTD',
                                                     ifelse(grepl('total.customer', label), 'Total', NA))))))) %>% 
    arrange(circuit.name) %>% 
    mutate(prevention_type = ifelse(grepl('evm', label), 'EVM',
                                    ifelse(grepl('routine.vegetation', label), 'routine_veg',
                                           ifelse(grepl('covered.conductor', label), 'covered_conductor',
                                                  ifelse(grepl('poles.replaced', label), 'poles_replaced',
                                                         ifelse(grepl('underground', label), 'underground',
                                                                # multiple types of lidar be warned
                                                                ifelse(grepl('lidar', label), 'lidar',
                                                                       ifelse(grepl('detailed.overhead', label), 'overhead_inspections',
                                                                              ifelse(grepl('sectionalization', label), 'sectionalization_devices', 
                                                                                     ifelse(grepl('psps.events', label), 'psps_cust_minutes',
                                                                                            ifelse(grepl('epss.fast', label), 'epss_cust_minutes',
                                                                                     NA))))))))))) %>% 
    mutate(units = ifelse(grepl('miles', label), 'miles',
                          ifelse(grepl('number', label), 'number',
                                 ifelse(grepl('minutes', label), 'minutes', NA))))
  
  # Convert to zero if missing
  df <- df %>% 
    mutate(quantity=ifelse(is.na(quantity), 0, quantity),
           tier.2.oh.miles=ifelse(is.na(tier.2.oh.miles), 0, tier.2.oh.miles),
           tier.3.oh.miles=ifelse(is.na(tier.3.oh.miles), 0, tier.3.oh.miles))
  
  # Create daily dataset
  dt_start <- ymd('2021-01-01')
  dt_end   <- ymd('2021-12-31')
  dt_range <- seq(dt_start,dt_end, 1)
  df_treatment <- crossing(unique(df$circuit.name), dt_range)
  
  # Bring in treatment data
  df_treatment<-df_treatment %>% 
    rename(circuit.name='unique(df$circuit.name)',
           date='dt_range') %>% 
    mutate(year=year(date))
  
  # Bring in 
  hftd <- unique(df$HFTD)
  ptypes <- unique(df$prevention_type)
  ptypes <- ptypes[ptypes!='lidar']
  
  # Loop through and create panel data
  for(j in 1:length(ptypes)) {
    
    for(i in 1:length(hftd)) {
      
      cname <- paste0(ptypes[j], '_', hftd[i])
      
      # Reorganize
      tmp<-df %>% 
        select(-label) %>% 
        filter(prevention_type==ptypes[j]) %>% 
        filter(HFTD==hftd[i]) %>% 
        select(circuit.name, year, quantity)
      names(tmp)[3] <- cname
      
      # Merge 
      df_treatment<-left_join(df_treatment, tmp)
      
    }
    
  }
  
  # Drop NA columns
  not_all_na <- function(x) any(!is.na(x))
  df_treatment <- df_treatment %>% 
    select(where(not_all_na))
  
  # Set to NA if zero
  names(df_treatment) <- tolower(names(df_treatment))
  df_treatment <- df_treatment %>% 
    mutate_at(vars(`psps_cust_minutes_total`:`sectionalization_devices_hftd-3`),
              function(x){ifelse(is.na(x), 0, x)})
  
  
  ##################################################################
  # Bring in distribution of veg management projects weekly - 2021
  df_dist2021 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                            sheet='2021', skip=6) %>% 
    select(week_ending...1, evm_miles_cum)
  
  # Calculate shares for how EVM progresses throughout 2021
  df_dist2021 <- df_dist2021 %>% 
    mutate(total=max(evm_miles_cum),
           evm_progress=evm_miles_cum/total,
           date=ymd(week_ending...1)) %>% 
    select(-week_ending...1, -evm_miles_cum, -total) 
  
  # Merge in 
  df_treatment <- left_join(df_treatment, df_dist2021)
  
  # 2021
  pdata <- df_treatment %>% 
    filter(year==2021) %>% 
    select(date, year, evm_progress) %>% 
    unique()
  x_2021 <- interpolate_progress(pdata)
  pdata$evm_progress_intp <- x_2021
  
  # Bring in EVM progress distribution to treatment data
  df_treatment <- left_join(df_treatment, pdata)
  
  
  #########################################################
  #~~~~~~~~~~~ REPEAT PROCESS FOR SYS HARDENING ~~~~~~~~~~#
  
  # Bring in distribution of sys hardening projects weekly - 2021
  df_dist2021 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                            sheet='2021', skip=6) %>% 
    select(week_ending...5, sys_hardening_cum) %>% 
    filter(!(is.na(week_ending...5) | is.na(sys_hardening_cum)))
  
  # Calculate shares for how sys hardening progresses throughout 2021
  df_dist2021 <- df_dist2021 %>% 
    mutate(total=max(sys_hardening_cum),
           hard_progress=sys_hardening_cum/total,
           date=ymd(week_ending...5)) %>% 
    select(-week_ending...5, -sys_hardening_cum, -total) 
  
  # Merge in 
  df_treatment <- left_join(df_treatment, df_dist2021)
  
  # 2021
  pdata <- df_treatment %>% 
    filter(year==2021) %>% 
    select(date, year, hard_progress) %>% 
    unique()
  x_2021 <- interpolate_progress(pdata, switch_hardening = T)
  pdata$hard_progress_intp <- x_2021
  
  # Bring in EVM progress distribution to treatment data
  df_treatment <- left_join(df_treatment, pdata)
  
  # Apply
  df_treatment <- df_treatment %>% 
    mutate(across(`evm_non-hftd`:`evm_hftd-3`, ~.x * evm_progress_intp)) %>% 
    mutate(across(`covered_conductor_non-hftd`:`sectionalization_devices_hftd-3`, ~.x * hard_progress_intp))
  
  return(df_treatment)
  
}

# Run function on 2021 data
df_2021 <- reshapeGridData(df_2021)

################################################################################
# Convert trees worked to miles of EVM at the circuit-level 2021
################################################################################

# Load 2021 work-verified EVM miles
df_evm_2021 <- read_excel(path="./data/PGE/2022 WMP/Data Requests/WMP-Discovery2022_DR_CalAdvocates_011-Q01Atch01.xlsx")

# Select relevant variables
df_evm_2021 <- df_evm_2021 %>% 
  select(`Column 1: Circuit Name`,
         `Column 4: Forecasted Tree Work`,
         `Column 9: Miles Complete and Verified as of 12/31/21 (Audited)`)
# And adjust names
names(df_evm_2021) <- c('circuit.name', 'forecasted_tree_work',
                        'evm_miles_2021_total')

# Summarise across multiple circuit segments
df_evm_2021 <- df_evm_2021 %>% 
  group_by(circuit.name) %>% 
  summarise_all(sum) %>% 
  filter(!(is.na(circuit.name))) %>% 
  filter(circuit.name!='Total') %>% 
  filter(!is.na(forecasted_tree_work)) %>% 
  # fix circuit name discrepancy
  mutate(circuit.name = ifelse(circuit.name=='SAN JOAQUIN NO2 1103',
                               'SAN JOAQUIN #2 1103', circuit.name))

# Bring in actual trees worked 2021
x <- df_2021 %>% 
  filter(date=='2021-12-31') %>% 
  select(circuit.name, starts_with('evm')) %>% 
  select(-evm_progress_intp, -evm_progress) %>% 
  mutate(evm_total_trees=`evm_non-hftd`+`evm_other-hftd`+
           `evm_hftd-2`+`evm_hftd-3`)
df_evm_2021 <- full_join(x, df_evm_2021)

# Calculate tree density per mile
df_evm_2021 <- df_evm_2021 %>% 
  mutate(evm_density=evm_total_trees/evm_miles_2021_total) %>% 
  # reclassify couple instances of zero
  mutate(evm_density=ifelse(evm_density==0, NA, evm_density))

# Get average density
density_mean <- mean(df_evm_2021$evm_density, na.rm=T)

# Apply average density to other circuits
df_evm_2021 <- df_evm_2021 %>% 
  mutate(evm_density=ifelse(is.na(evm_density), density_mean,
                            evm_density))

# Identify a couple circuits flagged with EVM in one dataset but not other
tmp <- df_evm_2021 %>% 
  filter(evm_total_trees==0 & evm_miles_2021_total!=0)

# Merge in density estimates
df_2021 <- left_join(df_2021, df_evm_2021 %>% 
                       select(circuit.name, evm_density))

# Convert to miles
df_2021 <- df_2021 %>% 
  mutate(across(`evm_non-hftd`:`evm_hftd-3`, ~.x / evm_density))

# Save cleaned 2021 data
save(df_2021, file='./intermediate/Intermediate Hardening Data/compiled_2021_treatment.RData')

################################################################################
# Merge 2021 with 2018-2020
################################################################################

load('./intermediate/Intermediate Hardening Data/compiled_treatment_data.RData')
df_treatment <- bind_rows(df_treatment, df_2021)
save(df_treatment, file='./intermediate/Intermediate Hardening Data/compiled_treatment_2014_2021.RData')

###########################################
# Get average tree density across 2020-21 #
###########################################

####### BRING IN 2020

# Load data on grid hardening
df <- read_excel(path='./data/PGE/2021 WMP/Data Requests/CalAdvocates_035 (2021WMP-01)/WildfireMitigationPlans_DR_CalAdvocates_035-Q04-Atch01.xlsx',
                 sheet = '4_Distribution', skip=2)

# Column names
names(df) <- tolower(make.names(names(df)))

df <- df %>% 
  # Total veg mgmt miles
  mutate(total_veg_mgmt=miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020 +
           miles.of.evm.work.in.hftd.tier.2.in.2020..+
           miles.of.evm.work.in.hftd.tier.3.in.2020.,
         
         # CHeck if total exceeds circuit miles
         greater_than_length = ifelse(total_veg_mgmt>total.circuit.miles, 1, 0),
         
         # Check which HFTD segments equal total circuit length
         is_nonhftd_evm = total.circuit.miles==miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020,
         is_hftd2_evm   = total.circuit.miles==miles.of.evm.work.in.hftd.tier.2.in.2020..,
         is_hftd3_evm   = total.circuit.miles==miles.of.evm.work.in.hftd.tier.3.in.2020.
  )

df <- df %>% 
  mutate(miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020 = 
           ifelse(greater_than_length==1 & is_nonhftd_evm, 0, miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020),
         miles.of.evm.work.in.hftd.tier.2.in.2020..=ifelse(greater_than_length==1 & is_hftd2_evm, 0, 
                                                           miles.of.evm.work.in.hftd.tier.2.in.2020..),
         miles.of.evm.work.in.hftd.tier.3.in.2020.=ifelse(greater_than_length==1 & is_hftd3_evm, 0,
                                                          miles.of.evm.work.in.hftd.tier.3.in.2020.)) %>% 
  select(-(total_veg_mgmt:is_hftd3_evm)) %>% 
  select(circuit.name, miles.of.enhanced.vegetation.management..evm..in.non.hftd.areas.in.2020,
         miles.of.evm.work.in.hftd.tier.2.in.2020..,
         miles.of.evm.work.in.hftd.tier.3.in.2020.)

# Update names
names(df)[2:4] <- c('evm_miles_non_2020',
                    'evm_miles_tier2_2020',
                    'evm_miles_tier3_2020')

# Load 2021 file
df_2021 <- read_excel(path='./data/PGE/2022 WMP/Data Requests/WMP-Discovery2022_DR_MGRA_002-Q16Atch01.xlsx',
                      sheet = '1_Distribution', skip=4)

# Column names
names(df_2021) <- tolower(make.names(names(df_2021)))

# Select tree vars
df_2021 <- df_2021 %>% 
  select(circuit.name, 
         number.of.trees.that.were.worked.on.for.evm.in.non.hftd.in.2020,
         number.of.trees.that.were.worked.on.for.evm.in.other.hftd.in.2020,
         number.of.trees.that.were.worked.on.for.evm.in.tier.2.hftd.in.2020,
         number.of.trees.that.were.worked.on.for.evm.in.tier.3.hftd.in.2020)
names(df_2021)[2:5] <- c('evm_trees_non_2020',
                         'evm_trees_other_2020',
                         'evm_trees_tier2_2020',
                         'evm_trees_tier3_2020')

# Merge together
df_evm<-full_join(df, df_2021)

# Calculate total trees and miles
df_evm <- df_evm %>% 
  mutate(miles_total=evm_miles_non_2020+evm_miles_tier2_2020+
           evm_miles_tier3_2020,
         trees_total=evm_trees_non_2020+evm_trees_other_2020+
           evm_trees_tier2_2020+evm_trees_tier3_2020) %>% 
  # Calc density
  mutate(evm_density_2020=trees_total/miles_total) %>% 
  # fix nas
  mutate(evm_density_2020=ifelse(evm_density_2020==Inf, NA,
                                 evm_density_2020)) %>% 
  mutate(evm_density_2020 = ifelse(evm_density_2020>1E4, NA, 
                                   evm_density_2020))

# Get average density
density_mean <- mean(df_evm$evm_density_2020, na.rm=T)
density_median <- median(df_evm$evm_density_2020, na.rm=T)

################################################################################
# LOAD 2022 data
################################################################################

# Load 2022 data on grid hardening
df_2022 <- read_excel(path='./data/PGE/2023 WMP/Data Requests/WMP-Discovery2023_DR_CalAdvocates_003-Q001Atch01.xlsx',
                      sheet = '1_Distribution', skip=4)

# Column names
names(df_2022) <- tolower(make.names(names(df_2022)))

# Write similar function in previous steps for reshaping Excel workbook data
reshapeGridData2022 <- function(df) {
  
  # Reshape
  df <- df %>% 
    rename(circuit.saidi = circuit.saidi..system.average.interruption.duration.index..for.2022,
           circuit.saifi = circuit.saifi..system.average.interruption.frequency.index..for.2022,
           circuit.maifi = circuit.maifi..momentary.average.interruption.frequency.index..for.2022) %>% 
    gather(key='label', value='quantity', 
           -(circuit.name:circuit.voltage), -circuit.saidi,
             -circuit.saifi, -circuit.maifi) %>% 
    filter(!grepl('saidi', label) & !grepl('saifi', label) & 
             !grepl('maifi', label)) %>% 
    mutate(year=ifelse(grepl('2018', label), 2018, 
                       ifelse(grepl('2019', label), 2019,
                              ifelse(grepl('2020', label), 2020,
                                     ifelse(grepl('2021', label), 2021,
                                            ifelse(grepl('2022', label), 2022, NA)))))) %>% 
    mutate(HFTD = ifelse(grepl('non.hftd', label), 'Non-HFTD', 
                         ifelse(grepl('tier.2', label), 'HFTD-2',
                                ifelse(grepl('tier.3', label), 'HFTD-3', 
                                       ifelse(grepl('non.high.fire', label), 'Non-HFTD',
                                              ifelse(grepl('other.hftd', label), 'Other-HFTD',
                                                     ifelse(grepl('total.customer', label), 'Total', NA))))))) %>% 
    arrange(circuit.name) %>% 
    mutate(prevention_type = ifelse(grepl('evm', label), 'EVM',
                                    ifelse(grepl('routine.vegetation', label), 'routine_veg',
                                           ifelse(grepl('covered.conductor', label), 'covered_conductor',
                                                  ifelse(grepl('poles.replaced', label), 'poles_replaced',
                                                         ifelse(grepl('underground', label), 'underground',
                                                                # multiple types of lidar be warned
                                                                ifelse(grepl('lidar', label), 'lidar',
                                                                       ifelse(grepl('detailed.overhead', label), 'overhead_inspections',
                                                                              ifelse(grepl('sectionalization', label), 'sectionalization_devices', 
                                                                                     ifelse(grepl('psps.events', label), 'psps_cust_minutes',
                                                                                            ifelse(grepl('epss.fast', label), 'epss_cust_minutes',
                                                                                                   NA))))))))))) %>% 
    mutate(units = ifelse(grepl('miles', label), 'miles',
                          ifelse(grepl('number', label), 'number',
                                 ifelse(grepl('minutes', label), 'minutes', NA))))
  
  # Convert to zero if missing
  df <- df %>% 
    mutate(quantity=as.numeric(quantity),
           quantity=ifelse(is.na(quantity), 0, quantity),
           tier.2.oh.miles=ifelse(is.na(tier.2.oh.miles), 0, tier.2.oh.miles),
           tier.3.oh.miles=ifelse(is.na(tier.3.oh.miles), 0, tier.3.oh.miles))
  
  # Create daily dataset
  dt_start <- ymd('2022-01-01')
  dt_end   <- ymd('2022-12-31')
  dt_range <- seq(dt_start,dt_end, 1)
  df_treatment <- crossing(unique(df$circuit.name), dt_range)
  
  # Bring in treatment data
  df_treatment<-df_treatment %>% 
    rename(circuit.name='unique(df$circuit.name)',
           date='dt_range') %>% 
    mutate(year=year(date))
  
  # Bring in 
  hftd <- unique(df$HFTD)
  ptypes <- unique(df$prevention_type)
  ptypes <- ptypes[ptypes!='lidar']
  
  # Loop through and create panel data
  for(j in 1:length(ptypes)) {
    
    for(i in 1:length(hftd)) {
      
      cname <- paste0(ptypes[j], '_', hftd[i])
      
      # Reorganize
      tmp<-df %>% 
        select(-label) %>% 
        filter(prevention_type==ptypes[j]) %>% 
        filter(HFTD==hftd[i]) %>% 
        select(circuit.name, year, quantity)
      names(tmp)[3] <- cname
      
      # Merge 
      df_treatment<-left_join(df_treatment, tmp)
      
    }
    
  }
  
  # Drop NA columns
  not_all_na <- function(x) any(!is.na(x))
  df_treatment <- df_treatment %>% 
    select(where(not_all_na))
  
  # Set to NA if zero
  names(df_treatment) <- tolower(names(df_treatment))
  df_treatment <- df_treatment %>% 
    mutate_at(vars(`psps_cust_minutes_total`:`sectionalization_devices_hftd-3`),
              function(x){ifelse(is.na(x), 0, x)})
  
  
  ##################################################################
  # Bring in distribution of veg management projects weekly - 2021
  # use 2021 distribution, ***assume representative
  df_dist2021 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                            sheet='2021', skip=6) %>% 
    select(week_ending...1, evm_miles_cum)
  
  # Calculate shares for how EVM progresses throughout 2021
  df_dist2021 <- df_dist2021 %>% 
    mutate(total=max(evm_miles_cum),
           evm_progress=evm_miles_cum/total,
           date=ymd(week_ending...1)) %>% 
    select(-week_ending...1, -evm_miles_cum, -total) 
  df_dist2021$date<-df_dist2021$date+365
  
  # Merge in 
  df_treatment <- left_join(df_treatment, df_dist2021)
  
  # 2022
  pdata <- df_treatment %>% 
    filter(year==2022) %>% 
    select(date, year, evm_progress) %>% 
    unique()
  x_2022 <- interpolate_progress(pdata)
  pdata$evm_progress_intp <- x_2022
  
  # Bring in EVM progress distribution to treatment data
  df_treatment <- left_join(df_treatment, pdata)
  
  
  #########################################################
  #~~~~~~~~~~~ REPEAT PROCESS FOR SYS HARDENING ~~~~~~~~~~#
  
  # Bring in distribution of sys hardening projects weekly - 2021
  df_dist2021 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                            sheet='2021', skip=6) %>% 
    select(week_ending...5, sys_hardening_cum) %>% 
    filter(!(is.na(week_ending...5) | is.na(sys_hardening_cum)))
  
  # Calculate shares for how sys hardening progresses throughout 2021
  df_dist2021 <- df_dist2021 %>% 
    mutate(total=max(sys_hardening_cum),
           hard_progress=sys_hardening_cum/total,
           date=ymd(week_ending...5)) %>% 
    select(-week_ending...5, -sys_hardening_cum, -total) 
  df_dist2021$date<-df_dist2021$date+365
  
  # Merge in 
  df_treatment <- left_join(df_treatment, df_dist2021)
  
  # 2022
  pdata <- df_treatment %>% 
    filter(year==2022) %>% 
    select(date, year, hard_progress) %>% 
    unique()
  x_2022 <- interpolate_progress(pdata, switch_hardening = T)
  pdata$hard_progress_intp <- x_2022
  
  # Bring in EVM progress distribution to treatment data
  df_treatment <- left_join(df_treatment, pdata)
  
  # Apply
  df_treatment <- df_treatment %>% 
    mutate(across(`evm_non-hftd`:`evm_hftd-3`, ~.x * evm_progress_intp)) %>% 
    mutate(across(`covered_conductor_non-hftd`:`sectionalization_devices_hftd-3`, ~.x * hard_progress_intp))
  
  return(df_treatment)
  
}

# Run function on 2022 data
df_2022 <- reshapeGridData2022(df_2022)

################################################################################
# Convert trees worked to miles of EVM at the circuit-level 2022
################################################################################

# Load 2022 work-verified EVM miles
df_evm_2022 <- read_excel(path="./data/PGE/2023 WMP/Data Requests/WMP-Discovery2023_DR_CalAdvocates_006-Q003Atch01.xlsx",
                          sheet = '2022 EVM Miles Completed')

# Select relevant variables
df_evm_2022 <- df_evm_2022 %>% 
  select(CIRCUITNAME,
         SEGMENT_REMAINING_MILES)
# And adjust names
names(df_evm_2022) <- c('circuit.name', 'evm_miles_2022_total')

# Summarise across multiple circuit segments
df_evm_2022 <- df_evm_2022 %>% 
  group_by(circuit.name) %>% 
  summarise_all(sum) %>% 
  filter(!(is.na(circuit.name))) %>% 
  filter(circuit.name!='Total') %>% 
  # fix circuit name discrepancy
  mutate(circuit.name = ifelse(circuit.name=='SAN JOAQUIN NO2 1103',
                               'SAN JOAQUIN #2 1103', circuit.name),
         circuit.name = ifelse(circuit.name=='SAN JOAQUIN NO3 1103',
                               'SAN JOAQUIN #3 1103', circuit.name))

# Bring in actual trees worked 2022
x <- df_2022 %>% 
  filter(date=='2022-12-31') %>% 
  select(circuit.name, starts_with('evm')) %>% 
  select(-evm_progress_intp, -evm_progress) %>% 
  mutate(evm_total_trees=`evm_non-hftd`+`evm_other-hftd`+
           `evm_hftd-2`+`evm_hftd-3`)
df_evm_2022 <- full_join(x, df_evm_2022)

# Calculate tree density per mile
df_evm_2022 <- df_evm_2022 %>% 
  mutate(evm_density=evm_total_trees/evm_miles_2022_total) %>% 
  # reclassify couple instances of zero
  mutate(evm_density=ifelse(evm_density==0, NA, evm_density))

# Get average density
density_mean <- mean(df_evm_2022$evm_density, na.rm=T)

#### set aside
df_evm_2022_setaside <- df_evm_2022

# Apply average density to other circuits
df_evm_2022 <- df_evm_2022 %>% 
  mutate(evm_density=ifelse(is.na(evm_density), density_mean,
                             evm_density))

# Identify a couple circuits flagged with EVM in one dataset but not other
tmp <- df_evm_2021 %>% 
  filter(evm_total_trees==0 & evm_miles_2021_total!=0)

# Merge in density estimates
df_2022 <- left_join(df_2022, df_evm_2022 %>% 
                       select(circuit.name, evm_density))

# Convert to miles
df_2022 <- df_2022 %>% 
  mutate(across(`evm_non-hftd`:`evm_hftd-3`, ~.x / evm_density))

# Save cleaned 2022 data
save(df_2022, file='./intermediate/Intermediate Hardening Data/compiled_2022_treatment.RData')

################################################################################
# Merge 2022 with 2018-2021
################################################################################

load('./intermediate/Intermediate Hardening Data/compiled_treatment_2014_2021.RData')
df_treatment<-bind_rows(df_treatment, df_2022)
save(df_treatment, file='./intermediate/compiled_treatment_2014_2022.RData')

################################################################################
# LOAD 2023 data
################################################################################

# Load 2023 data on grid hardening
df_2023 <- read_excel(path='./data/PGE/2025 WMP/Data Requests/WMP-Discovery2023-2025_DR_CalAdvocates_038-Q001Atch01.xlsx',
                      sheet = '1_Distribution', skip=3)

### CHECK SOME MISSING CIRCUITS FROM 2023 UPDATE ###
circs_2023 <- sort(unique(df_2023$`Circuit Name`))
tmp <- df_treatment %>% filter(year==2022)
circs_2022 <- sort(unique(tmp$circuit.name))
circs_missing <- circs_2022[!circs_2022 %in% circs_2023]

# Column names
names(df_2023) <- tolower(make.names(names(df_2023)))

# Write similar function in previous steps for reshaping Excel workbook data
reshapeGridData2023 <- function(df) {

  ## Add a couple circuits that are dropped in 2023, particularly one that
  ## had an EPSS ignition, assume no prevention measures
  df <- bind_rows(df, 
                 data.frame(circuit.name=c('CAMBRIA 1101'))) %>% 
    bind_rows(data.frame(circuit.name=c('CAMBRIA 1102')))
  
  # Reshape
  df <- df %>% 
    rename(circuit.saidi = circuit.saidi..system.average.interruption.duration.index..for.2023,
           circuit.saifi = circuit.saifi..system.average.interruption.frequency.index..for.2023,
           circuit.maifi = circuit.maifi..momentary.average.interruption.frequency.index..for.2023) %>% 
    gather(key='label', value='quantity', 
           -(circuit.segment.name:circuit.voltage), -circuit.saidi,
           -circuit.saifi, -circuit.maifi) %>% 
    filter(!grepl('saidi', label) & !grepl('saifi', label) & 
             !grepl('maifi', label)) %>% 
    mutate(year=ifelse(grepl('2018', label), 2018, 
                       ifelse(grepl('2019', label), 2019,
                              ifelse(grepl('2020', label), 2020,
                                     ifelse(grepl('2021', label), 2021,
                                            ifelse(grepl('2022', label), 2022, 
                                                   ifelse(grepl('2023', label), 2023, NA))))))) %>% 
    mutate(HFTD = ifelse(grepl('non.hftd', label), 'Non-HFTD', 
                         ifelse(grepl('tier.2', label), 'HFTD-2',
                                ifelse(grepl('tier.3', label), 'HFTD-3', 
                                       ifelse(grepl('non.high.fire', label), 'Non-HFTD',
                                              ifelse(grepl('other.hftd', label), 'Other-HFTD',
                                                     ifelse(grepl('total.customer', label), 'Total', NA))))))) %>% 
    arrange(circuit.name) %>% 
    mutate(prevention_type = ifelse(grepl('evm', label), 'EVM',
                                    ifelse(grepl('tree', label), 'EVM',
                                    ifelse(grepl('routine.vegetation', label), 'routine_veg',
                                           ifelse(grepl('covered.conductor', label), 'covered_conductor',
                                                  ifelse(grepl('poles.replaced', label), 'poles_replaced',
                                                         ifelse(grepl('underground', label), 'underground',
                                                                # multiple types of lidar be warned
                                                                ifelse(grepl('lidar', label), 'lidar',
                                                                       ifelse(grepl('ground.based', label), 'overhead_inspections',
                                                                              ifelse(grepl('aerial.inspections', label), 'overhead_inspections',
                                                                                  ifelse(grepl('sectionalization', label), 'sectionalization_devices', 
                                                                                         ifelse(grepl('psps.events', label), 'psps_cust_minutes',
                                                                                                ifelse(grepl('epss.fast', label), 'epss_cust_minutes',
                                                                                                       NA))))))))))))) %>% 
    mutate(units = ifelse(grepl('miles', label), 'miles',
                          ifelse(grepl('number', label), 'number',
                                 ifelse(grepl('minutes', label), 'minutes', NA))))
  
  # Convert to zero if missing
  df <- df %>% 
    mutate(quantity=as.numeric(quantity),
           quantity=ifelse(is.na(quantity), 0, quantity),
           tier.2.oh.miles=ifelse(is.na(tier.2.oh.miles), 0, tier.2.oh.miles),
           tier.3.oh.miles=ifelse(is.na(tier.3.oh.miles), 0, tier.3.oh.miles))
  
  # Create daily dataset
  dt_start <- ymd('2023-01-01')
  dt_end   <- ymd('2023-12-31')
  dt_range <- seq(dt_start,dt_end, 1)
  df_treatment <- crossing(unique(df$circuit.name), dt_range)
  
  # Bring in treatment data
  df_treatment<-df_treatment %>% 
    rename(circuit.name='unique(df$circuit.name)',
           date='dt_range') %>% 
    mutate(year=year(date))
  
  # Bring in 
  hftd <- unique(df$HFTD)
  ptypes <- unique(df$prevention_type)
  ptypes <- ptypes[ptypes!='lidar']
  
  # Loop through and create panel data
  for(j in 1:length(ptypes)) {
    
    for(i in 1:length(hftd)) {
      
      cname <- paste0(ptypes[j], '_', hftd[i])
      
      # Reorganize
      tmp<-df %>% 
        select(-label) %>% 
        filter(prevention_type==ptypes[j]) %>% 
        filter(HFTD==hftd[i]) %>% 
        group_by(circuit.name, year, HFTD) %>% 
        summarise(quantity=sum(quantity)) %>% 
        select(circuit.name, year, quantity)
      names(tmp)[3] <- cname
      
      # Merge 
      df_treatment<-left_join(df_treatment, tmp)
      
    }
    
  }
  
  # Drop NA columns
  not_all_na <- function(x) any(!is.na(x))
  df_treatment <- df_treatment %>% 
    select(where(not_all_na))
  
  # Set to NA if zero
  names(df_treatment) <- tolower(names(df_treatment))
  df_treatment <- df_treatment %>% 
    mutate_at(vars(`psps_cust_minutes_total`:`evm_hftd-3`),
              function(x){ifelse(is.na(x), 0, x)})
  
  
  ##################################################################
  # Bring in distribution of veg management projects weekly - 2021
  ##################################################################
  df_dist2021 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                            sheet='2021', skip=6) %>% 
    select(week_ending...1, evm_miles_cum)
  
  # Calculate shares for how EVM progresses throughout 2021
  df_dist2021 <- df_dist2021 %>% 
    mutate(total=max(evm_miles_cum),
           evm_progress=evm_miles_cum/total,
           date=ymd(week_ending...1)) %>% 
    select(-week_ending...1, -evm_miles_cum, -total) 
  df_dist2021$date<-df_dist2021$date+(365*2)
  
  # Merge in 
  df_treatment <- left_join(df_treatment, df_dist2021)
  
  # 2023
  pdata <- df_treatment %>% 
    filter(year==2023) %>% 
    select(date, year, evm_progress) %>% 
    unique()
  x_2022 <- interpolate_progress(pdata)
  pdata$evm_progress_intp <- x_2022
  
  # Bring in EVM progress distribution to treatment data
  df_treatment <- left_join(df_treatment, pdata)
  
  #########################################################
  # Repeat for System Hardening
  #########################################################
  
  # Bring in distribution of sys hardening projects weekly - 2021
  df_dist2021 <- read_excel('./intermediate/Workbooks/Temporal Hardening Distribution.xlsx',
                            sheet='2021', skip=6) %>% 
    select(week_ending...5, sys_hardening_cum) %>% 
    filter(!(is.na(week_ending...5) | is.na(sys_hardening_cum)))
  
  # Calculate shares for how sys hardening progresses throughout 2021
  df_dist2021 <- df_dist2021 %>% 
    mutate(total=max(sys_hardening_cum),
           hard_progress=sys_hardening_cum/total,
           date=ymd(week_ending...5)) %>% 
    select(-week_ending...5, -sys_hardening_cum, -total) 
  df_dist2021$date<-df_dist2021$date+(365*2)
  
  # Merge in 
  df_treatment <- left_join(df_treatment, df_dist2021)
  
  # 2023
  pdata <- df_treatment %>% 
    filter(year==2023) %>% 
    select(date, year, hard_progress) %>% 
    unique()
  x_2022 <- interpolate_progress(pdata, switch_hardening = T)
  pdata$hard_progress_intp <- x_2022
  
  # Bring in EVM progress distribution to treatment data
  df_treatment <- left_join(df_treatment, pdata)
  
  # Apply
  df_treatment <- df_treatment %>% 
    mutate(across(`evm_non-hftd`:`evm_hftd-3`, ~.x * evm_progress_intp)) %>% 
    mutate(across(`covered_conductor_non-hftd`:`sectionalization_devices_hftd-3`, ~.x * hard_progress_intp))
  
  return(df_treatment)
  
}

# Run function on 2022 data
df_2023 <- reshapeGridData2023(df_2023)

################################################################################
# 2023 Tree Density -- use past 2021 and 2022 tree densities
################################################################################

# Tree density 2020
x <- df_evm %>% 
  select(circuit.name, evm_density_2020) %>% 
  filter(!is.na(evm_density_2020))

# Tree density 2022
y <- df_evm_2022_setaside %>% 
  select(circuit.name, evm_density) %>% 
  filter(!is.na(evm_density)) %>% 
  rename(evm_density_2022 = evm_density)

# Just use median tree density for conversion from trees to miles
evm_density <- median(c(x$evm_density_2020,y$evm_density_2022))

# Convert to miles
df_2023 <- df_2023 %>% 
  mutate(across(`evm_non-hftd`:`evm_hftd-3`, ~.x / evm_density))

################################################################################
# Merge 2023 with 2018-2022
################################################################################

load('./intermediate/Intermediate Hardening Data/compiled_treatment_2014_2022.RData')
df_treatment<-bind_rows(df_treatment, df_2023)
save(df_treatment, file='./intermediate/compiled_treatment_2014_2023.RData')
