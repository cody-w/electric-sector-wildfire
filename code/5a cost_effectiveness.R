################################################################################
## Project: Ignitions
## Purpose: Cost-effectiveness analysis
################################################################################

########## PARAMETER SET
r_wacc <- 0.075        # cost of capital
r_disc <- 0.025        # real social discount rate
ug_years <- 40         # lifetime of undg. asset
evm_years <- 10        # lifetime of veg mgmt
evm_maint <- 0.01      # maintance costs veg mgmt %
ug_maint  <- 0.01      # maintenance costs undg. %
risk_2050 <- 0.5       # increase in risk by 2050
psps_criteria <- 0.075 # threshold for PSPS usage
########################

#######################
### VOLL DATA FRAME ###
#######################

# See workbook for value of lost load assumptions
voll <- data.frame(customer=c('Residential', 'Small_CI', 'Large_CI', 
                              'Medical'),
                   usage_kw=c(6.7*1000/8760, 16.7*1000/8760, 203.2*1000/8760,
                              6.7*1000/8760),
                   value_kwh=c(3, 25, 15,
                               100)) %>% 
  mutate(value_kwh_low=value_kwh*1/3,
         value_kwh_high=value_kwh*5/3)

#################
### Load data ###
#################

# Load dataset
load(file='./intermediate/Regression Dataset/reg_matched_data.RData')
rm(pdata); gc()

# Load reg models
load(file='./intermediate/Regression Dataset/rscore_models.RData')
# Drop non-primary specifications
rm(mm_risk_match_high, mm_risk_match_high2,
   mm_risk_match_mod, mm_risk_match_mod2,
   mm_risk_r3_high,
   mm_risk_r3_mod, mm_risk_r3_mod2); gc()

# Load expected parcels burned from wildfire consequence analysis
# these were run in steps due to long computation times
load('./intermediate/Missoula Fire Lab/damages_1_100.RData')
df_damages <- out_damages
load('./intermediate/Missoula Fire Lab/damages_101_200.RData')
df_damages <- bind_rows(df_damages, out_damages)
load('./intermediate/Missoula Fire Lab/damages_201_300.RData')
df_damages <- bind_rows(df_damages, out_damages)
load('./intermediate/Missoula Fire Lab/damages_301_400.RData')
df_damages <- bind_rows(df_damages, out_damages)
rm(out_damages); gc()

# Predict probability of wildfire size / spread
if (SWITCH_NEW_LOAD) {
  # Load model to predict probability of wildfire class size
  load(file='./intermediate/Missoula Fire Lab/wildfire_size_probability_model.RData')
  
  # Predict
  x <- predict(forest, newdata=reg_data, type='prob')
  save(x, file='./intermediate/Missoula Fire Lab/wildfire_probability.RData')

} else {
  load(file='./intermediate/Missoula Fire Lab/wildfire_probability.RData')
}

# Stitch together
reg_data <- cbind(reg_data, x)

# UPDATE UNDERGROUND UNITS - back into miles from tens of miles
reg_data <- reg_data %>% 
  mutate(underground_units=underground_units*10)

# Identify 2021 pilot circuits
epss_2021_circs <- reg_data %>% 
  filter(year==2021) %>% 
  filter(is_epss==1)
epss_2021_circs <- unique(epss_2021_circs$circuit.name)

# filter to circuits with simulated wildfire consequence
reg_data <- reg_data %>%
  filter(circuit.name %in% unique(df_damages$circuit.name))

################################################################################
# Reliability data
################################################################################

# This data is used and summarized in the Outage Costs workbook
# voll dataframe is used to value outages

# Load full dataset to get units of PSPS and EPSS
load(file='./intermediate/Regression Dataset/regression_dataset_clean_full.RData')
df_hours <- df %>% 
  group_by(year) %>% 
  summarise(psps_customer_hours=sum(psps_customer_hours),
            psps_customer_hours_res=sum(psps_customer_hours_res),
            psps_customer_hours_ci=sum(psps_customer_hours_ci),
            psps_customer_hours_med=sum(psps_customer_hours_med),
            psps_customer_hours_other=sum(psps_customer_hours_other),
            # Fast-trip
            epss.customer.hours=sum(epss.customer.hours),
            epss.customer.hours.res=sum(epss.customer.hours.res),
            epss.customer.hours.med=sum(epss.customer.hours.med),
            epss.customer.hours.large.ci=sum(epss.customer.hours.large.ci))

# Get shares of outages by customer type
df_hours_share <- df_hours %>% 
  select(-year) %>% 
  summarise_all(sum) %>% 
  mutate(psps_customer_hours=psps_customer_hours_res + psps_customer_hours_ci +
           psps_customer_hours_med + psps_customer_hours_other,
         epss.customer.hours = epss.customer.hours.res + epss.customer.hours.med + 
           epss.customer.hours.large.ci) %>% 
  mutate(across(contains('psps'), ~ . / psps_customer_hours)) %>% 
  mutate(across(contains('epss'), ~ . / epss.customer.hours))

# Bring in shares to VOLL data frame
voll$cust_share <- c(df_hours_share$psps_customer_hours_res[1],
                     df_hours_share$psps_customer_hours_ci[1],
                     df_hours_share$psps_customer_hours_med[1],
                     df_hours_share$psps_customer_hours_other[1])

################################################################################
# Cost Assumptions
################################################################################

# Calculate annualized capex amount 
calcCapex <- function(cpex, r, N) {
  out=cpex * 
    (r*(1+r)^N / ((1+r)^N-1))  
  return(out)
}

# PSPS costs per customer-hr 
psps_cost_hr <- 10
psps_cost_hr_lo <- 5
psps_cost_hr_hi <- 15


# EVM Costs per mile#
evm_cost <- 250E3
evm_cost_lo <- 200E3
evm_cost_hi <- 300E3

# Annualize evm cost over X years
e_cost <- calcCapex(evm_cost, r=r_wacc, N=evm_years)

# Underground Costs per mile #
ug_cost <- 3E6
ug_cost_lo <- 2E6
ug_cost_hi <- 4E6

# EPSS Costs  per customer-hr#
epss_cost_hr <- 25
epss_cost_hr_hi <- 35
epss_cost_hr_lo <- 15

rm(df); gc()

################################################################################
# FIND UNITS DEPLOYED OF EACH PREVENTION TYPE BY YEAR BY CIRCUIT
################################################################################

# This function returns a vector of future dates with depreciated capital
# investment amounts -- just for undg. and veg mgmt. (because veg. mgmt. will degrade over time)
getUnits <- function(in_dat, horizon_evm, horizon_ug) {
  
  # Fixed vars to loop through 
  in_vars <- c('evm_units', 'underground_units')
  out_list <- list()
  for(i in 1:length(in_vars)) {
    
    # Loop through each year (2018-2022)
    out_units <- data.frame()
    for (y in 2018:2023) {
      print(y)
      
      # Start with EVM
      tmp_year <- in_dat %>% 
        filter(year==y) %>% 
        select(circuit.name, date, year, 
               any_of(in_vars[i])) %>% 
        rename(i_var = any_of(in_vars[i]))
      tmp_max <- tmp_year %>% 
        group_by(circuit.name) %>% 
        summarise(i_max=max(i_var))
      
      # Find last year's evm amount to subtract from cumulative
      x <- in_dat %>% 
        filter(year==(y-1)) %>%
        rename(i_var = any_of(in_vars[i])) %>% 
        group_by(circuit.name) %>% 
        summarise(i_prev=max(i_var))
      tmp_max <- left_join(tmp_max, x, by='circuit.name') %>% 
        mutate(i_max=i_max-i_prev) %>% 
        select(-i_prev)
      
      # Create time series
      if (in_vars[i]=='evm_units') {
        horz=horizon_evm
      } else if (in_vars[i]=='underground_units') {
        horz=horizon_ug
      }
      tmp_timeseries <- seq.Date(from=as.Date(paste(y+1, 1, 1, sep='-')), 
                                 to = as.Date(paste((y+horz-1), 12, 31, sep='-')),
                                 by = '1 day')
      
      # Create dataframe with time series data
      tmp_timeseries <- crossing(tmp_max, tmp_timeseries) %>%
        rename(date=tmp_timeseries) %>% 
        mutate(dep_counter=1) %>% 
        group_by(circuit.name) %>%
        mutate(dep_counter=cumsum(dep_counter),
               dep_max=max(dep_counter)) %>%
        mutate(i_dep=i_max - (i_max * dep_counter/dep_max)) %>% 
        # Also KEEP NON DEPRECIATED AMOUNT
        rename(i_nodep=i_max)
      
      # Save circuitname date and depreciated units
      tmp <- tmp_timeseries %>% 
        select(circuit.name, date, i_dep, i_nodep) %>% 
        mutate(lab = paste0(in_vars[i], '_', y))
      
      # Get in year units
      x <- left_join(tmp_year, x, by='circuit.name') %>% 
        mutate(i_dep=i_var-i_prev) %>% 
        select(circuit.name, date, i_dep) %>% 
        mutate(lab = paste0(in_vars[i], '_', y),
               i_nodep=i_dep)
      tmp <- bind_rows(x, tmp)
      
      
      # Store
      out_units <- bind_rows(out_units, tmp)
    }
    
    out_list[[i]] <- out_units
  }

  return(out_list)  
} 

# Run function
out_units <- getUnits(reg_data, horizon_evm = evm_years,
                      horizon_ug = ug_years)

#################################################################################
# Get SAMPLE OF WEATHER DATA IN FUTURE YEARS BASED ON CLIMATOLOGY AND FUTURE RISK
#################################################################################

getWeatherTimeSeries <- function(in_dat, horizon_max, risk_by_2050=0.50,
                                 seed=42, ignition_switch=F,
                                 combined_risk_switch=F) {
  
  # Create vector of risk increases
  risk_vector <- seq(2024, 2023+horizon_max-1, 1)
  tmp <- risk_by_2050/(2050-(2022))
  risk_vector <- data.frame(year=risk_vector,
                            risk=tmp) %>% 
    mutate(risk=cumsum(risk))
  
  # Rank each circuit's combination of ignition and wildfire risk
  if (ignition_switch==F & combined_risk_switch==F) {
    in_dat <- in_dat %>% 
      group_by(circuit.name, month) %>% 
      mutate(risk_rank=percent_rank(1-small)) 
    print('Wildfire Risk')
  } else if (ignition_switch==T & combined_risk_switch==F) {
    in_dat <- in_dat %>% 
      group_by(circuit.name, month) %>% 
      mutate(risk_rank=percent_rank(risk_score)) 
    print('Ignition Risk')
  } else if (combined_risk_switch==T) {
    in_dat <- in_dat %>% 
      mutate(risk_comb = risk_score * (1-small)) %>% 
      group_by(circuit.name, month) %>% 
      mutate(risk_rank=percent_rank(risk_comb)) 
    print('Combined Risk')
  } else {
    print('ERROR!')
  }

  # Set seed
  set.seed(seed)
  
  # Container
  out_risk <- data.frame()
  options(warn = -1)
  
  # Loop through years, max time horizon is undergrounding investment duration
  for (i in 1:(horizon_max-1)) {
    
    print(i+2023)
    
    # Define risk percentile
    risk_p <- risk_vector$risk[i]
    risk_p <- ifelse(risk_p>=0.9, 0.9, risk_p)
    
    # Create draw of data
    tmp <- in_dat %>% 
      filter(risk_rank>risk_p) %>% 
      select(circuit.name, date, month,
             is_r3, risk_score, small:medium) %>% 
      sample_n(size=31, replace=T) %>% 
      mutate(day=1,
             day=cumsum(day),
             year=2023+i,
             date_sim=ymd(paste(year, month, day, sep='-'))) %>% 
      # Some dates will fail here by design (leap years, Sep 31, etc)
      filter(!is.na(date_sim)) %>% 
      ungroup() %>% 
      select(-date) %>% 
      arrange(circuit.name, date_sim) %>% 
      rename(date=date_sim)
    # Store
    out_risk <- bind_rows(out_risk, tmp)
    
  }
  
  # Export- different options for risk sampling- the third chunk is primarily used
  # which increases both ignition risk and wildfire spread simultaneously
  # the other two increase ignition and wildfire spread risk in isolation
  if (ignition_switch==F & combined_risk_switch==F) {
    save(out_risk, 
         file=paste0('./intermediate/Intermediate Climatology/risk_sample_wildfire_escalate_', 
                     gsub('[.]', '', risk_by_2050), 
                     '_', horizon_max,
                     '.RData'))
  } else if (ignition_switch==T & combined_risk_switch==F) {
    save(out_risk, 
         file=paste0('./intermediate/Intermediate Climatology/risk_ignition_sample_escalate_', 
                     gsub('[.]', '', risk_by_2050), 
                     '_', horizon_max,
                     '.RData'))
  } else if (combined_risk_switch==T) {
    save(out_risk, 
         file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_', 
                     gsub('[.]', '', risk_by_2050), 
                     '_', horizon_max,
                     '.RData'))
  }
    
  return(out_risk)

}

# Only need to run once to get climatology / future weather sample
if (SWITCH_NEW_LOAD) {
  # Base case - 40 year horizon for underground, 50% risk increase by 2050
  out_risk <- getWeatherTimeSeries(reg_data,
                                   horizon_max = 40,
                                   risk_by_2050=0.5,
                                   combined_risk_switch=T)
  # 40 year horizon, 25% and 75% risk increases
  out_risk <- getWeatherTimeSeries(reg_data,
                                   horizon_max = 40,
                                   risk_by_2050=0.25,
                                   combined_risk_switch=T)
  out_risk <- getWeatherTimeSeries(reg_data,
                                   horizon_max = 40,
                                   risk_by_2050=0.75,
                                   combined_risk_switch=T)
  # 50% risk increase, 30 and 50 year horizon
  out_risk <- getWeatherTimeSeries(reg_data,
                                   horizon_max = 30,
                                   risk_by_2050=0.5,
                                   combined_risk_switch=T)
  out_risk <- getWeatherTimeSeries(reg_data,
                                   horizon_max = 50,
                                   risk_by_2050=0.5,
                                   combined_risk_switch=T)
}

# Load base case - others are used for uncertainty analysis / Monte Carlo
load(file='./intermediate/Intermediate Climatology/risk_sample_escalate_05_40.RData')

################################################################################
# COMBINE RISK SAMPLE AND TREATMENT DATA
################################################################################

# This function constructs a circuit-day dataset by combining the previous steps,
# merging future risk profiles with depreciated stocks of undergrounding and veg mgmt.
constructDataset <- function(in_units, in_risk, in_data,
                             psps_rule=0.05) {

  out_master <- in_data %>% 
    select(circuit.name, date, year,
           risk_score, evm_units, underground_units,
           is_r3, is_epss_enable, is_psps, small:medium) %>% 
    filter(year>=2018) %>% 
    # Add non depreciated amount
    mutate(evm_units_nodep=evm_units, 
           underground_units_nodep=underground_units)
  
  # Deselect
  in_risk <- in_risk %>% 
    select(-year)
  
  # Bring in vectors of veg mgmt. data
  tmp_labs <- unique(in_units[[1]]$lab)
  tmp_evm_shell <- data.frame(circuit.name='COTTONWOOD 1103',
                          date=as.Date('2022-12-30'))
  for (i in 1:length(tmp_labs)) {
    tmp <- in_units[[1]] %>% 
      filter(lab==tmp_labs[i])
    names(tmp)[which(names(tmp)=='i_dep')] <- tmp_labs[i]
    names(tmp)[which(names(tmp)=='i_nodep')] <- paste(tmp_labs[i], 'nodep',
                                                      sep='_')
    tmp <- tmp %>% 
      select(-lab)
    tmp_evm_shell <- full_join(tmp_evm_shell, tmp,
                               by=c('circuit.name', 'date'))
    print(tmp_labs[i])
  }
  
  # Set missings to NA
  tmp_evm_shell[,tmp_labs] <- lapply(tmp_evm_shell[,tmp_labs], function(x){
    ifelse(is.na(x), 0, x)
  })
  tmp_evm_shell[,paste(tmp_labs, 'nodep', sep='_')] <- 
    lapply(tmp_evm_shell[,paste(tmp_labs, 'nodep', sep='_')], function(x){
      ifelse(is.na(x), 0, x)
    })
  tmp_evm_shell <- tmp_evm_shell %>% arrange(circuit.name, date)
  
  # Bring in vectors of undg. data
  tmp_labs <- unique(in_units[[2]]$lab)
  tmp_ug_shell <- data.frame(circuit.name='COTTONWOOD 1103',
                              date=as.Date('2022-12-30'))
  for (i in 1:length(tmp_labs)) {
    tmp <- in_units[[2]] %>% 
      filter(lab==tmp_labs[i])
    names(tmp)[which(names(tmp)=='i_dep')] <- tmp_labs[i]
    names(tmp)[which(names(tmp)=='i_nodep')] <- paste(tmp_labs[i], 'nodep',
                                                      sep='_')
    tmp <- tmp %>% 
      select(-lab)
    tmp_ug_shell <- full_join(tmp_ug_shell, tmp, by=c('circuit.name', 'date'))
    print(tmp_labs[i])
  }
  
  # Set missings to NA
  tmp_ug_shell[,tmp_labs] <- lapply(tmp_ug_shell[,tmp_labs], function(x){
    ifelse(is.na(x), 0, x)
  })
  tmp_ug_shell[,paste(tmp_labs, 'nodep', sep='_')] <- 
    lapply(tmp_ug_shell[,paste(tmp_labs, 'nodep', sep='_')], function(x){
      ifelse(is.na(x), 0, x)
  })
  tmp_ug_shell <- tmp_ug_shell %>% arrange(circuit.name, date)
  
  ##### Get 2018-2022 data
  out_master <- left_join(out_master, tmp_evm_shell %>%
                            filter(year(date)>=2018 & year(date)<=2023),
                          by=c('circuit.name', 'date'))
  out_master <- left_join(out_master, tmp_ug_shell %>%
                            filter(year(date)>=2018 & year(date)<=2023),
                          by=c('circuit.name', 'date'))
  
  ###### Get post 2023 data
  in_risk <- left_join(in_risk, tmp_evm_shell, by=c('circuit.name', 'date')) %>% 
    left_join(tmp_ug_shell, by=c('circuit.name', 'date'))
  
  # Get total units
  in_risk <- in_risk %>% 
    mutate(evm_units=evm_units_2018+evm_units_2019+evm_units_2020+
             evm_units_2021+evm_units_2022+evm_units_2023,
           evm_units_nodep=evm_units_2018_nodep+evm_units_2019_nodep+
             evm_units_2020_nodep+evm_units_2021_nodep+evm_units_2022_nodep+
             evm_units_2023_nodep,
           underground_units=underground_units_2018+underground_units_2019+
             underground_units_2020+underground_units_2021+underground_units_2022+
             underground_units_2023,
           underground_units_nodep=underground_units_2018_nodep+underground_units_2019_nodep+
             underground_units_2020_nodep+underground_units_2021_nodep+
             underground_units_2022_nodep+underground_units_2023_nodep) %>% 
    # Add epss enable if R3+ day
    mutate(is_epss_enable=ifelse(is_r3==1, 1, 0),
           year=year(date))
  
  ### Combine into master dataframe
  out_master <- bind_rows(out_master, in_risk) %>% 
    arrange(circuit.name, date)
  
  ### Bring in data on circuit length and treatment group
  tmp_length <- in_data %>% 
    filter(date=='2022-12-30') %>% 
    select(circuit.name, non.hftd.oh.miles, tier.2.oh.miles, tier.3.oh.miles,
           is_any_high, is_any_mid) %>% 
    #unique() %>% 
    mutate(total_miles = non.hftd.oh.miles+tier.2.oh.miles+tier.3.oh.miles)
  out_master <- left_join(out_master, tmp_length, by='circuit.name')
  
  ## 
  x <- out_master %>% 
    filter(circuit.name=='CALISTOGA 1101')
  
  # Set post 2023 units to missings
  out_master[,c('underground_units', 'underground_units_nodep', 
                tmp_labs, paste(tmp_labs, 'nodep', sep='_'))] <- 
    lapply(out_master[,c('underground_units', 'underground_units_nodep', 
                         tmp_labs, paste(tmp_labs, 'nodep', sep='_'))],
           function(x){
              ifelse(is.na(x), 0, x)
  })
  tmp_labs <- unique(in_units[[1]]$lab)
  out_master[,c('evm_units', 'evm_units_nodep', 
                tmp_labs, paste(tmp_labs, 'nodep', sep='_'))] <-
    lapply(out_master[,c('evm_units', 'evm_units_nodep', 
                         tmp_labs, paste(tmp_labs, 'nodep', sep='_'))],
           function(x){
              ifelse(is.na(x), 0, x)
    })
  
  # Calculate if in high evm group or not
  out_master <- out_master %>% 
    mutate(is_high_evm=ifelse(evm_units/total_miles>=0.5, 1, 0),
           is_high_evm_nodep=ifelse(evm_units_nodep/total_miles>=0.5, 1, 0))
  
  # Get high and medium treated circuits
  tmp <- out_master %>% 
    mutate(evm_share=evm_units_nodep/total_miles) %>% 
    group_by(circuit.name) %>% 
    summarise(evm_share=max(evm_share)) %>% 
    mutate(is_any_high=ifelse(evm_share>=0.5, 1, 0),
           is_any_mid=ifelse(evm_share>=0.1 & evm_share<0.5, 1, 0))
  
  # Dummy holders
  out_master <- out_master %>% 
    select(-is_any_mid, -is_any_high) %>% 
    left_join(tmp %>% select(-evm_share), by='circuit.name') %>% 
    mutate(is_mid_evm=0, covered_units=0) %>% 
    # PSPS RULE
    mutate(is_psps=ifelse(is.na(is_psps) & extreme>=psps_rule, 1,
                          ifelse(is.na(is_psps) & extreme<psps_rule, 0, is_psps)))
  
  # one more refresh of evm units and undg. units
  out_master <- out_master %>% 
    mutate(evm_units=evm_units_2018+evm_units_2019+evm_units_2020+
             evm_units_2021+evm_units_2022+evm_units_2023,
           evm_units_nodep=evm_units_2018_nodep+evm_units_2019_nodep+
             evm_units_2020_nodep+evm_units_2021_nodep+evm_units_2022_nodep+
             evm_units_2023_nodep,
           underground_units=underground_units_2018+underground_units_2019+
             underground_units_2020+underground_units_2021+underground_units_2022+
             underground_units_2023,
           underground_units_nodep=underground_units_2018_nodep+underground_units_2019_nodep+
             underground_units_2020_nodep+underground_units_2021_nodep+
             underground_units_2022_nodep+underground_units_2023_nodep) %>% 
    mutate(month=month(date),
           day=day(date))
  
  # Export
  return(out_master)  
}

# Load weather data / risk
load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                 gsub('[.]', '', risk_2050),
                 '_', ug_years,
                 '.RData'))

# Construct dataset
df_main <- constructDataset(in_units=out_units, in_risk=out_risk,
                            in_data=reg_data,
                            psps_rule = psps_criteria)

################################################################################
# AVOIDED IGNITIONS FUNCTION MODULE
################################################################################

# Write function to get ignitions avoided depending on future risk increase and 
# amounts of future undergrounding and veg. mgmt. and fast-trip
estimateIgnitions <- function(in_dat, in_model,
                              in_damages,
                              depreciate_VEG=T) {

  #####################
  # AVOIDED IGNITIONS #
  #####################
  
  # Get base ignitions
  ign_base <- predict(in_model, newdata = in_dat %>% 
                        mutate(is_high_evm=0, 
                               is_epss_enable=0,
                               underground_units=0,
                               is_psps=0),
                      type='response')
  
  # Get veg ignitions
  ign_veg <- predict(in_model, newdata = in_dat %>% 
                       mutate(is_epss_enable=0,
                              underground_units=0,
                              is_psps=0),
                     type='response')
  
  # Get EPSS ignitions
  ign_epss <- predict(in_model, newdata = in_dat %>% 
                        mutate(is_high_evm=0, 
                               underground_units=0,
                               is_psps=0),
                      type='response')
  
  # Get PSPS ignitions
  ign_psps <- predict(in_model, newdata = in_dat %>% 
                      mutate(is_high_evm=0, 
                             is_epss_enable=0,
                             underground_units=0),
                    type='response')
  
  # Alternate UG ignitions w/o econometric model- just zero out proportionally
  ign_ug <- in_dat %>% 
    select(circuit.name, date, risk_score, tier.2.oh.miles, tier.3.oh.miles,
           underground_units) %>% 
    cbind(ign_base) %>% 
    mutate(ign_change = ign_base * (1-(underground_units/
                                        ((tier.2.oh.miles+tier.3.oh.miles)*100))))
  
  ##################################
  #### Avoided structures burned ###
  ##################################
  
  # Re-format
  tmp <- in_damages %>% 
    mutate(exp_structures=mean_bldg_acre*fire_size) %>% 
    select(circuit.name, fire_size, exp_structures) %>% 
    spread(key=fire_size, value=exp_structures)
  names(tmp)[which(names(tmp)!='circuit.name')] <- c('struct_small', 'struct_med',
                                                     'struct_large', 'struct_ext')
  
  # Merge
  in_dat <- left_join(in_dat, tmp, by='circuit.name') %>% 
    mutate(exp_structures = small*struct_small + medium*struct_med +
             large*struct_large + extreme*struct_ext)
  
  ### Compile in dataset
  out_dat <- in_dat %>% 
    mutate(ign_base=ign_base,
           ign_veg=ign_veg,
           ign_epss=ign_epss,
           ign_ug=ign_ug$ign_change,
           
           avoid_veg  = ign_base-ign_veg,
           avoid_epss = ign_base-ign_epss,
           avoid_ug   = ign_base-ign_ug,
           avoid_psps = ign_base-ign_psps,
           
           struct_veg  = avoid_veg * exp_structures,
           struct_epss = avoid_epss * exp_structures,
           struct_ug   = avoid_ug * exp_structures,
           struct_psps = avoid_psps * exp_structures)
  
  ### Depreciate the avoided ignitions from VEG MGMT
  if (depreciate_VEG) {
    out_dat <- out_dat %>% 
      group_by(circuit.name) %>% 
      mutate(dep_counter=ifelse(year>2023 & is_high_evm>0, 1, 0),
             dep_counter=cumsum(dep_counter),
             dep_max=max(dep_counter),
             
             avoid_veg_dep=ifelse(dep_max>0, 
                                  avoid_veg - (avoid_veg * dep_counter/dep_max), 0), 
             
             struct_veg_dep=ifelse(dep_max>0,
                                   struct_veg - (struct_veg * dep_counter/dep_max),
                                   0)) %>% 
      mutate(avoid_veg=avoid_veg_dep,
             struct_veg=struct_veg_dep) %>% 
      select(-avoid_veg_dep, -struct_veg_dep) %>% 
      ungroup()
  }
  
  # Summarise by year - all sample
  out_dat <- out_dat %>% 
    select(year, avoid_veg:struct_psps) %>% 
    group_by(year) %>% 
    summarise_all(sum, na.rm=T)
  
  return(out_dat)
  
}

# Run function, including switch to diminish benefits of veg mgmt.
df_ign <- estimateIgnitions(in_dat = df_main %>%
                              filter(is_r3==1) %>%
                              mutate(is_high_evm=is_high_evm_nodep,
                                     underground_units=underground_units_nodep) %>%
                              ungroup(),
                            in_model = mm_risk_r3_high2,
                            in_damages = df_damages,
                            depreciate_VEG = T)

################################################################################
# Get Annual Additions of EVM & UNDG.
################################################################################

# This function summarizes investment data into annual perspective, to conduct
# cost-effectiveness assessment
getAnnualInvestment <- function(in_data) {
  
  # For EVM
  out <- in_data %>% 
    filter(is_any_high==1) %>% 
    filter(month(date)==12 & day(date)==31) %>% 
    group_by(year) %>% 
    summarise(evm_units=sum(evm_units),
              evm_units_2018=sum(evm_units_2018),
              evm_units_2019=sum(evm_units_2019),
              evm_units_2020=sum(evm_units_2020),
              evm_units_2021=sum(evm_units_2021),
              evm_units_2022=sum(evm_units_2022),
              evm_units_2023=sum(evm_units_2023),
              
              evm_units_nodep=sum(evm_units_nodep),
              evm_units_2018_nodep=sum(evm_units_2018_nodep),
              evm_units_2019_nodep=sum(evm_units_2019_nodep),
              evm_units_2020_nodep=sum(evm_units_2020_nodep),
              evm_units_2021_nodep=sum(evm_units_2021_nodep),
              evm_units_2022_nodep=sum(evm_units_2022_nodep),
              evm_units_2023_nodep=sum(evm_units_2023_nodep)) %>% 
    # Get additions
    ungroup() %>% 
    mutate(lag_units=dplyr::lag(evm_units_nodep,1),
           evm_units_add=evm_units_nodep-lag_units,
           evm_units_add=ifelse(is.na(evm_units_add), evm_units_nodep, evm_units_add),           evm_units_add=ifelse(year>2022, 0, evm_units_add)) %>% 
    select(year, evm_units:evm_units_2023_nodep, evm_units_add)
  
  # For UNDG
  tmp <- in_data %>% 
    filter(month(date)==12 & day(date)==31) %>% 
    group_by(year) %>% 
    summarise(underground_units=sum(underground_units),
              underground_units_2018=sum(underground_units_2018),
              underground_units_2019=sum(underground_units_2019),
              underground_units_2020=sum(underground_units_2020),
              underground_units_2021=sum(underground_units_2021),
              underground_units_2022=sum(underground_units_2022),
              underground_units_2023=sum(underground_units_2023),
              
              underground_units_nodep=sum(underground_units_nodep),
              underground_units_2018_nodep=sum(underground_units_2018_nodep),
              underground_units_2019_nodep=sum(underground_units_2019_nodep),
              underground_units_2020_nodep=sum(underground_units_2020_nodep),
              underground_units_2021_nodep=sum(underground_units_2021_nodep),
              underground_units_2022_nodep=sum(underground_units_2022_nodep),
              underground_units_2023_nodep=sum(underground_units_2023_nodep)) %>% 
    # Get additions
    ungroup() %>% 
    mutate(lag_units=dplyr::lag(underground_units_nodep,1),
           underground_units_add=underground_units_nodep-lag_units,
           underground_units_add=ifelse(is.na(underground_units_add), underground_units_nodep, underground_units_add),
           underground_units_add=ifelse(year>2023, 0, underground_units_add)) %>% 
    select(year, underground_units:underground_units_2023_nodep, underground_units_add)
    
  # Join
  out <- full_join(out, tmp, by='year')
  return(out)
  
}

# Run function
df_units <- getAnnualInvestment(df_main)

################################################################################
# Calculate Costs
################################################################################

# Cost Function
calculateCosts <- function(in_ign, in_units,
                           r_disc, r_wacc, hh_pct,
                           c_veg, c_ug, c_epss_hr, c_psps_hr,
                           c_routine_veg = 400E6/25E3,
                           maint_veg, maint_ug, share_overhead_ug=0.75,
                           year_pv=2023,
                           lost_load, outages) {
  
  # Costs of EVM and UNDG
  ign_all <- left_join(in_ign, in_units, by='year') %>% 
    # Calculate upfront veg and ug cost
    mutate(cost_veg = evm_units_add*100*c_veg,
           cost_ug  = underground_units_add*c_ug)
  
  # Adjust voll format
  tmp <- lost_load %>% 
    mutate(voll=usage_kw*value_kwh) %>% 
    select(customer, voll) %>% 
    spread(key=customer, value=voll)
  
  # Get outage costs
  outage_costs <- outages %>% 
    cbind(tmp) %>% 
    mutate(outage_psps_cost = psps_customer_hours_res*Residential +
             psps_customer_hours_ci*Small_CI + 
             psps_customer_hours_med*Medical +
             psps_customer_hours_other*Large_CI) %>% 
    # Fast-trip costs
    mutate(outage_epss_cost = epss.customer.hours.res*Residential +
             epss.customer.hours.med*Medical +
             epss.customer.hours.large.ci*Large_CI)
  
  sum(outage_costs$outage_psps_cost)/1E6
  sum(outage_costs$outage_epss_cost)/1E6
  outage_costs$outage_psps_cost/1E6
  outage_costs$outage_epss_cost/1E6
  
  # Bring in outage costs 
  ign_all <- ign_all %>% 
    left_join(outage_costs %>% select(year, 
                                      psps_customer_hours, epss.customer.hours,
                                      outage_psps_cost, outage_epss_cost))
  
  # Ratepayer perspective, with undergrounding costs depreciated over time
  ign_all <- ign_all %>% 
    mutate(lag_2018=dplyr::lag(underground_units_2018, 1),
           lag_2019=dplyr::lag(underground_units_2019, 1),
           lag_2020=dplyr::lag(underground_units_2020, 1),
           lag_2021=dplyr::lag(underground_units_2021, 1),
           lag_2022=dplyr::lag(underground_units_2022, 1),
           lag_2023=dplyr::lag(underground_units_2023, 1),
           
           ug_dep = (lag_2018-underground_units_2018) + 
             (lag_2019-underground_units_2019) + 
             (lag_2020-underground_units_2020) + 
             (lag_2021-underground_units_2021) + 
             (lag_2022-underground_units_2022) + 
             (lag_2023-underground_units_2023) + 
             underground_units_add) %>% 
    select(-(lag_2018:lag_2023)) %>% 
    mutate(ug_dep_cost=ug_dep*c_ug,
           ug_dep=ifelse(is.na(ug_dep), 0, ug_dep),
           ug_dep_cost=ifelse(is.na(ug_dep_cost), 0, ug_dep_cost)) %>% 
    # Get regulated rate of return 
    # Discounting future payments using real social discount rate
    mutate(ug_return=underground_units*c_ug*r_wacc,
           ug_return_disc=ifelse(year>year_pv, ug_return/((1+r_disc)^(year-year_pv)),
                                 ug_return),
           ug_dep_cost_disc=ifelse(year>year_pv, ug_dep_cost/((1+r_disc)^(year-year_pv)),
                                   ug_dep_cost))
  
  ### Maintenance costs and avoided routine veg mgmt. costs
  ign_all <- ign_all %>% 
    # Add annual maintenance costs for EVM and UG
    mutate(cost_veg_maint = evm_units_nodep * 100 * c_veg * maint_veg,
           cost_veg_maint_disc  = ifelse(year>year_pv, cost_veg_maint/((1+r_disc)^(year-year_pv)),
                                         cost_veg_maint),
           cost_ug_maint = underground_units_nodep * c_ug * maint_ug,
           cost_ug_maint_disc = ifelse(year>year_pv, cost_ug_maint/((1+r_disc)^(year-year_pv)),
                                       cost_ug_maint)) %>% 
    # Add avoided routine veg costs from undg.
    # assume undergrounding only avoids x% of overhead line
    mutate(cost_routine_veg = underground_units_nodep * c_routine_veg * share_overhead_ug,
           cost_routine_veg_disc = ifelse(year>year_pv, cost_routine_veg/((1+r_disc)^(year-year_pv)),
                                          cost_routine_veg))
  
  ### Discount future avoided ignitions and structures burned
  ign_all <- ign_all %>% 
    mutate(avoid_veg_disc = ifelse(year>year_pv, avoid_veg/((1+r_disc)^(year-year_pv)),
                                   avoid_veg),
           avoid_ug_disc = ifelse(year>year_pv, avoid_ug/((1+r_disc)^(year-year_pv)),
                                  avoid_ug),
           avoid_epss_disc = ifelse(year>year_pv, avoid_epss/((1+r_disc)^(year-year_pv)),
                                    avoid_epss),
           avoid_psps_disc = ifelse(year>year_pv, avoid_psps/((1+r_disc)^(year-year_pv)),
                                    avoid_psps),
           # Structures 
           struct_veg_disc = ifelse(year>year_pv, struct_veg/((1+r_disc)^(year-year_pv)),
                                     struct_veg),
           struct_ug_disc = ifelse(year>year_pv, struct_ug/((1+r_disc)^(year-year_pv)),
                                     struct_ug),
           struct_epss_disc = ifelse(year>year_pv, struct_epss/((1+r_disc)^(year-year_pv)),
                                     struct_epss),
           struct_psps_disc = ifelse(year>year_pv, struct_psps/((1+r_disc)^(year-year_pv)),
                                     struct_psps))
  
  ### Calculate cost per avoided ignition 
  out_all <- ign_all %>% 
    mutate(cost_veg = cost_veg + cost_veg_maint_disc,
           # Ratepayer undergrounding
           cost_ug_ratepayer = ug_dep_cost_disc + ug_return_disc + 
             cost_ug_maint_disc - cost_routine_veg_disc,
           # Social undergrounding
           cost_ug_social = cost_ug + cost_ug_maint_disc -
             cost_routine_veg_disc,
           # EPSS costs, including outages, don't look at future costs
           cost_epss = epss.customer.hours * c_epss_hr + outage_epss_cost,
           cost_epss = ifelse(year>year_pv, 0, cost_epss),
           # PSPS costs, including outages, don't look at future costs
           cost_psps = psps_customer_hours * c_psps_hr + outage_psps_cost,
           cost_psps = ifelse(year>year_pv, 0, cost_psps),
           # Zero out future EPSS and PSPS ignitions and structures
           avoid_epss = ifelse(year>year_pv, 0, avoid_epss),
           struct_epss = ifelse(year>year_pv, 0, struct_epss),
           avoid_epss_disc = ifelse(year>year_pv, 0, avoid_epss_disc),
           struct_epss_disc = ifelse(year>year_pv, 0, struct_epss_disc),
           avoid_psps = ifelse(year>year_pv, 0, avoid_psps),
           struct_psps = ifelse(year>year_pv, 0, struct_psps),
           avoid_psps_disc = ifelse(year>year_pv, 0, avoid_psps_disc),
           struct_psps_disc = ifelse(year>year_pv, 0, struct_psps_disc)) %>% 
    # Exclude 2019 PSPS
    mutate(avoid_psps = ifelse(year==2019, 0, avoid_psps),
           struct_psps = ifelse(year==2019, 0, struct_psps),
           avoid_psps_disc = ifelse(year==2019, 0, avoid_psps_disc),
           struct_psps_disc = ifelse(year==2019, 0, struct_psps_disc),
           cost_psps = ifelse(year==2019, 0, cost_psps)) %>%
    # Collapse costs
    select(cost_veg, cost_ug_ratepayer, cost_ug_social,
           cost_epss, cost_psps,
           avoid_veg_disc:struct_psps_disc) %>% 
    ungroup() %>% 
    summarise_all(sum) %>% 
    mutate(cost_ign_veg=cost_veg/avoid_veg_disc,
           cost_ign_ug_ratepayer=cost_ug_ratepayer/avoid_ug_disc,
           cost_ign_ug_social=cost_ug_social/avoid_ug_disc,
           cost_ign_epss=cost_epss/avoid_epss_disc,
           cost_ign_psps=cost_psps/avoid_psps_disc,
           # Structures 
           cost_struct_veg=cost_veg/struct_veg_disc,
           cost_struct_ug_ratepayer=cost_ug_ratepayer/struct_ug_disc,
           cost_struct_ug_social=cost_ug_social/struct_ug_disc,
           cost_struct_epss=cost_epss/struct_epss_disc,
           cost_struct_psps=cost_psps/struct_psps_disc,
           
           # Combined Fast-Trip and PSPS costs
           cost_ign_combined = (cost_epss+cost_psps)/
             (avoid_epss_disc + avoid_psps_disc),
           cost_struct_combined = (cost_epss+cost_psps)/
             (struct_epss_disc + struct_psps_disc))
    

  return(out_all)
  
}


# Run function
df_costs <- calculateCosts(in_ign = df_ign,
                           in_units = df_units,
                           r_disc=r_disc, r_wacc=r_wacc,
                           c_veg=evm_cost, c_ug=ug_cost,
                           c_epss_hr=epss_cost_hr,
                           c_psps_hr=psps_cost_hr,
                           c_routine_veg = 400E6/25E3,
                           maint_veg = evm_maint,
                           maint_ug = ug_maint,
                           year_pv = 2023,
                           lost_load = voll,
                           outages = df_hours)

df_costs$cost_ign_veg/1E6
df_costs$cost_ign_ug_ratepayer/1E6
df_costs$cost_ign_ug_social/1E6
df_costs$cost_ign_epss/1E6
df_costs$cost_ign_psps/1E6

df_costs$cost_struct_veg/1E6
df_costs$cost_struct_ug_ratepayer/1E6
df_costs$cost_struct_ug_social/1E6
df_costs$cost_struct_epss/1E6
df_costs$cost_struct_psps/1E6

################################################################################
# Run Vegetation Cost Scenarios
################################################################################

runVegScenarios <- function(in_reg_data,
                            in_voll,
                            in_hours,
                            in_mdl,
                            ev_range,
                            disc_range,
                            evm_cost_range,
                            evm_effectiveness,
                            risk_range,
                            year_pv) {
  
  # Turn off summarise options
  options(dplyr.summarise.inform = FALSE)
  
  # Containers
  out_ign <- data.frame()
  out_struct <- data.frame()
  
  # Loop through lifespans of evm
  for (i in 1:length(ev_range)) {
    
    # Get units
    out_units <- getUnits(in_reg_data, horizon_evm = ev_range[i], 
                          horizon_ug = 40)
    
    # Loop through risk increase scenarios
    for (r in 1:length(risk_range)) {
      
      ###############################
      # Load ignition risk increase
      ###############################
      load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_', 
                       gsub('[.]', '', risk_range[r]), 
                       '_', 40,
                       '.RData'))
      
      # Construct dataset
      df_main <- constructDataset(in_units=out_units, in_risk=out_risk,
                                  in_data=in_reg_data,
                                  psps_rule = psps_criteria)
      
      # Get annual units
      df_units <- getAnnualInvestment(df_main)
      
      # Loop through EVM effectiveness 
      for (effect in 1:length(evm_effectiveness)) {
        
        # Change effectiveness
        in_mdl$coefficients[which(names(coefficients(in_mdl))=='is_high_evm')] <- evm_effectiveness[effect]
      
        # Run function, including switch to diminish benefits of veg mgmt.
        df_ign <- estimateIgnitions(in_dat = df_main %>% 
                                      filter(is_r3==1) %>% 
                                      mutate(is_high_evm=is_high_evm_nodep,
                                             underground_units=underground_units_nodep) %>% 
                                      ungroup(),
                                    in_model = in_mdl,
                                    in_damages = df_damages,
                                    depreciate_VEG = T)
        
        # Loop through unit costs
        for (u in 1:length(evm_cost_range)) {
          
          # Loop through discount range
          for (disc in 1:length(disc_range)) {
          
            # Cost function
            df_costs <- calculateCosts(in_ign = df_ign, 
                                       in_units = df_units,
                                       r_disc=disc_range[disc], #####
                                       r_wacc=r_wacc,
                                       c_veg=evm_cost_range[u], #####
                                       c_ug=ug_cost,
                                       c_epss_hr=epss_cost_hr, 
                                       c_psps_hr=psps_cost_hr,
                                       c_routine_veg = 400E6/25E3,
                                       maint_veg = evm_maint,
                                       maint_ug = ug_maint,
                                       year_pv = year_pv,
                                       lost_load = in_voll,
                                       outages = in_hours)
            
            # Save results
            df_costs$evm_horizon <- ev_range[i]
            df_costs$r_disc <- disc_range[disc]
            df_costs$unit_cost <- evm_cost_range[u]
            df_costs$risk_range <- risk_range[r]
            df_costs$effectiveness <- evm_effectiveness[effect]
            
            # Set aside
            tmp <- df_costs %>% 
              select(evm_horizon:effectiveness, cost_veg, 
                     avoid_veg_disc, cost_ign_veg, 
                     struct_veg_disc, cost_struct_veg)
            out_ign <- bind_rows(out_ign, tmp)
            
            # PRINT
            print('')
            print('Ignitions........')
            print(paste0(ev_range[i], ', ', disc_range[disc], ', ',
                         round(evm_effectiveness[effect],2), ', ', evm_cost_range[u],
                         ', ', risk_range[r]))
            
          }
        }
      }
    }
  }
  
  # Export
  return(out_ign)
  options(dplyr.summarise.inform = TRUE)
}

# Get standard error for vegetation effectiveness!
ss <- robustErrors(mm_risk_r3_high2)
effect_range <- c(coefficients(mm_risk_r3_high2)['is_high_evm'] - 
                    ss['is_high_evm']*1.645,
                  coefficients(mm_risk_r3_high2)['is_high_evm'],
                  coefficients(mm_risk_r3_high2)['is_high_evm'] +
                    ss['is_high_evm']*1.645)

# Run veg cost scenarios
cost_veg <- runVegScenarios(in_reg_data = reg_data,
                            in_voll = voll, in_hours = df_hours,
                            in_mdl = mm_risk_r3_high2,
                            ev_range = c(5, 10, 15),
                            disc_range = c(r_disc-0.015, r_disc, r_disc+0.015),
                            evm_cost_range = c(evm_cost_lo, evm_cost, evm_cost_hi),
                            evm_effectiveness = effect_range,
                            risk_range = c(0.25, 0.50, 0.75),
                            year_pv = 2023)

# Save scenarios
save(cost_veg, file='./intermediate/Intermediate Cost-Effectiveness/scenarios_veg.RData')

###############################################################################
# Run Underground Cost Scenarios
################################################################################

runUndgScenarios <- function(in_reg_data, in_voll,
                            in_mdl, in_hours,
                            ug_range,
                            disc_range, wacc_range,
                            ug_cost_range, risk_range,
                            year_pv=2023) {
 
  # Turn off summarise options
  options(dplyr.summarise.inform = FALSE)
  
  # Containers
  out_ign <- data.frame()
  out_struct <- data.frame()
  
  # Loop through lifespans of evm
  for (i in 1:length(ug_range)) {
    
    # Get units
    out_units <- getUnits(in_reg_data, horizon_evm = 10, 
                          horizon_ug = ug_range[i])
    
    # For 40-year horizon / project life, vary risk increase between 25 & 75%
    if (ug_range[i]==40) {
      
      # Loop through risk increase scenarios
      for (r in 1:length(risk_range)) {
        
        ###############################
        # Load ignition risk increase
        ###############################
        load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_', 
                         gsub('[.]', '', risk_range[r]), 
                         '_', ug_range[i],
                         '.RData'))
        
        # Construct dataset
        df_main <- constructDataset(in_units=out_units, in_risk=out_risk,
                                    in_data=in_reg_data,
                                    psps_rule = psps_criteria)
        # Get annual units
        df_units <- getAnnualInvestment(df_main)
        
        # Run function, including switch to diminish benefits of veg mgmt.
        df_ign <- estimateIgnitions(in_dat = df_main %>% 
                                      filter(is_r3==1) %>% 
                                      mutate(is_high_evm=is_high_evm_nodep,
                                             underground_units=underground_units_nodep) %>% 
                                      ungroup(),
                                    in_model = in_mdl,
                                    in_damages = df_damages,
                                    depreciate_VEG = T)
        
        # Loop through unit costs
        for (u in 1:length(ug_cost_range)) {
          
          # Loop through discount range
          for (disc in 1:length(disc_range)) {
            
            # Cost function
            df_costs <- calculateCosts(in_ign = df_ign, 
                                       in_units = df_units,
                                       r_disc=disc_range[disc], #####
                                       r_wacc=wacc_range[disc], ####
                                       c_veg=evm_cost,
                                       c_ug=ug_cost_range[u], ####
                                       c_epss_hr=epss_cost_hr, 
                                       c_psps_hr=psps_cost_hr,
                                       c_routine_veg = 400E6/25E3,
                                       maint_veg = evm_maint,
                                       maint_ug = ug_maint,
                                       year_pv = year_pv,
                                       lost_load = in_voll,
                                       outages = in_hours)
            
            # Save results
            df_costs$ug_horizon <- ug_range[i]
            df_costs$r_disc <- disc_range[disc]
            df_costs$unit_cost <- ug_cost_range[u]
            df_costs$risk_range <- risk_range[r]
            
            # Set aside
            tmp <- df_costs %>% 
              select(ug_horizon:risk_range, cost_ug_ratepayer, 
                     cost_ug_social, avoid_ug_disc,
                     cost_ign_ug_ratepayer, cost_ign_ug_social,
                     struct_ug_disc,
                     cost_struct_ug_ratepayer, cost_struct_ug_social)
            out_ign <- bind_rows(out_ign, tmp)
            
            # PRINT
            print('')
            print('Ignitions........')
            print(paste0(ug_range[i], ', ', disc_range[disc], ', ',
                         ug_cost_range[u],
                         ', ', risk_range[r]))
            
            
          }
        }
      }
      
    # For the 30 and 50-year project horizon, just model the base case 50% risk increase
    } else if (ug_range[i] != 40) {
      
      ###############################
      # Load ignition risk increase
      ###############################
      load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_', 
                       gsub('[.]', '', risk_range[2]), 
                       '_', ug_range[i],
                       '.RData'))
      
      # Construct dataset
      df_main <- constructDataset(in_units=out_units, in_risk=out_risk,
                                  in_data=in_reg_data,
                                  psps_rule = psps_criteria)
      # Get annual units
      df_units <- getAnnualInvestment(df_main)
      
      # Run function, including switch to diminish benefits of veg mgmt.
      df_ign <- estimateIgnitions(in_dat = df_main %>% 
                                    filter(is_r3==1) %>% 
                                    mutate(is_high_evm=is_high_evm_nodep,
                                           underground_units=underground_units_nodep) %>% 
                                    ungroup(),
                                  in_model = in_mdl,
                                  in_damages = df_damages,
                                  depreciate_VEG = T)
      
      # Loop through unit costs
      for (u in 1:length(ug_cost_range)) {
        
        # Loop through discount range
        for (disc in 1:length(disc_range)) {
          
          # Cost function
          df_costs <- calculateCosts(in_ign = df_ign, 
                                     in_units = df_units,
                                     r_disc=disc_range[disc], ##### scenario
                                     r_wacc=wacc_range[disc], ##### scenario
                                     c_veg=evm_cost,
                                     c_ug=ug_cost_range[u],   ##### scenario
                                     c_epss_hr=epss_cost_hr, 
                                     c_psps_hr=psps_cost_hr,
                                     c_routine_veg = 400E6/25E3,
                                     maint_veg = evm_maint,
                                     maint_ug = ug_maint,
                                     year_pv = year_pv,
                                     lost_load = in_voll,
                                     outages = in_hours)
          
          
          # Save results
          df_costs$ug_horizon <- ug_range[i]
          df_costs$r_disc <- disc_range[disc]
          df_costs$unit_cost <- ug_cost_range[u]
          df_costs$risk_range <- risk_range[2]
          
          # Set aside
          tmp <- df_costs %>% 
            select(ug_horizon:risk_range, cost_ug_ratepayer, cost_ug_social,
                   avoid_ug_disc,
                   cost_ign_ug_ratepayer, cost_ign_ug_social,
                   struct_ug_disc,
                   cost_struct_ug_ratepayer, cost_struct_ug_social)
          out_ign <- bind_rows(out_ign, tmp)
          
          # PRINT
          print('')
          print('Ignitions........')
          print(paste0(ug_range[i], ', ', disc_range[disc], ', ',
                       ug_cost_range[u],
                       ', ', risk_range[2]))
          
          
        }
      }
    }
  }
  
  # Export
  return(out_ign)
  options(dplyr.summarise.inform = TRUE)
   
}

# Run underground scenarios
cost_undg <- runUndgScenarios(in_reg_data = reg_data,
                            in_voll = voll,
                            in_hours = df_hours,
                            in_mdl = mm_risk_r3_high2,
                            ug_range = c(30, 40, 50),
                            disc_range = c(r_disc-0.015, r_disc, r_disc+0.015),
                            wacc_range = c(r_wacc-0.015, r_wacc, r_wacc+0.015),
                            ug_cost_range = c(ug_cost_lo, ug_cost, ug_cost_hi),
                            risk_range = c(0.25, 0.50, 0.75),
                            year_pv = 2023)

# Save Scenarios
save(cost_undg, file='./intermediate/Intermediate Cost-Effectiveness/scenarios_undg.RData')

################################################################################
# Run Operational / Dynamic Grid Mgmt. Cost Scenarios
################################################################################

# Function
runOperationalScenarios <- function(in_reg_data,
                                    in_voll,
                                    in_mdl,
                                    in_hours,
                                    fast_trip_cost_range,
                                    psps_cost_range,
                                    fast_trip_effectiveness,
                                    year_pv=2023) {

  # Turn off summarise options
  options(dplyr.summarise.inform = FALSE)
  
  # Containers
  out_ign <- data.frame()
  
  # Get units
  out_units <- getUnits(in_reg_data, horizon_evm = 10, 
                        horizon_ug = 40)
  
  # Load ignition risk increase
  load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_', 
                   gsub('[.]', '', 0.50), 
                   '_', 40,
                   '.RData'))
  
  # Construct dataset
  df_main <- constructDataset(in_units=out_units, in_risk=out_risk,
                              in_data=in_reg_data,
                              psps_rule = psps_criteria)
  
  # Get annual units
  df_units <- getAnnualInvestment(df_main)
  
  # Loop through fast-trip effectiveness 
  for (effect in 1:length(fast_trip_effectiveness)) {
    
    # Change effectiveness of fast-trip settings
    in_mdl$coefficients[which(names(coefficients(in_mdl))=='is_epss_enable')] <- 
      fast_trip_effectiveness[effect]
    
    # Run function, including switch to diminish benefits of veg mgmt.
    df_ign <- estimateIgnitions(in_dat = df_main %>% 
                                  filter(is_r3==1) %>% 
                                  mutate(is_high_evm=is_high_evm_nodep,
                                         underground_units=underground_units_nodep) %>% 
                                  ungroup(),
                                in_model = in_mdl,
                                in_damages = df_damages,
                                depreciate_VEG = T)
    
    # Loop through PSPS and Fast-Trip Costs
    for (u in 1:length(fast_trip_cost_range)) {
      
      # Loop through value of lost load assumptions
      voll_loop <- c('value_kwh_low', 'value_kwh', 'value_kwh_high')
      for (v in 1:length(voll_loop)) {
        tmp_voll <- in_voll
        tmp_voll[,'value_kwh'] <- in_voll[,voll_loop[v]]
        
        # Cost function
        df_costs <- calculateCosts(in_ign = df_ign, 
                                   in_units = df_units,
                                   r_disc=r_disc,
                                   r_wacc=r_wacc,
                                   c_veg=evm_cost,
                                   c_ug=ug_cost,
                                   c_epss_hr=fast_trip_cost_range[u],     ######
                                   c_psps_hr=psps_cost_range[u],
                                   c_routine_veg = 400E6/25E3,
                                   maint_veg = evm_maint,
                                   maint_ug = ug_maint,
                                   year_pv = year_pv,
                                   lost_load = tmp_voll,
                                   outages = in_hours)
        
        # Save results
        df_costs$unit_cost_fast_trip <- fast_trip_cost_range[u]
        df_costs$unit_cost_psps <- psps_cost_range[u]
        df_costs$voll <- voll_loop[v]
        df_costs$effectiveness <- fast_trip_effectiveness[effect]
        
        # Variables
        tmp <- df_costs %>% 
          select(unit_cost_fast_trip:effectiveness, cost_epss, cost_psps,
                 avoid_epss_disc, avoid_psps_disc,
                 struct_epss_disc, struct_psps_disc,
                 cost_ign_epss, cost_ign_psps,
                 cost_ign_combined,
                 cost_struct_epss, cost_struct_psps,
                 cost_struct_combined)
        
        # Store 
        out_ign <- bind_rows(out_ign, tmp)
        
        # PRINT
        print('')
        print('Ignitions........')
        print(paste0(fast_trip_cost_range[u], ', ', fast_trip_effectiveness[effect],
                     ', ', voll_loop[v]))
        
      }
    }
  }
  
  return(out_ign)
  options(dplyr.summarise.inform = TRUE)
  
}


# Get standard error for fast-trip effectiveness
effect_range_epss <- c(coefficients(mm_risk_r3_high2)['is_epss_enable'] - 
                         ss['is_epss_enable']*1.645,
                       coefficients(mm_risk_r3_high2)['is_epss_enable'],
                       coefficients(mm_risk_r3_high2)['is_epss_enable'] +
                         ss['is_epss_enable']*1.645)

cost_operational <- runOperationalScenarios(in_reg_data = reg_data,
                                            in_voll = voll, in_hours = df_hours,
                                            in_mdl = mm_risk_r3_high2,
                                            fast_trip_cost_range = c(epss_cost_hr_lo, epss_cost_hr, epss_cost_hr_hi),
                                            psps_cost_range =  c(psps_cost_hr_lo, psps_cost_hr, psps_cost_hr_hi),
                                            fast_trip_effectiveness = effect_range_epss)

# Save Scenarios
save(cost_operational, file='./intermediate/Intermediate Cost-Effectiveness/scenarios_operational.RData')
