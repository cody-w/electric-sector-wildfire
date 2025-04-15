################################################################################
## Project: Ignitions
## Purpose: Create main underground cost curve figure data
################################################################################

# Load dataset
load(file='./intermediate/Regression Dataset/reg_matched_data.RData')
rm(pdata); gc()

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

# Load wildfire probability
load(file='./intermediate/Missoula Fire Lab/wildfire_probability.RData')
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

df_length <- reg_data %>% 
  filter(date=='2020-12-30') %>% 
  select(circuit.name, tier.2.oh.miles, tier.3.oh.miles) %>% 
  mutate(tier.2.oh.miles=tier.2.oh.miles*100,
         tier.3.oh.miles=tier.3.oh.miles*100,
         hftd_miles=tier.2.oh.miles+tier.3.oh.miles)

########## PARAMETER SET
# Capex assumptions
INTEREST_RATE <- 0.05
DISCOUNT_RATE <- 0.025
YEARS <- 40
COST_UG <- 3.75E6
risk_2050 <- 0.50
psps_criteria <- 0.05
EPSS_EFFECT <- 0.82
EPSS_RANGE  <- c(0.72, 0.92)
PSPS_EFFECT <- 0.99
VEG_MI <- 15E3
ANNUAL_EPSS_HOURS <- 6.5E6
ANNUAL_PSPS_HOURS <- 12E6
COST_EPSS_HR      <- 25
COST_PSPS_HR      <- 10
VOLL <- 10
UG_MAINT <- 0.01

# Outage averages for assigning outages
x_outage <- reg_data %>%
  filter(epss.customer.hours>0 | psps_customer_hours>0) %>%
  mutate(epss.customer.hours=ifelse(epss.customer.hours==0, NA, epss.customer.hours),
         psps_customer_hours=ifelse(psps_customer_hours==0, NA, psps_customer_hours)) %>%
  group_by(circuit.name) %>%
  summarise(mean_epss=mean(epss.customer.hours, na.rm=T),
            mean_psps=mean(psps_customer_hours, na.rm=T)) %>%
  ungroup() %>%
  mutate(mean_epss=ifelse(is.na(mean_epss), mean(mean_epss,na.rm=T),
                             mean_epss),
         mean_psps=ifelse(is.na(mean_psps), mean(mean_psps,na.rm=T),
                           mean_psps))

################################################################################
# Construct dataset -------------------------------------------------------
################################################################################

# Load weather data / risk
load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                 gsub('[.]', '', risk_2050),
                 '_', 40,
                 '.RData'))

# Add expected structure damages
mergeStructuresReshape <- function(in_df,
                                   in_damages,
                                   in_length,
                                   psps_rule=0.05) {
  
  
  # Reshape
  x <- in_damages %>% 
    select(circuit.name, mean_bldg_acre, fire_size) %>% 
    spread(key=fire_size, value=mean_bldg_acre)
  
  # Names
  names(x)[names(x)!='circuit.name'] <- 
    paste0('bldg_acre_', c('small', 'medium', 'large', 'extreme'))
  
  # Merge and add PSPS definition
  in_df <- left_join(in_df %>% 
                       mutate(is_psps=ifelse(risk_score>=psps_rule, 1, 0)),
                     in_length, by='circuit.name')
  in_df <- left_join(in_df, x, by='circuit.name')
  
  # Export
  return(in_df)
  
}

# Run function
panel <- mergeStructuresReshape(in_df=out_risk, in_damages=df_damages,
                                in_length=df_length,
                                psps_rule = psps_criteria)

# Undergrounding damage calculation ---------------------------------------

estimateUndergroundingBenefit <- function(in_panel,
                                          disc_rate=DISCOUNT_RATE,
                                          epss_coef=EPSS_EFFECT,
                                          psps_coef=PSPS_EFFECT,
                                          ignition_rank=T) {
  
  
  # Calculate structure loss in expectation
  dam <- in_panel %>% 
    # Adjsut ignition risk for EPSS & PSPS
    mutate(risk_score=ifelse(is_psps==1, risk_score*(1-psps_coef), risk_score),
           risk_score=ifelse(is_r3==1, risk_score*(1-epss_coef), risk_score)) %>% 
    # Caclulate structures
    mutate(struct = ((small*bldg_acre_small*1) + (medium*bldg_acre_medium*150) + 
                       (large*bldg_acre_large*2000) + (extreme*bldg_acre_extreme*20E3)) * 
             risk_score)
  
  # Get into discount-annual format
  dam_annual <- dam %>% 
    group_by(circuit.name, hftd_miles, year) %>% 
    summarise(struct=sum(struct),
              ignition=sum(risk_score)) %>% 
    # Discount structures
    mutate(t=year-2023,
           struct_disc=struct/((1+disc_rate)^t),
           ignition_disc=ignition/((1+disc_rate)^t))
  
  # Get rank of riskiest circuits to target
  if(ignition_rank==T) {
    dam_rank <- dam_annual %>% 
      group_by(circuit.name, hftd_miles) %>% 
      summarise(struct_total=sum(struct),
                struct_disc=sum(struct_disc),
                ignition_total=sum(ignition),
                ignition_disc=sum(ignition_disc)) %>% 
      arrange(-ignition_disc)
  } else {
    dam_rank <- dam_annual %>% 
      group_by(circuit.name, hftd_miles) %>% 
      summarise(struct_total=sum(struct),
                struct_disc=sum(struct_disc),
                ignition_total=sum(ignition),
                ignition_disc=sum(ignition_disc)) %>% 
    arrange(-struct_disc)
  }
  
  
  # Export
  return(dam_rank)
}

# Run function
df_struct <- estimateUndergroundingBenefit(in_panel=panel,
                                           ignition_rank = F)

# Undergrounding cost calculation -----------------------------------------

estimateUndergroundingCost <- function(in_panel,
                                       in_miles,
                                       ug_cost=COST_UG,
                                       disc_rate=DISCOUNT_RATE,
                                       interest_rate=INTEREST_RATE,
                                       no_years=YEARS,
                                       # Additional cost parameters
                                       routine_veg_mi=VEG_MI,
                                       ug_maint=UG_MAINT,
                                       epss_hrs=ANNUAL_EPSS_HOURS,
                                       psps_hrs=ANNUAL_PSPS_HOURS,
                                       epss_cost=COST_EPSS_HR,
                                       psps_cost=COST_PSPS_HR,
                                       lost_load=VOLL,
                                       mean_outage=NA) {
  

  # Create shell
  cost_shell <- data.frame(year=rep(2024:(2024+no_years-1),length(in_miles)),
                           hftd_miles=rep(in_miles, each=no_years))
  
  # Calculate annual interest payment
  payment <- (interest_rate * (in_miles*ug_cost)) / 
    (1 - (1 + interest_rate)^(-no_years))
  cost_shell$loan <- rep(payment, each=no_years)
  
  # Get avoided routine veg costs
  cost_shell <- cost_shell %>% 
    mutate(avoid_veg=hftd_miles*routine_veg_mi)
  
  # Scale up EPSS and PSPS hours each year with increasing risk
  y <- in_panel %>% 
    group_by(year) %>% 
    summarise(total_epss=sum(is_r3), 
              total_psps=sum(is_psps)) %>% 
    mutate(min_r3 = min(total_epss),
           min_psps = min(total_psps),
           grow_r3 = total_epss/min_r3,
           grow_psps = total_psps/min_psps,
           total_epss_hours = epss_hrs,
           total_psps_hours = psps_hrs,
           # Scale up the hours each year
           total_epss_hours = total_epss_hours * grow_r3,
           total_psps_hours = total_psps_hours * grow_psps) %>% 
    select(year, total_epss, total_psps,
           total_epss_hours, total_psps_hours)
  
  # Get avoided VOLL & avoided EPSS/PSPS costs
  x <- in_panel %>% 
    group_by(circuit.name, year, hftd_miles) %>% 
    summarise(is_r3=sum(is_r3), is_psps=sum(is_psps)) %>% 
    # Bring in scaled up epss/psps hours 
    left_join(y, by='year') %>% 
    # Bring in average outage by circuit
    left_join(mean_outage, by='circuit.name') %>% 
    # impute missing average outage
    ungroup() %>% 
    mutate(mean_epss=ifelse(is.na(mean_epss), mean(mean_epss,na.rm=T), 
                            mean_epss),
           mean_psps=ifelse(is.na(mean_psps), mean(mean_psps,na.rm=T),
                            mean_psps)) %>% 
    # Create weights for allocating total outage hours to circuits
    group_by(year) %>% 
    mutate(weight_total_epss = sum(mean_epss),
           weight_total_psps = sum(mean_psps),
           weight_epss_hours = mean_epss/weight_total_epss,
           weight_psps_hours = mean_psps/weight_total_psps,
           weight_epss_event = is_r3/total_epss,
           weight_psps_event = is_psps/total_psps,
           combined_weight_epss = weight_epss_event + weight_epss_hours,
           combined_weight_psps = weight_psps_event + weight_psps_hours) %>% 
    group_by(year) %>% 
    mutate(total_epss_weight = sum(combined_weight_epss),
           total_psps_weight = sum(combined_weight_psps),
           weight_epss = combined_weight_epss / total_epss_weight,
           weight_psps = combined_weight_psps / total_psps_weight) %>% 
    # Avoided hours
    mutate(avoid_epss = weight_epss * total_epss_hours,
           avoid_psps = weight_psps * total_psps_hours) %>% 
    # Avoided value of lost load (reliability costs!)
    mutate(voll_savings = (avoid_epss+avoid_psps)*lost_load) %>% 
    # Avoided PSPS and EPSS program costs
    mutate(avoid_epss_cost=avoid_epss*epss_cost,
           avoid_psps_cost=avoid_psps*psps_cost) %>% 
    # Get totals for merge
    mutate(avoid_op_cost=avoid_epss_cost+avoid_psps_cost) %>% 
    select(circuit.name:hftd_miles, voll_savings, avoid_op_cost)
  
  # Bring together and discount
  cost_shell <- cost_shell %>% 
    filter(year<max(x$year)) %>% 
    left_join(x, by=c('year', 'hftd_miles')) %>% 
    mutate(t=year-2023,
           ug_maintenance=hftd_miles * ug_cost * ug_maint,
           total_cost=loan + ug_maintenance - 
             avoid_veg - voll_savings - avoid_op_cost,
           cost_disc=total_cost/((1+disc_rate)^t)) %>% 
    group_by(circuit.name,hftd_miles) %>% 
    summarise(total_cost=sum(total_cost),
              loan=sum(loan),
              ug_maintenance=sum(ug_maintenance),
              voll_savings=sum(voll_savings),
              avoid_op_cost=sum(avoid_op_cost),
              avoid_veg=sum(avoid_veg),
              cost_disc=sum(cost_disc))
  
  # Export
  return(cost_shell)
  
}

df_cost <- estimateUndergroundingCost(in_panel=panel,
                                      in_miles=df_struct$hftd_miles,
                                      no_years=YEARS,
                                      mean_outage = x_outage)

# Net benefit curve -------------------------------------------------------

costEffectiveness <- function(in_struct, in_cost,
                              ignition_rank=T) {
  
  # Merge
  net <- full_join(in_struct, in_cost, 
                   by=c('circuit.name', 'hftd_miles'))
  
  # Cost-effectiveness
  if(ignition_rank==T) {
    net <- net %>% 
      mutate(cost_ignition=cost_disc/ignition_disc) %>% 
      arrange(cost_ignition)
    
    net <- net %>% 
      ungroup() %>% 
      mutate(running_hftd = cumsum(hftd_miles),
             running_ignition = cumsum(ignition_disc),
             max_ignition=max(running_ignition),
             remaining_ignition=max_ignition-running_ignition,
             running_cost = cumsum(cost_disc)) %>% 
      select(-max_ignition)
    
  } else {
    net <- net %>% 
      mutate(cost_struct=cost_disc/struct_disc) %>% 
      arrange(cost_struct)
    
    # Calculate running amounts
    net <- net %>% 
      ungroup() %>% 
      mutate(running_hftd = cumsum(hftd_miles),
             running_struct = cumsum(struct_disc),
             max_struct=max(running_struct),
             remaining_struct=max_struct-running_struct,
             running_cost = cumsum(cost_disc)) %>% 
      select(-max_struct)
  }
  
  # Export
  return(net)
  
}

# Run function
main <- costEffectiveness(in_struct=df_struct, in_cost=df_cost, 
                          ignition_rank = F)

# Run version of cost curve with Fast-Trip --------------------------------

# STRUCTURE BASIS
df_struct <- estimateUndergroundingBenefit(in_panel=panel, 
                                           epss_coef = EPSS_EFFECT,
                                           ignition_rank = F)
df_cost <- estimateUndergroundingCost(in_panel=panel,
                                      in_miles=df_struct$hftd_miles,
                                      no_years=YEARS, 
                                      epss_cost=COST_EPSS_HR,
                                      epss_hrs = ANNUAL_EPSS_HOURS,
                                      mean_outage = x_outage)
main <- costEffectiveness(in_struct=df_struct, in_cost=df_cost, 
                          ignition_rank = F)

# Run version of cost curve without Fast-Trip --------------------------------

# STRUCTURE BASIS
df_struct <- estimateUndergroundingBenefit(in_panel=panel,
                                           epss_coef = 0,
                                           ignition_rank = F)
df_cost <- estimateUndergroundingCost(in_panel=panel,
                                      in_miles=df_struct$hftd_miles,
                                      no_years=YEARS, 
                                      epss_cost = 0,
                                      epss_hrs = 0,
                                      mean_outage = x_outage)
main_nofast <- costEffectiveness(in_struct=df_struct, in_cost=df_cost, 
                                 ignition_rank = F)

# Save results
save(main, main_nofast, file='./intermediate/Intermediate Cost-Effectiveness/cost_curves_structure_basis.RData')

# Monte-carlo analysis ----------------------------------------------------

# Function for montecarlo
monteCarlo <- function(
  in_risk,
  in_damages,
  seed_monte=2000,
  n_draws=100,
  cost_range=0.5E6,
  disc_range=c(DISCOUNT_RATE-.02,
               DISCOUNT_RATE+.02),
  interest_range=c(INTEREST_RATE-.02,
                   INTEREST_RATE+.02),
  voll_range=2.5,
  epss_effect_range=0.05,
  epss_cost_range=c(15, 25, 35),
  psps_cost_range=c(5, 10, 15),
  fast_trip_switch=F,
  ignit_switch=T) {
  
  # Set seed
  set.seed(seed_monte)
  
  ### Create parameter space ####
  tmp1 <- rnorm(n=n_draws, mean=DISCOUNT_RATE, sd = 0.01)
  tmp2 <- rnorm(n=n_draws, mean=COST_UG, sd=cost_range)
  tmp3 <- rnorm(n=n_draws, mean=VOLL, sd=voll_range)
  tmp4 <- rnorm(n=n_draws, mean=EPSS_EFFECT, sd=epss_effect_range)
  tmp5 <- sample(1:1E3, size=n_draws)
  tmp6 <- sample(epss_cost_range, size=n_draws, replace = T)
  tmp7 <- sample(psps_cost_range, size=n_draws, replace = T)
  
  # Container for parameters
  params <- data.frame(draw=1:n_draws,
                       di=tmp1, co_ug=tmp2, vll=tmp3,
                       eps=tmp4, sed=tmp5,
                       eps_c=tmp6, ps_c=tmp7) %>%
    mutate(inter=di+0.025,
           di=ifelse(di<0, 0.001, di),
           vll=ifelse(vll<0, 0.01, vll),
           eps=ifelse(eps>1, 0.99, eps))
  
  
  ######## MONTE CARLO LOOP ########
  monte <- data.frame()
  monte80 <- data.frame()
  out_param <- data.frame()
  
  for (i in 1:n_draws) {
    
    # Get paramaters for current draw
    tmp_param <- params %>% filter(draw==i)
    
    ### Loop through buildings per acre ###
    draw_damages <- data.frame()
    for(g in 1:nrow(in_damages)) {
      
      # Pull first row
      d <- in_damages[g,]
      x <- rnorm(n=1, mean=d$mean_bldg_acre, sd=d$sd_bldg_acre/3)
      x <- ifelse(x<0, 0.01, x)
      tmp <- data.frame(circuit.name=d$circuit.name[1],
                        fire_size=d$fire_size[1],
                        mean_bldg_acre=x)
      
      # Store
      draw_damages <- bind_rows(draw_damages, tmp)
    }
    
    ### Bring in damages to panel set
    # Run function
    panel <- mergeStructuresReshape(in_df=in_risk, in_damages=df_damages,
                                    in_length=df_length,
                                    psps_rule = psps_criteria)
    
    ### Draw discount rate, EPSS effectiveness, interest rate
    disc <- tmp_param$di[1]
    intr <- tmp_param$inter[1]
    epss <- tmp_param$eps[1]
    undg <- tmp_param$co_ug[1]
    value_load <- tmp_param$vll[1]
    p_cost <- tmp_param$ps_c[1]
    ep_cost <- tmp_param$eps_c[1]
    
    ### Structures ###
    if(fast_trip_switch==F) {
      df_struct <- estimateUndergroundingBenefit(in_panel=panel,
                                                 disc_rate = disc,
                                                 epss_coef = epss,
                                                 ignition_rank = ignit_switch)
      
      ### Cost
      df_cost <- estimateUndergroundingCost(in_panel=panel, 
                                            in_miles=df_struct$hftd_miles,
                                            ug_cost = undg,
                                            disc_rate = disc,
                                            interest_rate = intr,
                                            epss_cost = ep_cost,
                                            psps_cost = p_cost,
                                            lost_load = value_load,
                                            mean_outage = x_outage)
      
      ### Combine cost-effectiveness ###
      main_tmp <- costEffectiveness(in_struct=df_struct, in_cost=df_cost,
                                    ignition_rank = ignit_switch)
      
    } else if (fast_trip_switch==T) {
      df_struct <- estimateUndergroundingBenefit(in_panel=panel,
                                                 disc_rate = disc,
                                                 epss_coef = 0,
                                                 ignition_rank = ignit_switch)
      
      ### Cost
      df_cost <- estimateUndergroundingCost(in_panel=panel, 
                                            in_miles=df_struct$hftd_miles,
                                            ug_cost = undg,
                                            disc_rate = disc,
                                            interest_rate = intr,
                                            epss_cost = 0,
                                            psps_cost = 0,
                                            epss_hrs = 0,
                                            mean_outage = x_outage)
      
      ### Combine cost-effectiveness ###
      main_tmp <- costEffectiveness(in_struct=df_struct, in_cost=df_cost,
                                    ignition_rank = ignit_switch)
      
    }
    
    
    ### Record draws ###
    main_tmp$draw <- i
    monte <- bind_rows(monte, main_tmp)
    print(i)
    print(undg); print(disc); print(epss); print(p_cost); print(ep_cost)
    print('')
    tmp_param <- data.frame(draw=i, e=epss, d=disc, c_ug=undg, vll=value_load,
                            c_eps=ep_cost, p_eps=p_cost)
    out_param <- bind_rows(out_param, tmp_param)
    
  }
  
  
  return(list(monte, out_param, params))
  
  
}

###################
# Structure basis #
###################

# Run monte carlo without fast-trip 
system.time({
  
  # Central risk assumption
  load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                   gsub('[.]', '', 0.50),'_', 40, '.RData'))
  tmp_notrip <- monteCarlo(in_risk = out_risk,
                                 in_damages=df_damages,
                                 seed_monte=2000,
                                 n_draws=100, fast_trip_switch = T,
                                 ignit_switch = F)
  # Low risk assumption
  load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                   gsub('[.]', '', 0.25),'_', 40, '.RData'))
  tmp_lo_notrip <- monteCarlo(in_risk = out_risk,
                    in_damages=df_damages,
                    seed_monte=2000,
                    n_draws=50, fast_trip_switch = T,
                    ignit_switch = F)
  
  # High risk assumption
  load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                   gsub('[.]', '', 0.75),'_', 40, '.RData'))
  tmp_hi_notrip <- monteCarlo(in_risk = out_risk,
                       in_damages=df_damages,
                       seed_monte=2000,
                       n_draws=50, fast_trip_switch = T,
                       ignit_switch = F)
})

# Run monte carlo with fast-trip
system.time({
  
  # Central risk assumption
  load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                   gsub('[.]', '', 0.50),'_', 40, '.RData'))
  tmp <- monteCarlo(in_risk = out_risk,
                          in_damages=df_damages,
                          seed_monte=2000,
                          n_draws=100, fast_trip_switch = F,
                          ignit_switch = F)
  
  # Low risk assumption
  load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                   gsub('[.]', '', 0.25),'_', 40, '.RData'))
  tmp_lo <- monteCarlo(in_risk = out_risk,
                    in_damages=df_damages,
                    seed_monte=2000,
                    n_draws=50, fast_trip_switch = F,
                    ignit_switch = F)
  
  # High risk assumption
  load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                   gsub('[.]', '', 0.75),'_', 40, '.RData'))
  tmp_hi <- monteCarlo(in_risk = out_risk,
                    in_damages=df_damages,
                    seed_monte=2000,
                    n_draws=50, fast_trip_switch = F,
                    ignit_switch = F)
})

# Save Monte Carlo
save(tmp_notrip, tmp_lo_notrip, tmp_hi_notrip,
     tmp, tmp_lo, tmp_hi,
     file='./intermediate/Intermediate Cost-Effectiveness/monte_structure.RData')

################################################################################
# PLOT COST CURVE ---------------------------------------------------------
# Structure BASIS
################################################################################

# Load structure results
load(file='./intermediate/Intermediate Cost-Effectiveness/monte_structure.RData')

# Combine each risk run
out_monte_notrip <- bind_rows(tmp_notrip[[1]], tmp_lo_notrip[[1]]) %>% 
  bind_rows(tmp_hi_notrip[[1]])
out_monte <- bind_rows(tmp[[1]], tmp_lo[[1]]) %>% 
  bind_rows(tmp_hi[[1]])

# Get parameter distribution
param_notrip <- bind_rows(tmp_notrip[[3]], tmp_lo_notrip[[3]]) %>% 
  bind_rows(tmp_hi_notrip[[3]])
param_trip <- bind_rows(tmp[[3]], tmp_lo[[3]]) %>% 
  bind_rows(tmp_hi[[3]])

# Get confidence intervals
ci_high <- 0.95
ci_low  <- 1-ci_high

# Confidence intervals NO FAST TRIP - loop through steps 
steps <- seq(0, 25E3, 1000)
graph_cis_notrip <- data.frame()
monte <- out_monte_notrip %>% 
  mutate(cost_ignition=ifelse(cost_struct<0, 0, cost_struct))
for (i in 2:length(steps)) {
  
  # Subset
  tmp <- monte %>% 
    filter(running_hftd>=steps[i-1] & running_hftd<steps[i])
  tmp_cost_ci <- quantile(tmp$cost_struct, c(ci_low, ci_high))
  tmp_median <- mean(tmp$cost_struct)
  #tmp_struct_ci <- quantile(tmp$remai, c(ci_low, ci_high))
  
  # Get total cost
  tmp_total_cost_ci <- quantile(tmp$total_cost, c(ci_low, ci_high))
  tmp_total_median <- mean(tmp$total_cost)
  
  graph_cis_notrip <- bind_rows(graph_cis_notrip, 
                                data.frame(running_hftd=mean(c(steps[i-1], steps[i])),
                                           #  ci_struct_low=tmp_struct_ci[1],
                                           #  ci_struct_upper=tmp_struct_ci[2],
                                           ci_cost_low=tmp_cost_ci[1],
                                           ci_cost_upper=tmp_cost_ci[2],
                                           ci_cost_median=tmp_median,
                                           
                                           # Total cost
                                           ci_total_cost_low=tmp_total_cost_ci[1],
                                           ci_total_cost_upper=tmp_total_cost_ci[2],
                                           ci_total_cost_median=tmp_total_median))
  
}

# Get 8000 mile mark confidence intervals
ci_cost_80_notrip <- monte %>% 
  mutate(diff=abs(running_hftd-8000)) %>% 
  group_by(draw) %>% 
  mutate(min_diff=min(diff)) %>% 
  filter(diff==min_diff)
ci_cost_80_notrip <- quantile(ci_cost_80_notrip$cost_struct, c(ci_low, ci_high))

########################################################
### Estimate reduction in total cost of undergrounding
########################################################
z <- out_monte %>% 
  mutate(diff=abs(running_hftd-8000)) %>% 
  group_by(draw) %>% 
  mutate(min_diff=min(diff)) %>% 
  filter(diff==min_diff)

x_reduce <- data.frame()
for (i in 1:nrow(z)) {
  tmp <- z[i,]
  
  # Filter to equivalent risk reduction in structures
  tmp_no_trip <- out_monte_notrip %>% 
    filter(draw==tmp$draw) %>% 
    mutate(diff=abs(running_struct-tmp$running_struct)) %>% 
    mutate(min_diff=min(diff)) %>% 
    filter(diff==min_diff)
  
  # Get amount of total cost reduced
  tmp_out <- data.frame(draw=tmp$draw,
                        reduce_cost=tmp$running_cost - tmp_no_trip$running_cost)
  x_reduce <- bind_rows(x_reduce, tmp_out)
  
}

quantile(x_reduce$reduce_cost/1E9, c(ci_low, 0.5, ci_high))

# Repeat for with fast-trip -----------------------------------------------

# Confidence intervals - loop through steps 
steps <- seq(0, 25E3, 1000)
graph_cis_trip <- data.frame()
monte <- out_monte
for (i in 2:length(steps)) {
  
  # Subset
  tmp <- monte %>% 
    filter(running_hftd>=steps[i-1] & running_hftd<steps[i])
  tmp_cost_ci <- quantile(tmp$cost_struct, c(ci_low, ci_high))
  tmp_median <- median(tmp$cost_struct)

  graph_cis_trip <- bind_rows(graph_cis_trip, 
                              data.frame(running_hftd=mean(c(steps[i-1], steps[i])),
                                         ci_cost_low=tmp_cost_ci[1],
                                         ci_cost_upper=tmp_cost_ci[2],
                                         ci_cost_median=tmp_median))
  
}

# Get 8000 mile mark confidence intervals
ci_cost_80_trip <- monte %>% 
  mutate(diff=abs(running_hftd-8000)) %>% 
  group_by(draw) %>% 
  mutate(min_diff=min(diff)) %>% 
  filter(diff==min_diff)
ci_cost_80_trip <- quantile(ci_cost_80_trip$cost_struct, c(ci_low, ci_high))

# Avoid negatives
graph_cis_trip <- graph_cis_trip %>% 
  mutate(ci_cost_low=ifelse(ci_cost_low<0, 0, ci_cost_low))

#######################
# Plot cost curve
#######################
ggplot(data=graph_cis_notrip, aes(x=running_hftd/1E3, 
                      y=cost_struct/1E6)) +
  # Confidence intervals
  geom_ribbon(data=graph_cis_notrip, aes(ymin=ci_cost_low/1E6,
                                         ymax=ci_cost_upper/1E6,
                                         x=running_hftd/1E3,
                                         y=0),
              fill='gray20', alpha=0.2) +
  # Confidence intervals
  geom_ribbon(data=graph_cis_trip, aes(ymin=ci_cost_low/1E6,
                                       ymax=ci_cost_upper/1E6,
                                       x=running_hftd/1E3,
                                       y=0),
              fill='gray20', alpha=0.2) +
  # Main
  geom_line(data=graph_cis_trip, aes(x=running_hftd/1E3, 
                                     y=ci_cost_median/1E6),
            linewidth=1, color=color_orange, linetype='solid') +
  # Main without fast-trip
  geom_line(data=graph_cis_notrip, aes(x=running_hftd/1E3, 
                                       y=ci_cost_median/1E6),
            linewidth=1, color=color_orange, linetype='dashed') +
  # Vertical line
  geom_vline(xintercept = 8, linewidth=.75) +
  # Text box
  annotate("label", x=14, y=200,
           label='Includes fast-trip settings and\npreventative power shutoffs',
           hjust=0, fill=alpha('white', alpha=0.9),
           size=10, lineheight=unit(0.3, 'cm')) +
  annotate("label", x=16.4, y=15,
           label='Does not include fast-trip settings\nand preventative power shutoffs',
           hjust=0, fill=alpha('white', alpha=0.9),
           size=10, lineheight=unit(0.3, 'cm')) +
  
  # Theme
  theme_matplotlib() +
  theme(axis.title = element_text(lineheight = unit(0.3, 'cm')),
        axis.text = element_text(size=36)) +
  labs(x='Overhead HFTD Circuit-Miles Buried Underground \n(Thousands of Miles)',
       y='Cost per Avoided Structure Burned \n(Millions of Dollars)') +
  scale_y_continuous(expand=c(0.01,0.01)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0, 25, 4)) +
  coord_cartesian(ylim=c(0,210), xlim=c(0.3, 24))
ggsave('./plots/Main figures/cost_curve_structure.png', w=8, h=5)

################################################################################
# PLOT AVOIDED STRUCTURES ---------------------------------------------------------
# Structure BASIS
################################################################################

# Confidence intervals NO FAST TRIP - loop through steps 
steps <- seq(0, 25E3, 1000)
graph_cis_notrip <- data.frame()
monte <- out_monte_notrip
for (i in 2:length(steps)) {
  
  # Subset
  tmp <- monte %>% 
    filter(running_hftd>=steps[i-1] & running_hftd<steps[i])
  tmp_cost_ci <- quantile(tmp$remaining_struct, c(ci_low, ci_high))
  tmp_median <- mean(tmp$remaining_struct)

  graph_cis_notrip <- bind_rows(graph_cis_notrip, 
                                data.frame(running_hftd=mean(c(steps[i-1], steps[i])),
                                           #  ci_struct_low=tmp_struct_ci[1],
                                           #  ci_struct_upper=tmp_struct_ci[2],
                                           ci_cost_low=tmp_cost_ci[1],
                                           ci_cost_upper=tmp_cost_ci[2],
                                           ci_cost_median=tmp_median))
  
}

# Get 8000 mile mark confidence intervals
ci_cost_80_notrip <- monte %>% 
  mutate(diff=abs(running_hftd-8000)) %>% 
  group_by(draw) %>% 
  mutate(min_diff=min(diff)) %>% 
  filter(diff==min_diff)
ci_cost_80_notrip <- quantile(ci_cost_80_notrip$remaining_struct, c(ci_low, ci_high))


# Repeat for with fast-trip -----------------------------------------------

# Confidence intervals - loop through steps 
steps <- seq(0, 25E3, 1000)
graph_cis_trip <- data.frame()
monte <- out_monte
for (i in 2:length(steps)) {
  
  # Subset
  tmp <- monte %>% 
    filter(running_hftd>=steps[i-1] & running_hftd<steps[i])
  tmp_cost_ci <- quantile(tmp$remaining_struct, c(ci_low, ci_high))
  tmp_median <- median(tmp$remaining_struct)
  #tmp_struct_ci <- quantile(tmp$remai, c(ci_low, ci_high))
  
  graph_cis_trip <- bind_rows(graph_cis_trip, 
                              data.frame(running_hftd=mean(c(steps[i-1], steps[i])),
                                         #  ci_struct_low=tmp_struct_ci[1],
                                         #  ci_struct_upper=tmp_struct_ci[2],
                                         ci_cost_low=tmp_cost_ci[1],
                                         ci_cost_upper=tmp_cost_ci[2],
                                         ci_cost_median=tmp_median))
  
}

# Get 8000 mile mark confidence intervals
ci_cost_80_trip <- monte %>% 
  mutate(diff=abs(running_hftd-8000)) %>% 
  group_by(draw) %>% 
  mutate(min_diff=min(diff)) %>% 
  filter(diff==min_diff)
ci_cost_80_trip <- quantile(ci_cost_80_trip$remaining_struct, c(ci_low, ci_high))

# Avoid negatives
graph_cis_trip <- graph_cis_trip %>% 
  mutate(ci_cost_low=ifelse(ci_cost_low<0, 0, ci_cost_low))

#######################
# Plot cost curve
#######################
ggplot(data=monte, aes(x=running_hftd/1E3, 
                      y=cost_struct/1E6)) +
  # Confidence intervals
  geom_ribbon(data=graph_cis_notrip, aes(ymin=ci_cost_low/1E3,
                                         ymax=ci_cost_upper/1E3,
                                         x=running_hftd/1E3,
                                         y=0),
              fill='gray20', alpha=0.2) +
  # Confidence intervals
  geom_ribbon(data=graph_cis_trip, aes(ymin=ci_cost_low/1E3,
                                       ymax=ci_cost_upper/1E3,
                                       x=running_hftd/1E3,
                                       y=0),
              fill='gray20', alpha=0.2) +
  # Main
  geom_line(data=graph_cis_trip, aes(x=running_hftd/1E3, 
                                     y=ci_cost_median/1E3),
            linewidth=1, color=color_bu, linetype='solid') +
  # Main without fast-trip
  geom_line(data=graph_cis_notrip, aes(x=running_hftd/1E3, 
                                       y=ci_cost_median/1E3),
            linewidth=1, color=color_bu, linetype='dashed') +
  # Vertical line
  geom_vline(xintercept = 8, linewidth=.75) +
  # Text box
  annotate("label", x=0.75, y=1,
           label='Includes fast-trip settings and\npreventative power shutoffs',
           hjust=0, fill=alpha('white', alpha=0.9),
           size=10, lineheight=unit(0.3, 'cm')) +
  annotate("label", x=0.75, y=19,
           label='Does not include fast-trip settings\nand preventative power shutoffs',
           hjust=0, fill=alpha('white', alpha=0.9),
           size=10, lineheight=unit(0.3, 'cm')) +
  
  # Theme
  theme_matplotlib() +
  theme(axis.title = element_text(lineheight = unit(0.3, 'cm')),
        axis.text = element_text(size=36)) +
  labs(x='Overhead HFTD Circuit-Miles Buried Underground \n(Thousands of Miles)',
       y='Structures Burned \n(Thousands of Structures,\nDiscounted over 40 years)') +
  scale_y_continuous(expand=c(0.01,0.01)) +
  scale_x_continuous(expand=c(0,0), breaks=seq(0, 25, 4)) +
  coord_cartesian(ylim=c(0,20), xlim=c(0.3, 24))
ggsave('./plots/Main figures/damage_curve_structure.png', w=8, h=5)