################################################################################
## Project: Ignitions
## Purpose: Match treat/control, run regression models
################################################################################

# Load dataset
load(file='./intermediate/Regression Dataset/regression_dataset_clean_full.RData')

# Update EVM share
df <- df %>%
  mutate(evm_share=evm_units/(non.hftd.oh.miles+tier.2.oh.miles+tier.3.oh.miles))

################################################################################
# Identify doses / treatment groups for vegetation management
################################################################################

if(SWITCH_NEW_LOAD) {
  # high-dose is 50% or more of circuit treated with veg. mgmt.
  # low-dose is 10-50%
  dose <- df %>%
    mutate(evm_share=evm_units/(non.hftd.oh.miles+tier.2.oh.miles+tier.3.oh.miles)) %>%
    filter(month(date)==12 & day(date)==31) %>%
    mutate(treat_group=ifelse(evm_share>=0.5, 'High-dose',
                              ifelse(evm_share>=0.1 & evm_share<0.5, 'Low-dose',
                                     'No dose'))) %>%
    select(circuit.name, year, treat_group, hftd_share) %>%
    mutate(is_hftd=ifelse(hftd_share>0, 'HFTD', 'Non-HFTD'))
  
  # Identify high-dose year
  x <- dose %>%
    filter(treat_group=='High-dose') %>%
    group_by(circuit.name) %>%
    mutate(min_year=min(year)) %>%
    filter(year==min_year) %>%
    mutate(treat_group_year = paste0('High-dose ', year)) %>%
    select(circuit.name, treat_group_year)
  
  # Merge in group-year
  dose <- left_join(dose, x) %>%
    # Drop circuits without length
    filter(!is.na(hftd_share))
  
  # Find no-dose never treated
  x <- dose %>%
    filter(year==2022) %>%
    filter(treat_group=='No dose') %>%
    select(circuit.name) %>%
    mutate(treat_group_nodose = 'No dose')
  
  # Merge in no dose
  dose <- left_join(dose, x) %>%
    mutate(treat_group_year=ifelse(is.na(treat_group_year), treat_group_nodose,
                                   treat_group_year))
  
  # Find low-dose
  x <- dose %>%
    filter(is.na(treat_group_year)) %>%
    filter(treat_group=='Low-dose') %>%
    filter(year!=2018) %>%
    group_by(circuit.name) %>%
    mutate(min_year=min(year)) %>%
    filter(year==min_year) %>%
    mutate(treat_group_lowdose = paste0('Low-dose ', year)) %>%
    select(circuit.name, treat_group_lowdose)
  
  # merge in low dose
  dose <- left_join(dose, x) %>%
    mutate(treat_group_year=ifelse(is.na(treat_group_year), treat_group_lowdose,
                                   treat_group_year)) %>%
    select(-treat_group_nodose, -treat_group_lowdose) %>%
    select(circuit.name, treat_group_year) %>%
    unique()
  save(dose, file='./intermediate/Regression Dataset/dose.RData')
  
} else {
  # Load doses
  load(file='./intermediate/Regression Dataset/dose.RData')
}

# Merge in to dataset
df <- left_join(df, dose)
rm(dose);gc()

################################################################################
# Prepare data for EVM caliper matching
################################################################################

# Write function - same prepping approach for ignition risk model
prepData <- function(data, IGN_TYPE='VEG',
                     HIST_IGN=T, W_VARS, H_VARS, O_VARS,
                     HFTD_SHARE) {
  
  # Subset to relevant variables
  data <- data %>%
    mutate(month=month(date)) %>% 
    select(circuit.name, date, year,
           month, treat_group_year,
           hftd_share,
           any_of(W_VARS),
           any_of(tier_length_keep),
           any_of(H_VARS),
           total.ug.miles,
           any_of(O_VARS),
           'evm_share',
           'evm_units',
           'is_ignition',
           'is_vegequip_ignition',
           'is_veg_ignition',
           'is_hftd_ignition',
           'is_epss_ignition',
           'is_psps',
           'is_epss',
           'region') %>%
    # Drop non-HFTD
    filter(hftd_share>=HFTD_SHARE)
  
  # Add cumulative vegetation and other ignitions
  data <- data %>%
    group_by(circuit.name) %>%
    mutate(cum_veg_ignitions=cumsum(is_veg_ignition),
           cum_ignitions=cumsum(is_ignition),
           cum_ignitions=cum_ignitions-cum_veg_ignitions)
  
  # Only focus on vegetation ignitions
  if(IGN_TYPE=='VEG') {
    data <- data %>%
      filter(!(is_ignition>=1 & is_veg_ignition==0)) %>% 
      mutate(is_ignition=is_veg_ignition)
  }
  
  # Only focus on vegetation and equipment ignitions
  if(IGN_TYPE=='VEGEQUIP') {
    data <- data %>%
      filter(!(is_ignition>=1 & is_vegequip_ignition==0))
  }
  
  # Drop any nas
  data <- stats::na.omit(data)
  return(data)
  
}

# Control group HFTD share?
# Require circuits to have at least 1% overlap with HFTD
h_share <- 0.01

#### Ignition type
# specify all ignitions or vegetation-caused
# ALL, VEG
ignition_type <- 'VEG'

#### Weather variables
w_vars <- enviro_vars_keep

#### Hardening variables
h_vars <- c('covered_units', 'underground_units')

#### Operational variables
o_vars <- c('psps_customer_hours', 'epss.customer.hours')

# Matching variables
p_covars <- c(w_vars, tier_length_keep, h_vars, o_vars, 'total.ug.miles')

# Pscore parameters
d_method <- 'randomforest'
c_caliper <- 0.1
n_match <- 1

# Run prep data function
reg_data <- prepData(df, IGN_TYPE = ignition_type, HIST_IGN = T,
                     W_VARS = w_vars, H_VARS = h_vars, O_VARS = o_vars,
                     HFTD_SHARE = h_share)

# Bring in fitted fire potential index
load('./intermediate/Fire potential index/fitted_fpi_forest.RData')
reg_data <- left_join(reg_data, out)

# Bring in risk score
load('./intermediate/Intermediate Ignitions/risk_score_output.RData')
reg_data <- left_join(reg_data, risk)
rm(risk);gc()

# Identify circuits that had fast-trip piloted in 2021
epss_2021_circs <- reg_data %>%
  filter(year==2021) %>%
  filter(is_epss==1)
epss_2021_circs <- unique(epss_2021_circs$circuit.name)

# Identify if EPSS is enabled
reg_data <- reg_data %>%
  # Ensure R3 conditions if PSPS event observed
  mutate(is_r3 = ifelse(is_psps==1, 1, is_r3)) %>% 
  mutate(is_epss_enable = ifelse(year<2021, 0,
                                 ifelse(is_r3==1, 1, 0)),
         is_epss_enable = ifelse(year==2021 &
                                   !(circuit.name %in% epss_2021_circs),
                                 0, is_epss_enable),
         is_epss_enable = ifelse(date<'2021-07-28', 0, is_epss_enable))

# Ensure EPSS or PSPS is not switched on when an ignition occurs
# and there is documentation that no ignition had occurred
reg_data <- reg_data %>% 
  mutate(is_epss_enable=ifelse(is_epss==1, 1, is_epss_enable),
         is_epss_enable=ifelse(is_epss_enable==1 & is_ignition>=1 &
                                 is_epss_ignition==0, 
                               0, is_epss_enable),
         is_psps=ifelse(is_ignition>=1, 0, is_psps))

# Make sure R3 switch if EPSS event
reg_data <- reg_data %>% 
  mutate(is_r3=ifelse(is_epss==1, 1, is_r3))

# Regression vars
reg_vars <- c(w_vars, tier_length_keep, h_vars, 'is_psps',
              'is_epss_enable')

# Create dummy variables for levels of treatment - high-dose
tmp <- reg_data %>% 
  filter(grepl('High-dose', treat_group_year)) %>% 
  filter(evm_share>=0.5) %>% 
  group_by(circuit.name) %>% 
  mutate(min_date=min(date)) %>% 
  filter(date==min_date) %>% 
  select(circuit.name, treat_group_year, min_date)

# Create dummy variables for levels of treatment - low-dose
tmplo <- reg_data %>% 
  filter(grepl('Low-dose', treat_group_year)) %>% 
  filter(evm_share>=0.1) %>% 
  group_by(circuit.name) %>% 
  mutate(min_date=min(date)) %>% 
  filter(date==min_date) %>% 
  select(circuit.name, treat_group_year, min_date) 
tmp <- bind_rows(tmp, tmplo); rm(tmplo)

# Combine
reg_data <- left_join(reg_data, tmp)

# Treatment variable
reg_data <- reg_data %>% 
  mutate(is_high_evm = ifelse(date>=min_date & grepl('High', treat_group_year),
                              1, 0),
         is_mid_evm = ifelse(date>=min_date & grepl('Low', treat_group_year), 
                             1, 0))
# Any treatment variable
reg_data <- reg_data %>% 
  mutate(is_any_high = ifelse(grepl('High', treat_group_year), 1, 0),
         is_any_mid  = ifelse(grepl('Low', treat_group_year), 1, 0),
         is_any_treat = is_any_high + is_any_mid)

# Update low-dose treatment variable 
reg_data <- reg_data %>% 
  mutate(is_mid_evm = ifelse(is_mid_evm==0 & evm_share<0.5 & evm_share>=0.1, 1, is_mid_evm))

# Check for missing risk score
x <- reg_data %>% 
  filter(is.na(risk_score))
table(x$circuit.name)
reg_data <- reg_data %>% 
  filter(!is.na(risk_score))

################################################################################
# Regression specification
################################################################################

load(file='../../intermediate_24/reg_data_for_bootstrap_Aug2024.RData')

risk_binary <- formula(paste0('is_ignition ~ is_high_evm + is_mid_evm + ',
                              'is_any_high + is_any_mid + ',
                            'underground_units + covered_units + is_psps + is_epss_enable',
                            ' + risk_score'))

risk_binary_fes <- formula(paste0('is_ignition ~ is_high_evm + is_mid_evm + ', 
                              'is_any_high + is_any_mid + ',
                              'underground_units + covered_units + is_psps + is_epss_enable',
                              ' + risk_score + factor(region)'))

# Update underground units to tens of miles
reg_data <- reg_data %>% 
  mutate(underground_units=underground_units*10)

# Predict with binary treatment - no matching
mm_risk <- glm(formula = risk_binary, data=reg_data %>% 
                 mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
               family=binomial(link='logit'))
summary(mm_risk)

################################################################################
# Matching methods -- match directly on risk score
################################################################################

# Write function to match directly on risk score
matchRiskScore <- function(dat, std=0.1, n=1) {
  #dat <- reg_data
  #std <- 0.1
  #n   <- 1
  
  # Get average risk scores
  risk <- dat %>%
    #filter(year<=2019) %>% 
    group_by(circuit.name, treat_group_year) %>%
    summarise(risk_score=mean(risk_score))
  
  # Find standard deviation of risk score
  s_hat <- sd(risk$risk_score)
  
  # Find control circuits
  d_cont <- risk %>% 
    filter(treat_group_year=='No dose')
  
  # Find high circuits
  d_high <- risk %>% 
    filter(grepl('High', treat_group_year))
  
  # Find mid circuits
  d_mid <- risk %>% 
    filter(grepl('Low', treat_group_year))
  
  # Combine for high
  d_high <- crossing(circuit.high = d_high$circuit.name,
                      circuit.control = d_cont$circuit.name)
  d_high <- left_join(d_high, risk %>% select(-treat_group_year),
                      by=c('circuit.high'='circuit.name')) %>% 
    rename(risk_treat=risk_score)
  d_high <- left_join(d_high, risk %>% select(-treat_group_year),
                      by=c('circuit.control'='circuit.name')) %>% 
    rename(risk_control=risk_score,
           circuit.treat=circuit.high) %>% 
    mutate(treat_group='High')
  
  # Combine for mid
  d_mid  <- crossing(circuit.mid =  d_mid$circuit.name,
                      circuit.control = d_cont$circuit.name)
  d_mid <- left_join(d_mid, risk %>% select(-treat_group_year),
                     by=c('circuit.mid'='circuit.name')) %>% 
    rename(risk_treat=risk_score)
  d_mid <- left_join(d_mid, risk %>% select(-treat_group_year),
                      by=c('circuit.control'='circuit.name')) %>% 
    rename(risk_control=risk_score,
           circuit.treat=circuit.mid) %>% 
    mutate(treat_group='Mid')
  
  # Average across
  d_high <- bind_rows(d_high, d_mid)
  
  # Calculate distance
  d_dist <- d_high %>% 
    mutate(dist=abs(risk_treat-risk_control)) %>% 
    group_by(circuit.treat) %>% 
    mutate(min_dist=min(dist)) %>% 
    filter(dist==min_dist) %>% 
    # Check if distance is within one/tenth standard deviation
    mutate(caliper_match = ifelse(dist<=std*s_hat, TRUE, FALSE))
  
  # If second match repeat process
  if(n==2) {
    
    # Find existing matches and remove them
    tmp <- d_dist %>% 
      select(circuit.treat, circuit.control) %>% 
      mutate(is_match = TRUE)
    d_dist2 <- d_high %>% 
      left_join(tmp) %>% 
      mutate(is_match = ifelse(is.na(is_match), F, is_match)) %>% 
      filter(is_match==F)
    
    # Find second match
    d_dist2 <- d_dist2 %>% 
      mutate(dist=abs(risk_treat-risk_control)) %>% 
      group_by(circuit.treat) %>% 
      mutate(min_dist=min(dist)) %>% 
      filter(dist==min_dist) %>% 
      # Check if distance is within one/tenth standard deviation
      mutate(caliper_match = ifelse(dist<=std*s_hat, TRUE, FALSE))
  }
  
  # Return matches
  if(n==2) {
    return(list(d_dist, d_dist2)) 
  } else {
    return(d_dist)
  }
}

# Run function
matches <- matchRiskScore(reg_data, std=0.1, n=n_match)

# Compile matched data - treated
mtreat <- matches %>% 
  filter(caliper_match) %>% 
  select(circuit.treat)
mtreat <- left_join(mtreat,
                    reg_data, by=c('circuit.treat'='circuit.name')) %>% 
  rename(circuit.name=circuit.treat) %>% 
  mutate(is_control_high=0)
mcontrol <- matches %>% 
  filter(caliper_match) %>% 
  mutate(is_control_high = ifelse(treat_group=='High', 1, 0)) %>% 
  ungroup() %>% 
  select(circuit.control, is_control_high)
mcontrol <- left_join(mcontrol, reg_data,
                      relationship='many-to-many',
                      by=c('circuit.control'='circuit.name')) %>% 
  rename(circuit.name=circuit.control)

# bind rows
pdata <- bind_rows(mtreat, mcontrol)

################################################################################
# All days of year - with only one control match
# primary model with two matches comes in next section
################################################################################

# Predict with binary treatment
mm_risk_match_high <- glm(formula = risk_binary, data=pdata %>% 
                       filter(is_any_high==1 | is_control_high==1) %>% 
                       mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
               family=binomial(link='logit'))
mm_risk_match_mod <- glm(formula = risk_binary, data=pdata %>% 
                            filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                            mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                          family=binomial(link='logit'))

################################################################################
# Just focus on R3+ days
################################################################################

mm_risk_r3_high <- glm(formula = risk_binary, data=pdata %>% 
                    filter(is_r3==1) %>% 
                    filter(is_any_high==1 | is_control_high==1) %>% 
                    mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
               family=binomial(link='logit'))
mm_risk_r3_mod <- glm(formula = risk_binary, data=pdata %>% 
                           filter(is_r3==1) %>% 
                           filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                           mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                         family=binomial(link='logit'))

################################################################################
# Match on TWO control units
################################################################################

# Run function
matches <- matchRiskScore(reg_data, std=0.1, n=2)
# Save matches for covariate table
save(matches, file='./intermediate/Regression Dataset/matched_circuits.RData')

### First match 
# Compile matched data - treated
mtreat <- matches[[1]] %>% 
  ungroup() %>% 
  filter(caliper_match) %>% 
  select(circuit.treat) %>% 
  mutate(is_control_high=0)
mtreat <- left_join(mtreat,
                    reg_data, by=c('circuit.treat'='circuit.name')) %>% 
  rename(circuit.name=circuit.treat)

# Control
mcontrol <- matches[[1]] %>% 
  ungroup() %>% 
  filter(caliper_match) %>% 
  mutate(is_control_high = ifelse(treat_group=='High', 1, 0)) %>% 
  ungroup() %>% 
  select(circuit.control, is_control_high)
mcontrol <- left_join(mcontrol, reg_data,
                      relationship='many-to-many',
                      by=c('circuit.control'='circuit.name')) %>% 
  rename(circuit.name=circuit.control)

# bind rows
pdata <- bind_rows(mtreat, mcontrol)

### SECOND match 
# Control
mcontrol <- matches[[2]] %>% 
  ungroup() %>% 
  filter(caliper_match) %>% 
  mutate(is_control_high = ifelse(treat_group=='High', 1, 0)) %>% 
  ungroup() %>% 
  select(circuit.control, is_control_high)
mcontrol <- left_join(mcontrol, reg_data,
                      relationship='many-to-many',
                      by=c('circuit.control'='circuit.name')) %>% 
  rename(circuit.name=circuit.control)

# bind rows
pdata2 <- bind_rows(pdata, mcontrol)

# Get weights for no. of matches
m1 <- matches[[1]] %>% 
  filter(caliper_match)
m2 <- matches[[2]] %>% 
  filter(caliper_match)
m1 <- bind_rows(m1, m2) %>% 
  group_by(circuit.treat, circuit.control) %>% 
  summarise(count=n()) %>% 
  group_by(circuit.treat) %>% 
  mutate(treat_matches=n()) %>% 
  select(-count) %>% 
  group_by(circuit.control) %>% 
  mutate(control_matches=n(),
         control_id = 1,
         control_id = cumsum(control_id),
         weight = ifelse(treat_matches==1, 1, 
                         ifelse(treat_matches==2, 0.5, NA))) %>% 
  select(circuit.control, control_id, weight)

## Find # of occurrences
pdata2 <- pdata2 %>% 
  group_by(circuit.name, date) %>% 
  mutate(control_id=n())

# Merge in weights
pdata2 <- pdata2 %>% 
  left_join(m1, by=c('circuit.name'='circuit.control', 
                     'control_id'='control_id'))
pdata2$weight <- ifelse(is.na(pdata2$weight), 1, pdata2$weight)

################################################################################
# All days of year - with two control matches
################################################################################

# Predict with binary treatment
mm_risk_match_high2 <- glm(formula = risk_binary, data=pdata2 %>% 
                            filter(is_any_high==1 | is_control_high==1) %>% 
                            mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                          family=binomial(link='logit'))
mm_risk_match_mod2 <- glm(formula = risk_binary, data=pdata2 %>% 
                           filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                           mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                         family=binomial(link='logit'))
################################################################################
# Subset to R3+ only days - two control matches - *primary specification
################################################################################

mm_risk_r3_high2 <- glm(formula = risk_binary, data=pdata2 %>% 
                             filter(is_r3==1) %>% 
                             filter(is_any_high==1 | is_control_high==1) %>% 
                             mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                           family=binomial(link='logit'))
mm_risk_r3_mod2 <- glm(formula = risk_binary, data=pdata2 %>% 
                            filter(is_r3==1) %>% 
                            filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                            mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                          family=binomial(link='logit'))

################################################################################
# Save models for evaluating fit and predicting avoided outcomes
save(mm_risk_match_high, mm_risk_match_mod,
     mm_risk_r3_high, mm_risk_r3_mod,
     mm_risk_match_high2, mm_risk_match_mod2,
     mm_risk_r3_high2, mm_risk_r3_mod2,
     file='./intermediate/Regression Dataset/rscore_models.RData')
################################################################################

################################################################################
# Export data matched samples
save(reg_data, pdata, pdata2,
     file='./intermediate/Regression Dataset/reg_matched_data.RData')
################################################################################

################################################################################
# Get AUC and confidence intervals
################################################################################

# Get AUCs
auc1 <- round(roc(ifelse(reg_data$is_ignition>1, 1, reg_data$is_ignition), 
               mm_risk$fitted.values)$auc, 3)
# High one match
tmp <- pdata %>% filter(is_any_high==1 | is_control_high==1)
auc2 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_high$fitted.values)$auc, 3)
# Medium one match
tmp <- pdata %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc3 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_mod$fitted.values)$auc, 3)
# High one match R3
tmp <- pdata %>% filter(is_r3==1) %>%  filter(is_any_high==1 | is_control_high==1)
auc4 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_high$fitted.values)$auc, 3)
# Medium one match R3
tmp <- pdata %>% filter(is_r3==1) %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc5 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_mod$fitted.values)$auc, 3)

# High two match
tmp <- pdata2 %>% filter(is_any_high==1 | is_control_high==1)
auc6 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_high2$fitted.values)$auc, 3)
# Medium two match
tmp <- pdata2 %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc7 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_mod2$fitted.values)$auc, 3)
# High two match R3
tmp <- pdata2 %>% filter(is_r3==1) %>%  filter(is_any_high==1 | is_control_high==1)
auc8 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_high2$fitted.values)$auc, 3)
# Medium two match R3
tmp <- pdata2 %>% filter(is_r3==1) %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc9 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_mod2$fitted.values)$auc, 3)

# Get ses - one match
se1 <- robustErrors(mm_risk)
se2 <- robustErrors(mm_risk_match_high)
se3 <- robustErrors(mm_risk_match_mod)
se4 <- robustErrors(mm_risk_r3_high)
se5 <- robustErrors(mm_risk_r3_mod)

# Second match
se6 <- robustErrors(mm_risk_match_high2)
se7 <- robustErrors(mm_risk_match_mod2)
se8 <- robustErrors(mm_risk_r3_high2)
se9 <- robustErrors(mm_risk_r3_mod2)


# Confidence intervals
tmp <- getCIs(coef(mm_risk), se1)
cis <- list(tmp)
tmp <- getCIs(coef(mm_risk_match_high), se2)
cis[[2]] <- tmp
tmp <- getCIs(coef(mm_risk_match_mod), se3)
cis[[3]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_high), se4)
cis[[4]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_mod), se5)
cis[[5]] <- tmp

# Second match
tmp <- getCIs(coef(mm_risk_match_high2), se6)
cis[[6]] <- tmp
tmp <- getCIs(coef(mm_risk_match_mod2), se7)
cis[[7]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_high2), se8)
cis[[8]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_mod2), se9)
cis[[9]] <- tmp


# Print table with incidence rates for DOUBLE MATCH - HIGH TREATMENT
# Table 1
stargazer(list(mm_risk,
               mm_risk_match_high2, mm_risk_r3_high2),
          type='latex',
          keep = c('is_epss_enable',
                   'is_high_evm',
                   'is_mid_evm',
                   'is_any_high', 'is_any_mid',
                   'underground_units',
                   'is_psps'),
          covariate.labels = c('$\\beta_2$: Veg. Mgmt. ($D_i$=High x $T_{it}$=Post)',
                               '$\\beta_3$: Veg. Mgmt. ($D_i$=Moderate x $T_{it}$=Post)',
                               '$\\beta_4$: Veg. Mgmt. ($D_i$=High)',
                               '$\\beta_5$: Veg. Mgmt. ($D_i$=Moderate)', 
                               '$\\beta_6$: Underground (tens of miles)', 
                               '$\\beta_7$: PSPS (Z_i=1)',
                               '$\\beta_1$: Fast-Trip ($F_{it}$)'),
          apply.coef=function(x){exp(x)-1},
          ci.custom = cis[c(1,6,8)],
          omit.stat = c('f', 'ser', 'rsq', 'aic'),
          font.size = 'small', column.sep.width = '1pt', no.space = T,
          out.header = F,
          header = F,
          digits=2,
          dep.var.caption = 'Incidence rate - Vegetation-caused ignitions',
          column.labels = c('', '',
                            '', ''),
          add.lines = list(c('Risk-score covariate', 'Yes', 'Yes', 'Yes'),
                           c('Risk-score matching', 'No', 'Yes', 'Yes'),
                           c('High-fire risk days only', 'No', 'No', 'Yes'),
                           c('Matched control neighbors (N)', '0', '2', '2'),
                           c('AUC', auc1, auc6, auc8)),
          dep.var.labels.include = F,
          star.cutoffs = NA,
          label='main_reg_table',
          title='Table 1')



# Print table with incidence rates for DOUBLE MATCH - MODERATE TREATMENT
stargazer(list(mm_risk,
               mm_risk_match_mod2, mm_risk_r3_mod2),
          type='latex',
          keep = c('is_epss_enable',
                   'is_high_evm',
                   'is_mid_evm',
                   'is_any_high', 'is_any_mid',
                   'underground_units',
                   'is_psps'),
          covariate.labels = c('$\\beta_2$: Veg. Mgmt. ($D_i$=High x $T_{it}$=Post)',
                               '$\\beta_3$: Veg. Mgmt. ($D_i$=Moderate x $T_{it}$=Post)',
                               '$\\beta_4$: Veg. Mgmt. ($D_i$=High)',
                               '$\\beta_5$: Veg. Mgmt. ($D_i$=Moderate)', 
                               '$\\beta_6$: Underground (tens of miles)', 
                               '$\\beta_7$: PSPS (Z_i=1)',
                               '$\\beta_1$: Fast-Trip ($F_{it}$)'),
          apply.coef=function(x){exp(x)-1},
          ci.custom = cis[c(1,6,8)],
          omit.stat = c('f', 'ser', 'rsq', 'aic'),
          font.size = 'small', column.sep.width = '1pt', no.space = T,
          out.header = F,
          header = F,
          digits=2,
          dep.var.caption = 'Incidence rate - Vegetation-caused ignitions',
          column.labels = c('', '',
                            '', ''),
          add.lines = list(c('Risk-score covariate', 'Yes', 'Yes', 'Yes'),
                           c('Risk-score matching', 'No', 'Yes', 'Yes'),
                           c('High-fire risk days only', 'No', 'No', 'Yes'),
                           c('Matched control neighbors (N)', '0', '2', '2'),
                           c('AUC', auc1, auc6, auc8)),
          dep.var.labels.include = F,
          star.cutoffs = NA,
          label='main_reg_table_moderate',
          title='Extended Data Table 5')

################################################################################
# Robustness checks 
################################################################################

# Print table with incidence rates for SINGLE MATCH - HIGH TREATMENT
stargazer(list(mm_risk,
               mm_risk_match_high, mm_risk_r3_high),
          type='latex',
          keep = c('is_epss_enable',
                   'is_high_evm',
                   'is_mid_evm',
                   'is_any_high', 'is_any_mid',
                   'underground_units',
                   'is_psps'),
          covariate.labels = c('$\\beta_2$: Veg. Mgmt. ($D_i$=High x $T_{it}$=Post)',
                               '$\\beta_3$: Veg. Mgmt. ($D_i$=Moderate x $T_{it}$=Post)',
                               '$\\beta_4$: Veg. Mgmt. ($D_i$=High)',
                               '$\\beta_5$: Veg. Mgmt. ($D_i$=Moderate)', 
                               '$\\beta_6$: Underground (tens of miles)', 
                               '$\\beta_7$: PSPS (Z_i=1)',
                               '$\\beta_1$: Fast-Trip ($F_{it}$)'),
          apply.coef=function(x){exp(x)-1},
          ci.custom = cis[c(1,2,4)],
          omit.stat = c('f', 'ser', 'rsq', 'aic'),
          font.size = 'small', column.sep.width = '1pt', no.space = T,
          out.header = F,
          header = F,
          digits=2,
          dep.var.caption = 'Incidence rate - Vegetation-caused ignitions',
          column.labels = c('', '',
                            '', ''),
          add.lines = list(c('Risk-score covariate', 'Yes', 'Yes', 'Yes'),
                           c('Risk-score matching', 'No', 'Yes', 'Yes'),
                           c('High-fire risk days only', 'No', 'No', 'Yes'),
                           c('Matched control neighbors (N)', '0', '1', '1'),
                           c('AUC', auc1, auc2, auc4)),
          dep.var.labels.include = F,
          star.cutoffs = NA,
          label='main_reg_table',
          title='Appendix')

# Print table with incidence rates for SINGLE MATCH - MODERATE TREATMENT
stargazer(list(mm_risk,
               mm_risk_match_mod, mm_risk_r3_mod),
          type='latex',
          keep = c('is_epss_enable',
                   'is_high_evm',
                   'is_mid_evm',
                   'is_any_high', 'is_any_mid',
                   'underground_units',
                   'is_psps'),
          covariate.labels = c('$\\beta_2$: Veg. Mgmt. ($D_i$=High x $T_{it}$=Post)',
                               '$\\beta_3$: Veg. Mgmt. ($D_i$=Moderate x $T_{it}$=Post)',
                               '$\\beta_4$: Veg. Mgmt. ($D_i$=High)',
                               '$\\beta_5$: Veg. Mgmt. ($D_i$=Moderate)', 
                               '$\\beta_6$: Underground (tens of miles)', 
                               '$\\beta_7$: PSPS (Z_i=1)',
                               '$\\beta_1$: Fast-Trip ($F_{it}$)'),
          apply.coef=function(x){exp(x)-1},
          ci.custom = cis[c(1,2,4)],
          omit.stat = c('f', 'ser', 'rsq', 'aic'),
          font.size = 'small', column.sep.width = '1pt', no.space = T,
          out.header = F,
          header = F,
          digits=2,
          dep.var.caption = 'Incidence rate - Vegetation-caused ignitions',
          column.labels = c('', '',
                            '', ''),
          add.lines = list(c('Risk-score covariate', 'Yes', 'Yes', 'Yes'),
                           c('Risk-score matching', 'No', 'Yes', 'Yes'),
                           c('High-fire risk days only', 'No', 'No', 'Yes'),
                           c('Matched control neighbors (N)', '0', '1', '1'),
                           c('AUC', auc1, auc2, auc4)),
          dep.var.labels.include = F,
          star.cutoffs = NA,
          label='main_reg_table',
          title='Appendix')




################################################################################
# Repeat with region FEs
################################################################################

# Predict with binary treatment
mm_risk <- glm(formula = risk_binary_fes, data=reg_data %>% 
                 mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
               family=binomial(link='logit'))

# Predict with binary treatment
mm_risk_match_high <- glm(formula = risk_binary_fes, data=pdata %>% 
                            filter(is_any_high==1 | is_control_high==1) %>% 
                            mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                          family=binomial(link='logit'))
mm_risk_match_mod <- glm(formula = risk_binary_fes, data=pdata %>% 
                           filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                           mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                         family=binomial(link='logit'))

mm_risk_r3_high <- glm(formula = risk_binary_fes, data=pdata %>% 
                         filter(is_r3==1) %>% 
                         filter(is_any_high==1 | is_control_high==1) %>% 
                         mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                       family=binomial(link='logit'))
mm_risk_r3_mod <- glm(formula = risk_binary_fes, data=pdata %>% 
                        filter(is_r3==1) %>% 
                        filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                        mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                      family=binomial(link='logit'))

# Predict with binary treatment
mm_risk_match_high2 <- glm(formula = risk_binary_fes, data=pdata2 %>% 
                             filter(is_any_high==1 | is_control_high==1) %>% 
                             mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                           family=binomial(link='logit'))
mm_risk_match_mod2 <- glm(formula = risk_binary_fes, data=pdata2 %>% 
                            filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                            mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                          family=binomial(link='logit'))

# Subset to R3+ only days
mm_risk_r3_high2 <- glm(formula = risk_binary_fes, data=pdata2 %>% 
                          filter(is_r3==1) %>% 
                          filter(is_any_high==1 | is_control_high==1) %>% 
                          mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                        family=binomial(link='logit'))
mm_risk_r3_mod2 <- glm(formula = risk_binary_fes, data=pdata2 %>% 
                         filter(is_r3==1) %>% 
                         filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                         mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                       family=binomial(link='logit'))

# Get AUCs
auc1 <- round(roc(ifelse(reg_data$is_ignition>1, 1, reg_data$is_ignition), 
                  mm_risk$fitted.values)$auc, 3)
# High one match
tmp <- pdata %>% filter(is_any_high==1 | is_control_high==1)
auc2 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_high$fitted.values)$auc, 3)
# Medium one match
tmp <- pdata %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc3 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_mod$fitted.values)$auc, 3)
# High one match R3
tmp <- pdata %>% filter(is_r3==1) %>%  filter(is_any_high==1 | is_control_high==1)
auc4 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_high$fitted.values)$auc, 3)
# Medium one match R3
tmp <- pdata %>% filter(is_r3==1) %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc5 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_mod$fitted.values)$auc, 3)


# Get AUCs

# High two match
tmp <- pdata2 %>% filter(is_any_high==1 | is_control_high==1)
auc6 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_high2$fitted.values)$auc, 3)
# Medium two match
tmp <- pdata2 %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc7 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_mod2$fitted.values)$auc, 3)
# High two match R3
tmp <- pdata2 %>% filter(is_r3==1) %>%  filter(is_any_high==1 | is_control_high==1)
auc8 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_high2$fitted.values)$auc, 3)
# Medium two match R3
tmp <- pdata2 %>% filter(is_r3==1) %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc9 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_mod2$fitted.values)$auc, 3)

# Get ses - one match
se1 <- robustErrors(mm_risk)
se2 <- robustErrors(mm_risk_match_high)
se3 <- robustErrors(mm_risk_match_mod)
se4 <- robustErrors(mm_risk_r3_high)
se5 <- robustErrors(mm_risk_r3_mod)

# Second match
se6 <- robustErrors(mm_risk_match_high2)
se7 <- robustErrors(mm_risk_match_mod2)
se8 <- robustErrors(mm_risk_r3_high2)
se9 <- robustErrors(mm_risk_r3_mod2)


# Confidence intervals
tmp <- getCIs(coef(mm_risk), se1)
cis <- list(tmp)
tmp <- getCIs(coef(mm_risk_match_high), se2)
cis[[2]] <- tmp
tmp <- getCIs(coef(mm_risk_match_mod), se3)
cis[[3]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_high), se4)
cis[[4]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_mod), se5)
cis[[5]] <- tmp

# Second match
tmp <- getCIs(coef(mm_risk_match_high2), se6)
cis[[6]] <- tmp
tmp <- getCIs(coef(mm_risk_match_mod2), se7)
cis[[7]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_high2), se8)
cis[[8]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_mod2), se9)
cis[[9]] <- tmp


# Print table with incidence rates for DOUBLE MATCH - HIGH TREATMENT
stargazer(list(mm_risk,
               mm_risk_match_high2, mm_risk_r3_high2),
          type='latex',
          keep = c('is_epss_enable',
                   'is_high_evm',
                   'is_mid_evm',
                   'is_any_high', 'is_any_mid',
                   'underground_units',
                   'is_psps'),
          covariate.labels = c('$\\beta_2$: Veg. Mgmt. ($D_i$=High x $T_{it}$=Post)',
                               '$\\beta_3$: Veg. Mgmt. ($D_i$=Moderate x $T_{it}$=Post)',
                               '$\\beta_4$: Veg. Mgmt. ($D_i$=High)',
                               '$\\beta_5$: Veg. Mgmt. ($D_i$=Moderate)', 
                               '$\\beta_6$: Underground (Tens of miles)', 
                               '$\\beta_7$: PSPS (Z_i=1)',
                               '$\\beta_1$: Fast-Trip ($F_{it}$)'),
          apply.coef=function(x){exp(x)-1},
          ci.custom = cis[c(1,6,8)],
          omit.stat = c('f', 'ser', 'rsq', 'aic'),
          font.size = 'small', column.sep.width = '1pt', no.space = T,
          out.header = F,
          header = F,
          digits=2,
          dep.var.caption = 'Incidence rate - Vegetation-caused ignitions',
          column.labels = c('', '',
                            '', ''),
          add.lines = list(c('Risk-score covariate', 'Yes', 'Yes', 'Yes'),
                           c('Risk-score matching', 'No', 'Yes', 'Yes'),
                           c('High-fire risk days only', 'No', 'No', 'Yes'),
                           c('Matched control neighbors (N)', '0', '2', '2'),
                           c('AUC', auc1, auc6, auc8)),
          dep.var.labels.include = F,
          star.cutoffs = NA,
          label='main_reg_table',
          title='Appendix')



# Print table with incidence rates for DOUBLE MATCH - MODERATE TREATMENT
stargazer(list(mm_risk,
               mm_risk_match_mod2, mm_risk_r3_mod2),
          type='latex',
          keep = c('is_epss_enable',
                   'is_high_evm',
                   'is_mid_evm',
                   'is_any_high', 'is_any_mid',
                   'underground_units',
                   'is_psps'),
          covariate.labels = c('$\\beta_2$: Veg. Mgmt. ($D_i$=High x $T_{it}$=Post)',
                               '$\\beta_3$: Veg. Mgmt. ($D_i$=Moderate x $T_{it}$=Post)',
                               '$\\beta_4$: Veg. Mgmt. ($D_i$=High)',
                               '$\\beta_5$: Veg. Mgmt. ($D_i$=Moderate)', 
                               '$\\beta_6$: Underground (Tens of miles)', 
                               '$\\beta_7$: PSPS (Z_i=1)',
                               '$\\beta_1$: Fast-Trip ($F_{it}$)'),
          apply.coef=function(x){exp(x)-1},
          ci.custom = cis[c(1,6,8)],
          omit.stat = c('f', 'ser', 'rsq', 'aic'),
          font.size = 'small', column.sep.width = '1pt', no.space = T,
          out.header = F,
          header = F,
          digits=2,
          dep.var.caption = 'Incidence rate - Vegetation-caused ignitions',
          column.labels = c('', '',
                            '', ''),
          add.lines = list(c('Risk-score covariate', 'Yes', 'Yes', 'Yes'),
                           c('Risk-score matching', 'No', 'Yes', 'Yes'),
                           c('High-fire risk days only', 'No', 'No', 'Yes'),
                           c('Matched control neighbors (N)', '0', '2', '2'),
                           c('AUC', auc1, auc6, auc8)),
          dep.var.labels.include = F,
          star.cutoffs = NA,
          label='main_reg_table',
          title='Appendix')

################################################################################
# Repeat HIGH RISK CIRCUITS ADDED BACK IN
################################################################################

# Find high risk circuits not matched (3x of them)
x <- matches[[1]] %>% 
  filter(caliper_match==F & treat_group=='High')
y <- reg_data %>% 
  filter(circuit.name %in% unique(x$circuit.treat))
pdata2_robust <- bind_rows(pdata2, y)

# Predict with binary treatment
mm_risk <- glm(formula = risk_binary, data=reg_data %>% 
                 mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
               family=binomial(link='logit'))

# Predict with binary treatment
mm_risk_match_high2 <- glm(formula = risk_binary, data=pdata2_robust %>% 
                             filter(is_any_high==1 | is_control_high==1) %>% 
                             mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                           family=binomial(link='logit'))
mm_risk_match_mod2 <- glm(formula = risk_binary, data=pdata2_robust %>% 
                            filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                            mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                          family=binomial(link='logit'))

# Subset to R3+ only days
mm_risk_r3_high2 <- glm(formula = risk_binary, data=pdata2_robust %>% 
                          filter(is_r3==1) %>% 
                          filter(is_any_high==1 | is_control_high==1) %>% 
                          mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                        family=binomial(link='logit'))
mm_risk_r3_mod2 <- glm(formula = risk_binary, data=pdata2_robust %>% 
                         filter(is_r3==1) %>% 
                         filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0)) %>% 
                         mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition)),
                       family=binomial(link='logit'))

# Get AUCs
auc1 <- round(roc(ifelse(reg_data$is_ignition>1, 1, reg_data$is_ignition), 
                  mm_risk$fitted.values)$auc, 3)
# High one match
tmp <- pdata %>% filter(is_any_high==1 | is_control_high==1)
auc2 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_high$fitted.values)$auc, 3)
# Medium one match
tmp <- pdata %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc3 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_mod$fitted.values)$auc, 3)
# High one match R3
tmp <- pdata %>% filter(is_r3==1) %>%  filter(is_any_high==1 | is_control_high==1)
auc4 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_high$fitted.values)$auc, 3)
# Medium one match R3
tmp <- pdata %>% filter(is_r3==1) %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc5 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_mod$fitted.values)$auc, 3)


# Get AUCs

# High two match
tmp <- pdata2_robust %>% filter(is_any_high==1 | is_control_high==1)
auc6 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_high2$fitted.values)$auc, 3)
# Medium two match
tmp <- pdata2_robust %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc7 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_match_mod2$fitted.values)$auc, 3)
# High two match R3
tmp <- pdata2_robust %>% filter(is_r3==1) %>%  filter(is_any_high==1 | is_control_high==1)
auc8 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_high2$fitted.values)$auc, 3)
# Medium two match R3
tmp <- pdata2_robust %>% filter(is_r3==1) %>% filter(is_any_mid==1 | (is_control_high==0 & is_any_high==0))
auc9 <- round(roc(ifelse(tmp$is_ignition>1, 1, tmp$is_ignition), 
                  mm_risk_r3_mod2$fitted.values)$auc, 3)

# Get ses - one match
se1 <- robustErrors(mm_risk)
se2 <- robustErrors(mm_risk_match_high)
se3 <- robustErrors(mm_risk_match_mod)
se4 <- robustErrors(mm_risk_r3_high)
se5 <- robustErrors(mm_risk_r3_mod)

# Second match
se6 <- robustErrors(mm_risk_match_high2)
se7 <- robustErrors(mm_risk_match_mod2)
se8 <- robustErrors(mm_risk_r3_high2)
se9 <- robustErrors(mm_risk_r3_mod2)


# Confidence intervals
tmp <- getCIs(coef(mm_risk), se1)
cis <- list(tmp)
tmp <- getCIs(coef(mm_risk_match_high), se2)
cis[[2]] <- tmp
tmp <- getCIs(coef(mm_risk_match_mod), se3)
cis[[3]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_high), se4)
cis[[4]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_mod), se5)
cis[[5]] <- tmp

# Second match
tmp <- getCIs(coef(mm_risk_match_high2), se6)
cis[[6]] <- tmp
tmp <- getCIs(coef(mm_risk_match_mod2), se7)
cis[[7]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_high2), se8)
cis[[8]] <- tmp
tmp <- getCIs(coef(mm_risk_r3_mod2), se9)
cis[[9]] <- tmp

# Print table with incidence rates for DOUBLE MATCH - HIGH TREATMENT
stargazer(list(mm_risk,
               mm_risk_match_high2, mm_risk_r3_high2),
          type='latex',
          keep = c('is_epss_enable',
                   'is_high_evm',
                   'is_mid_evm',
                   'is_any_high', 'is_any_mid',
                   'underground_units',
                   'is_psps'),
          covariate.labels = c('$\\beta_2$: Veg. Mgmt. ($D_i$=High x $T_{it}$=Post)',
                               '$\\beta_3$: Veg. Mgmt. ($D_i$=Moderate x $T_{it}$=Post)',
                               '$\\beta_4$: Veg. Mgmt. ($D_i$=High)',
                               '$\\beta_5$: Veg. Mgmt. ($D_i$=Moderate)', 
                               '$\\beta_6$: Underground (Tens of miles)', 
                               '$\\beta_7$: PSPS (Z_i=1)',
                               '$\\beta_1$: Fast-Trip ($F_{it}$)'),
          apply.coef=function(x){exp(x)-1},
          ci.custom = cis[c(1,6,8)],
          omit.stat = c('f', 'ser', 'rsq', 'aic'),
          font.size = 'small', column.sep.width = '1pt', no.space = T,
          out.header = F,
          header = F,
          digits=2,
          dep.var.caption = 'Incidence rate - Vegetation-caused ignitions',
          column.labels = c('', '',
                            '', ''),
          add.lines = list(c('Risk-score covariate', 'Yes', 'Yes', 'Yes'),
                           c('Risk-score matching', 'No', 'Yes', 'Yes'),
                           c('High-fire risk days only', 'No', 'No', 'Yes'),
                           c('Matched control neighbors (N)', '0', '2', '2'),
                           c('AUC', auc1, auc6, auc8)),
          dep.var.labels.include = F,
          star.cutoffs = NA,
          label='reg_table_without_dropping_high_risk',
          title='Appendix')
