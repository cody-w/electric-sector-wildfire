################################################################################
##
## File:    Functions
## Purpose: Various functions and definitions
##
################################################################################

# CALFIRE REGIONS
tmp <- data.frame(unitcode=c('AEU',
                             'BEU',
                             'BTU',
                             'CZU',
                             'FKU',
                             'HUU',
                             'LMU',
                             'LNU',
                             'MEU',
                             'MMU',
                             'MRN', ### Adding MARIN to Sonoma-Napa
                             'NEU',
                             'SBC', ### Adding SBC to Central Coast
                             'SCU',
                             'SHU',
                             'SLU', ### Adding San Luis Obisbo to Central Coast
                             'SKU',
                             'TCU',
                             'TGU'),
                  region=c('Amador-El Dorado',
                           'San Benito-Monterey',
                           'Butte',
                           'San Mateo-Santa Cruz',
                           'Fresno-Kings',
                           'Humboldt-Del Norte',
                           'Lassen-Modoc',
                           'Sonoma-Lake-Napa',
                           'Mendocino',
                           'Madera-Mariposa',
                           'Sonoma-Lake-Napa', # Entry for Marin
                           'Nevada-Yuba-Placer',
                           'San Benito-Monterey', # Entry for Santa Barbara
                           'Santa Clara',
                           'Shasta-Trinity',
                           'San Benito-Monterey', # Entry for San Luis Obisbo
                           'Siskiyou',
                           'Tuolumne-Calaveras',
                           'Tehama-Glenn'))

# Create hierarchy
tmp2 <- data.frame(region=c('Amador-El Dorado',
                            'San Benito-Monterey',
                            'Butte',
                            'San Mateo-Santa Cruz',
                            'Fresno-Kings',
                            'Humboldt-Del Norte',
                            'Lassen-Modoc',
                            'Sonoma-Lake-Napa',
                            'Mendocino',
                            'Madera-Mariposa',
                            'Nevada-Yuba-Placer',
                            'Santa Clara',
                            'Shasta-Trinity',
                            'Siskiyou',
                            'Tuolumne-Calaveras',
                            'Tehama-Glenn'),
                   region_agg=c('Northern Sierra',
                                'Central Coast',
                                'Northern Sierra',
                                'Central Coast',
                                'Southern Sierra',
                                'Northern Coast',
                                'Southern Cascades',
                                'Sonoma-Napa',
                                'Northern Coast',
                                'Southern Sierra',
                                'Northern Sierra',
                                'Central Coast',
                                'Southern Cascades',
                                'Southern Cascades',
                                'Northern Sierra',
                                'Southern Cascades'))
calfire_region <- left_join(tmp, tmp2)



# ROBUST ERRORS
robustErrors <- function(model) {
  
  out <- coeftest(model,
                  vcov = vcovHC(model, type='HC0'))[,2]
  return(out)
  
}

### R2 function
getR2 <- function(m) {
  hats <- m$fitted.values
  ys   <- m$data$is_ignition
  num  <- sum(((ys - hats)/sqrt(hats))^2)
  denom <- sum(((ys - mean(ys))/sqrt(mean(ys)))^2)
  r2 <- 1-(num/denom)
  return(r2)
}

# Get confidence intervals
getCIs <- function(m_coefs, m_ses) {
  
  # Drop missings
  m_coefs <- m_coefs[!is.na(m_coefs)]
  
  ci_upper <- exp(m_coefs + m_ses*1.96)-1
  ci_lower <- exp(m_coefs - m_ses*1.96)-1
  #ci_upper <- ifelse(ci_upper>100000, 100, ci_upper)
  out <- cbind(ci_lower, ci_upper)
  return(out)
}

# Get combined effect
combineEffect <- function(m, in_se) {
  tmp1 <- coefficients(m)['is_high_evm:is_epss_enable']+
    coefficients(m)['is_high_evm']+
    coefficients(m)['is_epss_enable']
  tmp2 <-
    in_se['is_high_evm:is_epss_enable']+in_se['is_high_evm']+in_se['is_epss_enable']
  
  print('High effect:')
  print(paste0(round(exp(tmp1)-1, 3), ' (', 
               round(exp(tmp1-1.96*tmp2)-1, 3), ', ', 
               round(exp(tmp1+1.96*tmp2)-1, 3), ')'))
  xco <- tmp1
  xcilo <- exp(tmp1-1.96*tmp2)-1
  xcihi <- exp(tmp1+1.96*tmp2)-1
  
  
  tmp1 <-   coefficients(m)['is_epss_enable:is_mid_evm']+
    coefficients(m)['is_mid_evm']+
    coefficients(m)['is_epss_enable']
  tmp2 <-
    in_se['is_epss_enable:is_mid_evm']+in_se['is_mid_evm']+in_se['is_epss_enable']
  
  print('Medium effect:')
  print(paste0(round(exp(tmp1)-1, 3), ' (', 
               round(exp(tmp1-1.96*tmp2)-1, 3), ', ', 
               round(exp(tmp1+1.96*tmp2)-1, 3), ')'))
  
  yco <- tmp1
  ycilo <- exp(tmp1-1.96*tmp2)-1
  ycihi <- exp(tmp1+1.96*tmp2)-1
  
  return(list(xco, yco, xcilo, xcihi,
              ycilo, ycihi))
}

# Get combined effect - version2
combineEffect2 <- function(m, in_se) {
  tmp1 <- coefficients(m)['is_high_evm:is_epss_enable']+
    coefficients(m)['is_high_evm']+
    coefficients(m)['is_epss_enable']
  tmp2 <-
    in_se['is_high_evm:is_epss_enable']+in_se['is_high_evm']+in_se['is_epss_enable']
  
  print('High effect:')
  print(paste0(round(exp(tmp1)-1, 3), ' (', 
               round(exp(tmp1-1.96*tmp2)-1, 3), ', ', 
               round(exp(tmp1+1.96*tmp2)-1, 3), ')'))
  xco <- tmp1
  xcilo <- exp(tmp1-1.96*tmp2)-1
  xcihi <- exp(tmp1+1.96*tmp2)-1
  
  
  tmp1 <-   coefficients(m)['is_mid_evm:is_epss_enable']+
    coefficients(m)['is_mid_evm']+
    coefficients(m)['is_epss_enable']
  tmp2 <-
    in_se['is_mid_evm:is_epss_enable']+in_se['is_mid_evm']+in_se['is_epss_enable']
  
  print('Medium effect:')
  print(paste0(round(exp(tmp1)-1, 3), ' (', 
               round(exp(tmp1-1.96*tmp2)-1, 3), ', ', 
               round(exp(tmp1+1.96*tmp2)-1, 3), ')'))
  
  yco <- tmp1
  ycilo <- exp(tmp1-1.96*tmp2)-1
  ycihi <- exp(tmp1+1.96*tmp2)-1
  
  return(list(xco, yco, xcilo, xcihi,
              ycilo, ycihi))
}


# Regression formatting ---------------------------------------------------

#--- treatment labels
treatment <- paste( 'evm_units',
                    #'poles_units',
                    'underground_units',
                    'covered_units',
                    'is_psps_section',
                    'is_epss',
                    sep=' + ')
treatment_hftd <- paste( 'evm_hftd',
                         #'poles_hftd',
                         'underground_hftd',
                         'covered_hftd',
                         'is_psps_section',
                         sep=' + ')
treatment_share <- paste( 'evm_share',
                          #'poles_share', 
                          'underground_share',
                          'covered_share',
                          'is_psps_section',
                          sep=' + ')
treatment_hftd_share <- paste( 'evm_hftd_share',
                               #'poles_hftd_share', 
                               'underground_hftd_share',
                               'covered_hftd_share',
                               'is_psps_section',
                               sep=' + ')
# covariate labels
treatment_labels <- c('Enhc. veg. (00s miles)',
                      #'Poles replaced (00s poles)',
                      'Undrgnd. (00s miles)',
                      'Cov. cond. (00s miles)',
                      'PSPS (binary)',
                      'EPSS (binary)')

#--- length
tier_length <- paste('non.hftd.oh.miles', 
                     'tier.2.oh.miles',
                     'tier.3.oh.miles',
                     sep=' + ')

#--- weather vars
enviro_vars <-  paste('fm100', 'fm1000', 'pr', 'tmmx',
                      'vs', 'vpd', 'rmin', 'erc', 'etr',
                      'pet', 'sph', 'srad',
                      'is_wind_north',
                      'is_wind_east',
                      'is_wind_south',
                      'elevation', 
                      'mean_veg_height',
                      'max_veg_height',
                      'conductor_age',
                      sep=' + ')

#--- fixed effects
year_fes <- 'factor(year)'
region_fes <- 'region_agg'

# More labels/latex formatting
treatment_keep <- str_split(treatment, ' \\+ ')[[1]]
treatment_keep_hftd <- str_split(treatment_hftd, ' \\+ ')[[1]]
treatment_keep_share <- str_split(treatment_share, ' \\+ ')[[1]]
treatment_keep_hftd_share <- str_split(treatment_hftd_share, ' \\+ ')[[1]]

#--- length
tier_length_label <- c('Overhead circuit length Non-HFTD (00s miles)',
                       'Overhead circuit length HFTD-2 (00s miles)',
                       'Overhead circuit length HFTD-3 (00s miles)')
tier_length_keep <- str_split(tier_length, ' \\+ ')[[1]]

enviro_vars_keep <- str_split(enviro_vars, ' \\+ ')[[1]]

risk_label <- c('Risk score ($\\theta$)')