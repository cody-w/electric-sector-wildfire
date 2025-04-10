################################################################################
## Project: Ignitions
## Purpose: Estimate ignition probability random forest model (ignition risk)
################################################################################

# Load dataset
load(file='./intermediate/Regression Dataset/regression_dataset_clean_full.RData')
options(dplyr.summarise.inform = FALSE)

# Write function to clean up and prep for prediction model
prepData <- function(data, IGN_TYPE='VEG',
                     HIST_IGN=T, W_VARS, H_VARS, O_VARS,
                     HFTD_SHARE) {
  
  # Subset to relevant variables
  data <- data %>%
    mutate(month=month(date)) %>% 
    select(circuit.name, date, year,
           month, 
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
      filter(!(is_ignition>=1 & is_veg_ignition==0))
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

################################################################################
################################################################################
# Parameters
################################################################################

# Control group HFTD share?
# Require circuits to have at least 1% overlap with HFTD
h_share <- 0.01

# Ignition type
# specify all ignitions or vegetation-caused
# ALL, VEG
ignition_type <- 'VEG'

# Weather variables
w_vars <- enviro_vars_keep

# Hardening variables
h_vars <- c('covered_units', 'underground_units')

# Operational variables
o_vars <- c('psps_customer_hours', 'epss.customer.hours')

# Matching variables
p_covars <- c(w_vars, tier_length_keep, h_vars, o_vars, 'total.ug.miles')

################################################################################
# Risk score module
################################################################################

# Run prep data function
reg_data <- prepData(df, IGN_TYPE = 'VEG', HIST_IGN = T,
                     W_VARS = w_vars, H_VARS = h_vars, O_VARS = o_vars,
                     HFTD_SHARE = h_share)

################################################################################
# Full forest model 
################################################################################\\

# Load randomforest and parameters
set.seed(42)
tr_split  <- 0.75 # 75% training/testing split

# Get X vars (covars)
x_vars <- c(w_vars, tier_length_keep,
            'hftd_share', 'month', 'region')

# Define repeated cross validation with 10 folds and 3 repeats
repeat_cv <- trainControl(method='repeatedcv', 
                          number = 10, 
                          repeats = 3,
                          classProbs = T,
                          sampling = 'down',
                          summaryFunction = twoClassSummary)

# Tune grid
tgrid <- expand.grid(mtry=c(3, 6, 9))

# Training data
rf_data <- reg_data %>% 
  ungroup() %>% 
  filter(year>=2015 & year<=2019) %>% 
  select(is_ignition, any_of(x_vars)) %>% 
  mutate(is_ignition=factor(ifelse(is_ignition>=1, 'Pos', 'Neg'),
                            levels=c('Neg', 'Pos')))

# Training/testing data
train_data <- rf_data %>% 
  filter(is_ignition=='Pos') %>% 
  sample_n(replace=F, 
           size=round(tr_split*table(rf_data$is_ignition)[2]))
train_data <- bind_rows(train_data, rf_data %>% 
                          filter(is_ignition=='Neg') %>% 
                          sample_n(replace=F, 
                                   size=round(tr_split*table(rf_data$is_ignition)[1]))
)
test_data <- setdiff(rf_data, train_data)

################################################################################
# Train model
################################################################################

# Run from scratch
if (SWITCH_NEW_LOAD) {
  system.time({
    forest <- train(
      is_ignition~.,
      data=train_data,
      method='rf',
      ntree=500,
      trControl=repeat_cv,
      tuneGrid=tgrid,
      metric='ROC',
    )
  })
  save(forest, file = './intermediate/Intermediate Ignitions/forest_object.RData')  
} else {
  load(file = './intermediate/Intermediate Ignitions/forest_object.RData')
}

# Fit on test data
yhat <- predict(forest, type='prob', 
                newdata = test_data)

# Get AUC
roc_obj <- roc(test_data$is_ignition, yhat[,2])

# Get recall
x <- ifelse(yhat[,2]>=0.5, 1, 0)
y <- table(x, test_data$is_ignition)
y[2,2]/(y[2,2] + y[1,2])

# Get precision
y[2,2]/(y[2,2] + y[2,1])

table(test_data$is_ignition)

################################################################################
# Calibrate probabilities 
################################################################################

# Get parameters
n_pos <- table(test_data$is_ignition)[2]
n_neg <- table(test_data$is_ignition)[1]
beta  <- n_pos/n_neg

# Get calibrated probability
yhat_cal <- (yhat[,2]*beta)/(yhat[,2]*beta - yhat[,2] + 1)

# Create graphing data
graph <- data.frame(calibrated=yhat_cal,
                    not_calibrated=yhat[,2]) %>% 
  gather()
ggplot(data=graph, aes(x=value,color=key)) +
  stat_ecdf(linewidth=1) +
  theme_matplotlib() +
  labs(x='Predicted probability on test data', 
       y='Cumulative Distribution Function',
       color='') +
  theme(legend.position = 'inside',
        legend.position.inside = c(0.86, 0.2)) +
  scale_color_manual(values=c(color_bu, color_br))
ggsave('./plots/Supplementary and misc/cdf_adjusted_risk_score.png', w=8, h=5)

################################################################################
# Plot AUC
################################################################################
df_roc <- data.frame(y=test_data$is_ignition,
                     yhat=yhat[,2]) 

# Loop through thresholds
ts <- seq(0.01, max(df_roc$yhat-0.001), by=0.025)
out_roc <- data.frame()
for (t in 1:(length(ts)-1)) {
  tmp <- df_roc %>% 
    mutate(is_pos=ifelse(yhat>=ts[t], 'Pos', 'Neg'))
  
  # True positive
  tp <- table(tmp$is_pos, tmp$y)[2,2] /
    (table(tmp$is_pos, tmp$y)[1,2] + table(tmp$is_pos, tmp$y)[2,2])
  
  # False positive
  fp <- table(tmp$is_pos, tmp$y)[2,1] /
    (table(tmp$is_pos, tmp$y)[1,1] + table(tmp$is_pos, tmp$y)[2,1])
  
  tmp <- data.frame(tp=tp, fp=fp, threshold=ts[t])
  out_roc <- bind_rows(out_roc, tmp)
}

x <- round(roc_obj$auc, 3)

# Plot AUC curve
ggplot(data=out_roc, aes(x=fp, y=tp)) +
  geom_line(linewidth=1, color=color_bu) +
  theme_matplotlib() +
  annotate(geom='text', label=paste0('AUC: ', x),
           x=0.1, y=0.95, size=10) +
  labs(x='False positive rate', y='True positive rate')
ggsave('./plots/Extended data/roc_plot_update.png', w=8,h=5)

################################################################################
# Confusion matrix
################################################################################

yhat_raw <- predict(forest, newdata = test_data, type='raw')
yhat_rawprob <- predict(forest, newdata = test_data, type='prob')

confusion_matrix <- as.data.frame(table(yhat_raw, 
                                        test_data$is_ignition)) %>% 
  rename(Var1=yhat_raw) %>% 
  group_by(Var2) %>% 
  mutate(tot=sum(Freq),
         Freq=Freq/tot) %>% 
  mutate(Var1=factor(Var1, levels=c('Pos', 'Neg')))

ggplot(data = confusion_matrix,
       mapping = aes(x = Var2,
                     y = Var1)) +
  geom_tile(aes(fill = Freq), color='gray50') +
  geom_text(aes(label = round(Freq, 3), vjust = 1),
            size=10) +
  scale_fill_gradient2(low = color_br,
                       mid ='white',
                       high = color_bu,
                       midpoint = 0.5) +
  theme_matplotlib() +
  theme(legend.position = 'none',
        plot.title = element_text(size=34)) +
  labs(x='Actual', y='Predicted')
ggsave('./plots/Extended data/conf_matrix_full_undersample.png', w=6, h=4)

################################################################################
# Plot feature importance
################################################################################

# Get feature importance
graph <- varImp(forest)
graph <- graph$importance %>% 
  arrange(-Overall)

# Rank and order and take top 20
graph$rank <- 1:nrow(graph)
graph <- graph %>% 
  filter(rank<=20)

# Get names
graph$var <- row.names(graph)
graph$var <- factor(graph$var, levels=unique(graph$var))

# Nicer names
tmp <- data.frame(var=c('vpd',
                        'etr',
                        'pet',
                        'tier.2.oh.miles',
                        'tmmx',
                        'mean_veg_height',
                        'vs',
                        'rmin',
                        'erc',
                        'tier.3.oh.miles',
                        'srad',
                        'fm100',
                        'hftd_share',
                        'conductor_age',
                        'non.hftd.oh.miles',
                        'fm1000',
                        'max_veg_height',
                        'sph',
                        'elevation',
                        'month'),
                  lab=c('Vapor pressure deficit',
                        'Evapotranspiration rate',
                        'Potential evapotranspiration',
                        'Circuit length in HFTD Tier-2',
                        'Maximum temperature',
                        'Mean forest canopy height',
                        'Wind speed',
                        'Minimum relative humidity',
                        'Energy release component',
                        'Circuit Length in HFTD Tier-3',
                        'Shortwave downward radiation',
                        '100-hour dead fuel moisture',
                        'Share of circuit length in HFTD',
                        'Age of conductor',
                        'Circuit length outside of HFTD',
                        '1,000-hour dead fuel moisture',
                        'Maximum forest canopy height',
                        'Specific humidity',
                        'Elevation above sea-level',
                        'Month of year'))
graph <- left_join(graph, tmp) %>% 
  arrange(Overall)
graph$lab <- factor(graph$lab, levels=unique(graph$lab))

ggplot(data=graph, aes(x=lab, y=Overall)) +
  geom_bar(stat='identity', fill=color_bu) +
  coord_flip() +
  labs(y='Feature importance', x='') +
  theme_matplotlib()
ggsave('./plots/Extended data/feature_importance.png', w=8, h=5)

################################################################################
# Fit on all reg data
################################################################################

yhat <- predict(forest, type='prob', newdata = reg_data)
reg_data$risk_score <- yhat[,2]

# Adjust posterior probabilities 
reg_data <- reg_data %>% 
  mutate(risk_score_cal = (risk_score*beta)/(risk_score*beta - risk_score +1))

# Save risk score model results for later use
risk <- reg_data %>%
  ungroup() %>%
  select(circuit.name, date, risk_score_cal) %>%
  rename(risk_score=risk_score_cal)
save(risk, file='./intermediate/Intermediate Ignitions/risk_score_output.RData')



