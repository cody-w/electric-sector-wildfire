################################################################################
## Project: Ignitions
## Purpose: Calculate expected structure losses at the circuit level
################################################################################

# Load intersections of circuits-wildfires-structures dataset ------------------
load('./intermediate/Missoula Fire Lab/damages_1_100.RData')
df_damages <- out_damages
load('./intermediate/Missoula Fire Lab/damages_101_200.RData')
df_damages <- bind_rows(df_damages, out_damages)
load('./intermediate/Missoula Fire Lab/damages_201_300.RData')
df_damages <- bind_rows(df_damages, out_damages)
load('./intermediate/Missoula Fire Lab/damages_301_400.RData')
df_damages <- bind_rows(df_damages, out_damages)
rm(out_damages); gc()


# Wildfire probability model ----------------------------------------------

# Load dataset 
load(file='./intermediate/Regression Dataset/regression_dataset_clean_full.RData')

# Subset to fires -- ignitions that have acreage
ign <- df %>%
  select(circuit.name, date, year, acreage,
         vpd:max_veg_height, region,
         is_wind_north:is_wind_south,
         tier.2.oh.miles:non.hftd.oh.miles, -th) %>%
  filter(!is.na(acreage))

# Add month and seasons
ign$month <- month(ign$date)

# Create larger bins of fire sizes
ign <- ign %>%
  mutate(is_10 = ifelse(acreage<10, 1, 0),
         is_300 = ifelse(acreage>=10 & acreage<300, 1, 0),
         is_5000 = ifelse(acreage>=300 & acreage<10000, 1, 0),
         is_10000plus = ifelse(acreage>=10000, 1, 0)) %>%
  mutate(fire.size_group = ifelse(is_10==1, 'small',
                                  ifelse(is_300==1, 'medium',
                                         ifelse(is_5000, 'large',
                                                ifelse(is_10000plus, 'extreme', NA)))))
ign$fire.size_group <- factor(ign$fire.size_group)
ign$fire.size_group <- relevel(ign$fire.size_group, ref = "small")
ign <- na.omit(ign)

# Select relevant vars
ign <- ign %>%
  select(-circuit.name, -date, -acreage, -(is_10:is_10000plus)) %>% 
  # DROP 2023 - initial wildfire model was run before 2023 data was available
  # consider adding 2023 for future work / refinement 
  filter(year>=2014 & year<=2022)

# Define repeated cross validation with 10 folds and 3 repeats
repeat_cv <- trainControl(method='repeatedcv',
                          number = 10,
                          repeats = 3,
                          classProbs = T,
                          summaryFunction = multiClassSummary)
# Tune grid
tgrid <- expand.grid(mtry=c(3, 6, 9))

# Training and testing data
set.seed(2023)
tr <- sample_n(ign, round(0.75*nrow(ign)))
te <- setdiff(ign, tr)

###############
# Train model #
###############

# This model uses empirical data on wildfire sizes to predict probability of
# an ignition spreading into wildfire size class

if (SWITCH_NEW_LOAD) {
  
  # Train
  system.time({
    forest <- train(
      fire.size_group~.,
      data=tr,
      method='rf',
      ntree=500,
      trControl=repeat_cv,
      tuneGrid=tgrid
    )
  })
  forest
  
  # Predict on full dataset
  te <- df %>%
    select(circuit.name, date, year,
           vpd:max_veg_height, region,
           is_wind_north:is_wind_south,
           tier.2.oh.miles:non.hftd.oh.miles) %>%
    mutate(month=month(date))
  te <- na.omit(te)
  
  # Predict probabilities
  x <- predict(forest, newdata = te, type='prob')
  
  # Fold into data
  ign_out <- cbind(te, x)
  save(forest, file='./intermediate/Missoula Fire Lab/wildfire_size_probability_model.RData')
  save(ign_out, file='./intermediate/Missoula Fire Lab/wildfire_probability.RData')
  
} 