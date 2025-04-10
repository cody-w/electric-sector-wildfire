################################################################################
## Project: Ignitions
## Purpose: Predict fast-trip settings enablement based on fire potential index
################################################################################

options(dplyr.summarise.inform = FALSE)

################################################################################
# Predict fire potential index
################################################################################

# Load dataset
load(file='./intermediate/Regression Dataset/regression_dataset_clean_full.RData')

# Subset to fpi data
df_fpi <- df %>% 
  filter(!is.na(fpi))

# Classify if R3 or greater
df_fpi <- df_fpi %>% 
  mutate(is_r3 = ifelse(fpi%in%c('R3', 'R4', 'R5',
                                     'R5+'), 1, 0),
         fpi_num = gsub('R', '', fpi),
         fpi_num = gsub('\\+', '', fpi_num),
         fpi_num = as.numeric(fpi_num),
         month=month(date))

# Exclude any missings
df_fpi <- df_fpi %>% 
  select(circuit.name, date, year, month, fpi, fpi_num, is_r3,
         is_psps, is_epss, epss.customer.hours,
         mean_veg_height, max_veg_height,
         vpd:elevation, is_wind_north:is_wind_south,
         tier.2.oh.miles:non.hftd.oh.miles)
df_fpi <- na.omit(df_fpi)

# Bring in acreage
tmp <- df %>% 
  filter(!is.na(acreage)) %>% 
  select(circuit.name, date, acreage)
df_fpi <- left_join(df_fpi, tmp)

# Plot incident-level FPI data
graph <- df_fpi %>%
  mutate(day=day(date),
         date2=ymd(paste('2000', month, day, sep='-')),
         acre_bin = ifelse(acreage<10, '0-10',
                           ifelse(acreage>=10 & acreage <300, '10-300',
                                  ifelse(acreage>=300 & acreage <=5000, '300-5000',
                                         ifelse(acreage>5000, '5000+', NA))))) %>%
  filter(!is.na(acre_bin))
ggplot(data=graph, aes(x=date2,y=fpi_num, size=acre_bin,
                       color=acre_bin)) +
  geom_point(alpha=0.8) +
  scale_color_manual(values=c('gray', color_bu, color_br, color_orange)) +
  scale_x_date(date_labels = '%b', date_breaks = '2 months') +
  theme_matplotlib() +
  labs(x='2015-2022', y='Fire potential index (FPI)',
       size='Ignition acreage', color='Ignition acreage')
ggsave('./plots/Supplementary and misc/fpi_acreage_scatter.png', w=8, h=5)

################################################################################
# Random forest - predict FPI
################################################################################

# Training and testing data
set.seed(42)
tr <- sample_n(df_fpi, round(0.75*nrow(df_fpi)))
te <- setdiff(df_fpi, tr)

# Select variables
tr <- tr %>% 
  select(is_r3, year, month, is_psps:elevation, 
         tier.2.oh.miles:non.hftd.oh.miles) %>% 
  mutate(is_r3 = ifelse(is_r3==1, 'Pos', 'Neg'))

# Define repeated cross validation control
repeat_cv <- trainControl(method='repeatedcv', 
                          number = 10, 
                          repeats = 3,
                          classProbs = T,
                          summaryFunction = twoClassSummary)

# Tune grid
tgrid <- expand.grid(mtry=c(3))

# Fit the model on the training data
forest <- train(
  is_r3~.,
  data=tr,
  method='rf',
  ntree=250,
  trControl=repeat_cv,
  tuneGrid=tgrid,
  metric='ROC',
)

# Predict on test data
yhat <- predict(forest, type='prob', newdata=te)

# Predict on all data
df_test <- df %>% 
  mutate(month=month(date)) %>% 
  filter(!is.na(tier.2.oh.miles)) %>% 
  filter(!is.na(vs))
yhat <- predict(forest, newdata = df_test)
df_test$fpi <- yhat

# Check 2022 - circuit days with fast-trip enabled
xx <- df_test %>% 
  filter(year==2022) %>% 
  filter(month%in%6:10)
sum(xx$fpi=='Pos')/nrow(xx)

# Output
out <- df_test %>% 
  select(circuit.name, date, fpi) %>% 
  mutate(is_r3 = ifelse(fpi=='Pos', 1, 0)) %>% 
  select(-fpi)
save(out, file='./intermediate/Fire potential index/fitted_fpi_forest.RData')
