################################################################################
## Project: Ignitions
## Purpose: Create other figures, including HFTD map
################################################################################

# Load HFTD district and other spatial outlines
load('./data/Spatial Repository/HFTD.RData')
load('./data/Spatial Repository/CA_State.RData')
load('./data/Spatial Repository/CA_Utility.RData')

# Filter to PG&E
poly_utility <- poly_utility %>% 
  filter(ADDRESS=='77 BEALE STREET')

# Load matches
load(file='./intermediate/Regression Dataset/matched_circuits.RData')

##########################
# GET IGNITION LOCATIONS #
##########################

# Ignition tracker
load(file='./intermediate/Intermediate Ignitions/ignition_tracker_2014_2023.RData')

# Create spatial points
df_points <- st_as_sf(df, coords = c('longitude', 'latitude'), 
                      crs=st_crs(4326))
# Drop one ignition off the coast
df_points <- st_intersection(df_points, poly_ca) %>% 
  mutate(label="Ignition")

# Create box for map inset
pol = st_polygon(
  list(
    cbind(
      c(-123.02424, -123.02424, -122.50505, -122.50505, -123.02424),
      c(38.32784, 38.46961, 38.46961, 38.32784, 38.32784))
  )
)
pol <- st_sfc(pol, crs=st_crs(4326))

# Plot
ggplot() +
  geom_sf(data=poly_ca, color='gray30', fill='gray90') +
  geom_sf(data=poly_hftd, aes(fill=HFTD), alpha=0.9, color=NA) +
  geom_sf(data=poly_utility %>% mutate(label='PG&E Territory'),
          fill=NA, aes(color=label), linetype='dashed') +
  geom_sf(data=df_points,
          alpha=0.2, fill=color_red, color=color_red,
          shape=21, size=0.5) +
  geom_sf(data=pol, color='black', fill=NA, linewidth=0.5) +
  annotate(x=-118,y=35,geom='text',
           label='Ignitions on PG&E\nDistribution Circuits',
           lineheight=0.3, hjust=0, size=8) +
  theme_matplotlib() +
  scale_fill_manual(values=c(color_br, color_orange)) +
  scale_color_manual(values=c('gray20')) +
  theme(panel.grid.major = element_blank(),
        axis.text = element_text(size=24),
        legend.position = 'inside',
        legend.position.inside = c(0.225, 0.22),
        legend.text = element_text(size=28, lineheight = 0.5),
        legend.title = element_text(size=28, lineheight = 0.33, hjust=0),
        legend.spacing = unit(0.01, units='cm'),
        legend.margin = margin()) +
  guides(color = guide_legend(order=2),
         fill  = guide_legend(order=1)) +
  labs(fill='High-Fire\nThreat District', x='', y='', color='')
ggsave(file='./plots/Main figures/hftd_map_ignitions.png',
       w=5, h=5, units='in')

##############################
########### INSET MAP CIRCUIT 
##############################

# Load circuits
load('./intermediate/PGE_circuits.RData')

# Treated circuit
tmp_lines <- lines_OH %>% 
  filter(circuit=='DUNBAR 1101' |
           circuit=='MOLINO 1102')
df_points <- st_transform(df_points, st_crs(lines_OH))

# Matched circuit
match_lines <- lines_OH %>% 
  filter(circuit=='MOLINO 1102')

### Subset to ignitions around circuit
dd <- 200
units(dd) <- 'm'
tmp_points <- st_intersection(df_points, st_buffer(tmp_lines, dist=dd))
tmp_points <- unique(tmp_points)

# Re-transform
tmp_points <- st_transform(tmp_points, st_crs(poly_hftd))
tmp_lines  <- st_transform(tmp_lines, st_crs(poly_hftd))

# Crop HFTD
tmp_hftd <- st_crop(poly_hftd, tmp_lines)

# Plot circuit inset
ggplot() +
  geom_sf(data=tmp_hftd, aes(fill=HFTD), color=NA, alpha=0.75) +
  geom_sf(data=tmp_lines, color='gray20') +
  geom_sf(data=tmp_points,
          alpha=0.75, fill=color_red, 
          color=color_red, shape=21, size=2) +
  geom_sf(data=pol, color='black', fill=NA, linewidth=1) +
  theme_matplotlib() +
  theme(axis.text = element_blank(),
        panel.grid.major = element_blank(),
        legend.position = 'none',
        axis.ticks = element_blank(),
        panel.border = element_blank()) +
  scale_fill_manual(values=c(color_br, color_orange))
ggsave('./plots/Main figures/hftd_map_inset.png', w=7, h=3)


rm(lines_OH, poly_ca, poly_hftd); gc()

################################################################################
# Load data ####################################################################
################################################################################

# Load dataset
load(file='./intermediate/Regression Dataset/regression_dataset_clean_full.RData')

df <- df %>%
  filter(year>2015)
options(dplyr.summarise.inform = FALSE)

# Load doses
load(file='./intermediate/Regression Dataset/dose.RData')

# Merge in to dataset
df <- left_join(df, dose)
rm(dose);gc()

################################################################################
# Matching data ################################################################
################################################################################

# Load dataset
load(file='./intermediate/Regression Dataset/reg_matched_data.RData')
rm(pdata2); gc()

# Load matches
load(file='./intermediate/Regression Dataset/matched_circuits.RData')

################################################################################
# Visualization of matching via scatterplot
################################################################################

# Graphing data
graph <- matches[[1]] %>% 
  mutate(dummy_risk=risk_treat,
         caliper_match=ifelse(caliper_match, 'Matched Circuit',
                              'Unmatched Circuit'),
         treat_group=ifelse(treat_group=='High', 'High Veg. Mgmt. Treatment\n(>=50% of Circuit Length)',
                            'Moderate Veg. Mgmt. Treatment\n(10-49% of Circuit Length)'),
         lab_shape = 'First Match')
graph2 <- matches[[2]] %>% 
  mutate(dummy_risk=risk_treat,
         caliper_match=ifelse(caliper_match, 'Matched Circuit',
                              'Unmatched Circuit'),
         treat_group=ifelse(treat_group=='High', 'High Veg. Mgmt. Treatment\n(>=50% of Circuit Length)',
                            'Moderate Veg. Mgmt. Treatment\n(10-49% of Circuit Length)'),
         lab_shape = 'Second Match')
  
# Plot
ggplot(bind_rows(graph, graph2), 
       aes(x=risk_treat, y=risk_control, 
           color=caliper_match, shape=lab_shape)) +
  geom_line(data=graph, aes(x=risk_treat, y=dummy_risk),
            color='gray40', linewidth=1) +
  geom_point(alpha=0.8) +
  theme_matplotlib() +
  theme(strip.text = element_text(size=32, lineheight = 0.33),
        legend.position = c(0.15, 0.82),
        legend.margin = margin(),
        legend.spacing = unit(0.001, 'cm')) +
  scale_color_manual(values=c(color_bu, color_br)) +
  labs(x='Mean Daily Ignition Risk of "Treated" Circuit',
       y='Mean Daily Ignition Risk of "Matched Control" Circuit',
       color='', shape='') +
  facet_wrap(~treat_group)
ggsave('./plots/Extended data/risk_score_matched_line.png', w=8, h=5)

################################################################################
########### Cumulative prevention measures ###########
################################################################################

df <- df %>% 
  filter(!is.na(hftd_share))

graph <- df %>% 
  filter(hftd_share>0.01) %>% 
  group_by(date) %>% 
  summarise(underground_units=sum(underground_units),
            evm_units=sum(evm_units),
            covered_units=sum(covered_units))

### Bring in fitted fire potential index
load('./intermediate/Fire potential index/fitted_fpi_forest.RData')
df <- left_join(df, out)

# Identify 2021 pilot circuits
epss_2021_circs <- df %>%
  filter(year==2021) %>%
  filter(is_epss==1)
epss_2021_circs <- unique(epss_2021_circs$circuit.name)

# Identify if EPSS is enabled
df <- df %>% 
  mutate(is_epss_enable = ifelse(year<2021, 0,
                                 ifelse(is_r3==1, 1, 0)),
         is_epss_enable = ifelse(year==2021 &
                                   !(circuit.name %in% epss_2021_circs),
                                 0, is_epss_enable),
         is_epss_enable = ifelse(date<'2021-07-28', 0, is_epss_enable))

# Get graphing data
graph <- df %>% 
  mutate(month=month(date)) %>% 
  filter(hftd_share>0.01) %>% 
  mutate(psps_miles = is_psps*(tier.2.oh.miles+tier.3.oh.miles),
         epss_miles = is_epss_enable*(tier.2.oh.miles+tier.3.oh.miles)) %>% 
  group_by(date) %>%
  summarise(underground_units=sum(underground_hftd),
            evm_units=sum(evm_hftd),
            covered_units=sum(covered_hftd),
            psps_miles=sum(psps_miles),
            epss_miles=sum(epss_miles))

graph_long <- graph %>% 
  gather(key='key', value='value', -date) %>% 
  mutate(label=ifelse(grepl('underg', key), 'Underground',
                      ifelse(grepl('evm', key), 'Enhanced vegetation mgmt.',
                             ifelse(grepl('covered', key), 'Covered conductor',
                                    ifelse(grepl('epss', key), 'Fast-trip enabled',
                                           'Public safety power shutoff')))))
graph_long$label <- factor(graph_long$label, levels=rev(c('Enhanced vegetation mgmt.',
                                                          'Underground',
                                                          'Covered conductor',
                                                          'Fast-trip enabled',
                                                          'Public safety power shutoff')))
  
# Plot
ggplot(data=graph_long, aes(x=date,y=value/10,color=label)) +
  geom_line(linewidth=0.5) +
  theme_matplotlib() +
  labs(x='', y='HFTD Miles (thousands)', color='Prevention measure') +
  theme(legend.position = c(0.25,0.7),
        legend.background = element_rect(fill=alpha('white', 0.7))) +
  scale_color_manual(values=c("#A6611A", "#DFC27D", 'gray50',
                              "#80CDC1", "#018571"))
ggsave('./plots/Main figures/cumulative_prevention_measures_realtime.png',
       w=8, h=5)


################################################################################
########### Modeled increase in PSPS and Fast-Trip ###########
################################################################################

# Inputs
ANNUAL_EPSS_HOURS <- 6.5E6
ANNUAL_PSPS_HOURS <- 12E6
psps_criteria <- 0.05

# Load weather data / risk
load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                 gsub('[.]', '', 0.50), '_', 40,
                 '.RData'))

# Model increased fast-trip and PSPS customer-hours
y_central <- out_risk %>% 
  mutate(is_psps = ifelse(risk_score>=psps_criteria, 1, 0)) %>% 
  group_by(year) %>% 
  summarise(total_epss=sum(is_r3), 
            total_psps=sum(is_psps)) %>% 
  mutate(min_r3 = min(total_epss),
         min_psps = min(total_psps),
         grow_r3 = total_epss/min_r3,
         grow_psps = total_psps/min_psps,
         total_epss_hours = ANNUAL_EPSS_HOURS,
         total_psps_hours = ANNUAL_PSPS_HOURS,
         # Scale up the hours each year
         total_epss_hours = total_epss_hours * grow_r3,
         total_psps_hours = total_psps_hours * grow_psps) %>% 
  select(year, total_epss, total_psps,
         total_epss_hours, total_psps_hours) %>% 
  mutate(total_epss_hours=total_epss_hours/1E6,
         total_psps_hours=total_psps_hours/1E6)

# Load weather data / risk
load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                 gsub('[.]', '', 0.75), '_', 40,
                 '.RData'))

# Model increased fast-trip and PSPS customer-hours
y_high <- out_risk %>% 
  mutate(is_psps = ifelse(risk_score>=psps_criteria, 1, 0)) %>% 
  group_by(year) %>% 
  summarise(total_epss=sum(is_r3), 
            total_psps=sum(is_psps)) %>% 
  mutate(min_r3 = min(total_epss),
         min_psps = min(total_psps),
         grow_r3 = total_epss/min_r3,
         grow_psps = total_psps/min_psps,
         total_epss_hours = ANNUAL_EPSS_HOURS,
         total_psps_hours = ANNUAL_PSPS_HOURS,
         # Scale up the hours each year
         total_epss_hours = total_epss_hours * grow_r3,
         total_psps_hours = total_psps_hours * grow_psps) %>% 
  select(year, total_epss, total_psps,
         total_epss_hours, total_psps_hours) %>% 
  mutate(total_epss_hours=total_epss_hours/1E6,
         total_psps_hours=total_psps_hours/1E6)

# Load weather data / risk
load(file=paste0('./intermediate/Intermediate Climatology/risk_combined_sample_escalate_',
                 gsub('[.]', '', 0.25), '_', 40,
                 '.RData'))

# Model increased fast-trip and PSPS customer-hours
y_low <- out_risk %>% 
  mutate(is_psps = ifelse(risk_score>=psps_criteria, 1, 0)) %>% 
  group_by(year) %>% 
  summarise(total_epss=sum(is_r3), 
            total_psps=sum(is_psps)) %>% 
  mutate(min_r3 = min(total_epss),
         min_psps = min(total_psps),
         grow_r3 = total_epss/min_r3,
         grow_psps = total_psps/min_psps,
         total_epss_hours = ANNUAL_EPSS_HOURS,
         total_psps_hours = ANNUAL_PSPS_HOURS,
         # Scale up the hours each year
         total_epss_hours = total_epss_hours * grow_r3,
         total_psps_hours = total_psps_hours * grow_psps) %>% 
  select(year, total_epss, total_psps,
         total_epss_hours, total_psps_hours) %>% 
  mutate(total_epss_hours=total_epss_hours/1E6,
         total_psps_hours=total_psps_hours/1E6)

# Load main reg data
load('./intermediate/Regression Dataset/reg_matched_data.RData')

# Actual PSPS and EPSS
tmp <- reg_data %>% 
  group_by(year) %>% 
  summarise(is_psps=sum(is_psps),
            is_epss_enable=sum(is_epss_enable), 
            epss.customer.hours=sum(epss.customer.hours)/1E6,
            psps_customer_hours=sum(psps_customer_hours)/1E6)

# Graph actuals fast-trip
ggplot(data=y_central, aes(x=year, y=total_epss_hours)) +
  geom_smooth(se=F, method='lm', linetype='dashed', color=main_scheme[3]) +
  geom_point(shape=21, fill=main_scheme[3]) + 
  
  # High risk
  geom_smooth(data=y_high, aes(x=year, y=total_epss_hours),
              se=F, method='lm', linetype='dashed', color=main_scheme[1]) +
  geom_point(data=y_high, aes(x=year, y=total_epss_hours),
             shape=21, fill=main_scheme[1]) +
  
  # Low risk
  geom_smooth(data=y_low, aes(x=year, y=total_epss_hours),
              se=F, method='lm', linetype='dashed', color=main_scheme[4]) +
  geom_point(data=y_low, aes(x=year, y=total_epss_hours),
             shape=21, fill=main_scheme[4]) +
  
  # Actuals
  geom_point(data=tmp %>% filter(year>=2021), 
             aes(x=year, y=epss.customer.hours)) +
  theme_matplotlib() +
  # Format
  coord_cartesian(ylim=c(0, 11.5)) +
  theme(axis.title = element_text(lineheight = unit(0.3, 'cm')),
        axis.text = element_text(size=34)) + 
  labs(x='', y='Customer-Hours of Fast-Trip Outages\n(Millions in HFTD Sample)') +
  # Annotations
  annotate(geom='text', label='High-Risk Scenario',
           x=2021, y=11.5, size=14, color=main_scheme[1], hjust=0) +
  annotate(geom='text', label='Baseline Risk Scenario',
           x=2021, y=10.9, size=14, color=main_scheme[3], hjust=0) +
  annotate(geom='text', label='Low-Risk Scenario',
         x=2021, y=10.3, size=14, color=main_scheme[4], hjust=0) +
  annotate(geom='text', label='Actual',
           x=2021, y=9.7, size=14, color='black', hjust=0)
ggsave('./plots/Extended data/future_fast_trip_hours.png',
       w=8, h=5)

# Graph actuals PSPS
ggplot(data=y_central, aes(x=year, y=total_psps_hours)) +
  geom_smooth(se=F, method='lm', linetype='dashed', color=main_scheme[3]) +
  geom_point(shape=21, fill=main_scheme[3]) + 

  # High risk
  geom_smooth(data=y_high, aes(x=year, y=total_psps_hours),
              se=F, method='loess', linetype='dashed', color=main_scheme[1]) +
  geom_point(data=y_high, aes(x=year, y=total_psps_hours),
             shape=21, fill=main_scheme[1]) +
  # Low risk
  geom_smooth(data=y_low, aes(x=year, y=total_psps_hours),
              se=F, method='lm', linetype='dashed', color=main_scheme[4]) +
  geom_point(data=y_low, aes(x=year, y=total_psps_hours),
             shape=21, fill=main_scheme[4]) +
  # Actuals
  geom_point(data=tmp %>% filter(year>=2018), 
             aes(x=year, y=psps_customer_hours)) +
  theme_matplotlib() +
  # Format
  coord_cartesian(ylim=c(0, 160)) +
  theme(axis.title = element_text(lineheight = unit(0.3, 'cm')),
        axis.text = element_text(size=34)) + 
  labs(x='', y='Customer-Hours of PSPS Outages\n(Millions in HFTD Sample)') +
  # Annotations
  annotate(geom='text', label='High-Risk Scenario',
           x=2019, y=145, size=14, color=main_scheme[1], hjust=0) +
  annotate(geom='text', label='Baseline Risk Scenario',
           x=2019, y=136, size=14, color=main_scheme[3], hjust=0) +
  annotate(geom='text', label='Low-Risk Scenario',
           x=2019, y=127, size=14, color=main_scheme[4], hjust=0) +
  annotate(geom='text', label='Actual',
           x=2019, y=118, size=14, color='black', hjust=0)
ggsave('./plots/Extended data/future_psps_hours.png',
       w=8, h=5)

################################################################################
########### Ignition summary statistics ###########
################################################################################

# Load dataset
load(file='./intermediate/Regression Dataset/regression_dataset_clean_full.RData')

df <- df %>%
  filter(year>=2015)
options(dplyr.summarise.inform = FALSE)

# Calculate ignitions per-year
graph_ign <- df %>% 
  mutate(hftd=ifelse(tier.2.oh.miles+tier.3.oh.miles>=1/100, 'HFTD', 'Non-HFTD')) %>%
  group_by(hftd, circuit.name) %>% 
  summarise(is_ignition=sum(is_ignition)) %>% 
  group_by(hftd) %>% 
  mutate(circuit_count=n()) %>% 
  group_by(hftd, circuit_count) %>% 
  summarise(is_ignition=sum(is_ignition)) %>% 
  # Ignitions per circuit-year
  mutate(ignition.circ.yr = is_ignition/circuit_count/length(2015:2023))

# Get more on distribution of wildfire sizes
graph_acres_distr <- df %>% 
  filter(!is.na(acreage)) %>% 
  mutate(hftd=ifelse(tier.2.oh.miles+tier.3.oh.miles>=1/100, 'HFTD', 'Non-HFTD')) %>%
  mutate(is10=ifelse(acreage>10, 1, 0),
         is300=ifelse(acreage>300, 1, 0),
         is5000=ifelse(acreage>5000, 1, 0)) %>% 
  group_by(hftd) %>% 
  summarise(mean_acreage=mean(acreage),
            median_acreage=median(acreage),
            is10=sum(is10),
            is300=sum(is300),
            is5000=sum(is5000),
            count=n()) %>% 
  mutate(is10=is10/count,
         is300=is300/count,
         is5000=is5000/count)

################################################################################
########### Ignition by driver ###########
################################################################################

# Acres burned
graph_acres <- df %>% 
  mutate(hftd=ifelse(hftd_share>0.1, 'HFTD', 'Non-HFTD')) %>% 
  filter(!is.na(acreage)) %>% 
  mutate(ign_type=ifelse(is_veg_ignition>=1, 'Veg', 'Other')) %>% 
  group_by(hftd, ign_type) %>% 
  summarise(acreage.mean=mean(acreage),
            acreage.sum=sum(acreage),
            acreage.sd=sd(acreage),
            count=n())

# Veg / HFTD acres burned proportion
graph_acres[graph_acres$hftd=='HFTD' & graph_acres$ign_type=='Veg' & 
              !is.na(graph_acres$hftd),'acreage.sum'] / sum(graph_acres$acreage.sum)


# Transmission ignitions share
dist  <- c(401, 411, 477)
trans <- c(22, 23, 24)
years <- c(2019, 2018, 2017)
sum(dist)/(sum(dist)+sum(trans))