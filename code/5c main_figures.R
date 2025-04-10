################################################################################
##
## Project: Ignitions
## Date:    September 4, 2024
## File:    Figures
## Purpose: Create key figures, mainly cost-effectiveness bar plots 
##
################################################################################

# Cost per avoided ignition -----------------------------------------------

# Load scenarios
load('./intermediate_24/scenarios_veg_Nov2024.RData')
load('./intermediate_24/scenarios_undg_Nov2024.RData')
load('./intermediate_24/scenarios_operational.RData')

# Get base vegetation management scenario
graph <- cost_veg %>% 
  filter(r_disc==median(r_disc) & evm_horizon==median(evm_horizon) &
           unit_cost==median(unit_cost) & risk_range==median(risk_range) &
           effectiveness==median(effectiveness)) %>% 
  mutate(label='Cost per avoided ignition') %>% 
  select(cost_ign_veg, cost_struct_veg, label) %>% 
  rename(cost_ign=cost_ign_veg,
         cost_struct=cost_struct_veg)  %>% 
  mutate(category='veg')

# Get base undergrounding scenario -- SOCIAL
tmp <- cost_undg %>% 
  filter(r_disc==median(r_disc) & ug_horizon==median(ug_horizon) &
           unit_cost==median(unit_cost) & risk_range==median(risk_range)) %>% 
  mutate(label='Cost per avoided ignition') %>% 
  select(cost_ign_ug_social, cost_struct_ug_social, label) %>% 
  rename(cost_ign=cost_ign_ug_social,
         cost_struct=cost_struct_ug_social) %>% 
  mutate(category='ug social')
graph <- bind_rows(graph, tmp)

# Get base undergrounding scenario -- RATEPAYER
tmp <- cost_undg %>% 
  filter(r_disc==median(r_disc) & ug_horizon==median(ug_horizon) &
           unit_cost==median(unit_cost) & risk_range==median(risk_range)) %>% 
  mutate(label='Cost per avoided ignition') %>% 
  select(cost_ign_ug_ratepayer, cost_struct_ug_ratepayer, label) %>% 
  rename(cost_ign=cost_ign_ug_ratepayer,
         cost_struct=cost_struct_ug_ratepayer) %>% 
  mutate(category='ug ratepayer')
graph <- bind_rows(graph, tmp)

# Get base fast-trip scenario
tmp <- cost_operational %>% 
  filter(unit_cost_fast_trip==median(unit_cost_fast_trip) &
           voll == 'value_kwh' & 
           effectiveness==median(effectiveness)) %>% 
  mutate(label='Cost per avoided ignition') %>% 
  select(cost_ign_epss, cost_struct_epss, label) %>% 
  rename(cost_ign=cost_ign_epss,
         cost_struct=cost_struct_epss) %>% 
  mutate(category='fast-trip')
graph <- bind_rows(graph, tmp)

# Get base fast-trip / epss combined
tmp <- cost_operational %>% 
  filter(unit_cost_psps==median(unit_cost_psps) &
           voll == 'value_kwh' & 
           effectiveness==median(effectiveness)) %>% 
  unique() %>% 
  mutate(label='Cost per avoided ignition') %>% 
  select(cost_ign_combined, cost_struct_combined, label) %>% 
  rename(cost_ign=cost_ign_combined,
         cost_struct=cost_struct_combined) %>% 
  mutate(category='combined')
graph <- bind_rows(graph, tmp)

# Duplicate for structures burned
tmp <- graph %>% 
  select(-cost_ign) %>% 
  rename(cost_ign=cost_struct) %>% 
  mutate(label='Cost per avoided structure burned')
graph <- bind_rows(graph %>% select(-cost_struct),
                   tmp)

######### Format error bars ####################################################

graph_error <- data.frame()

##############
# Unit Costs #
##############
t_label <- c('Lower Unit Costs', NA, 'Higher Unit Costs')
veg_unit <- unique(cost_veg$unit_cost) %>% sort()
undg_unit <- unique(cost_undg$unit_cost %>% sort())
fast_unit <- unique(cost_operational$unit_cost_fast_trip %>% sort())
psps_unit <- unique(cost_operational$unit_cost_psps) %>% sort()
for (i in c(1,3)) {
  
  # Get cost vegetation
  x <- cost_veg %>% 
    filter(r_disc==median(r_disc) & evm_horizon==median(evm_horizon) &
             unit_cost==veg_unit[i] &  ##############
             risk_range==median(risk_range) &
             effectiveness==median(effectiveness)) %>% 
    select(cost_ign_veg, cost_struct_veg) %>% 
    rename(cost_ign=cost_ign_veg, cost_struct=cost_struct_veg) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='veg')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost undergrounding - social
  x <- cost_undg %>% 
    filter(r_disc==median(r_disc) & ug_horizon==median(ug_horizon) &
             unit_cost==undg_unit[i] &  ##############
           risk_range==median(risk_range)) %>% 
    rename(cost_ign=cost_ign_ug_social, cost_struct=cost_struct_ug_social) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='ug social')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost undergrounding - ratepayer
  x <- cost_undg %>% 
    filter(r_disc==median(r_disc) & ug_horizon==median(ug_horizon) &
             unit_cost==undg_unit[i] &  ##############
           risk_range==median(risk_range)) %>% 
    rename(cost_ign=cost_ign_ug_ratepayer, cost_struct=cost_struct_ug_ratepayer) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='ug ratepayer')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost fast-trip 
  x <- cost_operational %>% 
    filter(unit_cost_fast_trip==fast_unit[i] &
             voll == 'value_kwh' & 
             effectiveness==median(effectiveness)) %>% 
    rename(cost_ign=cost_ign_epss, cost_struct=cost_struct_epss) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='fast-trip')
  graph_error <- bind_rows(graph_error, x)
  
  # Get COMBINED PSPS & Fast-Trip
  x <- cost_operational %>% 
    filter(unit_cost_psps==psps_unit[i] &
             unit_cost_fast_trip==fast_unit[i],
             effectiveness==median(effectiveness),
             voll == 'value_kwh') %>% 
    unique() %>% 
    rename(cost_ign=cost_ign_combined, cost_struct=cost_struct_combined) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='combined') %>% 
    unique()
  graph_error <- bind_rows(graph_error, x)
}

#################
# Discount Rate #
#################
t_label <- c('Lower Discount Rate\nand Cost of Capital', NA, 
             'Higher Discount Rate\nand Cost of Capital')
disc_range <- unique(cost_veg$r_disc) %>% sort()
for (i in c(1,3)) {
  
  # Get cost vegetation
  x <- cost_veg %>% 
    filter(r_disc==disc_range[i] & 
             evm_horizon==median(evm_horizon) &
             unit_cost==median(unit_cost) &
           risk_range==median(risk_range) &
             effectiveness==median(effectiveness)) %>% 
    select(cost_ign_veg, cost_struct_veg) %>% 
    rename(cost_ign=cost_ign_veg, cost_struct=cost_struct_veg) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='veg')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost undergrounding - social
  x <- cost_undg %>% 
    filter(r_disc==disc_range[i] & ug_horizon==median(ug_horizon) &
             unit_cost==median(unit_cost) & 
           risk_range==median(risk_range)) %>% 
    rename(cost_ign=cost_ign_ug_social, cost_struct=cost_struct_ug_social) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='ug social')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost undergrounding - ratepayer
  x <- cost_undg %>% 
    filter(r_disc==disc_range[i] & ug_horizon==median(ug_horizon) &
             unit_cost==median(unit_cost) & 
             risk_range==median(risk_range)) %>% 
    rename(cost_ign=cost_ign_ug_ratepayer, cost_struct=cost_struct_ug_ratepayer) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='ug ratepayer')
  graph_error <- bind_rows(graph_error, x)
}

#####################
# Lifetime of Asset #
#####################
t_label <- c('Shorter Lifetime', NA, 
             'Longer Lifetime')
veg_range <- unique(cost_veg$evm_horizon) %>% sort()
ug_range <- unique(cost_undg$ug_horizon) %>% sort()
for (i in c(1,3)) {
  
  # Get cost vegetation
  x <- cost_veg %>% 
    filter(r_disc==median(r_disc) & 
             evm_horizon==veg_range[i] &
             unit_cost==median(unit_cost) &
             risk_range==median(risk_range) &
             effectiveness==median(effectiveness)) %>% 
    select(cost_ign_veg, cost_struct_veg) %>% 
    rename(cost_ign=cost_ign_veg, cost_struct=cost_struct_veg) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='veg')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost undergrounding - social
  x <- cost_undg %>% 
    filter(r_disc==median(r_disc) &
             ug_horizon==ug_range[i] &
             unit_cost==median(unit_cost) & 
             risk_range==median(risk_range)) %>% 
    rename(cost_ign=cost_ign_ug_social, cost_struct=cost_struct_ug_social) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='ug social')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost undergrounding - ratepayer
  x <- cost_undg %>% 
    filter(r_disc==median(r_disc) &
             ug_horizon==ug_range[i] &
             unit_cost==median(unit_cost) & 
             risk_range==median(risk_range)) %>% 
    rename(cost_ign=cost_ign_ug_ratepayer, cost_struct=cost_struct_ug_ratepayer) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='ug ratepayer')
  graph_error <- bind_rows(graph_error, x)
}

#####################
# Risk Range        #
#####################
t_label <- c('Lower Future Risk Increase', NA, 
             'Higher Future Risk Increase')
r_range <- unique(cost_undg$risk_range) %>% sort()
for (i in c(1,3)) {
  
  # Get cost vegetation
  x <- cost_veg %>% 
    filter(r_disc==median(r_disc) & 
             evm_horizon==median(evm_horizon) &
             unit_cost==median(unit_cost) &
             risk_range==r_range[i] &
             effectiveness==median(effectiveness)) %>% 
    select(cost_ign_veg, cost_struct_veg) %>% 
    rename(cost_ign=cost_ign_veg, cost_struct=cost_struct_veg) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='veg')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost undergrounding - social
  x <- cost_undg %>% 
    filter(r_disc==median(r_disc) & 
             ug_horizon==median(ug_horizon) &
             unit_cost==median(unit_cost) & 
           risk_range==r_range[i]) %>% 
    rename(cost_ign=cost_ign_ug_social, cost_struct=cost_struct_ug_social) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='ug social')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost undergrounding - ratepayer
  x <- cost_undg %>% 
    filter(r_disc==median(r_disc) & 
             ug_horizon==median(ug_horizon) &
             unit_cost==median(unit_cost) &  
           risk_range==r_range[i]) %>% 
    rename(cost_ign=cost_ign_ug_ratepayer, cost_struct=cost_struct_ug_ratepayer) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='ug ratepayer')
  graph_error <- bind_rows(graph_error, x)
}

######################
# Value of Lost Load #
######################

t_label <- c(NA, 'Higher Outage Costs',
             'Lower Outage Costs')
voll_range <- unique(cost_operational$voll) %>% sort()
for (i in c(2,3)) {
  
  # Get cost fast-trip 
  x <- cost_operational %>% 
    filter(unit_cost_fast_trip==median(unit_cost_fast_trip) &
             voll == voll_range[i] & 
             effectiveness==median(effectiveness)) %>% 
    rename(cost_ign=cost_ign_epss, cost_struct=cost_struct_epss) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='fast-trip')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost PSPS & Fast-Trip
  x <- cost_operational %>% 
    filter(unit_cost_psps==median(unit_cost_psps) &
             unit_cost_fast_trip==median(unit_cost_fast_trip),
             voll == voll_range[i] & 
             effectiveness==median(effectiveness)) %>% 
    rename(cost_ign=cost_ign_combined, cost_struct=cost_struct_combined) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='combined') %>% 
    unique()
  graph_error <- bind_rows(graph_error, x)
}


########################
# Coefficient Estimate #
########################

t_label <- c('Lower Effectiveness', NA, 'Higher Effectiveness')
             
veg_range <- unique(cost_veg$effectiveness) %>% sort(decreasing = T)
fast_trip_range <- unique(cost_operational$effectiveness) %>% sort(decreasing = T)

for (i in c(1,3)) {
  
  # Get cost vegetation
  x <- cost_veg %>% 
    filter(r_disc==median(r_disc) & 
             evm_horizon==median(evm_horizon) &
             unit_cost==median(unit_cost) & 
           risk_range==median(risk_range) &
             effectiveness==veg_range[i]) %>%  
    select(cost_ign_veg, cost_struct_veg) %>% 
    rename(cost_ign=cost_ign_veg, cost_struct=cost_struct_veg) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='veg')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost fast-trip 
  x <- cost_operational %>% 
    filter(unit_cost_fast_trip==median(unit_cost_fast_trip) &
             voll == 'value_kwh' & 
             effectiveness==fast_trip_range[i]) %>% 
    rename(cost_ign=cost_ign_epss, cost_struct=cost_struct_epss) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='fast-trip')
  graph_error <- bind_rows(graph_error, x)
  
  # Get cost fast-trip and PSPS combined
  x <- cost_operational %>% 
    filter(unit_cost_fast_trip==median(unit_cost_fast_trip) &
             unit_cost_psps==median(unit_cost_psps) & 
             voll == 'value_kwh' & 
             effectiveness==fast_trip_range[i]) %>% 
    rename(cost_ign=cost_ign_combined, cost_struct=cost_struct_combined) %>% 
    select(cost_ign, cost_struct) %>% 
    gather(key='key', value='value') %>% 
    mutate(lab_legend=t_label[i], label='combined')
  graph_error <- bind_rows(graph_error, x)
  

}


############################
### Format graphing data ###
############################

# markers
graph_error <- graph_error %>% 
  mutate(point_type=ifelse(grepl('Lifetime', lab_legend), 'Lifetime of Asset',
                           ifelse(grepl('Cost of Capital', lab_legend), 'Discount Rate &\nCost of Capital',
                                  ifelse(grepl('Discount', lab_legend), 'Discount Rate &\nCost of Capital',
                                         ifelse(grepl('Risk Increase', lab_legend), 
                                                'Future Risk Increase', 
                                            ifelse(grepl('Effectiveness', lab_legend), 'Mitigation Effectiveness', 
                                                ifelse(grepl('Unit Costs', lab_legend), 'Unit Cost',
                                                              ifelse(grepl('Outage', lab_legend), 'Outage Costs', NA))))))))
# Greater or fewer
graph_error <- graph_error %>% 
  mutate(point_direction=ifelse(grepl('Shorter', lab_legend) |
                                  grepl('Fewer', lab_legend) | 
                                  grepl('Lower', lab_legend) |
                                  grepl('Smaller', lab_legend), 'Lower', 
                                'Higher'))

# Factor levels for labels
tmp<-data.frame(category=c('fast-trip', 'ug social', 'veg', 'ug ratepayer', 'combined'),
                label_y=c('Fast-Trip\nSettings', 
                          'Underground\n(Social)', 
                          'Enhanced\nVegetation\nManagement',
                          'Underground',
                          'Fast-Trip &\nPublic-Safety\nPower Shutoff'))
tmp_levels <- c('Fast-Trip\nSettings', 
                'Fast-Trip &\nPublic-Safety\nPower Shutoff',
                'Underground\n(Social)', 
                'Underground',
                'Enhanced\nVegetation\nManagement')
graph <- left_join(graph, tmp)
graph$label_y <- factor(graph$label_y, levels=tmp_levels)
graph_error <- left_join(graph_error, tmp, by=c('label'='category'))
graph_error$label_y <- factor(graph_error$label_y, levels=tmp_levels)

# Create wide format for error bars
graph_error_wide <- graph_error %>% 
  select(-lab_legend) %>% 
  spread(key=point_direction, value=value)

# Levels for error bars
# Line shapes
xx <- data.frame(point_type = unique(graph_error$point_type))
xx$point_type <- factor(xx$point_type,
                        levels=c('Mitigation Effectiveness',
                                 'Outage Costs',
                                 'Unit Cost',
                                 'Lifetime of Asset',
                                 'Future Risk Increase',
                                 'Discount Rate &\nCost of Capital'))
xx <- xx %>% 
  arrange(point_type) %>% 
  mutate(line_type=c('solid', 'solid', 'solid',
                     'dashed', 'dashed', 'dashed'))
graph_error_wide<-left_join(graph_error_wide, xx)
graph_error_wide$point_type <- factor(graph_error_wide$point_type,
                                      levels=c('Mitigation Effectiveness',
                                               'Outage Costs',
                                               'Unit Cost',
                                               'Lifetime of Asset',
                                               'Future Risk Increase',
                                               'Discount Rate &\nCost of Capital'))


# Colors
### Format graph
pal <- c("#DFC27D", "#A6611A", "#80CDC1", "#80CDC1", "#018571")
pal <- c("#DFC27D", "#A6611A", "#80CDC1", "#018571")

# Color scheme
col_risk <- "#7A0078"
col_coef <- "#2238AA"
col_voll <- "#80A4B3"
col_life <- "#DF7626"
col_unit <- "#C23C1E"
col_social <- "#53A21A"
col_capital <- "#343009"

col_one <- "#B73856"
col_two <- "#2F63F4"
col_three <- "#53A21A"
col_four <- "#1B3508"

################################################################################
################################################################################
############ Plot cost per avoided ignition
################################################################################

ggplot() +
 # Bar plots
  geom_bar(data=graph %>%
             filter(label=='Cost per avoided ignition') %>% 
             filter(category != 'ug social'),
           aes(x=label_y, y=(cost_ign/1E6),
               fill=label_y),
           stat='identity', show.legend = F) +
  # Error bars
  geom_errorbar(data=graph_error_wide %>% 
                  filter(key=='cost_ign') %>% 
                  filter(label !='ug social'),
                aes(x=label_y, ymin=(Lower/1E6),
                    ymax=(Higher/1E6),
                    color=point_type,
                    linetype=point_type),
                width=0.75, linewidth=0.5,
                position='dodge') +
  theme_matplotlib() +
  theme(axis.title=element_text(lineheight=unit(0.3, 'cm')), 
        legend.text = element_text(size=30, lineheight = unit(0.3, 'cm')),
        legend.box.just = 'left',
        legend.box = 'horizontal',
        legend.spacing = unit(0.75, 'cm'),
        legend.margin = margin(),
        legend.title = element_text(size=32),
        legend.position = 'right',
        legend.background = element_rect(fill=alpha('white', alpha=0.7)),
        axis.text.x = element_text(lineheight = 0.35)) +
  # Axis and colors
  scale_fill_manual(values = pal) +
  guides(fill=F, color=guide_legend(ncol=1)) +
  scale_color_manual(name='Sensitivity',
                     values=c(col_one, '#13447C', col_two,
                              "#DFC27D", brbg_pal[1], col_three)) +
  scale_linetype_manual(name='Sensitivity',
                        values=c(rep('solid', 5), rep('solid', 2))) +
  labs(x='', y='Cost per Avoided Ignition\n$2023 Millions') +
  coord_cartesian(ylim=c(0,76)) +
  scale_y_continuous(expand=c(0,0))
ggsave('./plots/plots_updated/cost_avoided_ignition.png', w=9, h=6, units='in') 

################################################################################
################################################################################
############ Plot cost per avoided structure
################################################################################


ggplot() +
  # Bar plots
  geom_bar(data=graph %>%
             filter(label=='Cost per avoided structure burned') %>% 
             filter(category != 'ug social'),
           aes(x=label_y, y=(cost_ign/1E6),
               fill=label_y),
           stat='identity', show.legend = F) +
  # Error bars
  geom_errorbar(data=graph_error_wide %>% 
                  filter(key=='cost_struct') %>% 
                  filter(label !='ug social'),
                aes(x=label_y, ymin=(Lower/1E6),
                    ymax=(Higher/1E6),
                    color=point_type,
                    linetype=point_type),
                width=0.75, linewidth=0.5,
                position='dodge') +
  theme_matplotlib() +
  theme(axis.title=element_text(lineheight=unit(0.3, 'cm')), 
        legend.text = element_text(size=30, lineheight = unit(0.3, 'cm')),
        legend.box.just = 'left',
        legend.box = 'horizontal',
        legend.spacing = unit(0.75, 'cm'),
        legend.margin = margin(),
        legend.title = element_text(size=32),
        legend.position = 'right',
        legend.background = element_rect(fill=alpha('white', alpha=0.7)),
        axis.text.x = element_text(lineheight = 0.35)) +
  # Axis and colors
  scale_fill_manual(values = pal) +
  guides(fill=F, color=guide_legend(ncol=1)) +
  scale_color_manual(name='Sensitivity',
                     values=c(col_one, '#13447C', col_two,
                              "#DFC27D", col_capital, col_three)) +
  scale_linetype_manual(name='Sensitivity',
                        values=c(rep('solid', 5), rep('solid', 2))) +
  labs(x='', y='Cost per Avoided Structure Burned\n$2023 Millions') +
  coord_cartesian(ylim=c(0,9.1)) +
  scale_y_continuous(expand=c(0,0))
ggsave('./plots/plots_updated/cost_avoided_structure.png', w=9, h=6, units='in') 


### Quick calc on excess returns
x1 <- graph %>% 
  filter(label=='Cost per avoided structure burned') %>% 
  filter(category=='ug ratepayer')
x2 <- graph_error_wide %>% 
  filter(key=='cost_struct') %>% 
  filter(label=='ug ratepayer') %>% 
  filter(point_type=='Discount Rate &\nCost of Capital')
round((x2$Lower - x1$cost_ign) / x1$cost_ign * 100, 1)

### More on total $ cost difference for excess returns
x1 <- cost_undg %>% 
  filter(r_disc==median(r_disc) & 
           ug_horizon==median(ug_horizon) &
           unit_cost==median(unit_cost) &  
           risk_range==median(r_range))
x2 <- cost_undg %>% 
  filter(r_disc==min(r_disc) & 
           ug_horizon==median(ug_horizon) &
           unit_cost==median(unit_cost) &  
           risk_range==median(r_range))


###########################################################################
# Avoided ignitions by month and type -------------------------------------
###########################################################################

# Load dataset
load(file='./intermediate_24/reg_data_for_bootstrap_Aug2024.RData')

# Load reg models
load(file='./intermediate_24/rscore_models_Aug2024.RData')
rm(mm_risk_match_high, mm_risk_match_high2,
   mm_risk_match_mod, mm_risk_match_mod2,
   mm_risk_r3_high,
   mm_risk_r3_mod, mm_risk_r3_mod2); gc()

##################
# Write function
##################
avoidIgnitions <- function(in_dat, in_model) {
  
  # # Initialize
  # in_dat <- pdata %>%
  #   filter(is_r3==1) %>%
  #   mutate(underground_units=underground_units*10)
  # in_model  <- mm_risk_r3_high2

  # Get base ignitions
  ign_base <- predict(in_model, newdata = in_dat %>% 
                        mutate(is_high_evm=0, 
                               is_mid_evm=0,
                               is_epss_enable=0,
                               underground_units=0,
                               is_psps=0),
                      type='response')
  
  # Get veg ignitions
  ign_veg <- predict(in_model, newdata = in_dat %>% 
                       mutate(is_epss_enable=0,
                              is_mid_evm=0,
                              underground_units=0,
                              is_psps=0),
                     type='response')
  
  # Get EPSS ignitions
  ign_epss <- predict(in_model, newdata = in_dat %>% 
                        mutate(is_high_evm=0, 
                               is_mid_evm=0,
                               underground_units=0,
                               is_psps=0),
                      type='response')
  
  # Get PSPS ignitions
  ign_psps <- predict(in_model, newdata = in_dat %>% 
                        mutate(is_high_evm=0, 
                               is_epss_enable=0,
                               is_mid_evm=0,
                               underground_units=0),
                      type='response')
  
  # Alternate UG ignitions w/o econometric model- just zero out proportionally
  ign_ug <- in_dat %>% 
    select(circuit.name, date, risk_score, tier.2.oh.miles, tier.3.oh.miles,
           underground_units) %>% 
    cbind(ign_base) %>% 
    mutate(ign_change = ign_base * (1-(underground_units/
                                         ((tier.2.oh.miles+tier.3.oh.miles)*100))))
  
  
  # Avoided ignitions total
  avoid_veg <- sum(ign_veg)-sum(ign_base)
  avoid_epss <- sum(ign_epss)-sum(ign_base)
  avoid_ug <- sum(ign_ug$ign_change)-sum(ign_base)
  avoid_psps <- sum(ign_psps)-sum(ign_base)
  avoid_obs <- (avoid_veg + avoid_epss + avoid_ug + avoid_psps)
  
  # Create dataframe
  out <- cbind(ign_base, ign_veg)
  out <- cbind(out, ign_epss)
  out <- cbind(out, ign_ug$ign_change)
  out <- cbind(out, ign_psps)
  out <- cbind(in_dat %>% select(circuit.name, date, year),
               out) %>% 
    rename(ign_ug=V4)
  
  # Get avoided ignitions
  out <- out %>% 
    mutate(avoid_veg  = ign_base-ign_veg,
           avoid_epss = ign_base-ign_epss,
           avoid_ug   = ign_base-ign_ug,
           avoid_psps = ign_base-ign_psps,
           avoid_obs  = avoid_veg + avoid_epss + avoid_ug + avoid_psps,
           ign_obs = ign_base - avoid_obs,
           ign_obs = ifelse(ign_obs<0, 0, ign_obs))
  
  return(out)
  
}

# Run function
df_avoid <- avoidIgnitions(in_dat = pdata %>% 
                             filter(is_r3==1) %>% 
                             mutate(underground_units=underground_units*10), 
                           in_model = mm_risk_r3_high2)


in_year <- 2016

# Get avoided ignitions by month and year
graph <- df_avoid %>% 
  mutate(month=month(date)) %>% 
  select(year,month,avoid_veg:avoid_psps) %>% 
  group_by(year, month) %>% 
  summarise_all(sum) %>% 
  gather(key='key', value='value', -year, -month) %>% 
  filter(year>=in_year)

# Labels
tmp <- data.frame(key=unique(graph$key),
                  label=c('Veg. mgmt.',
                          'Fast-trip',
                          'Underground',
                          'PSPS'))
graph <- left_join(graph, tmp)
graph$label <- factor(graph$label, levels=rev(c(
  'Veg. mgmt.',
  'Underground',
  'Fast-trip',
  'PSPS')))

# Get secondary axis for modeled ignitions
tmp <- pdata %>% 
  filter(is_r3==1) %>% 
  select(circuit.name, date, is_ignition) %>% 
  mutate(is_ignition=ifelse(is_ignition>1, 1, is_ignition))
graph2 <- cbind(df_avoid, tmp %>% select(is_ignition))
graph2 <- graph2 %>% 
  mutate(month=month(date)) %>% 
  select(year, month, ign_obs, ign_base, is_ignition) %>% 
  group_by(year, month) %>% 
  summarise_all(sum)


# Plot avoided ignitions at observed levels
ss <- 2
aa <- 0.8

########## modeled vs. counterfactual ignitions

# arrow annotations
anno <- data.frame(year=2021,
                   x=5, xend=6.85,
                   y=19.5, yend=14.1)
annotext <- data.frame(year=2021,
                       label='With observed\nadaptation',
                       x=1, y=22.2)
anno2 <- data.frame(year=2021,
                    x=9, xend=9,
                    y=19, yend=22.75)
annotext2 <- data.frame(year=2021,
                        label='No adaptation',
                        x=12.5, y=24)
anno3 <- data.frame(year=2022,
                    x=5, xend=7,
                    y=21, yend=10.75)
annotext3 <- data.frame(year=2022,
                        label='Avoided ignitions by\nadaptation type',
                        x=8.1, y=24)

# Plot
ggplot() +
  # Bar plot with avoided ignition type
  geom_bar(data=graph, 
           aes(x=month, y=value, fill=label),
           stat='identity', color=NA) +
  # Counterfactual ignitions
  geom_line(data=graph2 %>% filter(year>=2018),
            aes(x=month,y=ign_base),  linewidth=1,
            color=color_red, linetype='dashed',
            alpha=aa) +
  geom_point(data=graph2 %>% filter(year>=2018),
             aes(x=month,y=ign_base), shape=22,
             color=color_red, fill='gray90', size=ss) +
  # Observed ignitions
  geom_line(data=graph2 %>% filter(year>=in_year),
            aes(x=month,y=ign_obs), linewidth=1,
            color='gray20', linetype='solid',
            alpha=aa) +
  geom_point(data=graph2 %>% filter(year>=in_year),
             aes(x=month,y=ign_obs), shape=21,
             color='gray20', fill='white', size=ss) +
  
  facet_wrap(~year, nrow = 2) +
  scale_fill_brewer(palette = 'BrBG', direction = 1) +
  theme_matplotlib() +
  labs(x='Month', y='Vegetation-caused ignitions \nduring high-fire risk days\n in matched sample',
       fill='Prevention measure') +
  scale_x_continuous(breaks=seq(2, 12, 2)) +
  theme(legend.position = 'bottom',
        axis.title.y = element_text(lineheight = 0.3),
        legend.text = element_text(size=32, lineheight = 0.1),
        legend.title = element_text(size=32),
        legend.box.spacing = unit(0.01, units='in'),
        axis.text=element_text(size=32)) +
  # Modeled ignitions annotation
  geom_segment(data=anno, aes(x=x, xend=xend, y=y, yend=yend),
               color='black') +
  geom_text(data=annotext, aes(x=x, y=y, label=label),
            size=9, lineheight=0.25, hjust=0)  +
  # Counterfactual annotation
  geom_segment(data=anno2, aes(x=x, xend=xend, y=y, yend=yend),
               color=color_red) +
  geom_text(data=annotext2, aes(x=x, y=y, label=label),
            size=9, color=color_red, hjust=1) +
  # Avoided annotation
  geom_segment(data=anno3, aes(x=x, xend=xend, y=y, yend=yend),
               color='black') +
  geom_text(data=annotext3, aes(x=x, y=y, label=label),
            size=9, color='black', hjust=1, lineheight=0.25) +
  coord_cartesian(ylim=c(0, 26))
ggsave('./plots/plots_updated/avoided_ignitions_modeled_R3.png', w=9, h=6, units='in')
