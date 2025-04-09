################################################################################
## Project: Ignitions
## Purpose: Load PSPS and fast-trip data
################################################################################

# One-off package need for Excel date times
require(openxlsx)

# Load treatment data
load(file='./intermediate/compiled_treatment_2014_2023.RData')
all_circuits<-unique(df_treatment$circuit.name)

# Example output with circuit names from successful weather matching
load('./intermediate/Intermediate GridMET/compiled_fm100.RData')

# Streamline circuits
df_treatment <- df_treatment %>%
  filter(circuit.name %in% out_results$circuit.name) %>%
  arrange(circuit.name, date)
rm(out_results)

circuits <- unique(df_treatment$circuit.name)

###########################
# Load PSPS worksheet     #
###########################

x <- read_excel(path='./data/PSPS reports/CPUC PSPS Event Data Oct 2013 Through August 2024.xlsx',
                skip=2)
psps<-x

# Names
names(psps) <- make.names(tolower(names(psps)))

# Subset to PG&E
psps<-psps %>% 
  filter(utility=='PG&E' | utility=='PGE')

# Take care of dates/time
psps<-psps %>% 
  mutate(outage.start.numeric=as.numeric(outage.start),
         full.restoration.numeric=as.numeric(full.restoration)) %>% 
  filter(!is.na(outage.start.numeric)) %>%
  filter(!is.na(full.restoration.numeric)) %>% 
  select(-outage.start.numeric,-full.restoration.numeric) %>% 
  mutate(datetime.start = openxlsx::convertToDateTime(outage.start),
         datetime.end   = openxlsx::convertToDateTime(full.restoration))

# Create date/time columns
psps <- psps %>% 
  mutate(year.start=year(datetime.start),
         month.start=month(datetime.start),
         day.start=day(datetime.start),
         hour.start=hour(datetime.start),
         minute.start=minute(datetime.start),
         # ending date/time
         year.end=year(datetime.end),
         month.end=month(datetime.end),
         day.end=day(datetime.end),
         hour.end=hour(datetime.end),
         minute.end=minute(datetime.end))


# Drop transmission lines
psps <- psps %>% 
  filter(!grepl(pattern = '60 kv', x=circuit.name, ignore.case = T)) %>% 
  filter(!grepl(pattern = '60KV', x=circuit.name, ignore.case = T)) %>% 
  filter(!grepl(pattern = '115 kv', x=circuit.name, ignore.case = T)) %>% 
  filter(!grepl(pattern = '115kv', x=circuit.name, ignore.case = T)) %>% 
  filter(!grepl(pattern = '230kv', x=circuit.name, ignore.case = T)) %>% 
  filter(!grepl(pattern = '230 kv', x=circuit.name, ignore.case = T)) %>% 
  filter(!grepl(pattern = '70 KV', x=circuit.name, ignore.case = T))

# Clean up circuit name for merging
psps <- psps %>% 
  mutate(circuit.name=gsub('\\*', '', circuit.name),
         circuit.name=gsub('-', ' ', circuit.name),
         circuit.name=toupper(circuit.name),
         circuit.name=gsub("NOTRE DAME  1104", "NOTRE DAME 1104", circuit.name),
         circuit.name=gsub("WYANDOTTE  1102", "WYANDOTTE 1102", circuit.name),
         circuit.name=gsub("WYANDOTTE1110", "WYANDOTTE 1110", circuit.name),
         circuit.name=gsub("BIG BEND1102", "BIG BEND 1102", circuit.name),
         circuit.name=trimws(circuit.name),
         circuit.name=gsub("PEORIA FLAT", "PEORIA", circuit.name),
         circuit.name=gsub("SAN LEANDRO 1109", "SAN LEANDRO U 1109", circuit.name),
         circuit.name=gsub("SAN LEANDRO 1114", "SAN LEANDRO U 1114", circuit.name),
         circuit.name=gsub('\\[1\\]', '', circuit.name),
         circuit.name=gsub('\\[2\\]', '', circuit.name),
         circuit.name=gsub('\\[3\\]', '', circuit.name),
         circuit.name=gsub('\\[4\\]', '', circuit.name),
         circuit.name=gsub('\\[5\\]', '', circuit.name),
         circuit.name=gsub('SAN JOAQUIN #3 PH', 'SAN JOAQUIN #3', circuit.name),
         circuit.name=gsub('SAN JOAQUIN POWER HOUSE NO 2 1103', 'SAN JOAQUIN #2 1103', circuit.name),
         circuit.name=gsub('SO. CAL EDISON NO. 3 1101', 'SO. CAL. EDISON #3 1101', circuit.name),
         circuit.name=gsub('SO. CAL. EDISON #3 110', 'SO. CAL. EDISON #3 1101', circuit.name),
         circuit.name=gsub('SO. CAL. EDISON #3 11011', 'SO. CAL. EDISON #3 1101', circuit.name),
         circuit.name=gsub('SYCAMORE REEK 1111', 'SYCAMORE CREEK 1111', circuit.name),
         circuit.name=gsub('FULTON 11043', 'FULTON 1104', circuit.name),
         circuit.name=gsub('FULTON 11073', 'FULTON 1107', circuit.name),
         circuit.name=gsub('FITCH MOUNTAIN 11132', 'FITCH MOUNTAIN 1113', circuit.name))

# Filter to circuits that will merge
psps <- psps %>% 
  filter(circuit.name %in% circuits)
sort(unique(psps$circuit.name)[!unique(psps$circuit.name)%in%all_circuits])

# Indicator variables for HFTD location
psps <- psps %>% 
  mutate(is_hftd_2 = ifelse(grepl('2', hftd), 1, 0),
         is_hftd_3 = ifelse(grepl('3', hftd), 1, 0),
         is_non_hftd = ifelse(grepl('NON', hftd, ignore.case = T), 1, 0),
         is_partial_non_hftd = ifelse(grepl('partially', hftd, ignore.case = T), 1, 0))

# Customers and hours
psps <- psps %>% 
  mutate(customers=as.numeric(total.customers.impacted),
         customers.res=as.numeric(residential.customers),
         customers.ci=as.numeric(commercial.industrial.customers),
         customers.med=as.numeric(medical.baseline.customers),
         customers.other=as.numeric(other.customers),
         outage.hours=as.numeric(outage.hours)) %>%
  mutate(date.start=ymd(paste(year.start, month.start, day.start, sep = '-')),
         date.end=ymd(paste(year.end, month.end, day.end, sep='-'))) %>% 
  select(circuit.name, date.start, date.end, outage.hours, customers, 
         customers.res:customers.other,
         is_hftd_2:is_partial_non_hftd,
         datetime.start:minute.end)

# Save intermedate PSPS File for customer-hrs
save(psps, file='./intermediate/psps.RData')

################################################################################
# EPSS / Fast-trip data
################################################################################

# 2022 EPSS
epss_path <- "./data/PGE/2023 WMP/2023-03-27_PGE_2023_WMP_R0_Appendix D ACI PG&E-22-32_Atch01_Redacted.xlsx"
df_epss <- read_excel(epss_path, sheet = '2022 EPSS Outage Data')

# Format dates/time
df_epss <- df_epss %>% 
  mutate(datetime.start=ymd_hms(FNL),
         datetime.end=ymd_hms(`End Date`)) %>% 
  mutate(year.start=year(datetime.start),
         month.start=month(datetime.start),
         day.start=day(datetime.start),
         hour.start=hour(datetime.start),
         minute.start=minute(datetime.start),
         # ending date/time
         year.end=year(datetime.end),
         month.end=month(datetime.end),
         day.end=day(datetime.end),
         hour.end=hour(datetime.end),
         minute.end=minute(datetime.end)) %>% 
  mutate(date.start=ymd(paste(year.start, month.start, day.start, sep='-')),
         date.end=ymd(paste(year.end, month.end, day.end, sep='-'))) %>% 
  # Customer classification
  mutate(customers=as.numeric(CESO),
         customers.med=as.numeric(`Medical Baseline`),
         customers.large.ci=as.numeric(`Critical Customer`),
         customers.res=customers-customers.med-customers.large.ci) %>% 
  # fix likely data entry error
  mutate(customers=ifelse(customers.res<0, customers.med+customers.large.ci,
                          customers),
         customers.res=ifelse(customers.res<0, 0, customers.res))


################################################################################
### 2023 EPSS data
################################################################################

epss_2023 <- read_excel("./data/PGE/Fast-trip proceeding/PGE - EPSS Monthly Report - 20240116 xls.xlsx",
                        sheet='YTD Outages 010123_123123')

# Format date/time
epss_2023 <- epss_2023 %>% 
  mutate(datetime.start=ymd_hms(FNL),
         datetime.end=ymd_hms(`End_Date`)) %>% 
  # Fix three missings
  mutate(datetime.end = as.POSIXct(ifelse(is.na(datetime.end),
                               datetime.start+Restoration_Time_Minutes,
                               datetime.end), tz='Pacific/Los Angeles')) %>% 
  mutate(year.start=year(datetime.start),
         month.start=month(datetime.start),
         day.start=day(datetime.start),
         hour.start=hour(datetime.start),
         minute.start=minute(datetime.start),
         # ending date/time
         year.end=year(datetime.end),
         month.end=month(datetime.end),
         day.end=day(datetime.end),
         hour.end=hour(datetime.end),
         minute.end=minute(datetime.end)) %>% 
  mutate(date.start=ymd(paste(year.start, month.start, day.start, sep='-')),
         date.end=ymd(paste(year.end, month.end, day.end, sep='-'))) %>% 
  # Fix missings
  mutate(date.start=as.Date(ifelse(is.na(date.start) & Circuit=='PETALUMA C 1109' & 
                             date.end=='2023-09-18', date.end, date.start))) %>% 
  # Customer classification
  mutate(customers=as.numeric(CESO),
         customers.med=as.numeric(Medical_Baseline),
         customers.large.ci=as.numeric(Critical_Customer),
         customers.res=customers-customers.med-customers.large.ci) %>% 
  # fix likely data entry error
  mutate(customers=ifelse(customers.res<0, customers.med+customers.large.ci,
                          customers),
         customers.res=ifelse(customers.res<0, 0, customers.res))


################################################################################
### 2021 EPSS data
################################################################################

# Load
epss_2021 <- read_excel("./data/PGE/Fast-trip proceeding/EPSS Outages Monthly Report_20220118.xlsx",
                        sheet='EPSS Outages - 2021 Season')

# Format date/time
epss_2021 <- epss_2021 %>% 
  mutate(datetime.start=ymd_hms(FNL),
         datetime.end=mdy_hm(`End Date`)) %>% 
  mutate(year.start=year(datetime.start),
         month.start=month(datetime.start),
         day.start=day(datetime.start),
         hour.start=hour(datetime.start),
         minute.start=minute(datetime.start),
         # ending date/time
         year.end=year(datetime.end),
         month.end=month(datetime.end),
         day.end=day(datetime.end),
         hour.end=hour(datetime.end),
         minute.end=minute(datetime.end)) %>% 
  mutate(date.start=ymd(paste(year.start, month.start, day.start, sep='-')),
         date.end=ymd(paste(year.end, month.end, day.end, sep='-'))) %>% 
  # Customer classification
  mutate(customers=as.numeric(CESO),
         customers.med=as.numeric(Medical_Baseline),
         customers.large.ci=as.numeric(Critical_Customers),
         customers.res=customers-customers.med-customers.large.ci) %>% 
  # fix likely data entry error
  mutate(customers=ifelse(customers.res<0, customers.med+customers.large.ci,
                          customers),
         customers.res=ifelse(customers.res<0, 0, customers.res))

################################################################################
# Expand to daily data (if PSPS / fast-trip overlaps multiple days
# be sure to record on each day and allocate outage)                
################################################################################

# Write function to convert to daily data
expand_PSPS <- function(in_psps, EPSS_switch=F) {
  
  # Date vector
  date_vector <- seq(in_psps$date.start, in_psps$date.end, 1)
  out         <- data.frame(date=date_vector)
  
  # Add in starting/ending times
  tmp<-in_psps %>% 
    select(date.start, hour.start, minute.start)
  out<-left_join(out, tmp, by=c('date'='date.start'))
  tmp<-in_psps %>% 
    select(date.end, hour.end, minute.end)
  out<-left_join(out, tmp, by=c('date'='date.end'))
  
  # Add hours per day of outage
  out<-out %>% 
    mutate(outage.hours.expand=ifelse(is.na(hour.start)&is.na(hour.end), 24, NA),
           outage.hours.expand=ifelse(!is.na(hour.start)&is.na(hour.end), 
                                      24-(hour.start+minute.start/60), outage.hours.expand),
           outage.hours.expand=ifelse(!is.na(hour.end)&is.na(hour.start),
                                      hour.end+minute.end/60, outage.hours.expand),
           outage.hours.expand=ifelse(!is.na(hour.start)&!is.na(hour.end), 
                                      hour.end+minute.end/60 - (hour.start+minute.start/60), 
                                      outage.hours.expand))
  
  # Bring in other info on customers, HFTD, circuit etc.
  if (EPSS_switch==F) {
    in_psps <- in_psps %>% 
      select(circuit.name, customers:is_partial_non_hftd)
  } else if (EPSS_switch==T) {
    in_psps <- in_psps %>% 
      select(Circuit, Cause, `Customer Minutes`, customers:customers.res)
  }

  
  out<-cbind(out,in_psps)
  return(out)  
}

# Run expand function to get panel data format (circuit x day)
psps_results <- data.frame()
for(i in 1:nrow(psps)) {
  tmp<-expand_PSPS(psps[i,])
  psps_results<-bind_rows(psps_results, tmp)
  print(i)
}

psps_results <- psps_results %>% 
  select(date, circuit.name, outage.hours.expand, customers:customers.other,
         hour.start:minute.end, is_hftd_2:is_partial_non_hftd)

# Run expand function for EPSS
epss_results <- data.frame()
for(i in 1:nrow(df_epss)) {
  tmp<-expand_PSPS(df_epss[i,], EPSS_switch = T)
  epss_results<-bind_rows(epss_results, tmp)
  print(i)
}

# Repeat for 2023 EPSS
epss_results_2023 <- data.frame()
epss_2023$`Customer Minutes` <- epss_2023$Customer_Minutes
for(i in 1:nrow(epss_2023)) {
  tmp<-expand_PSPS(epss_2023[i,], EPSS_switch = T)
  epss_results_2023<-bind_rows(epss_results_2023, tmp)
  print(i)
}

# Repeat for 2021 EPSS
epss_results_2021 <- data.frame()
for(i in 1:nrow(epss_2021)) {
  tmp<-expand_PSPS(epss_2021[i,], EPSS_switch = T)
  epss_results_2021<-bind_rows(epss_results_2021, tmp)
  print(i)
}

# Combine
epss_results <- bind_rows(epss_results, epss_results_2023) %>% 
  bind_rows(epss_results_2021) %>% 
  arrange(date)

##########################################################
# Investigate if any ignition occurred during PSPS event #
##########################################################

# Load ignition tracker
load('./intermediate/Intermediate Ignitions/ignition_tracker_2014_2023.RData')

# Select relevant variables
df_ignitions <- df %>% 
  select(date,circuit.name,hftd,
         time, suspected.initiating.event, land.use.at.origin) %>% 
  mutate(hour.ignition=hour(time),
         minute.ignition=minute(time)) %>% 
  rename(ignition_hftd=hftd) %>% 
  select(-time)

# Merge with PSPS data
psps<-left_join(psps_results, df_ignitions) %>% 
  group_by(circuit.name, date) %>% 
  mutate(count=n())

# Determine overlapping PSPS & ignition
tmp<-psps %>% 
  filter(!is.na(hour.ignition)) %>% 
  mutate(is_psps_ignition=ifelse(outage.hours.expand==24, 1, 0),
         is_psps_ignition=ifelse(!is.na(hour.start) & 
                                   (hour.start+minute.start/60)<=(hour.ignition+minute.ignition/60),
                                 1, is_psps_ignition),
         is_psps_ignition=ifelse(!is.na(hour.end) &
                                   (hour.end+minute.end/60)>=(hour.ignition+minute.ignition/60),
                                 1, is_psps_ignition))
# Control for if ignition occurred in different HFTD section than PSPS
tmp<-tmp %>% 
  mutate(is_psps_ignition=ifelse(ignition_hftd=='Tier 2' & is_hftd_2==0 & year(date)!=2021,
                                 0, is_psps_ignition),
         is_psps_ignition=ifelse(ignition_hftd=='Tier 3' & is_hftd_3==0 & year(date)!=2021,
                                 0, is_psps_ignition),
         is_psps_ignition=ifelse(ignition_hftd=='Non-HFTD' & is_non_hftd==0 & 
                                   is_partial_non_hftd==0 & year(date)!=2021,
                                 0, is_psps_ignition))

# Filter
tmp<-tmp %>% 
  select(date, circuit.name, ignition_hftd, is_psps_ignition,
         suspected.initiating.event, land.use.at.origin) %>% 
  rename(psps_ignition_cause=suspected.initiating.event,
         psps_ignition_landuse=land.use.at.origin,
         psps_ignition_HFTD=ignition_hftd)

# Update one double ignition
tmp<-tmp %>% 
  mutate(psps_ignition_cause=ifelse(circuit.name=='ROSSMOOR 1104',
                                    'Other', psps_ignition_cause)) %>% 
  unique()

# bring in 
psps_results <- left_join(psps_results, tmp)

# Prep to export #
psps_results <- psps_results %>% 
  mutate(psps_customer_hours=outage.hours.expand*customers,
         psps_customer_hours_res=outage.hours.expand*customers.res,
         psps_customer_hours_ci=outage.hours.expand*customers.ci,
         psps_customer_hours_med=outage.hours.expand*customers.med,
         psps_customer_hours_other=outage.hours.expand*customers.other) %>% 
  rename(psps_hours=outage.hours.expand,
         psps_customers=customers) %>% 
  select(date, circuit.name, psps_customer_hours:psps_customer_hours_other,
         psps_customers,
         psps_hours,is_psps_ignition, psps_ignition_cause:psps_ignition_landuse,
         psps_ignition_HFTD)

# Save
save(psps_results, file='./intermediate/psps_compiled_PGE.RData')

################################################################################
# Organize EPSS / fast-trip data #
################################################################################

# Note: ignitions occuring during fast-trip enablement are accounted for
# in ignition file processing (2e load_ignitions.R)

# Merge with EPSS data
epss_results <- epss_results %>% 
  rename(circuit.name=Circuit)

# Allocate customer minutes across days based on length
epss_results <- epss_results %>% 
  group_by(`Customer Minutes`, circuit.name, Cause) %>% 
  mutate(outage.hours.expand.total=sum(outage.hours.expand),
         outage.hours.share=outage.hours.expand/outage.hours.expand.total,
         epss.customer.minutes=`Customer Minutes`*outage.hours.share,
         epss.customer.minutes.res=`Customer Minutes`*outage.hours.share*(customers.res/customers),
         epss.customer.minutes.med=`Customer Minutes`*outage.hours.share*(customers.med/customers),
         epss.customer.minutes.large.ci=`Customer Minutes`*outage.hours.share*(customers.large.ci/customers)) %>% 
  ungroup() %>% 
  # Adjust for 
  select(circuit.name, date, Cause, 
         customers:customers.res,
         epss.customer.minutes:epss.customer.minutes.large.ci) %>% 
  arrange(date)

# Save
save(epss_results, file='./intermediate/epss_compiled_PGE.RData')
