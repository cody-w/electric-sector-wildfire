################################################################################
##
## Project: Ignitions
## Date:    Feb 23 2022
## File:    Load conductor age, etc.
## Purpose: 
##
################################################################################

# Bring in FeederID data
tmp_layers  <- st_layers("./data/PGE/2020 WMP/EDGIS2-12.gdb")
lines_OH    <- st_read("./data/PGE/2020 WMP/EDGIS2-12.gdb",
                       layer=tmp_layers$name[10])

# Filter to pull in circuit covariates
circuit_data <- lines_OH %>%
  as.data.frame() %>%
  select(-SHAPE)

# Average conductor age
tmp <- circuit_data %>%
  filter(!INSTALLJOBYEAR<1900) %>%
  filter(!is.na(INSTALLJOBYEAR)) %>%
  group_by(CIRCUITNAME) %>%
  mutate(total=sum(SHAPE_Length),
         share_wt=SHAPE_Length/total) %>%
  summarise(conductor_year=sum(INSTALLJOBYEAR*share_wt),
            wind_speed_code=sum(WINDSPEEDCODE*share_wt,na.rm=T),
            wind_median=median(WINDSPEEDCODE, na.rm=T)) %>%
  rename(circuit.name=CIRCUITNAME)

# Round wind speed code to 2 or 3
tmp$wind_speed_code <- round(tmp$wind_speed_code, digits=0)

# Opt to use median
tmp$wind_speed_code <- tmp$wind_median

# Drop
tmp$wind_median<-NULL

# Make sure missing circuits are not lost
circuits <- sort(unique(circuit_data$CIRCUITNAME))
circuit_data <- data.frame(circuit.name=circuits)
circuit_data <- left_join(circuit_data, tmp)
rm(lines_OH); gc()

############################################################
# Bring in data on proportion of circuit above/underground #
############################################################

# Load data on grid hardening
df <- read_excel(path='./data/PGE/2021 WMP/Data Requests/CalAdvocates_035 (2021WMP-01)/WildfireMitigationPlans_DR_CalAdvocates_035-Q04-Atch01.xlsx',
                 sheet = '4_Distribution', skip=2)

# Column names
names(df) <- tolower(make.names(names(df)))

# Rename
df <- df %>% 
  rename(saifi=circuit.saifi..system.average.interruption.frequency.index.,
         saidi=circuit.saidi..system.average.interruption.duration.index.,
         maifi=circuit.maifi..momentary.average.interruption.frequency.index.) %>% 
  select(circuit.name, total.oh.miles, total.ug.miles, 
         tier.2.oh.miles, tier.3.oh.miles, non.hftd.oh.miles,
         saidi, saifi, maifi)

# Set to zero if missing
df <- df %>% 
  mutate(total.oh.miles=ifelse(is.na(total.oh.miles), 0, total.oh.miles),
         tier.2.oh.miles=ifelse(is.na(tier.2.oh.miles), 0, tier.2.oh.miles),
         tier.3.oh.miles=ifelse(is.na(tier.3.oh.miles), 0, tier.3.oh.miles),
         non.hftd.oh.miles=ifelse(is.na(non.hftd.oh.miles), 0, non.hftd.oh.miles))

# Merge
circuit_data<-left_join(circuit_data, df)

######### Get averages for missing conductor year / wind speed code
tmp <- mean(circuit_data$conductor_year,na.rm=T)
circuit_data$conductor_year<-ifelse(is.na(circuit_data$conductor_year), tmp, 
                                    circuit_data$conductor_year)

# Save
save(circuit_data, file='./intermediate/circuit_covariates.RData')
