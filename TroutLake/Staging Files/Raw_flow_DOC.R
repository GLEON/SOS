# Script to produce uninterpolated time series of inflow volume and inflow DOC
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate, readxl)

# Trout ####
startDate <- ymd("2004-05-01")

# Inflow DOC: Originally in gather_Sw_doc.R
  # Inflow DOC (mg/L) data from NTL LTER; see TroutLake Comments Quirks for interpolation details
doc.mgL <- read_csv('./TroutLake/Staging Files/TL_Variables/TL_monthly_inflow_concentrations.csv') %>%
  mutate(datetime = ymd(sampledate)) %>% na.omit() %>% group_by(datetime) %>%
  summarise(SW_DOC = mean(doc)) %>% # Mean daily across multiple inflows
  write_csv('./TroutLake/Trout_inflow_DOC_noInterp.csv')

# Inflow volume set equal to outflow at USGS '05357245' (Trout River)
  # See script USGS_flowdata_grab.R for calculating based on separate inflows
Q_in <- readNWISdv("05357245", parameterCd= "00060", startDate=startDate, endDate='2017-12-31') %>%
  mutate(datetime= ymd(Date)) %>% 
  mutate(FlowOut = (X_00060_00003 *  0.028316846592), FlowIn = FlowOut) %>% 
  select(datetime, FlowOut, FlowIn) %>%
  write_csv('./TroutLake/Trout_inflow_volume_noInterp.csv')
