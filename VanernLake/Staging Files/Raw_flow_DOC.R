# Script to produce uninterpolated time series of inflow volume and inflow DOC
#install.packages('pacman')
pacman::p_load(tidyverse, lubridate, readxl)

# Vanern ####
startDate <- ymd("2001-04-25")

# Inflow DOC: Originally in gather_data.R, "Surface water DOC" section
  # Inflow DOC (mg/L) estimated from TOC (mg/L); raw file has multiple inflow tribs and mean 
  # daily TOC across inflows used
doc.mgL <- read_csv('./VanernLake/Staging Files/Vanern_Variables/Vanern_tributaries.csv') %>%
  mutate(sampledate = as.Date(sampledate, "%m/%d/%Y %H:%M")) %>% rename(datetime = sampledate) %>%
  filter(In.Out == 'Inflow', datetime >= startDate) %>% 
  group_by(datetime) %>%
  summarise(SW_TOC = mean(TOC.mg.L)) %>% # Mean daily across multiple inflows
  mutate(SW_TOC = round(SW_TOC, 1), SW_DOC = (SW_TOC * 0.9)) %>%
  write_csv('./VanernLake/Vanern_inflow_DOC_noInterp.csv')

inflow <- read_excel('./VanernLake/Staging Files/Vanern_Variables/1954-VARGÖNS KRV.xls', skip = 13) %>%
  rename(datetime = X__1, FlowOut = `Water Flow [m³/s]`) %>% select(-`Datakontroll\nvattenföring`) %>%
  mutate(datetime = ymd(datetime)) %>%
  filter(datetime >= startDate) %>% 
  mutate(FlowIn = FlowOut) %>%
  write_csv('./VanernLake/Vanern_inflow_volume_noInterp.csv')