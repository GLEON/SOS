library(dplyr)
library(viridisLite)
library(tidyr)

q = read.delim('outflow USGS 05357245 TROUT RIVER.txt',stringsAsFactors = F)
head(q)
q = q %>% mutate(date = as.Date(datetime)) %>%
  mutate(discharge_m3s = as.numeric(X153725_00060_00003) * 0.0283168) %>%
  select(date,discharge_m3s)
plot(q)

doc = read.csv('webb_chem_phys_all_sites.csv',stringsAsFactors = F)
head(doc)
unique(doc$site_name)
doc = doc %>% mutate(date = as.Date(sampledate)) %>%
  select(site_name,date,doc) %>%
  group_by(site_name,date) %>%
  summarise_at('doc',mean,na.rm=T) %>%
  filter(!is.na(doc))

sdoc = spread(doc,site_name,doc) %>%
  select(date:`STEVENSON CREEK`) %>%
  filter(complete.cases(.)) %>%
  mutate(AC = `ALLEQUASH CREEK` * sites$drainage_area[1]/sum(sites$drainage_area[1:3]),
         NC = `NORTH CREEK` * sites$drainage_area[2]/sum(sites$drainage_area[1:3]),
         SC = `STEVENSON CREEK` * sites$drainage_area[3]/sum(sites$drainage_area[1:3])) %>%
  mutate(SW_DOC = AC+NC+SC)

output = sdoc %>% select(date,DOCtot)
write.csv(output,'outflowDOC.csv',row.names = F)
write.csv(q,'outflowQ.csv',row.names = F)

