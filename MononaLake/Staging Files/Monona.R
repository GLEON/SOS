setwd('~/Documents/Rpackages/SOS/MononaLake/Staging Files/')

### Lake Monona ####
vol = 110*10^6 #m3

phys = read.csv('physical_limnology_of_the_north_temperate_lakes_primary_study_lakes.csv',stringsAsFactors = F)
phys$sampledate = as.Date(strptime(phys$sampledate,'%Y-%m-%d'))
######################## TEMPERATURE ########################

physWide <- phys %>%
  group_by(sampledate,depth) %>%
  dplyr::summarise(wtempA = mean(wtemp,na.rm=T),o2A = mean(o2,na.rm=T),datetime = first(sampledate)) %>%
  ungroup() %>%
  dplyr::select(datetime,depth,wtempA) %>%
  spread(depth,wtempA)

depths = as.numeric(colnames(physWide)[-1])
colnames(physWide)[-1] = paste('wtr_',colnames(physWide)[-1],sep='')

# Remove rows with no data 
indxRemove = apply(!(apply(X = physWide[,-1],1,is.na)),2,any)
physWide_2 = physWide[indxRemove,]
# Calculate thermocline depths
thermoDepths = ts.thermo.depth(physWide_2,na.rm=T)
thermoDepths$thermo.depth[thermoDepths$thermo.depth < 5 | is.na(thermoDepths$thermo.depth)] = 10

layerTemps = data.frame(date = thermoDepths$datetime,epiTemp = NA,hypoTemp = NA,stringsAsFactors = F)
for (d in 1:nrow(layerTemps)) {
  temps = physWide_2[physWide_2$datetime == layerTemps$date[d],][,-1]
  epiIndx = depths < thermoDepths$thermo.depth[d]
  layerTemps$epiTemp[d] = round(mean(as.numeric(temps[epiIndx]),na.rm=T),2)
  layerTemps$hypoTemp[d] = round(mean(as.numeric(temps[!epiIndx]),na.rm=T),2)
}

surfTemp = data.frame(date = physWide_2$datetime, surfTemp = physWide_2$wtr_0)

######################## OXYGEN ########################
o2Wide <- phys %>%
  group_by(sampledate,depth) %>%
  dplyr::summarise(wtempA = mean(wtemp,na.rm=T),o2A = mean(o2,na.rm=T),datetime = first(sampledate)) %>%
  ungroup() %>%
  dplyr::select(datetime,depth,o2A) %>%
  spread(depth,o2A)

o2 = data.frame(date = o2Wide$datetime,o2 = rowMeans(data.frame(o2Wide$`0`,o2Wide$`1`),na.rm=T),stringsAsFactors = F)
o2 = o2[!is.na(o2$o2),]

o2_out = right_join(surfTemp,o2,by='date')
colnames(o2_out) = c('datetime','wtr','DO_con')
write.csv(o2_out,'../MononaValidationDO.csv',row.names = F,quote=F)
######################## TP and DOC ########################
chem = read.csv('chemical_limnology_of_north_temperate_lakes_lter_primary_study_lakes__nutrients_ph_and_carbon.csv',stringsAsFactors = F)
chem$sampledate = as.Date(strptime(chem$sampledate,'%Y-%m-%d'))
chem$doc[chem$doc < 0 & !is.na(chem$doc)] = NA # Remove negative numbers
chem$totpuf_sloh[chem$totpuf_sloh < 0 & !is.na(chem$totpuf_sloh)] = NA # Remove negative numbers

# MEAN DOC and TP
chem2 = chem %>% select(date = sampledate, doc = doc, tp = totpuf_sloh) %>%
  group_by(date) %>%
  dplyr::summarise(docMean = round(mean(doc,na.rm=T),2),tp = round(mean(tp,na.rm=T),2)) %>%
  ungroup

# SURFACE DOC
chem3 = chem %>% select(date = sampledate,depth, doc = doc) %>%
  dplyr::filter(depth == 0 | depth==1) %>%
  group_by(date) %>%
  dplyr::summarise(docSurf = round(mean(doc,na.rm=T),2)) %>%
  ungroup

chem4 = left_join(chem2,chem3,by='date')

chem5 = data.frame(datetime = chem4$date,DOC = chem4$docSurf,DOCwc = round(chem4$docMean,3))
chem5 = chem5[!is.na(chem5$DOC),]
chem5 = chem5[chem5$DOC < 30,]
write.csv(chem5,'../MononaValidationDOC.csv',row.names = F,quote=F)
######################## SECCHI ########################
secchi = read.csv('north_temperate_lakes_lter__secchi_disk_depth__other_auxiliary_base_crew_sample_data.csv',stringsAsFactors = F)
secchi$sampledate = as.Date(strptime(secchi$sampledate,'%Y-%m-%d'))

secchi2 = secchi %>% select(date = sampledate,secchi = secnview)

######################## CHL ########################
chl = read.csv('north_temperate_lakes_lter__chlorophyll_-_madison_lakes_area.csv',stringsAsFactors = F)
chl$sampledate = as.Date(strptime(chl$sampledate,'%Y-%m-%d'))

chl2 = chl %>% select(date = sampledate, chl = correct_chl_fluor) %>%
  group_by(date) %>%
  dplyr::summarise(chl = mean(chl,na.rm=T)) %>%
  ungroup


######################## INFLOW ########################
inflow = read.csv('USGS-05428500_NWIS_YaharaOutlet.csv',stringsAsFactors = F)
inflow$datetime = as.Date(strptime(inflow$datetime,'%Y-%m-%d'))
inflow$Discharge_ft3_s = as.numeric(inflow$Discharge_ft3_s)* 0.0283168

inflow2 = inflow %>% select(date = datetime, discharge = Discharge_ft3_s )

######################## INFLOW DOC FROM MENDOTA ########################
inDOC = read.csv('../../MendotaLake_HD/MendotaValidationDOC.csv',stringsAsFactors = F)
inDOC$datetime = as.Date(strptime(inDOC$datetime,'%Y-%m-%d'))

######################## RAIN ########################
rain = read.csv('NLDAS2_dailyrain_1979_2014.csv',stringsAsFactors = F)
rain$date = as.Date(strptime(rain$date,'%Y-%m-%d'))

names(rain)= c('datetime','Rain')
write.csv(rain,'../MononaRain.csv',row.names = F,quote=F)

######################## COMBINE DATAFRAMES ########################
df = data.frame(datetime = seq.Date(as.Date('2003-12-02'),as.Date('2014-12-31'),by='day'))
df$Volume = vol
df$FlowIn[df$datetime %in% inflow2$date] = inflow2$discharge[inflow2$date %in% df$datetime]
df$FlowOut = df$FlowIn
df$Rain[df$datetime %in% rain$datetime] = rain$Rain[rain$datetime %in% df$datetime]
df$HypoTemp[df$datetime %in% layerTemps$date] = layerTemps$hypoTemp[layerTemps$date %in% df$datetime]
df$EpiTemp[df$datetime %in% layerTemps$date] = layerTemps$epiTemp[layerTemps$date %in% df$datetime]
df$TP[df$datetime %in% chem4$date] = chem4$tp[chem4$date %in% df$datetime]
df$Chla[df$datetime %in% chl2$date] = chl2$chl[chl2$date %in% df$datetime]
df$SW_DOC[df$datetime %in% inDOC$datetime] = inDOC$DOC[inDOC$datetime %in% df$datetime]
df$Secchi[df$datetime %in% secchi2$date] = secchi2$secchi[secchi2$date %in% df$datetime]

write.csv(df,'../MononaTS.csv',row.names = F,quote = F)
