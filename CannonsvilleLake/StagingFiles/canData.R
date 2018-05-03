setwd('~/Documents/Rpackages/SOS/CannonsvilleLake/StagingFiles/')
library(dplyr)
library(tidyr)
### Cannonsville Reservoir ####
vol = 362263907.7288 *0.3 #m3 to get residence time correct

######################## VOLUME ########################
stage = read.csv('Cannonsville_stage.csv',stringsAsFactors = F)
stage$DateTime = as.Date(strptime(stage$DateTime,'%Y-%m-%d'))

hypso = read.csv('hypsometry.csv',stringsAsFactors = F,header = F)
hypso = as.data.frame(t(hypso),stringsAsFactors = F)
hypso = hypso[-1,]
names(hypso) = c('Depth','Elev','Area')
hypso$Depth = as.numeric(hypso$Depth)
hypso$Area = as.numeric(hypso$Area)
plot(hypso$Depth,hypso$Area)

# Let’s fit it using R. When fitting polynomials you can either use
y = hypso$Area; q = hypso$Depth
model <- lm(y ~ stats::poly(q,3,raw=T))
a = predict(model,data.frame(q=1:52))

# Create new dataframe with volume 
newHyp = data.frame(depth=1:52,area=a,volume=cumsum(a))
plot(newHyp$depth,newHyp$volume)
y = newHyp$volume; q = 1:52
model2 <- lm(y ~ stats::poly(q,3,raw=T))

getVol <- function(depth){
  vol = predict(model2,data.frame(q=depth))
  return(vol)
}

stage$vol = getVol(stage$Stage)
stageOut = data.frame(date = stage$DateTime, Volume = stage$vol)

######################## METEOROLOGY ########################
met = read.csv('NLDAS2_Cannonsville_C6.csv',stringsAsFactors = F)
rain = met %>% mutate(date = as.Date(time)) %>%
  group_by(date) %>%
  summarise(rainD = 1000*sum(Rain)/24) %>%
  ungroup() %>%
  select(date,Rain = rainD)
write.csv(rain,'cannonsvilleRain.csv',row.names = F)

rainOut = rain
names(rainOut) = c('datetime','Rain')
write.csv(rainOut,'../CannonsvilleRain.csv',row.names = F,quote=F)

#### Cannonsville grab samples #####
can = read.csv('DSTRLIM9516.csv',stringsAsFactors = F)
can$dateTime = as.Date(strptime(can$COLLECT,'%d%b%Y'))
can = can[can$SITE == '4WDC',]
head(can)


######################## TEMPERATURE ########################
canWide <- can %>% select(dateTime,depth=ZSP,temp=TEMP) %>%
  mutate_each(funs(replace(.,.<0, NA)),temp) %>%
  group_by(dateTime,depth) %>%
  dplyr::summarise(wtempA = mean(temp,na.rm=T),datetime = first(dateTime)) %>%
  ungroup() %>%
  dplyr::select(datetime,depth,wtempA) %>%
  spread(depth,wtempA)

depths = as.numeric(colnames(canWide)[-1])[-31]
canWide = canWide[,1:ncol(canWide)-1] #remove last column NA

colnames(canWide)[-1] = paste('wtr_',colnames(canWide)[-1],sep='')

# Remove rows with no data 
indxRemove = apply(!(apply(X = canWide[,-1],1,is.na)),2,any)
canWide2 = canWide[indxRemove,]
# Calculate thermocline depths
thermoDepths = ts.thermo.depth(canWide2,na.rm=T)
thermoDepths$thermo.depth[thermoDepths$thermo.depth < 5 | is.na(thermoDepths$thermo.depth)] = 7

layerTemps = data.frame(date = thermoDepths$datetime,EpiTemp = NA,HypoTemp = NA,stringsAsFactors = F)
for (d in 1:nrow(layerTemps)) {
  temps = canWide2[canWide2$datetime == layerTemps$date[d],][,-1]
  epiIndx = depths < thermoDepths$thermo.depth[d]
  layerTemps$EpiTemp[d] = round(mean(as.numeric(temps[epiIndx]),na.rm=T),2)
  layerTemps$HypoTemp[d] = round(mean(as.numeric(temps[!epiIndx]),na.rm=T),2)
}

surfTemp = data.frame(date = canWide2$datetime, surfTemp = rowMeans(data.frame(canWide2$wtr_0,canWide2$wtr_3),na.rm=T))
write.csv(layerTemps,'cannonsvilleWaterTemps.csv',row.names = F)

######################## OXYGEN ########################
o2Wide <- can2 %>% select(dateTime,depth=ZSP,DO) %>%
  mutate_each(funs(replace(.,.<0, NA)),DO) %>%
  group_by(dateTime,depth) %>%
  dplyr::summarise(o2A = mean(DO,na.rm=T),datetime = first(dateTime)) %>%
  ungroup() %>%
  dplyr::select(datetime,depth,o2A) %>%
  spread(depth,o2A)

o2 = data.frame(date = o2Wide$datetime,o2 = rowMeans(data.frame(o2Wide$`0`,o2Wide$`2`,o2Wide$`3`),na.rm=T),stringsAsFactors = F)
o2 = o2[!is.na(o2$o2),]

o2_out = right_join(surfTemp,o2,by='date')
colnames(o2_out) = c('date','wtr','DO_con')
write.csv(o2_out,'cannonsvilleDO.csv',row.names = F)

o2SOS = o2_out
names(o2SOS) = c('datetime','wtr','DO_con')
write.csv(o2SOS,'../CannonsvilleValidationDO.csv',row.names = F,quote=F)

######################## TP and DOC ########################
# MEAN DOC and TP
chem2 = can2 %>% select(date = dateTime,depth=ZSP,DOC,TP,TDP,CHLA,CHLORA,secchi=ZSD) %>%
  mutate_each(funs(replace(.,.<0, NA)),DOC:CHLORA) %>%
  mutate(chl = rowMeans(cbind(CHLA,CHLORA),na.rm=T)) %>%
  group_by(date) %>%
  dplyr::summarise(doc = round(mean(DOC,na.rm=T),2),tp = round(mean(TP,na.rm=T),2),
                   tdp = round(mean(TDP,na.rm=T),2),chl = round(mean(chl,na.rm=T),2),
                   secchi = round(mean(secchi,na.rm=T),2)) %>% 
  ungroup 

# SURFACE DOC
chem3 =  can2 %>% select(date = dateTime,depth=ZSP,DOC,TP,TDP,CHLA,CHLORA,secchi=ZSD) %>%
  dplyr::filter(depth <=3) %>%
  mutate_each(funs(replace(.,.<0, NA)),DOC:CHLORA) %>%
  mutate(chl = rowMeans(cbind(CHLA,CHLORA),na.rm=T)) %>%
  group_by(date) %>%
  dplyr::summarise(surfdoc = round(mean(DOC,na.rm=T),2),surftp = round(mean(TP,na.rm=T),2),
                   surftdp = round(mean(TDP,na.rm=T),2),surfchl = round(mean(chl,na.rm=T),2)) %>%
  ungroup 

chem4 = left_join(chem2,chem3,by='date')
write.csv(chem4,'cannonsvilleCHEM.csv',row.names = F)
chemTPCHL = chem4 %>% select(date,TP=surftp,Chla = surfchl)
secchi = chem4 %>% select(date,Secchi = secchi)

chem5 = data.frame(datetime = chem4$date,DOC = chem4$surfdoc,DOCwc = chem4$doc)
chem5 = chem5[!is.na(chem5$DOC),]
write.csv(chem5,'../CannonsvilleValidationDOC.csv',row.names = F,quote=F)


######################## INFLOW ########################
inflow = read.csv('Neversink_river.csv',stringsAsFactors = F)
inflow2 = read.csv('Neversink_ungaged.csv',stringsAsFactors = F)

inflow$time = as.Date(strptime(inflow$time,'%Y-%m-%d'))
inflow2$time = as.Date(strptime(inflow2$time,'%Y-%m-%d'))

inflowAll = left_join(inflow,inflow2,by='time')
inflowAll$inflow = inflowAll$flow.x + inflowAll$flow.y
inflowOut = inflowAll %>% dplyr::select(date = time, FlowIn = inflow )
write.csv(inflowOut,'cannonsvilleQ.csv',row.names = F)

######################## INFLOW DOC ########################
inDOC = read.csv('CBS_DOC_1995-31Oct2016.csv',stringsAsFactors = F)
inDOC$Date = as.Date(strptime(inDOC$Date,'%Y-%m-%d'))
swDOC = inDOC %>% select(date = Date,SW_DOC = FNConc) 
write.csv(swDOC,'cannonsvilleSWDOC.csv',row.names = F)

## Test residence time ##
test = inflowOut %>% mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(Q = mean(discharge))
# Residence Time
vol / (mean(test$Q,na.rm = T) * (60*60*24*365))

######################## COMBINE DATAFRAMES ########################
df = data.frame(date = seq.Date(as.Date('2000-04-10'),as.Date('2015-12-31'),by='day'))
df = left_join(df,stageOut, by='date')
df = left_join(df,inflowOut,by='date')
df$FlowOut = df$FlowIn
df = left_join(df,rain,by='date')
df = left_join(df,layerTemps,by='date')
df = left_join(df,chemTPCHL,by='date')
df = left_join(df,swDOC,by='date')
df = left_join(df,secchi,by='date')
names(df)[1] = 'datetime'
df$Secchi[is.nan(df$Secchi)] = 2
 
write.csv(df,'../CannonsvilleTS.csv',row.names = F,quote = F)





