setwd('C:/Users/hdugan/Documents/Rpackages/SOS/')
# setwd("~/Documents/Rpackages/SOS")

LakeName = 'Trout'

getRT <- function (LakeName){
  ValidationFlag = 1
  WriteFiles = 1
  BootstrapFlag = 0
  timestampFormat =	'%Y-%m-%d'
  ##### INPUT FILE NAMES ################
  TimeSeriesFile <- paste('./',LakeName,'Lake/',LakeName,'TS.csv',sep='')
  RainFile <- paste('./',LakeName,'Lake/',LakeName,'Rain.csv',sep='')
  ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
  FreeParFile <- paste('./R/FMEresults/',LakeName,'_fitpars.csv',sep='')
  ValidationFileDOC <- paste('./',LakeName,'Lake/',LakeName,'ValidationDOC.csv',sep='')
  ValidationFileDO <- paste('./',LakeName,'Lake/',LakeName,'ValidationDO.csv',sep='')
  
  ##### LOAD PACKAGES ########################
  library(signal)
  library(zoo)
  library(lubridate)
  library(LakeMetabolizer)
  library(plyr)
  library(dplyr)
  
  ##### LOAD FUNCTIONS #######################
  source("./R/Model_March2017//SOS_Sedimentation.R")
  source("./R/Model_March2017/SOS_SWGW.R")
  source("./R/Model_March2017/SOS_GPP.R")
  source("./R/Model_March2017/SOS_Resp.R")
  source("./R/Model_March2017/modelDOC_7.R")
  source("./R/Model_March2017/SOS_fixToolik.R")
  
  ##### READ PARAMETER FILE ##################
  parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  for (i in 1:nrow(parameters)){ # assign parameters
    assign(parameters[i,1],parameters[i,2])
  }
  freeParams <- read.table(file = FreeParFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  freeParamsNames <- c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
  for (i in 1:4){ # assign parameters
    assign(freeParamsNames[i],freeParams[i,1])
  }
  ##### READ MAIN INPUT FILE #################
  RawData <- read.csv(TimeSeriesFile,header=T) #Read main data file with GLM outputs (physical input) and NPP input
  RawData$datetime <- as.POSIXct(strptime(RawData$datetime,timestampFormat),tz="GMT") #Convert time to POSIX
  cc = which(complete.cases(RawData))
  RawData = RawData[cc[1]:tail(cc,1),]
  
  # Fill time-series gaps (linear interpolation)
  ts_new <- data.frame(datetime = seq(RawData$datetime[1],RawData$datetime[nrow(RawData)],by="day")) #Interpolate gapless time-series
  InputData <- merge(RawData,ts_new,all=T)
  for (col in 2:ncol(InputData)){
    InputData[,col] <- na.approx(InputData[,col],na.rm = T)}
  InputData$Chla[InputData$Chla == 0] = 0.0001
  
  ##### READ RAIN FILE #######################
  RainData <- read.csv(RainFile,header=T,stringsAsFactors = F) #Read daily rain file (units=mm) Read separately and added back to main file to avoid issues of linear interpolation with rain data in length units
  RainData$datetime <- as.POSIXct(strptime(RainData$datetime,timestampFormat,tz='GMT'))
  
  InputData$Rain <- RainData$Rain[RainData$datetime %in% InputData$datetime] #Plug daily rain data into InputData file to integrate with original code.
  #### For TOOLIK ONLY #### (dealing with ice season)
  if (LakeName=='Toolik') {
    InputData = fixToolik(InputData,LakeName)
  }
  
  df = InputData %>% select(datetime,FlowIn,Rain) %>%
    mutate(Year = year(datetime), Flow_m3_d = FlowIn * 3600*24,
           GW_m3_d =  Flow_m3_d/(1-PropGW) - Flow_m3_d,
           Precip_m3_d = Rain*0.001*LakeArea) %>%
    group_by(Year) %>%
    summarise_each(.,funs(sum)) %>%
    ungroup() %>%
    select(Year,Flow_m3_d,GW_m3_d,Precip_m3_d)
  
  totalIn = mean(df$Flow_m3_d)+mean(df$GW_m3_d)+mean(df$Precip_m3_d)
  print(paste0('Inflow % ',round(mean(df$Flow_m3_d)/totalIn,2)))
  print(paste0('GW % ',round(mean(df$GW_m3_d)/totalIn,2)))
  print(paste0('Precip % ',round(mean(df$Precip_m3_d)/totalIn,2)))
  print(paste0('RT = ',LakeVolume/(mean(df$Flow_m3_d)+mean(df$GW_m3_d)+mean(df$Precip_m3_d))))
}

getRT('Trout')
getRT('Monona')
getRT('Harp')
getRT('Vanern')
getRT('Toolik')
