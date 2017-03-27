# setwd('C:/Users/hdugan/Documents/Rpackages/SOS/')
# setwd("~/Documents/Rpackages/SOS")
#Flags 1 for yes, else no.
LakeName = 'Trout'
ValidationFlag = 1
WriteFiles = 0
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
library(parallel)

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

###### Run Period and Time Step Setup #####
TimeStep <- as.numeric(InputData$datetime[2]-InputData$datetime[1]) #days
steps <- nrow(InputData)

##### Declare Output Data Storage ##########
POC_df = data.frame(Date = InputData$datetime, POCtotal_conc_gm3 = NA,
                    POCR_conc_gm3 = NA, POCL_conc_gm3 = NA,
                    NPPin_gm2y=NA,FlowIn_gm2y=NA,FlowOut_gm2y=NA,sedOut_gm2y=NA,leachOut_gm2y=NA,
                    POC_flowOut_gm2y = NA, POC_sedOut_gm2y = NA,
                    POCload_g = NA, POCalloch_g = NA, POCautoch_g = NA,
                    POCout_g = NA)
DOC_df = data.frame(Date = InputData$datetime,DOCtotal_conc_gm3 = NA,
                    DOCR_conc_gm3 = NA, DOCL_conc_gm3 = NA,
                    NPPin_gm2y=NA,FlowIn_gm2y=NA,FlowOut_gm2y=NA,respOut_gm2y=NA,leachIn_gm2y=NA,
                    DOC_flowOut_gm2y = NA, DOC_respOut_gm2y = NA,
                    DOCload_g = NA, DOCalloch_g = NA, DOCautoch_g = NA,
                    DOCout_g = NA)

##### Declare Data Storage - Sed ###########
SedData <- data.frame(Date = InputData$datetime,BurialScalingFactor_R=NA,MAR_oc_R=NA,POC_burial_R=NA,
                      BurialScalingFactor_L=NA,MAR_oc_L=NA,POC_burial_L=NA,
                      MAR_oc_total=NA,POC_burial_total=NA)

##### Declare Data Storage - NPP ###########
PPdata <- data.frame(Date = InputData$datetime,GPP_DOCL_rate=NA,GPP_POCL_rate=NA,NPP_DOCL_mass=NA,NPP_POCL_mass=NA, DOCL_massRespired=NA)
Metabolism <- data.frame(Date = InputData$datetime,NEP=NA,Oxygen=NA,Oxygen_conc=NA)

##### Declare Data Storage - SW/GW #########
SWGWData <- data.frame(Date = InputData$datetime,POCR_Aerial=NA, POCR_SW=NA, DOCR_Wetland=NA, 
                       DOCR_gw=NA, DOCR_SW=NA, DailyRain=NA, DOCR_precip=NA, Load_DOCR=NA, Load_POCR=NA,
                       POCR_massIn_g = NA, DOCR_massIn_g = NA, 
                       POCR_outflow = NA, DOCR_outflow = NA, POCL_outflow = NA, DOCL_outflow = NA)

#### Declare Data Storage - POC to DOC Leaching ####
LeachData <- data.frame(Date = InputData$datetime,POCR_leachOut = NA,DOCR_leachIn = NA,
                        POCL_leachOut = NA,DOCL_leachIn = NA)

##### Declare Data Storage - Source of Sink? #
SOS <- data.frame(Date = InputData$datetime,Source=NA,Sink=NA,Pipe=NA,Net=NA)

##### Carbon Concentration Initialization ################
POC_df$POCtotal_conc_gm3[1] <- POC_init # #Initialize POC concentration as baseline average
DOC_df$DOCtotal_conc_gm3[1] <- DOC_init #Initialize DOC concentration g/m3
DOC_df$DOCR_conc_gm3[1] <- DOC_init*0.8 #Initialize DOC concentration g/m3
DOC_df$DOCL_conc_gm3[1] <- DOC_init*0.2 #Initialize DOC concentration g/m3
POC_df$POCR_conc_gm3[1] <- POC_init*0.8 #Initialize POC concentration g/m3
POC_df$POCL_conc_gm3[1] <- POC_init*0.2 #Initialize POC concentration g/m3
Metabolism$Oxygen_conc[1] <- o2.at.sat.base(InputData$EpiTemp[1])

####################### Validation Output Setup ######################################

#DOC Validation Output Setup
ValidationDataDOC <- read.csv(ValidationFileDOC,header=T)
ValidationDataDOC$datetime <- as.Date(strptime(ValidationDataDOC$datetime,timestampFormat),tz="GMT") #Convert time to POSIX
ValidationDataDOC = ValidationDataDOC[complete.cases(ValidationDataDOC),]
outlier.limit = (mean(ValidationDataDOC$DOC) + 3*(sd(ValidationDataDOC$DOC))) # Calculate mean + 3 SD of DOC column
ValidationDataDOC = ValidationDataDOC[ValidationDataDOC$DOC <= outlier.limit,] # Remove rows where DOC > outlier.limit
ValidationDataDOC = ddply(ValidationDataDOC,'datetime',summarize,DOC=mean(DOC),DOCwc=mean(DOCwc))

#DO Validation Output Setup
ValidationDataDO <- read.csv(ValidationFileDO,header=T)
ValidationDataDO$datetime <- as.Date(strptime(ValidationDataDO$datetime,timestampFormat),tz="GMT") #Convert time to POSIX
ValidationDataDO = ValidationDataDO[complete.cases(ValidationDataDO),]
#Only compare to DO data during "production season."
ValidationDataDO = ValidationDataDO[yday(ValidationDataDO$datetime)>ProdStartDay & yday(ValidationDataDO$datetime)<ProdEndDay,]
ValidationDataDO = ddply(ValidationDataDO,'datetime',summarize,wtr=mean(wtr,na.rm = T),DO_con=mean(DO_con))

k <- 0.7 #m/d
PhoticDepth <- data.frame(datetime = InputData$datetime,PhoticDepth = log(100)/(1.7/InputData$Secchi))
IndxVal = ValidationDataDO$datetime %in% as.Date(PhoticDepth$datetime)
IndxPhotic = as.Date(PhoticDepth$datetime) %in% ValidationDataDO$datetime
ValidationDataDO = ValidationDataDO[IndxVal,]
ValidationDataDO$DO_sat <- o2.at.sat(ValidationDataDO[,1:2])[,2]  
ValidationDataDO$Flux <- k*(ValidationDataDO$DO_con - ValidationDataDO$DO_sat)/(0.5*PhoticDepth$PhoticDepth[IndxPhotic]) #g/m3/d
#SedData MAR OC 
ValidationDataMAROC <- ObservedMAR_oc #g/m2

# ####################### END MODEL SETUP #################################
# ####################### MAIN PROGRAM #############################################

for (i in 1:(steps)) {
  Q_sw <- InputData$FlowIn[i] #m3/s surface water flowrate at i
  Q_gw <- Q_sw/(1-PropGW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
  Q_out <- InputData$FlowOut[i] #m3/s: total outflow. Assume steady state pending dynamic output
  Rainfall <- InputData$Rain[i]/TimeStep #mm/day
  
  #Call GPP function
  PhoticDepth <- log(100)/(1.7/InputData$Secchi[i]) #Calc photic depth as function of Secchi depth
  if (PhoticDepth>LakeDepth){PhoticDepth<-LakeDepth} #QC - If photic depth calc'ed as greater than lake depth, photic depth = lake depth
  GPPrates <- GPP(InputData$Chla[i],InputData$TP[i],PhoticDepth,InputData$EpiTemp[i],yday(InputData$datetime[i])) #mg C/m^2/d
  PPdata$GPP_DOCL_rate[i] = 0.2*GPPrates$GPP_DOC_rate #mg C/m2/d
  PPdata$GPP_POCL_rate[i] = 0.2*GPPrates$GPP_POC_rate #mg C/m2/d #All NPP in POC
  PPdata$NPP_DOCL_mass[i] <- PPdata$GPP_DOCL_rate[i]*LakeArea*TimeStep/1000 #g
  PPdata$NPP_POCL_mass[i] <- PPdata$GPP_POCL_rate[i]*LakeArea*TimeStep/1000 #g
  
  #Call heterotrophic respiration function for recalitrant DOC pool (DOCR) and labile DOC pool (DOCL)
  DOCR_resp_rate <- Resp(DOC_df$DOCR_conc_gm3[i],InputData$EpiTemp[i],DOCR_RespParam) #g C/m3/d
  DOCL_resp_rate <- Resp(DOC_df$DOCL_conc_gm3[i],InputData$EpiTemp[i],DOCL_RespParam) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
  
  PPdata$DOCR_massRespired[i] = DOCR_resp_rate*LakeVolume*TimeStep #g C
  PPdata$DOCL_massRespired[i] = DOCL_resp_rate*LakeVolume*TimeStep #g C
  
  #Calc metabolism (DO) estimates for PP validation
  Metabolism$NEP[i] <- (PPdata$NPP_DOCL_mass[i] + PPdata$NPP_POCL_mass[i] - PPdata$DOCR_massRespired[i] - PPdata$DOCL_massRespired[i])/
    (LakeVolume*PhoticDepth/LakeDepth)/TimeStep #g/m3/d #volume of photic zone
  Metabolism$Oxygen[i] <- (Metabolism$NEP[i])*(32/12) #g/m3/d Molar conversion of C flux to O2 flux (lake metabolism)
  
  #Call SWGW Function (Surface Water/GroundWater)
  SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,AerialLoad, PropCanopy, LakePerimeter, WetlandLoad, PropWetlands, DOC_gw, 
                       InputData$SW_DOC[i], DOC_precip, LakeArea) # All in g/day, except DailyRain in m3/day
  #change these inputs to iterative [i] values when inputs are dynamic
  SWGWData[i,2:10] <- SWGW
  #LOAD DOC (g/d) = DOC_Wetland + DOC_GW + DOC_SW +DOC_Precip # g/d DOC
  #LOAD POC (g/d) = POC_Aerial + POC_SW # g/d POC roughly estimated as (0.1 * DOC)
  
  #Calculate load from SWGW_in
  SWGWData$DOCR_massIn_g[i] <- SWGWData$Load_DOCR[i]*TimeStep #g
  SWGWData$POCR_massIn_g[i] <- SWGWData$Load_POCR[i]*TimeStep #g
  
  #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
  SWGWData$POCR_outflow[i] <- SWGWData$Load_POCR[i]*(Q_out*60*60*24/LakeVolume)*TimeStep #g
  SWGWData$POCL_outflow[i] <- PPdata$NPP_POCL_mass[i]*(Q_out*60*60*24/LakeVolume)*TimeStep #g
  SWGWData$DOCR_outflow[i] <- DOC_df$DOCR_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
  SWGWData$DOCL_outflow[i] <- DOC_df$DOCL_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
  
  #POC mass left after outflow
  POCR_mass <- SWGWData$Load_POCR[i] - (SWGWData$Load_POCR[i]*(Q_out*60*60*24/LakeVolume)) #g/day
  POCL_mass <- PPdata$NPP_POCL_mass[i] - (PPdata$NPP_POCL_mass[i]*(Q_out*60*60*24/LakeVolume)) #g/day

  #Call Sedimentation Function
  SedOutput_R <- SedimentationFunction(BurialFactor_R,TimeStep,POCR_mass,LakeArea)
  SedOutput_L <- SedimentationFunction(BurialFactor_L,TimeStep,POCL_mass,LakeArea)
  SedData[i,2:4] = SedOutput_R
  SedData[i,5:7] = SedOutput_L
  SedData[i,8:9] = (SedOutput_L + SedOutput_R) [2:3]
  
  #Calc POC-to-DOC leaching
  LeachData$POCR_leachOut[i] <- POCR_mass*(1-BurialFactor_R)*TimeStep #g - POC concentration times leaching parameter
  LeachData$DOCR_leachIn[i] <- LeachData$POCR_leachOut[i]
  LeachData$POCL_leachOut[i] <- POCL_mass*(1-BurialFactor_L)*TimeStep #g - POC concentration times leaching parameter
  LeachData$DOCL_leachIn[i] <- LeachData$POCL_leachOut[i]
  
  if (i < steps) { #don't calculate for last time step
    Fatm = 0.7*(Metabolism$Oxygen_conc[i] - o2.at.sat.base(InputData$EpiTemp[i]))/(PhoticDepth/2)
    Metabolism$Oxygen_conc[i+1] = Metabolism$Oxygen_conc[i] + Metabolism$Oxygen[i] - Fatm
    
    #Update POC and DOC concentration values (g/m3) for whole lake
    POC_df$POCL_conc_gm3[i] <-  (POCL_mass - LeachData$POCL_leachOut[i] - SedData$POC_burial_L[i])/LakeVolume #g/m3
    POC_df$POCR_conc_gm3[i] <-  (POCR_mass - LeachData$POCR_leachOut[i] - SedData$POC_burial_R[i])/LakeVolume
    POC_df$POCtotal_conc_gm3[i] = POC_df$POCR_conc_gm3[i] + POC_df$POCL_conc_gm3[i]

    DOC_df$DOCL_conc_gm3[i+1] <- DOC_df$DOCL_conc_gm3[i] + ((PPdata$NPP_DOCL_mass[i] + LeachData$DOCL_leachIn[i] - SWGWData$DOCL_outflow[i] - PPdata$DOCL_massRespired[i])/LakeVolume) #g/m3
    DOC_df$DOCR_conc_gm3[i+1] <- DOC_df$DOCR_conc_gm3[i] + ((SWGWData$DOCR_massIn_g[i] + LeachData$DOCR_leachIn[i] - SWGWData$DOCR_outflow[i] - PPdata$DOCR_massRespired[i])/LakeVolume) #g/m3
    DOC_df$DOCtotal_conc_gm3[i+1] = DOC_df$DOCR_conc_gm3[i+1] + DOC_df$DOCL_conc_gm3[i+1]
    
    #Stop code and output error if concentrations go to negative
    if (DOC_df$DOCtotal_conc_gm3[i+1]<=0){stop("Negative DOC concentration!")}
  }
}

#Store POC and DOC fluxes as mass/area/time (g/m2/yr)
POC_df$NPPin_gm2y <-  PPdata$NPP_POCL_mass/LakeArea/(TimeStep/365)
POC_df$FlowIn_gm2y <- SWGWData$POCR_massIn_g/LakeArea/(TimeStep/365)
POC_df$FlowOut_gm2y <- (SWGWData$POCR_outflow + SWGWData$POCL_outflow)/LakeArea/(TimeStep/365)
POC_df$sedOut_gm2y <- SedData$POC_burial_total/LakeArea/(TimeStep/365)
POC_df$leachOut_gm2y <- (LeachData$POCR_leachOut + LeachData$POCL_leachOut)/LakeArea/(TimeStep/365)

DOC_df$NPPin_gm2y <- PPdata$NPP_DOCL_mass/LakeArea/(TimeStep/365)
DOC_df$FlowIn_gm2y <- SWGWData$DOCR_massIn_g/LakeArea/(TimeStep/365)
DOC_df$FlowOut_gm2y <- (SWGWData$DOCR_outflow + SWGWData$DOCL_outflow)/LakeArea/(TimeStep/365) 
DOC_df$respOut_gm2y<- (PPdata$DOCR_massRespired + PPdata$DOCL_massRespired)/LakeArea/(TimeStep/365) 
DOC_df$leachIn_gm2y <- (LeachData$DOCR_leachIn + LeachData$DOCL_leachIn)/LakeArea/(TimeStep/365)

#Cumulative DOC and POC fate (grams)
POC_df$POC_flowOut_gm2y <- cumsum(SWGWData$POCR_outflow + SWGWData$POCL_outflow)
POC_df$POC_sedOut_gm2y <- cumsum(SedData$POC_burial_total)
DOC_df$DOC_flowOut_gm2y = cumsum(SWGWData$DOCR_outflow + SWGWData$DOCL_outflow)
DOC_df$DOC_respOut_gm2y = cumsum(PPdata$DOCR_massRespired + PPdata$DOCL_massRespired)

#POC and DOC load (in) and fate (out) (g)
POC_df$POCload_g <- PPdata$NPP_POCL_mass + SWGWData$POCR_massIn_g #g
POC_df$POCalloch_g <- SWGWData$POCR_massIn_g
POC_df$POCautoch_g <- PPdata$NPP_POCL_mass
POC_df$POCout_g = SWGWData$POCR_outflow + SWGWData$POCL_outflow + SedData$POC_burial_total + LeachData$POCR_leachOut + LeachData$POCL_leachOut

DOC_df$DOCload_g <- PPdata$NPP_DOCL_mass + SWGWData$DOCR_massIn_g + LeachData$DOCR_leachIn + LeachData$DOCL_leachIn #g
DOC_df$DOCalloch_g <- SWGWData$DOCR_massIn_g
DOC_df$DOCautoch_g <- LeachData$POCL_leachOut

DOC_df$DOCload_g <- PPdata$NPP_DOCL_mass + SWGWData$DOCR_massIn_g #+ LeachData$DOCR_leachIn + LeachData$DOCL_leachIn #g
DOC_df$DOCalloch_g <- SWGWData$DOCR_massIn_g #+ LeachData$DOCR_leachIn
DOC_df$DOCautoch_g <- PPdata$NPP_DOCL_mass #+ LeachData$DOCL_leachIn
DOC_df$DOCout_g <- SWGWData$DOCR_outflow + SWGWData$DOCL_outflow + PPdata$DOCR_massRespired + PPdata$DOCL_massRespired #g

#OC mass sourced/sank at each time step
# SOS$Sink <- SedData$POC_sedOut
# SOS$Source <- SWGWData$POC_outflow + SWGWData$DOC_outflow + NPPdata$DOC_resp_mass - SWGWData$POC_massIn_g - SWGWData$DOC_massIn_g
# SOS$Pipe <- SWGWData$POC_outflow + SWGWData$DOC_outflow + NPPdata$DOC_resp_mass - NPPdata$DOC_mass - SWGWData$POC_massIn_g
# SOS$Net <- SOS$Sink - SOS$Source

############### MASS BALANCE CHECK ###############
#Change to total carbon stocks
FinalPOC <- POC_df$POCtotal_conc_gm3[steps] + (POC_df$POCload_g[steps] - POC_df$POCout_g[steps])/LakeVolume #g/m3
FinalDOC <- DOC_df$DOCtotal_conc_gm3[steps] + (DOC_df$DOCload_g[steps] - DOC_df$DOCout_g[steps])/LakeVolume #g/m3
#FinalPOC <-  POC_df$POC_conc_gm3[steps] + ((NPPdata$POC_mass[steps] + SWGWData$POC_massIn_g[steps] - SWGWData$POC_outflow[steps] - SedData$POC_sedOut[steps] - LeachData$POC_leachOut[steps])/LakeVolume) #g/m3
#FinalDOC <-  DOC_df$DOC_conc_gm3[steps] + ((NPPdata$DOC_mass[steps] + SWGWData$DOC_massIn_g[steps] + LeachData$DOC_leachIn[steps] - SWGWData$DOC_outflow[steps] - NPPdata$DOC_resp_mass[steps])/LakeVolume) #g/m3
DeltaPOC <- FinalPOC*LakeVolume -  POC_df$POCtotal_conc_gm3[1]*LakeVolume #g
DeltaDOC <- FinalDOC*LakeVolume - DOC_df$DOCtotal_conc_gm3[1]*LakeVolume #g
#Mass balance check (should be near zero)
POCcheck <- sum(POC_df$POCload_g -POC_df$POCout_g) - DeltaPOC
DOCcheck <- sum(DOC_df$DOCload_g - DOC_df$DOCout_g) - DeltaDOC

#Return mass balance checks
print(paste('POC Balance: ',POCcheck,' and DOC Balance: ',DOCcheck,sep=''))

######################## END MAIN PROGRAM #############################################
#Define plotting and validation time series
ConcOutputTimeSeries <- as.Date(c(InputData$datetime,InputData$datetime[length(InputData$datetime)]+86400))
OutputTimeSeries <- as.Date(InputData$datetime)

####################### Validation Output Setup ######################################
if (ValidationFlag==1){
  
  #DOC Validation Output Setup
  ModelDOC = DOC_df %>% mutate(datetime = as.Date(Date)) %>%
    select(datetime, Modelled = DOCtotal_conc_gm3)
  CalibrationOutputDOC = left_join(ValidationDataDOC,ModelDOC) 
  
  #DO Validation Output Setup
  ModelO2 = Metabolism %>% mutate(datetime = as.Date(Date)) %>%
    select(datetime,Flux_Modelled=Oxygen,Conc_Modelled=Oxygen_conc)
  CalibrationOutputDO = left_join(ValidationDataDO,ModelO2)
  
  #Plot Calibration DOC
  par(mfrow=c(2,1),mar=c(2,3,2,1),mgp=c(1.5,0.3,0),tck=-0.02)
  plot(CalibrationOutputDOC$datetime,CalibrationOutputDOC$DOC,type='o',pch=19,cex=0.7,ylab = 'DOC',xlab='',
       ylim = c(min(CalibrationOutputDOC[,2:3]),max(CalibrationOutputDOC[,2:3])),main=LakeName)
  lines(CalibrationOutputDOC$datetime,CalibrationOutputDOC$Modelled,col='red',lwd=1,type='o',pch=16,cex=0.7)
  abline(v = as.Date(paste0(unique(year(CalibrationOutputDOC$datetime)),'-01-01')),lty=2,col='grey50') #lines at Jan 1
  abline(v = as.Date(paste0(unique(year(CalibrationOutputDOC$datetime)),'-06-01')),lty=3,col='grey80') #lines at Jul 1
  
  #Plot Calibration DO
  plot(CalibrationOutputDO$datetime,CalibrationOutputDO$DO_con,type='o',pch=19,cex=0.7,ylab = 'DO Flux',xlab='',
       ylim = c(min(CalibrationOutputDO[,c(3,7)],na.rm = T),max(CalibrationOutputDO[,c(3,7)],na.rm = T)))
  lines(CalibrationOutputDO$datetime,CalibrationOutputDO$Conc_Modelled,col='darkgreen',type='o',pch=16,cex=0.7)
  abline(v = as.Date(paste0(unique(year(CalibrationOutputDOC$datetime)),'-01-01')),lty=2,col='grey50') #lines at Jan 1
  abline(v = as.Date(paste0(unique(year(CalibrationOutputDOC$datetime)),'-06-01')),lty=3,col='grey80') #lines at Jul 1
}

################## Calc goodness of fit #################
CalibrationOutputDOC = CalibrationOutputDOC[complete.cases(CalibrationOutputDOC),]
CalibrationOutputDO = CalibrationOutputDO[complete.cases(CalibrationOutputDO),]

RMSE_DOC <- sqrt((1/length(CalibrationOutputDOC[,1]))*sum((CalibrationOutputDOC$DOC-CalibrationOutputDOC$Modelled)^2)) #mg^2/L^2
RMSE_DO <- sqrt((1/length(CalibrationOutputDO[,1]))*sum((CalibrationOutputDO$DO_con - CalibrationOutputDO$Conc_Modelled)^2)) #mg^2/L^2
print(paste0('RMSE DOC ',RMSE_DOC))
print(paste0('RMSE DO ',RMSE_DO))

################## Bootstrapping of Residuals #################
if (BootstrapFlag==1){
  #save.image(file = "R/Model/lake.RData")
  # Calculate residuals
  resDOC = (CalibrationOutputDOC$DOC - CalibrationOutputDOC$Modelled)
  resDO = (CalibrationOutputDO$DO_con - CalibrationOutputDO$Conc_Modelled)

  set.seed(001) # just to make it reproducible
  #set number of psuedo observations
  pseudoObs_DOC = matrix(replicate(100,sample(resDOC) + CalibrationOutputDOC$DOC),ncol = length(resDOC)) # matrix of psuedo observations 
  pseudoObs_DO = matrix(replicate(100,sample(resDO) + CalibrationOutputDO$DO_con),ncol = length(resDO))
  pseudoObs = cbind(pseudoObs_DOC,pseudoObs_DO)
  num = length(resDOC)
  
  library(parallel)
  detectCores() # Calculate the number of cores
  cl <- makeCluster(4) # SET THIS NUMBER EQUAL TO OR LESS THAN THE CORES YOU HAVE
  
  source('R/Model/bootstrapDOC.R')
  # This applies the bootstrap function across multiple cores, works for Mac. 
  bootOut = parApply(cl = cl,MARGIN = 1,X = pseudoObs, FUN = bootstrapDOC,
                     datetimeDOC = CalibrationOutputDOC$datetime, 
                     datetimeDO = CalibrationOutputDO$datetime, 
                     LakeName = LakeName,
                     timestampFormat = timestampFormat)
  # Output results
  write.csv(bootOut,paste0('./',LakeName,'Lake/','Results/',LakeName,'_boostrapResults.csv'),row.names = F,quote=F)
  
  ###### This code be written as a loop instead. 
  bootParams = data.frame(DOCR_RespParam=NA,DOCL_RespParam=NA,R_auto=NA,BurialFactor_R=NA,
                          BurialFactor_L=NA,POC_lcR=NA,POC_lcL=NA,NLL = NA, Convergence = NA)
  for (b in 1:100) {
    pseudoDOC = data.frame(datetime = CalibrationOutputDOC$datetime, DOC = pseudoObs[b,], DOCwc = pseudoObs[b,])

    loopOut = bootstrapDOC(pseudoObs[1,],datetime = CalibrationOutputDOC$datetime, LakeName = LakeName,
                 timestampFormat = timestampFormat)
    ## New parameters from optimization output
    bootParams[b,] <- loopOut
  } # Loop instead?
}

################## Write results files ##################
if (WriteFiles==1){
  DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
  POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
  Input_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_InputData.csv',sep='')
  DOC_validation_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOCvalidation.csv',sep='')
  DO_validation_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOvalidation.csv',sep='')
  DO_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DO_Results.csv',sep='')
  
  write.csv(DOC_df,file = DOC_results_filename,row.names = F,quote = F)
  write.csv(POC_df,file = POC_results_filename,row.names = F,quote = F)
  write.csv(InputData,file = Input_filename,row.names = F,quote = F)
  write.csv(CalibrationOutputDOC,file = DOC_validation_filename,row.names = F,quote = F)
  write.csv(CalibrationOutputDO,file = DO_validation_filename,row.names = F,quote = F)

  write.csv(Metabolism,file = DO_results_filename,row.names = F,quote = F)
}

