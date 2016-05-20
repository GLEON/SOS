#SOS Carbon Flux Model
#This model reads time-series, rain, and configuration files per SOS input formats, models lake organic carbon flux
#and outputs results (DOC and POC flux time-series: 'LakeName_DOC_Results.csv and LakeName_POC_Results.csv').
#Configuration file should contain parameters generated from calibration routine "SOS_CentralFunction_optim4.R"

#User input lake name
LakeName = 'Vanern'

##### INPUT FILE NAMES ################
TimeSeriesFile <- paste('./',LakeName,'Lake/',LakeName,'TS.csv',sep='')
RainFile <- paste('./',LakeName,'Lake/',LakeName,'Rain.csv',sep='')
ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
ValidationFileDOC <- paste('./',LakeName,'Lake/',LakeName,'ValidationDOC.csv',sep='')
ValidationFileDO <- paste('./',LakeName,'Lake/',LakeName,'ValidationDO.csv',sep='')

##### LOAD PACKAGES ########################
library(signal)
library(zoo)
library(lubridate)
library(LakeMetabolizer)
library(dplyr)
library(plyr)

##### LOAD FUNCTIONS #######################
source("./R/Model/SOS_Sedimentation.R")
source("./R/Model/SOS_SWGW.R")
source("./R/Model/SOS_NPP.R")
source("./R/Model/SOS_Resp.R")
source("./R/Model/modelDOC_4.R")

##### READ MAIN INPUT FILE #################
RawData <- read.csv(TimeSeriesFile,header=T) #Read main data file with GLM outputs (physical input) and NPP input
RawData$datetime <- as.POSIXct(strptime(RawData$datetime,"%m/%d/%Y %H:%M"),tz="GMT") #Convert time to POSIX
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
RainData$datetime <- as.POSIXct(strptime(RainData$datetime,'%m/%d/%Y',tz='GMT'))

InputData$Rain <- RainData$Rain[RainData$datetime %in% InputData$datetime] #Plug daily rain data into InputData file to integrate with original code.

##### READ PARAMETER FILE ##################
parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
for (i in 1:nrow(parameters)){ # assign parameters
  assign(parameters[i,1],parameters[i,2])
}

###### Run Period and Time Step Setup #####
TimeStep <- as.numeric(InputData$datetime[2]-InputData$datetime[1]) #days
steps <- nrow(InputData)

##### Declare Output Data Storage ##########
POC_df = data.frame(Date = InputData$datetime,
                    POC_conc_gm3 = NA,
                    NPPin_gm2y=NA,FlowIn_gm2y=NA,FlowOut_gm2y=NA,sedOut_gm2y=NA,leachOut_gm2y=NA,
                    POC_out_g = NA, POC_FlowOut_cum_g = NA, POC_SedOut_cum_g = NA,
                    POC_in_g = NA, POC_in_alloch_g = NA, POC_in_autoch_g = NA,)
DOC_df = data.frame(Date = InputData$datetime,
                    DOC_conc_gm3 = NA,
                    NPPin_gm2y=NA,FlowIn_gm2y=NA,FlowOut_gm2y=NA,respOut_gm2y=NA,leachIn_gm2y=NA,
                    DOC_out_g = NA, DOC_FlowOut_cum_g = NA, DOC_RespOut_cum_g = NA,
                    DOC_in_g = NA, DOC_in_alloch_g = NA, DOC_in_autoch_g = NA,)

DOC_df = data.frame(Date = InputData$datetime,
                    DOC_conc_gm3 = NA,
                    NPPin_gm2y=NA,FlowIn_gm2y=NA,FlowOut_gm2y=NA,respOut_gm2y=NA,leachIn_gm2y=NA,
                    DOC_out_g = NA, DOC_FlowOut_cum_g = NA, DOC_RespOut_cum_g = NA,
                    DOC_in_g = NA, DOC_in_alloch_g = NA, DOC_in_autoch_g = NA)

##### Declare Data Storage - Sed ###########
SedData <- data.frame(Date = InputData$datetime, BurialScalingFactor=NA,MAR_oc=NA,POC_burial=NA,POC_sedOut = NA)

##### Declare Data Storage - NPP ###########
NPPdata <- data.frame(Date = InputData$datetime,DOC_rate=NA,POC_rate=NA,DOC_mass=NA,POC_mass=NA, DOC_resp_mass=NA)
Metabolism <- data.frame(Date = InputData$datetime,NEP=NA,Oxygen=NA)

##### Declare Data Storage - SW/GW #########
SWGWData <- data.frame(Date = InputData$datetime,POC_Aerial=NA, POC_SW=NA, DOC_Wetland=NA, 
                       DOC_gw=NA, DOC_SW=NA, DailyRain=NA, DOC_precip=NA, Load_DOC=NA, Load_POC=NA,
                       POC_massIn_g = NA, DOC_massIn_g = NA, POC_outflow = NA, DOC_outflow = NA)

#### Declare Data Storage - POC to DOC Leaching ####
LeachData <- data.frame(Date = InputData$datetime,POC_leachOut = NA,DOC_leachIn = NA)

##### Declare Data Storage - Source of Sink? #
SOS <- data.frame(Date = InputData$datetime,Source=NA,Sink=NA,Pipe=NA,Net=NA)

##### Carbon Concentration Initialization ################
POC_df$POC_conc_gm3[1] <- POC_init # #Initialize POC concentration as baseline average
DOC_df$DOC_conc_gm3[1] <- DOC_init #Initialize DOC concentration g/m3

####################### MAIN PROGRAM #############################################

for (i in 1:(steps)){
  
  Q_sw <- InputData$FlowIn[i] #m3/s surface water flowrate at i
  Q_gw <- Q_sw/(1-PropGW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
  Q_out <- InputData$FlowOut[i] #m3/s: total outflow. Assume steady state pending dynamic output
  Rainfall <- InputData$Rain[i]/TimeStep #mm/day
  
  #Call NPP Function
  PhoticDepth <- log(100)/(1.7/InputData$Secchi[i]) #Calc photic depth as function of Secchi depth
  if (PhoticDepth>LakeDepth){PhoticDepth<-LakeDepth} #QC - If photic depth calc'ed as greater than lake depth, photic depth = lake depth
  RawProduction <- NPP(InputData$Chla[i],InputData$TP[i],PhoticDepth,InputData$EpiTemp[i],yday(InputData$datetime[i]),ProdStartDay,ProdEndDay) #mg C/m^2/d
  NPPdata$DOC_rate[i] = RawProduction$NPP_DOC_rate
  NPPdata$POC_rate[i] = RawProduction$NPP_POC_rate
  
  #Call SWGW Function
  SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,AerialLoad, PropCanopy, LakePerimeter, WetlandLoad, PropWetlands, DOC_gw, PropGW, 
                       InputData$SW_DOC[i], DOC_precip, LakeArea) #change these inputs to iterative [i] values when inputs are dynamic
  SWGWData[i,2:10] <- SWGW
  
  #Call Sedimentation Function
  POC_mass <- POC_df$POC_conc_gm3[i]*LakeVolume
  SedOutput <- SedimentationFunction(BurialFactor,TimeStep,POC_mass,LakeArea)
  SedData[i,2:4] = SedOutput
  SedData$POC_sedOut[i] <- SedData$POC_burial[i] #g #WHY IS THIS REPEATED?
  
  #Call respiration function
  DOC_resp_rate <- Resp(DOC_df$DOC_conc_gm3[i],InputData$EpiTemp[i],RespParam) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
  NPPdata$DOC_resp_mass[i] = DOC_resp_rate*LakeVolume*TimeStep #g C
  # Calculations that do not have to be in the loop
  NPPdata$DOC_mass[i] <- NPPdata$DOC_rate[i]*(1-R_auto)*LakeArea*TimeStep/1000 #g
  NPPdata$POC_mass[i] <- NPPdata$POC_rate[i]*(1-R_auto)*LakeArea*TimeStep/1000 #g
  
  #Calc metabolism (DO) estimates for NPP validation
  Metabolism$NEP[i] <- (NPPdata$DOC_mass[i] + NPPdata$POC_mass[i] - NPPdata$DOC_resp_mass[i]*(PhoticDepth/LakeDepth))/(LakeVolume*PhoticDepth/LakeDepth)/TimeStep #g/m3/d
  Metabolism$Oxygen <- (Metabolism$NEP)*(32/12) #g/m3/d Molar conversion of C flux to O2 flux (lake metabolism)
  
  #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
  SWGWData$POC_outflow[i] <- POC_df$POC_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
  SWGWData$DOC_outflow[i] <- DOC_df$DOC_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
  #Calculate load from SWGW_in
  SWGWData$DOC_massIn_g[i] <- SWGWData$Load_DOC[i]*TimeStep #g
  SWGWData$POC_massIn_g[i] <- SWGWData$Load_POC[i]*TimeStep #g
  #Calc POC-to-DOC leaching
  LeachData$POC_leachOut[i] <- POC_df$POC_conc_gm3[i]*POC_lc*LakeVolume*TimeStep #g - POC concentration times leaching parameter
  LeachData$DOC_leachIn[i] <- LeachData$POC_leachOut[i]
  
  if (i < steps) { #don't calculate for last time step
    #Update POC and DOC concentration values (g/m3) for whole lake
    POC_df$POC_conc_gm3[i+1] <-  POC_df$POC_conc_gm3[i] + ((NPPdata$POC_mass[i] + SWGWData$POC_massIn_g[i] - SWGWData$POC_outflow[i] - SedData$POC_sedOut[i] - LeachData$POC_leachOut[i])/LakeVolume) #g/m3
    DOC_df$DOC_conc_gm3[i+1] <-  DOC_df$DOC_conc_gm3[i] + ((NPPdata$DOC_mass[i] + SWGWData$DOC_massIn_g[i] + LeachData$DOC_leachIn[i] - SWGWData$DOC_outflow[i] - NPPdata$DOC_resp_mass[i])/LakeVolume) #g/m3
    #Stop code and output error if concentrations go to negative
    if (POC_df$POC_conc_gm3[i+1]<=0){stop("Negative POC concentration!")}
    if (DOC_df$DOC_conc_gm3[i+1]<=0){stop("Negative DOC concentration!")}
  }
}

#Store POC and DOC fluxes as mass/area/time (g/m2/yr)
POC_df$NPPin_gm2y <-  NPPdata$POC_mass/LakeArea/(TimeStep/365)
POC_df$FlowIn_gm2y <- SWGWData$POC_massIn_g/LakeArea/(TimeStep/365)
POC_df$FlowOut_gm2y <- SWGWData$POC_outflow/LakeArea/(TimeStep/365)
POC_df$sedOut_gm2y <- SedData$POC_sedOut/LakeArea/(TimeStep/365)
POC_df$leachOut_gm2y <- LeachData$POC_leachOut/LakeArea/(TimeStep/365)

DOC_df$NPPin_gm2y <- NPPdata$DOC_mass/LakeArea/(TimeStep/365)
DOC_df$FlowIn_gm2y <- SWGWData$DOC_massIn_g/LakeArea/(TimeStep/365)
DOC_df$FlowOut_gm2y <- SWGWData$DOC_outflow/LakeArea/(TimeStep/365) 
DOC_df$respOut_gm2y<- NPPdata$DOC_resp_mass/LakeArea/(TimeStep/365) 
DOC_df$leachIn_gm2y <- LeachData$DOC_leachIn/LakeArea/(TimeStep/365)

#Cumulative DOC and POC fate (grams)
POC_df$POC_FlowOut_cum_g <- cumsum(SWGWData$POC_outflow)
POC_df$POC_SedOut_cum_g <- cumsum(SedData$POC_sedOut)
DOC_df$DOC_FlowOut_cum_g = cumsum(SWGWData$DOC_outflow)
DOC_df$DOC_RespOut_cum_g = cumsum(NPPdata$DOC_resp_mass)
#POC and DOC load (in) and fate (out) (g)
POC_df$POC_in_g <- NPPdata$POC_mass + SWGWData$POC_massIn_g #g
POC_df$POC_in_alloch_g <- SWGWData$POC_massIn_g
POC_df$POC_in_autoch_g <- NPPdata$POC_mass
POC_df$POC_out_g = SWGWData$POC_outflow + SedData$POC_sedOut + LeachData$POC_leachOut

DOC_df$DOC_in_g <- NPPdata$DOC_mass + SWGWData$DOC_massIn_g #g
DOC_df$DOC_in_alloch_g <- SWGWData$DOC_massIn_g
DOC_df$DOC_in_autoch_g <- NPPdata$DOC_mass
DOC_df$DOC_out_g = SWGWData$DOC_outflow + NPPdata$DOC_resp_mass #g

#OC mass sourced/sank at each time step
SOS$Sink <- SedData$POC_sedOut
SOS$Source <- SWGWData$POC_outflow + SWGWData$DOC_outflow + NPPdata$DOC_resp_mass - SWGWData$POC_massIn_g - SWGWData$DOC_massIn_g
SOS$Pipe <- SWGWData$POC_outflow + SWGWData$DOC_outflow + NPPdata$DOC_resp_mass - NPPdata$DOC_mass - SWGWData$POC_massIn_g
SOS$Net <- SOS$Sink - SOS$Source

############### MASS BALANCE CHECK ###############
#Change to total carbon stocks
FinalPOC <-  POC_df$POC_conc_gm3[steps] + ((NPPdata$POC_mass[steps] + SWGWData$POC_massIn_g[steps] - SWGWData$POC_outflow[steps] - SedData$POC_sedOut[steps] - LeachData$POC_leachOut[steps])/LakeVolume) #g/m3
FinalDOC <-  DOC_df$DOC_conc_gm3[steps] + ((NPPdata$DOC_mass[steps] + SWGWData$DOC_massIn_g[steps] + LeachData$DOC_leachIn[steps] - SWGWData$DOC_outflow[steps] - NPPdata$DOC_resp_mass[steps])/LakeVolume) #g/m3
DeltaPOC <- FinalPOC*LakeVolume -  POC_df$POC_conc_gm3[1]*LakeVolume #g
DeltaDOC <- FinalDOC*LakeVolume - DOC_df$DOC_conc_gm3[1]*LakeVolume #g
#Mass balance check (should be near zero)
POCcheck <- (sum(POC_df$POC_in_alloch_g) + sum(POC_df$POC_in_autoch_g) -  sum(POC_df$POC_out_g)) - DeltaPOC
DOCcheck <- (sum(DOC_df$DOC_in_alloch_g) + sum(DOC_df$DOC_in_autoch_g) + sum(LeachData$DOC_leachIn) - sum(DOC_df$DOC_out_g)) - DeltaDOC
#Return mass balance checks
print(paste('POC Balance: ',POCcheck,' and DOC Balance: ',DOCcheck,sep=''))

######################## END MAIN PROGRAM #############################################

#Define plotting and validation time series
ConcOutputTimeSeries <- as.Date(c(InputData$datetime,InputData$datetime[length(InputData$datetime)]+86400))
OutputTimeSeries <- as.Date(InputData$datetime)

#Write results files
DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')
write.csv(DOC_df,file = DOC_results_filename)
write.csv(POC_df,file = POC_results_filename)
write.csv(SOS,file = SOS_results_filename)



