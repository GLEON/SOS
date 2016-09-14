setwd('C:/Users/hdugan/Documents/Rpackages/SOS/')
#CarbonFluxModel <- function(LakeName,PlotFlag,ValidationFlag){
#Flags 1 for yes, else no.
LakeName = 'Harp'
OptimizationFlag = 1
PlotFlag = 0
ValidationFlag = 1
WriteFiles = 0
timestampFormat =	'%m/%d/%Y'
#timestampFormat =	'%Y-%m-%d'
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
source("./R/Model/SOS_GPP.R")
source("./R/Model/SOS_Resp.R")
source("./R/Model/modelDOC_5.R")

##### READ PARAMETER FILE ##################
parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
for (i in 1:nrow(parameters)){ # assign parameters
  assign(parameters[i,1],parameters[i,2])
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
Metabolism <- data.frame(Date = InputData$datetime,NEP=NA,Oxygen=NA)

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

####################### Validation Output Setup ######################################

#DOC Validation Output Setup
ValidationDataDOC <- read.csv(ValidationFileDOC,header=T)
ValidationDataDOC$datetime <- as.Date(as.POSIXct(strptime(ValidationDataDOC$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
ValidationDataDOC = ValidationDataDOC[complete.cases(ValidationDataDOC),]
outlier.limit = (mean(ValidationDataDOC$DOC) + 3*(sd(ValidationDataDOC$DOC))) # Calculate mean + 3 SD of DOC column
ValidationDataDOC = ValidationDataDOC[ValidationDataDOC$DOC <= outlier.limit,] # Remove rows where DOC > outlier.limit
ValidationDataDOC = ddply(ValidationDataDOC,'datetime',summarize,DOC=mean(DOC),DOCwc=mean(DOCwc))

#DO Validation Output Setup
ValidationDataDO <- read.csv(ValidationFileDO,header=T)
ValidationDataDO$datetime <- as.Date(as.POSIXct(strptime(ValidationDataDO$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
ValidationDataDO = ValidationDataDO[complete.cases(ValidationDataDO),]
#Only compare to DO data during "production season."
ValidationDataDO = ValidationDataDO[yday(ValidationDataDO$datetime)>ProdStartDay & yday(ValidationDataDO$datetime)<ProdEndDay,]
#ValidationDataDO = ValidationDataDO[ValidationDataDO$wtr >= 10,]


k <- 0.5 #m/d
PhoticDepth <- data.frame(datetime = InputData$datetime,PhoticDepth = log(100)/(1.7/InputData$Secchi))
IndxVal = ValidationDataDO$datetime %in% as.Date(PhoticDepth$datetime)
IndxPhotic = as.Date(PhoticDepth$datetime) %in% ValidationDataDO$datetime

ValidationDataDO = ValidationDataDO[IndxVal,]
ValidationDataDO$DO_sat <- o2.at.sat(ValidationDataDO[,1:2])[,2]  
ValidationDataDO$Flux <- k*(ValidationDataDO$DO_con - ValidationDataDO$DO_sat)/PhoticDepth$PhoticDepth[IndxPhotic] #g/m3/d
#SedData MAR OC 
ValidationDataMAROC <- ObservedMAR_oc #g/m2

#################### OPTIMIZATION ROUTINE ############################################
if (OptimizationFlag==1){
  min.calcModelNLL <- function(pars,ValidationDataDOC,ValidationDataDO,ValidationDataMAROC){
    modeled = modelDOC(pars[1],pars[2],pars[3],pars[4],pars[5],pars[6])
    
    obsIndx = ValidationDataDOC$datetime %in% modeled$datetime
    modIndx = modeled$datetime %in% ValidationDataDOC$datetime
    CalibrationOutputDOC <- data.frame(datetime = ValidationDataDOC[obsIndx,]$datetime,
                                       Measured = ValidationDataDOC[obsIndx,]$DOC, Modelled = modeled[modIndx,]$DOC_conc)
    #resDOC = scale(CalibrationOutputDOC$Measured - CalibrationOutputDOC$Modelled,center = F)
    resDOC = (CalibrationOutputDOC$Measured - CalibrationOutputDOC$Modelled)
    obsIndx = ValidationDataDO$datetime %in% modeled$datetime
    modIndx = modeled$datetime %in% ValidationDataDO$datetime
    CalibrationOutputDO <- data.frame(datetime = ValidationDataDO[obsIndx,]$datetime,
                                      Measured = ValidationDataDO[obsIndx,]$Flux, Modelled = modeled[modIndx,]$MetabOxygen)
    
    #resDO = scale(CalibrationOutputDO$Measured - CalibrationOutputDO$Modelled,center = F)
    DOScale = 5
    resDO = (CalibrationOutputDO$Measured - CalibrationOutputDO$Modelled) * DOScale
    sedScale = 0.001
    resSedData = (mean(modeled$SedData_MAR,na.rm = T) - ValidationDataMAROC) * sedScale #not scaled because it is 1 value
    
    res = c(resDOC,resDO,rep(resSedData,length(resDOC)))
    
    nRes 	= length(res)
    SSE 	= sum(res^2)
    sigma2 	= SSE/nRes
    NLL 	= 0.5*((SSE/sigma2) + nRes*log(2*pi*sigma2))
    print(paste('NLL: ',NLL,sep=''))
    print(paste('parameters: ',pars,sep=''))
    return(NLL)
  }
  ## Test call ##
  min.calcModelNLL(par = c(DOCR_RespParam,DOCL_RespParam,R_auto,BurialFactor_R,BurialFactor_L,POC_lc),
                   ValidationDataDOC = ValidationDataDOC,
                   ValidationDataDO = ValidationDataDO,ValidationDataMAROC = ValidationDataMAROC)
  # # 
  
  optimOut = optim(par = c(DOCR_RespParam,DOCL_RespParam,R_auto,BurialFactor_R,BurialFactor_L,POC_lc), 
                   min.calcModelNLL,ValidationDataDOC = ValidationDataDOC,
                   ValidationDataDO = ValidationDataDO,ValidationDataMAROC = ValidationDataMAROC, 
                   control = list(maxit = 200)) #setting maximum number of attempts for now
  #method = 'L-BFGS-B',lower=c(0,0,0) #To constrain
  
  print('Parameter estimates (burial, Rhet, Raut...')
  print(optimOut$par)
  ## New parameters from optimization output
  
  conv <- optimOut$convergence  #did model converge or not (0=yes, 1=no)
  NLL <- optimOut$value #value of nll
  
  
  DOCR_RespParam <- optimOut$par[1]
  DOCL_RespParam <- optimOut$par[2]
  R_auto <- optimOut$par[3]
  BurialFactor_R <- optimOut$par[4]
  BurialFactor_L <- optimOut$par[5] 
  POC_lc <- optimOut$par[6]
}
# 
# ####################### END OPTIMIZATION ROUTINE #################################
# ####################### MAIN PROGRAM #############################################
# Monona c(0.00059606,0.062588791,0.792906357,0.270671129,-0.011526258) 
# Harp: c(0.002230285,0.002745391,0.995244529,0.333380628,-0.009177321) #NLL 87
# Trout:  c( 0.0011254525  0.1520756581  0.7253578540  0.1120424232 -0.0003273584) #NLL: 225
# Mendota: c(0.0008463344,0.3467401103,0.5110130504,0.0986506182,0.0024431470)
# Vanern: c(0.001512181,0.030140450,0.758617333,0.328194576,-0.008648217) #NLL = 7.6

# Monona2 c(0.001760347 0.000825484 0.900335669 0.382236083 0.010878626 0.016376368) #NLL 287
# Vanern2: c(0.00146343281 0.01426637001 0.92157986978 0.42846889871 0.00008978815 0.00781871285) #NLL 4.37
# Mendota2: c(0.0014666541  0.4081472578  0.6001340076 -0.0289949672 -0.0000245212 -0.0004031551) #NLL 555
# Harp2: c(0.001945615 -0.267369996  0.985575162  0.208813556  0.444911813  0.010922284) #NLL 135
# Trout2: c(0.0007891784 0.1206761437 0.7792985859 0.2160982340 0.0007159844 0.0091884239) #NLL 215

for (i in 1:(steps)) {
  if (R_auto > 1){R_auto = 1}
  
  Q_sw <- InputData$FlowIn[i] #m3/s surface water flowrate at i
  Q_gw <- Q_sw/(1-PropGW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
  Q_out <- InputData$FlowOut[i] #m3/s: total outflow. Assume steady state pending dynamic output
  Rainfall <- InputData$Rain[i]/TimeStep #mm/day
  
  #Call GPP function
  PhoticDepth <- log(100)/(1.7/InputData$Secchi[i]) #Calc photic depth as function of Secchi depth
  if (PhoticDepth>LakeDepth){PhoticDepth<-LakeDepth} #QC - If photic depth calc'ed as greater than lake depth, photic depth = lake depth
  GPPrates <- GPP(InputData$Chla[i],InputData$TP[i],PhoticDepth,InputData$EpiTemp[i],yday(InputData$datetime[i])) #mg C/m^2/d
  PPdata$GPP_DOCL_rate[i] = GPPrates$GPP_DOC_rate #mg C/m2/d
  PPdata$GPP_POCL_rate[i] = GPPrates$GPP_POC_rate #mg C/m2/d
  PPdata$NPP_DOCL_mass[i] <- PPdata$GPP_DOCL_rate[i]*(1-R_auto)*LakeArea*TimeStep/1000 #g
  PPdata$NPP_POCL_mass[i] <- PPdata$GPP_POCL_rate[i]*(1-R_auto)*LakeArea*TimeStep/1000 #g
  
  #Call heterotrophic respiration function for recalitrant DOC pool (DOCR) and labile DOC pool (DOCL)
  DOCR_resp_rate <- Resp(DOC_df$DOCR_conc_gm3[i],InputData$EpiTemp[i],DOCR_RespParam) #g C/m3/d
  DOCL_resp_rate <- Resp(DOC_df$DOCL_conc_gm3[i],InputData$EpiTemp[i],DOCL_RespParam) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
  
  PPdata$DOCR_massRespired[i] = DOCR_resp_rate*LakeVolume*TimeStep #g C
  PPdata$DOCL_massRespired[i] = DOCL_resp_rate*LakeVolume*TimeStep #g C
  
  #Calc metabolism (DO) estimates for PP validation
  Metabolism$NEP[i] <- (PPdata$NPP_DOCL_mass[i] + PPdata$NPP_POCL_mass[i] - PPdata$DOCR_massRespired[i]*(PhoticDepth/LakeDepth) - PPdata$DOCL_massRespired[i]*(PhoticDepth/LakeDepth))/
    (LakeVolume*PhoticDepth/LakeDepth)/TimeStep #g/m3/d #volume of photic zone
  # Metabolism$NEP[i] <- (PPdata$NPP_DOCL_mass[i] + PPdata$NPP_POCL_mass[i] - PPdata$DOCR_massRespired[i] - PPdata$DOCL_massRespired[i])/
  #   (LakeVolume*PhoticDepth/LakeDepth)/TimeStep #g/m3/d #volume of photic zone
  Metabolism$Oxygen[i] <- (Metabolism$NEP[i])*(32/12) #g/m3/d Molar conversion of C flux to O2 flux (lake metabolism)
  
  #Call SWGW Function (Surface Water/GroundWater)
  SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,AerialLoad, PropCanopy, LakePerimeter, WetlandLoad, PropWetlands, DOC_gw, 
                       InputData$SW_DOC[i], DOC_precip, LakeArea) # All in g/day, except DailyRain in m3/day
  #change these inputs to iterative [i] values when inputs are dynamic
  SWGWData[i,2:10] <- SWGW
  #LOAD DOC (g/d) = DOC_Wetland + DOC_GW + DOC_SW +DOC_Precip # g/d DOC
  #LOAD POC (g/d) = POC_Aerial + POC_SW # g/d POC roughly estimated as (0.1 * DOC)
  
  #Call Sedimentation Function
  POCR_mass <- POC_df$POCR_conc_gm3[i]*LakeVolume
  POCL_mass <- POC_df$POCL_conc_gm3[i]*LakeVolume
  SedOutput_R <- SedimentationFunction(BurialFactor_R,TimeStep,POCR_mass,LakeArea)
  SedOutput_L <- SedimentationFunction(BurialFactor_L,TimeStep,POCL_mass,LakeArea)
  SedData[i,2:4] = SedOutput_R
  SedData[i,5:7] = SedOutput_L
  SedData[i,8:9] = (SedOutput_L + SedOutput_R) [2:3]
  
  #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
  SWGWData$POCR_outflow[i] <- POC_df$POCR_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
  SWGWData$POCL_outflow[i] <- POC_df$POCL_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
  SWGWData$DOCR_outflow[i] <- DOC_df$DOCR_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
  SWGWData$DOCL_outflow[i] <- DOC_df$DOCL_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
  #Calculate load from SWGW_in
  SWGWData$DOCR_massIn_g[i] <- SWGWData$Load_DOCR[i]*TimeStep #g
  SWGWData$POCR_massIn_g[i] <- SWGWData$Load_POCR[i]*TimeStep #g
  #Calc POC-to-DOC leaching
  LeachData$POCR_leachOut[i] <- POC_df$POCR_conc_gm3[i]*POC_lc*LakeVolume*TimeStep #g - POC concentration times leaching parameter
  LeachData$DOCR_leachIn[i] <- LeachData$POCR_leachOut[i]
  LeachData$POCL_leachOut[i] <- POC_df$POCL_conc_gm3[i]*POC_lc*LakeVolume*TimeStep #g - POC concentration times leaching parameter
  LeachData$DOCL_leachIn[i] <- LeachData$POCL_leachOut[i]
  
  if (i < steps) { #don't calculate for last time step
    #Update POC and DOC concentration values (g/m3) for whole lake
    
    POC_df$POCL_conc_gm3[i+1] <-  POC_df$POCL_conc_gm3[i] + ((PPdata$NPP_POCL_mass[i] - LeachData$POCL_leachOut[i] - SWGWData$POCL_outflow[i] - SedData$POC_burial_L[i])/LakeVolume) #g/m3
    POC_df$POCR_conc_gm3[i+1] <-  POC_df$POCR_conc_gm3[i] + ((SWGWData$POCR_massIn_g[i] - LeachData$POCR_leachOut[i] - SWGWData$POCR_outflow[i] - SedData$POC_burial_R[i])/LakeVolume)
    POC_df$POCtotal_conc_gm3[i+1] = POC_df$POCR_conc_gm3[i+1] + POC_df$POCL_conc_gm3[i+1]
    
    DOC_df$DOCL_conc_gm3[i+1] <- DOC_df$DOCL_conc_gm3[i] + ((PPdata$NPP_DOCL_mass[i] + LeachData$DOCL_leachIn[i] - SWGWData$DOCL_outflow[i] - PPdata$DOCL_massRespired[i])/LakeVolume) #g/m3
    DOC_df$DOCR_conc_gm3[i+1] <- DOC_df$DOCR_conc_gm3[i] + ((SWGWData$DOCR_massIn_g[i] + LeachData$DOCR_leachIn[i] - SWGWData$DOCR_outflow[i] - PPdata$DOCR_massRespired[i])/LakeVolume) #g/m3
    DOC_df$DOCtotal_conc_gm3[i+1] = DOC_df$DOCR_conc_gm3[i+1] + DOC_df$DOCL_conc_gm3[i+1]
    
    #Stop code and output error if concentrations go to negative
    if (POC_df$POCtotal_conc_gm3[i+1]<=0){stop("Negative POC concentration!")}
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
DOC_df$DOCautoch_g <- PPdata$NPP_DOCL_mass
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
  ValidationDOCIndeces = ValidationDataDOC$datetime %in% OutputTimeSeries
  modIndx = OutputTimeSeries %in% ValidationDataDOC$datetime
  
  CalibrationOutputDOC = data.frame(datetime = rep(NA,sum(ValidationDOCIndeces)),
                                    Measured = NA, MeasuredWC = NA ,Modelled = NA)
  CalibrationOutputDOC$datetime <- ValidationDataDOC$datetime[ValidationDOCIndeces]
  CalibrationOutputDOC$Measured <- ValidationDataDOC$DOC[ValidationDOCIndeces]
  CalibrationOutputDOC$MeasuredWC <- ValidationDataDOC$DOCwc[ValidationDOCIndeces]
  CalibrationOutputDOC$Modelled <- DOC_df$DOCtotal_conc_gm3[modIndx]
  
  #DO Validation Output Setup
  ValidationDataDO_match = ValidationDataDO[ValidationDataDO$datetime %in% OutputTimeSeries,]
  modIndx = OutputTimeSeries %in% ValidationDataDO_match$datetime
  CalibrationOutputDO = data.frame(datetime = ValidationDataDO_match$datetime,
                                   Measured = NA, Modelled = NA)
  
  PhoticDepth <- data.frame(datetime = InputData$datetime,PhoticDepth = log(100)/(1.7/InputData$Secchi))
  DO_sat <- o2.at.sat(ValidationDataDO_match[,1:2])  
  IndxPhotic = as.Date(PhoticDepth$datetime) %in% ValidationDataDO_match$datetime
  
  CalibrationOutputDO$Measured <- k*(ValidationDataDO_match$DO_con-DO_sat$do.sat)/PhoticDepth$PhoticDepth[IndxPhotic]
  CalibrationOutputDO$Modelled <- Metabolism$Oxygen[modIndx]
  
  #Plot Calibration DOC
  par(mfrow=c(2,1),mar=c(2,3,2,1),mgp=c(1.5,0.3,0),tck=-0.02)
  plot(CalibrationOutputDOC$datetime,CalibrationOutputDOC$Measured,type='o',pch=19,cex=0.7,ylab = 'DOC',xlab='',
       ylim = c(min(CalibrationOutputDOC[,2:3]),max(CalibrationOutputDOC[,2:3])),main=LakeName)
  lines(CalibrationOutputDOC$datetime,CalibrationOutputDOC$Modelled,col='red',lwd=2)
  lines(as.Date(DOC_df$Date),DOC_df$DOC_conc_gm3,col='darkgreen',lwd=2)
  abline(v = as.Date(paste0(unique(year(DOC_df$Date)),'-01-01')),lty=2,col='grey50') #lines at Jan 1
  abline(v = as.Date(paste0(unique(year(DOC_df$Date)),'-06-01')),lty=3,col='grey80') #lines at Jul 1
  
  #Plot Calibration DO
  plot(CalibrationOutputDO$datetime,CalibrationOutputDO$Measured,type='o',pch=19,cex=0.7,ylab = 'DO Flux',xlab='',
       ylim = c(min(CalibrationOutputDO[,2:3],na.rm = T),max(CalibrationOutputDO[,2:3],na.rm = T)))
  lines(CalibrationOutputDO$datetime,CalibrationOutputDO$Modelled,col='darkgreen',lwd=2)
  abline(h=0,lty=2)
  abline(v = as.Date(paste0(unique(year(DOC_df$Date)),'-01-01')),lty=2,col='grey50') #lines at Jan 1
  abline(v = as.Date(paste0(unique(year(DOC_df$Date)),'-06-01')),lty=3,col='grey80') #lines at Jul 1
}

################## PLOTTING ###########################################################

if (PlotFlag==1){
  #POC and DOC concentration in time (g/m3)
  par(mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02,cex=0.8)
  plot(OutputTimeSeries,DOC_df$DOC_conc_gm3,xlab='Date',ylab="DOC Conc (g/m3)",type="l")
  lines(ValidationDataDOC$datetime,ValidationDataDOC$DOC,col='red3')
  
}

################## Write results files ##################
if (WriteFiles==1){
  DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
  POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
  Input_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_InputData.csv',sep='')
  DOC_validation_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOCvalidation.csv',sep='')
  DO_validation_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOvalidation.csv',sep='')
  
  write.csv(DOC_df,file = DOC_results_filename,row.names = F,quote = F)
  write.csv(POC_df,file = POC_results_filename,row.names = F,quote = F)
  write.csv(InputData,file = Input_filename,row.names = F,quote = F)
  write.csv(CalibrationOutputDOC,file = DOC_validation_filename,row.names = F,quote = F)
  write.csv(CalibrationOutputDO,file = DO_validation_filename,row.names = F,quote = F)
}

