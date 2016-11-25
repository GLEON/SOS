
bootstrapDOC <- function(newObs,datetime,LakeName,timestampFormat = '%m/%d/%Y') {
  pseudoDOC = data.frame(datetime = datetime, DOC = newObs, DOCwc = newObs)
  
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
  
  # ##### Declare Output Data Storage ##########
  # POC_df = data.frame(Date = InputData$datetime, POCtotal_conc_gm3 = NA,
  #                     POCR_conc_gm3 = NA, POCL_conc_gm3 = NA,
  #                     NPPin_gm2y=NA,FlowIn_gm2y=NA,FlowOut_gm2y=NA,sedOut_gm2y=NA,leachOut_gm2y=NA,
  #                     POC_flowOut_gm2y = NA, POC_sedOut_gm2y = NA,
  #                     POCload_g = NA, POCalloch_g = NA, POCautoch_g = NA,
  #                     POCout_g = NA)
  # DOC_df = data.frame(Date = InputData$datetime,DOCtotal_conc_gm3 = NA,
  #                     DOCR_conc_gm3 = NA, DOCL_conc_gm3 = NA,
  #                     NPPin_gm2y=NA,FlowIn_gm2y=NA,FlowOut_gm2y=NA,respOut_gm2y=NA,leachIn_gm2y=NA,
  #                     DOC_flowOut_gm2y = NA, DOC_respOut_gm2y = NA,
  #                     DOCload_g = NA, DOCalloch_g = NA, DOCautoch_g = NA,
  #                     DOCout_g = NA)
  # 
  # ##### Declare Data Storage - Sed ###########
  # SedData <- data.frame(Date = InputData$datetime,BurialScalingFactor_R=NA,MAR_oc_R=NA,POC_burial_R=NA,
  #                       BurialScalingFactor_L=NA,MAR_oc_L=NA,POC_burial_L=NA,
  #                       MAR_oc_total=NA,POC_burial_total=NA)
  # 
  # ##### Declare Data Storage - NPP ###########
  # PPdata <- data.frame(Date = InputData$datetime,GPP_DOCL_rate=NA,GPP_POCL_rate=NA,NPP_DOCL_mass=NA,NPP_POCL_mass=NA, DOCL_massRespired=NA)
  # Metabolism <- data.frame(Date = InputData$datetime,NEP=NA,Oxygen=NA)
  # 
  # ##### Declare Data Storage - SW/GW #########
  # SWGWData <- data.frame(Date = InputData$datetime,POCR_Aerial=NA, POCR_SW=NA, DOCR_Wetland=NA, 
  #                        DOCR_gw=NA, DOCR_SW=NA, DailyRain=NA, DOCR_precip=NA, Load_DOCR=NA, Load_POCR=NA,
  #                        POCR_massIn_g = NA, DOCR_massIn_g = NA, 
  #                        POCR_outflow = NA, DOCR_outflow = NA, POCL_outflow = NA, DOCL_outflow = NA)
  # 
  # #### Declare Data Storage - POC to DOC Leaching ####
  # LeachData <- data.frame(Date = InputData$datetime,POCR_leachOut = NA,DOCR_leachIn = NA,
  #                         POCL_leachOut = NA,DOCL_leachIn = NA)
  # 
  # ##### Declare Data Storage - Source of Sink? #
  # SOS <- data.frame(Date = InputData$datetime,Source=NA,Sink=NA,Pipe=NA,Net=NA)
  # 
  # ##### Carbon Concentration Initialization ################
  # POC_df$POCtotal_conc_gm3[1] <- POC_init # #Initialize POC concentration as baseline average
  # DOC_df$DOCtotal_conc_gm3[1] <- DOC_init #Initialize DOC concentration g/m3
  # DOC_df$DOCR_conc_gm3[1] <- DOC_init*0.8 #Initialize DOC concentration g/m3
  # DOC_df$DOCL_conc_gm3[1] <- DOC_init*0.2 #Initialize DOC concentration g/m3
  # POC_df$POCR_conc_gm3[1] <- POC_init*0.8 #Initialize POC concentration g/m3
  # POC_df$POCL_conc_gm3[1] <- POC_init*0.2 #Initialize POC concentration g/m3
  
  ####################### Validation Output Setup ######################################
  #DO Validation Output Setup
  ValidationDataDO <- read.csv(ValidationFileDO,header=T)
  ValidationDataDO$datetime <- as.Date(as.POSIXct(strptime(ValidationDataDO$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
  ValidationDataDO = ValidationDataDO[complete.cases(ValidationDataDO),]

  k <- 0.5 #m/d
  PhoticDepth <- data.frame(datetime = InputData$datetime,PhoticDepth = log(100)/(1.7/InputData$Secchi))
  IndxVal = ValidationDataDO$datetime %in% as.Date(PhoticDepth$datetime)
  IndxPhotic = as.Date(PhoticDepth$datetime) %in% ValidationDataDO$datetime
  
  ValidationDataDO = ValidationDataDO[IndxVal,]
  ValidationDataDO$DO_sat <- o2.at.sat(ValidationDataDO[,1:2])[,2]  
  ValidationDataDO$Flux <- k*(ValidationDataDO$DO_con - ValidationDataDO$DO_sat)/PhoticDepth$PhoticDepth[IndxPhotic] #g/m3/d
  #SedData MAR OC 
  ValidationDataMAROC <- ObservedMAR_oc #g/m2
  
  
  min.calcModelNLL <- function(pars,ValidationDataDOC,ValidationDataDO,ValidationDataMAROC){
    modeled = modelDOC(pars[1],pars[2],pars[3],pars[4],pars[5],pars[6],pars[7])
    
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

  modelDOC <- function (DOCR_RespParam,DOCL_RespParam,R_auto,BurialFactor_R,BurialFactor_L,
                        POC_lcR,POC_lcL) {
    
    POC_out = data.frame(POCL_conc_gm3 = rep(NA,steps), POCR_conc_gm3 = rep(NA,steps), POCtotal_conc_gm3 = rep(NA,steps))
    DOC_out = data.frame(DOCL_conc_gm3 = rep(NA,steps), DOCR_conc_gm3 = rep(NA,steps), DOCtotal_conc_gm3 = rep(NA,steps))
    
    POC_out[1,] = c(POC_init*0.2, POC_init*0.8, POC_init)
    DOC_out[1,] = c(DOC_init*0.2, DOC_init*0.8, DOC_init)
    Metabolism_out = NA
    Sed_out = NA
    
    for (i in 1:(steps)){
      if (R_auto > 1){R_auto = 1}
      
      Q_sw <- InputData$FlowIn[i] #m3/s surface water flowrate at i
      Q_gw <- Q_sw/(1-PropGW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
      Q_out <- InputData$FlowOut[i] #m3/s: total outflow. Assume steady state pending dynamic output
      Rainfall <- InputData$Rain[i]/TimeStep #mm/day
      
      #Call GPP function
      PhoticDepth <- log(100)/(1.7/InputData$Secchi[i]) #Calc photic depth as function of Secchi depth
      if (PhoticDepth>LakeDepth){PhoticDepth<-LakeDepth} #QC - If photic depth calc'ed as greater than lake depth, photic depth = lake depth
      
      ######## Got rid of GPP function ##########
      #Scale areal value to volume
      CHL <- InputData$Chla[i]*PhoticDepth #KF: need to include conversion from L to m3 for this (i.e., *0.001)
      if (InputData$EpiTemp[i]>=4) {
        GPP_rate <- 10^(1.18+(0.92*log10(CHL))+(0.014*InputData$EpiTemp[i])) #mg C/m2/d
      } else {
        GPP_rate = 0 
      }
      
      GPP_Percent_DOC <- 71.4*CHL^(-0.22) #GPP as DOC, estimated as equal to 
      GPP_DOC_rate <- GPP_rate*(GPP_Percent_DOC/100)  #mg C/m2/d
      GPP_POC_rate <- GPP_rate*(1-(GPP_Percent_DOC/100))  #mg C/m2/d
      
      ############################
      PPdata_NPP_DOCL_mass <- 0*(1-R_auto)*LakeArea*TimeStep/1000 #g
      PPdata_NPP_POCL_mass <- (GPP_POC_rate + GPP_DOC_rate)*(1-R_auto)*LakeArea*TimeStep/1000 #g
      
      #Call heterotrophic respiration function for recalitrant DOC pool (DOCR) and labile DOC pool (DOCL)
      DOCR_resp_rate <- Resp(DOC_out$DOCR_conc_gm3[i],InputData$EpiTemp[i],DOCR_RespParam) #g C/m3/d
      DOCL_resp_rate <- Resp(DOC_out$DOCL_conc_gm3[i],InputData$EpiTemp[i],DOCL_RespParam) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
      
      PPdata_DOCR_massRespired = DOCR_resp_rate*LakeVolume*TimeStep #g C
      PPdata_DOCL_massRespired = DOCL_resp_rate*LakeVolume*TimeStep #g C
      
      #Calc metabolism (DO) estimates for PP validation
      Metabolism_out[i] <- (32/12) *(PPdata_NPP_DOCL_mass + PPdata_NPP_POCL_mass - PPdata_DOCR_massRespired - PPdata_DOCL_massRespired)/
        (LakeVolume*PhoticDepth/LakeDepth)/TimeStep #g/m3/d Molar conversion of C flux to O2 flux (lake metabolism)
      
      #Call SWGW Function (Surface Water/GroundWater)
      SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,AerialLoad, PropCanopy, LakePerimeter, WetlandLoad, PropWetlands, DOC_gw, 
                           InputData$SW_DOC[i], DOC_precip, LakeArea) # All in g/day, except DailyRain in m3/day
      
      #Call Sedimentation Function
      POCR_mass <- POC_out$POCR_conc_gm3[i]*LakeVolume
      POCL_mass <- POC_out$POCL_conc_gm3[i]*LakeVolume
      
      #Burial Recalitrant
      MAR_Roc <- POCR_mass*BurialFactor_R*365/LakeArea #g OC/(m^2 * yr)
      SedR_POC_burial <- MAR_Roc*(TimeStep/365)*LakeArea #g/d; Timestep with conversion from years to timestep units - days
      #Burial Labile
      MAR_Loc <- POCL_mass*BurialFactor_L*365/LakeArea #g OC/(m^2 * yr)
      SedL_POC_burial <- MAR_Loc*(TimeStep/365)*LakeArea #g/d; Timestep with conversion from years to timestep units - days
      Sed_out[i] = MAR_Loc + MAR_Roc
      
      #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
      SWGW_POCR_outflow <- POC_out$POCR_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
      SWGW_POCL_outflow <- POC_out$POCL_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
      SWGW_DOCR_outflow <- DOC_out$DOCR_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
      SWGW_DOCL_outflow <- DOC_out$DOCL_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
      
      #Calc POC-to-DOC leaching
      POCR_leachOut <- POC_out$POCR_conc_gm3[i]*POC_lcR*LakeVolume*TimeStep #g - POC concentration times leaching parameter
      POCL_leachOut <- POC_out$POCL_conc_gm3[i]*POC_lcL*LakeVolume*TimeStep #g - POC concentration times leaching parameter
      
      if (i < steps) { #don't calculate for last time step
        #Update POC and DOC concentration values (g/m3) for whole lake
        
        POC_out$POCL_conc_gm3[i+1] <-  POC_out$POCL_conc_gm3[i] + ((PPdata_NPP_POCL_mass - POCL_leachOut - SWGW_POCL_outflow - SedL_POC_burial)/LakeVolume) #g/m3
        POC_out$POCR_conc_gm3[i+1] <-  POC_out$POCR_conc_gm3[i] + ((SWGW$Load_POC - POCR_leachOut - SWGW_POCR_outflow - SedR_POC_burial)/LakeVolume)
        POC_out$POCtotal_conc_gm3[i+1] = POC_out$POCR_conc_gm3[i+1] + POC_out$POCL_conc_gm3[i+1]
        
        DOC_out$DOCL_conc_gm3[i+1] <- DOC_out$DOCL_conc_gm3[i] + ((PPdata_NPP_DOCL_mass + POCL_leachOut - SWGW_DOCL_outflow - PPdata_DOCL_massRespired)/LakeVolume) #g/m3
        DOC_out$DOCR_conc_gm3[i+1] <- DOC_out$DOCR_conc_gm3[i] + ((SWGW$Load_DOC + POCR_leachOut - SWGW_DOCR_outflow - PPdata_DOCR_massRespired)/LakeVolume) #g/m3
        DOC_out$DOCtotal_conc_gm3[i+1] = DOC_out$DOCR_conc_gm3[i+1] + DOC_out$DOCL_conc_gm3[i+1]
      }
    }
    
    # Final output
    return(data.frame('datetime' = as.Date(InputData$datetime), 'DOC_conc' = DOC_out$DOCtotal_conc_gm3,
                      'POC_conc' = POC_out$POCtotal_conc_gm3,
                      'MetabOxygen' = Metabolism_out,'SedData_MAR' = Sed_out))
  }
  
  
  min.calcModelNLL(par = c(DOCR_RespParam,DOCL_RespParam,R_auto,BurialFactor_R,BurialFactor_L,POC_lcR,POC_lcL),
                   ValidationDataDOC = pseudoDOC,
                   ValidationDataDO = ValidationDataDO,ValidationDataMAROC = ValidationDataMAROC)
  
  optimOut = optim(par = c(DOCR_RespParam,DOCL_RespParam,R_auto,BurialFactor_R,BurialFactor_L,POC_lcR,POC_lcL), 
                   min.calcModelNLL,ValidationDataDOC = pseudoDOC,
                   ValidationDataDO = ValidationDataDO,ValidationDataMAROC = ValidationDataMAROC, 
                   control = list(maxit = 150)) #setting maximum number of attempts for now
  
  ## New parameters from optimization output
  outV <- c(optimOut$par, optimOut$value, optimOut$convergence)
  return(outV)
}