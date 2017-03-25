
bootstrapDOC <- function(newObs,num,datetimeDOC,datetimeDO,LakeName,timestampFormat = '%Y-%m-%d') {
  pseudo_DOC = data.frame(datetime = datetimeDOC, DOC = newObs[1:num], DOCwc = newObs[1:num])
  pseudo_DO = data.frame(datetime = datetimeDO, DO = newObs[(num+1):length(newObs)])
  
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
  library(FME)
  
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
  
  ############################################################################
  
  DOC_DO_diff <- function(pars,cal_DOC,cal_DO){
    # DOC model 
    modeled = modelDOC(pars[1],pars[2],pars[3],pars[4])
    joinDOC = inner_join(cal_DOC,modeled,by='datetime')
    resDOC = joinDOC$DOC - joinDOC$DOC_conc
    joinDO = inner_join(cal_DO,modeled,by='datetime')
    resDO = joinDO$DO_con - joinDO$MetabOxygen.oxy_conc
    lengthScale = length(resDO)/length(resDOC)
    return(c(resDOC,resDO/lengthScale))
  }
  
  modelDOC <- function (DOCR_RespParam,DOCL_RespParam,BurialFactor_R,BurialFactor_L) {
    
    POC_out = data.frame(POCL_conc_gm3 = rep(NA,steps), POCR_conc_gm3 = rep(NA,steps), POCtotal_conc_gm3 = rep(NA,steps))
    DOC_out = data.frame(DOCL_conc_gm3 = rep(NA,steps), DOCR_conc_gm3 = rep(NA,steps), DOCtotal_conc_gm3 = rep(NA,steps))
    
    POC_out[1,] = c(POC_init*0.2, POC_init*0.8, POC_init)
    DOC_out[1,] = c(DOC_init*0.2, DOC_init*0.8, DOC_init)
    Metabolism_out = data.frame(oxy_mass = rep(NA,steps), oxy_conc = rep(NA,steps))
    Sed_out = NA
    Metabolism_out$oxy_conc[1] = o2.at.sat.base(InputData$EpiTemp[1])
    
    for (i in 1:(steps)){
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
      GPP_DOC_rate <- 0.2*GPP_rate*(GPP_Percent_DOC/100)  #mg C/m2/d
      GPP_POC_rate <- 0.2*GPP_rate*(1-(GPP_Percent_DOC/100))  #mg C/m2/d
      
      ############################
      PPdata_NPP_DOCL_mass <- (GPP_DOC_rate)*LakeArea*TimeStep/1000 #g
      PPdata_NPP_POCL_mass <- (GPP_POC_rate)*LakeArea*TimeStep/1000 #g
      
      #Call heterotrophic respiration function for recalitrant DOC pool (DOCR) and labile DOC pool (DOCL)
      DOCR_resp_rate <- Resp(DOC_out$DOCR_conc_gm3[i],InputData$EpiTemp[i],DOCR_RespParam) #g C/m3/d
      DOCL_resp_rate <- Resp(DOC_out$DOCL_conc_gm3[i],InputData$EpiTemp[i],DOCL_RespParam) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
      
      PPdata_DOCR_massRespired = DOCR_resp_rate*LakeVolume*TimeStep #g C
      PPdata_DOCL_massRespired = DOCL_resp_rate*LakeVolume*TimeStep #g C
      
      #Calc metabolism (DO) estimates for PP validation
      Metabolism_out$oxy_mass[i] <- (32/12) *(PPdata_NPP_DOCL_mass + PPdata_NPP_POCL_mass - PPdata_DOCR_massRespired - PPdata_DOCL_massRespired)/
        (LakeVolume*PhoticDepth/LakeDepth)/TimeStep #g/m3/d Molar conversion of C flux to O2 flux (lake metabolism)
      
      
      #Call SWGW Function (Surface Water/GroundWater)
      SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,AerialLoad, PropCanopy, LakePerimeter, WetlandLoad, PropWetlands, DOC_gw, 
                           InputData$SW_DOC[i], DOC_precip, LakeArea) # All in g/day, except DailyRain in m3/day
      
      #Call Sedimentation Function
      POCR_mass <- SWGW$Load_POC * (1-(Q_out*3600*24)/LakeVolume) #g/day
      POCL_mass <- PPdata_NPP_POCL_mass * (1-(Q_out*3600*24)/LakeVolume) #g/day
      
      #Burial Recalitrant
      # if (BurialFactor_R < 0 ){ BurialFactor_R = 0}
      MAR_Roc <- POCR_mass * BurialFactor_R * 365/LakeArea #g OC/(m^2 * yr)
      SedR_POC_burial <- POCR_mass * BurialFactor_R #g/d; Timestep with conversion from years to timestep units - days
      #Burial Labile
      # if (BurialFactor_L < 0 ){ BurialFactor_L = 0}
      MAR_Loc <- POCL_mass*BurialFactor_L*365/LakeArea #g OC/(m^2 * yr)
      SedL_POC_burial <-  POCL_mass*BurialFactor_L #g/d; Timestep with conversion from years to timestep units - days
      Sed_out[i] = MAR_Loc + MAR_Roc
      
      #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
      SWGW_DOCR_outflow <- DOC_out$DOCR_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
      SWGW_DOCL_outflow <- DOC_out$DOCL_conc_gm3[i]*Q_out*60*60*24*TimeStep #g
      
      #Calc POC-to-DOC leaching
      POCR_leachOut <- POCR_mass*(1-BurialFactor_R)*TimeStep #g - POC concentration times leaching parameter
      POCL_leachOut <- POCL_mass*(1-BurialFactor_L)*TimeStep #g - POC concentration times leaching parameter
      
      if (i < steps) { #don't calculate for last time step
        #Update POC and DOC concentration values (g/m3) for whole lake
        Fatm = 0.7*(Metabolism_out$oxy_conc[i] - o2.at.sat.base(InputData$EpiTemp[i]))/(PhoticDepth/2)
        Metabolism_out$oxy_conc[i+1] = Metabolism_out$oxy_conc[i] + Metabolism_out$oxy_mass[i] - Fatm
        
        POC_out$POCL_conc_gm3[i] <-  (POCL_mass - POCL_leachOut - SedL_POC_burial)/LakeVolume #g/m3
        POC_out$POCR_conc_gm3[i] <-  (POCR_mass - POCR_leachOut - SedR_POC_burial)/LakeVolume
        POC_out$POCtotal_conc_gm3[i] = POC_out$POCR_conc_gm3[i] + POC_out$POCL_conc_gm3[i]
        
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
  
  # Starting parameters cannot be negative, because of bounds we set 
  parStart = c(DOCR_RespParam,DOCL_RespParam,BurialFactor_R,BurialFactor_L)
  lowerBound = c(0.0003,0.003,0,0)
  upperBound = c(0.003,0.3,1,1)
  names(parStart) = c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
  
  # For difficult problems it may be efficient to perform some iterations with Pseudo, which will bring the algorithm 
  # near the vicinity of a (the) minimum, after which the default algorithm (Marq) is used to locate the minimum more precisely.
  Fit2 <- modFit(f = DOC_DO_diff, p=parStart,method = 'Pseudo',
                 cal_DOC = pseudo_DOC,
                 cal_DO = pseudo_DO,
                 lower= lowerBound,
                 upper= upperBound)
  
  return(Fit2$par)
}