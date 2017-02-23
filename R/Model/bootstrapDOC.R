
bootstrapDOC <- function(newObs,datetime,LakeName,timestampFormat = '%Y-%m-%d') {
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
  
  #### For TOOLIK ONLY #### (dealing with ice season)
  if (LakeName=='Toolik'){
    # Set FlowIn to 0 for ice-on periods for Toolik Inlet, based on historical data: 
    # http://toolik.alaska.edu/edc/journal/annual_summaries.php?summary=inlet
    # Used average ice on/off dates from 2006-2010 for 2001-2005 (no data available those years)
    
    icepath = paste0(LakeName,'Lake','/','ToolikInlet_IceOn_IceOff.csv')
    IceOnOff = read.csv(icepath)
    IceOnOff$IceOff = as.POSIXct(strptime(IceOnOff$IceOff,"%m/%d/%Y %H:%M"),tz="GMT") #Convert time to POSIX
    IceOnOff$IceOn = as.POSIXct(strptime(IceOnOff$IceOn,"%m/%d/%Y %H:%M"),tz="GMT") #Convert time to POSIX
    #str(IceOnOff)
    
    ice_func <- function(year,off,on, dataframe){
      ## ARGUMENTS ##
      # year: 4 digits, in quotes as character ('2002')
      # off: month-day in quotes ('05-09') = May 9th
      # on: same structure as off
      # dataframe = name of dataframe of interest
      
      day1 = paste0(year,'-01-01')
      year_num = as.numeric(year)
      year_before = as.character(year_num - 1)
      day1 = paste0(year_before, '-12-31') # there was a bug; R thought >= Jan 1 meant Jan 2 (must be something internal with date structure)
      day365 = paste0(year, '-12-31')
      iceoff = paste0(year,'-',off)
      iceon = paste0(year,'-',on)
      # create annual subset for specific year
      annual_subset = dataframe[dataframe$datetime > day1 & dataframe$datetime <= day365,]
      
      # extract data for that year before ice off
      pre_thaw = annual_subset[annual_subset$datetime < iceoff,]
      pre_thaw$FlowIn = rep(0,length(pre_thaw$FlowIn)) # set FlowIn = 0 during ice time
      pre_thaw$FlowOut = pre_thaw$FlowIn # we assume flow out = flow in
      
      # extract data for that year for between ice off and ice on (ice free season)
      ice_free_season = annual_subset[annual_subset$datetime >= iceoff & annual_subset$datetime < iceon,]
      
      # extract data for that year for after fall ice on
      post_freeze = annual_subset[annual_subset$datetime >= iceon,]
      post_freeze$FlowIn = rep(0,length(post_freeze$FlowIn))
      post_freeze$FlowOut = post_freeze$FlowIn
      
      # combine 3 annual subsets (pre thaw, ice free season, post freeze)
      annual_corrected = rbind.data.frame(pre_thaw,ice_free_season,post_freeze)
      return(annual_corrected)
    }
    
    years = as.character(IceOnOff$Year)
    iceoff_dates = IceOnOff$IceOff
    iceoff_dates = format(iceoff_dates, format='%m-%d')
    iceon_dates = IceOnOff$IceOn
    iceon_dates = format(iceon_dates, format='%m-%d')
    
    for (i in 1:length(years)){
      for (j in 1:length(iceoff_dates)) {
        for (k in 1:length(iceon_dates)) {
          x = ice_func(year = years[i], off = iceoff_dates[j], on = iceon_dates[k], dataframe = InputData)
          assign(paste0(LakeName,years[i]),x)
          x = NULL #get rid of extra output with unassigned name
        }
      }
    }
    
    ## Combine annual data frames into single for lake
    # I know this isn't the most dynamic code, but I was having trouble making the above loop output a single DF
    InputData = rbind(Toolik2001,Toolik2002,Toolik2003,Toolik2004,Toolik2005,Toolik2006,
                      Toolik2007,Toolik2008,Toolik2009,Toolik2010)
    
  }
  
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
  
  DOCdiff <- function(pars,ValidationDataDOC){
    # DOC model 
    modeled = modelDOC(pars[1],pars[2],pars[3],pars[4],pars[5],pars[6],pars[7])
    joinMod = inner_join(ValidationDataDOC,modeled,by='datetime')
    resDOC = joinMod$DOC - joinMod$DOC_conc
    return(resDOC)
  }
  # min.calcModelNLL <- function(pars,ValidationDataDOC,ValidationDataDO,ValidationDataMAROC){
  #   modeled = modelDOC(pars[1],pars[2],pars[3],pars[4],pars[5],pars[6],pars[7])
  #   
  #   obsIndx = ValidationDataDOC$datetime %in% modeled$datetime
  #   modIndx = modeled$datetime %in% ValidationDataDOC$datetime
  #   CalibrationOutputDOC <- data.frame(datetime = ValidationDataDOC[obsIndx,]$datetime,
  #                                      Measured = ValidationDataDOC[obsIndx,]$DOC, Modelled = modeled[modIndx,]$DOC_conc)
  #   #resDOC = scale(CalibrationOutputDOC$Measured - CalibrationOutputDOC$Modelled,center = F)
  #   resDOC = (CalibrationOutputDOC$Measured - CalibrationOutputDOC$Modelled)
  #   obsIndx = ValidationDataDO$datetime %in% modeled$datetime
  #   modIndx = modeled$datetime %in% ValidationDataDO$datetime
  #   CalibrationOutputDO <- data.frame(datetime = ValidationDataDO[obsIndx,]$datetime,
  #                                     Measured = ValidationDataDO[obsIndx,]$Flux, Modelled = modeled[modIndx,]$MetabOxygen)
  #   
  #   #resDO = scale(CalibrationOutputDO$Measured - CalibrationOutputDO$Modelled,center = F)
  #   DOScale = 5
  #   resDO = (CalibrationOutputDO$Measured - CalibrationOutputDO$Modelled) * DOScale
  #   sedScale = 0.001
  #   resSedData = (mean(modeled$SedData_MAR,na.rm = T) - ValidationDataMAROC) * sedScale #not scaled because it is 1 value
  #   
  #   res = c(resDOC,resDO,rep(resSedData,length(resDOC)))
  #   
  #   nRes 	= length(res)
  #   SSE 	= sum(res^2)
  #   sigma2 	= SSE/nRes
  #   NLL 	= 0.5*((SSE/sigma2) + nRes*log(2*pi*sigma2))
  #   print(paste('NLL: ',NLL,sep=''))
  #   print(paste('parameters: ',pars,sep=''))
  #   return(NLL)
  # }

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
  
  # Starting parameters cannot be negative, because of bounds we set 
  # parStart = pars
  # lowerBound = c(0,0,0.5,0,0,0,0)
  # upperBound = c(0.005,0.01,1,1,1,0.1,0.5)
  # parStart[(parStart - lowerBound) < 0] = lowerBound[(parStart - lowerBound) < 0]
  # parStart[(upperBound - parStart) < 0] = upperBound[(upperBound - parStart) < 0]
  # names(parStart) = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L','POC_lcR','POC_lcL')

  # For difficult problems it may be efficient to perform some iterations with Pseudo, which will bring the algorithm 
  # near the vicinity of a (the) minimum, after which the default algorithm (Marq) is used to locate the minimum more precisely.
  Fit2 <- modFit(f = DOCdiff, p=c(DOCR_RespParam,DOCL_RespParam,R_auto,BurialFactor_R,BurialFactor_L,POC_lcR,POC_lcL),
                 method = 'BFGS',control = list(maxit = 10),
                 ValidationDataDOC = pseudoDOC,
                 lower= c(0,0,0.5,0,0,0,0),
                 upper= c(0.005,0.01,1,1,1,0.1,0.5))
  
  newPars = Fit2$par
  modeled = modelDOC(newPars[1],newPars[2],newPars[3],newPars[4],newPars[5],newPars[6],newPars[7])
  joinMod = inner_join(ValidationDataDOC,modeled,by='datetime')
  #Goodness of fit
  library(hydroGOF)
  rmseFit = rmse(joinMod$DOC_conc, joinMod$DOC) #Harp 0.43 Trout 0.427 Monona 0.62 Vanern 0.32
  nseFit = NSE(joinMod$DOC_conc, joinMod$DOC) #Harp 0.09 Trtou -0.015 Monona 0.279 Vanern -0.03
  
  ## New parameters from optimization output
  outV <- c(Fit2$par, rmseFit, nseFit)
  return(outV)
}