
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

      DOC_out$DOCL_conc_gm3[i+1] <- DOC_out$DOCL_conc_gm3[i] + ((PPdata_NPP_DOCL_mass + POCL_leachOut - SWGW_DOCL_outflow - PPdata_DOCL_massRespired)/LakeVolume) #g/m3
      DOC_out$DOCR_conc_gm3[i+1] <- DOC_out$DOCR_conc_gm3[i] + ((SWGW$Load_DOC + POCR_leachOut - SWGW_DOCR_outflow - PPdata_DOCR_massRespired)/LakeVolume) #g/m3
      DOC_out$DOCtotal_conc_gm3[i+1] = DOC_out$DOCR_conc_gm3[i+1] + DOC_out$DOCL_conc_gm3[i+1]
    }
  }
  
  # Final output
  return(data.frame('datetime' = as.Date(InputData$datetime), 'DOC_conc' = DOC_out$DOCtotal_conc_gm3,
                    'MetabOxygen' = Metabolism_out$oxy_conc))
}

