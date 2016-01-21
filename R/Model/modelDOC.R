modelDOC <- function (BurialFactor_init,RespParam_init,R_auto_init,steps) {
  
  for (i in 1:(steps)){
    
    #Prevent negative parameter guesses from blowing model up
    if (BurialFactor_init<=0){BurialFactor_init<-10^-5}
    if (RespParam_init<=0){RespParam_init<-10^-5}
    if (R_auto_init<=0){R_auto_init<-10^-5}
    
    Q_sw <- InputData$FlowIn[i] #m3/s surface water flowrate at i
    Q_gw <- Q_sw/(1-prop_GW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
    Q_out <- InputData$FlowOut[i] #m3/s: total outflow. Assume steady state pending dynamic output
    Rainfall <- InputData$Rain[i]/TimeStep #mm/day
    
    #Call NPP Function
    PhoticDepth <- log(100)/(1.7/InputData$Secchi[i]) #Calc photic depth as function of Secchi depth
    if (PhoticDepth>lakeDepth){PhoticDepth<-lakeDepth} #QC - If photic depth calc'ed as greater than lake depth, photic depth = lake depth
    RawProduction <- NPP(InputData$Chla[i],InputData$TP[i],PhoticDepth,InputData$EpiTemp[i]) #mg C/m^2/d
    NPPdata[i,1:2] <- RawProduction
    NPPdata$DOC_mass[i] <- NPPdata$DOC_rate[i]*R_auto_init*lakeArea*TimeStep/1000 #g
    NPPdata$POC_mass[i] <- NPPdata$POC_rate[i]*R_auto_init*lakeArea*TimeStep/1000 #g
    
    #Call SWGW Function
    SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,Aoc_day, PC, lakePerim, Woc_day, PW, DOC_GW, prop_GW, 
                         InputData$SW_DOC[i], DOC_Precip, lakeArea) #change these inputs to iterative [i] values when inputs are dynamic
    SWGWData[i,1:9] <- SWGW
    
    #Calculate load from SWGW_in
    SWGW_mass_in$DOC[i] <- SWGWData$Load_DOC[i]*TimeStep #g
    SWGW_mass_in$POC[i] <- SWGWData$Load_POC[i]*TimeStep #g
    
    #Call Sedimentation Function
    POC_mass <- POC_conc[i,1]*lakeVol
    SedOutput <- SedimentationFunction(BurialFactor_init,TimeStep,POC_mass,lakeArea)
    SedData[i,1:3] = SedOutput
    POC_sed_out[i,1] <- SedData$POC_burial[i] #g
    
    #Call respiration function
    DOC_resp_rate <- Resp(DOC_conc[i,1],InputData$EpiTemp[i],RespParam_init) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
    MineralRespData$DOC_resp_mass[i] <- DOC_resp_rate*lakeVol*TimeStep #g C
    
    #Calc metabolism (DO) estimates for NPP validation
    Metabolism$NEP[i] <- (NPPdata$DOC_mass[i] + NPPdata$POC_mass[i] - MineralRespData$DOC_resp_mass[i]*(PhoticDepth/lakeDepth))/(lakeVol*PhoticDepth/lakeDepth)/TimeStep #g/m3/d
    Metabolism$Oxygen[i] <- Metabolism$NEP[i]*(32/12) #g/m3/d Molar conversion of C flux to O2 flux (lake metabolism)
    
    #Calc POC-to-DOC leaching
    LeachData$POC_out[i] <- POC_conc[i,1]*POC_lc*lakeVol*TimeStep #g - POC concentration times leaching parameter
    LeachData$DOC_in[i] <- LeachData$POC_out[i]
    
    #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
    POC_outflow[i,1] <- POC_conc[i,1]*Q_out*60*60*24*TimeStep #g
    DOC_outflow[i,1] <- DOC_conc[i,1]*Q_out*60*60*24*TimeStep #g
    
    #Update POC and DOC concentration values (g/m3) for whole lake
    #POC_conc[i+1,1] <-  POC_conc[i,1] + ((NPPdata$NPP_mass[i] + SWGW_mass_in$POC[i] - POC_outflow[i,1] - POC_sed_out[i,1] - SedData$POC_to_DIC[i])/lakeVol) #g/m3
    POC_conc[i+1,1] <-  POC_conc[i,1] + ((NPPdata$POC_mass[i] + SWGW_mass_in$POC[i] - POC_outflow[i,1] - POC_sed_out[i,1] - LeachData$POC_out[i])/lakeVol) #g/m3
    DOC_conc[i+1,1] <-  DOC_conc[i,1] + ((NPPdata$DOC_mass[i] + SWGW_mass_in$DOC[i] + LeachData$DOC_in[i] - DOC_outflow[i,1] - MineralRespData$DOC_resp_mass[i])/lakeVol) #g/m3
    
    #Stop code and output error if concentrations go to negative
    #if (POC_conc[i+1,1]<=0){stop("Negative POC concentration!")}
    if (DOC_conc[i+1,1]<=0){stop("Negative DOC concentration!")}
  }
  # Final output
  #ConcOutputTimeSeries <- c(InputData$datetime,InputData$datetime[length(InputData$datetime)]+86400)
  ConcOutputTimeSeries <- InputData$datetime
  return(data.frame('datetime' = ConcOutputTimeSeries, 'DOC_conc' = DOC_conc[1:steps,1],
                    'MetabOxygen' = Metabolism$Oxygen,'SedData_MAR' = SedData$MAR_oc))
}