modelDOC <- function (BurialFactor_init,RespParam_init,DOC_miner_const_init,steps) {
  for (i in 1:(steps)){
    
    Q_sw <- InputData$FlowIn[i] #m3/s surface water flowrate at i
    Q_gw <- Q_sw/(1-prop_GW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
    Q_out <- InputData$FlowOut[i] #m3/s: total outflow. Assume steady state pending dynamic output
    Rainfall <- InputData$Rain[i]/TimeStep #mm/day

    #Call NPP Function
    RawProduction <- NPP(InputData$Chla[i],InputData$TP[i],InputData$EpiTemp[i]) #mg C/m^2/d
    PhoticDepth <- log(100)/(1.7/InputData$Secchi[i]) #Calc photic depth as function of Secchi depth
    if (PhoticDepth>lakeDepth){PhoticDepth<-lakeDepth} #QC - If photic depth calc'ed as greater than lake depth, photic depth = lake depth
    NPPdata[i,1:2] <- RawProduction
    NPPdata$DOC_mass[i] <- NPPdata$DOC_rate[i]*PhoticDepth*lakeArea*TimeStep/1000 #g
    
    #Call SWGW Function
    SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,Aoc_day, PC, lakePerim, Woc_day, PW, DOC_GW, prop_GW, 
                         InputData$SW_DOC[i], DOC_Precip, lakeArea) #change these inputs to iterative [i] values when inputs are dynamic
    SWGWData[i,1:9] <- SWGW
    
    #Calculate load from SWGW_in
    SWGW_mass_in$DOC[i] <- SWGWData$Load_DOC[i]*TimeStep #g
     
    #Call respiration function
    DOC_resp_rate <- Resp(DOC_conc[i,1],InputData$EpiTemp[i],RespParam_init) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
    MineralRespData$DOC_resp_mass[i] <- DOC_resp_rate*lakeVol*TimeStep #g C
    
    #Calc DOC mineralization out #! Hilary and Paul's DOC mineralization klug
    MineralRespData$DOC_miner_mass[i] = DOC_conc[i,1]*lakeVol*DOC_miner_const_init #g Current concentration multiplied by lakevolume and a mineralization constant in units of 1/d
    
    #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
    DOC_outflow[i,1] <- DOC_conc[i,1]*Q_out*60*60*24*TimeStep #g
    
    #Update POC and DOC concentration values (g/m3) for whole lake
    DOC_conc[i+1,1] <-  DOC_conc[i,1] + ((NPPdata$DOC_mass[i] + SWGW_mass_in$DOC[i] - DOC_outflow[i,1] - MineralRespData$DOC_resp_mass[i] 
                                          - MineralRespData$DOC_miner_mass[i])/lakeVol) #g/m3
    
    #Stop code and output error if concentrations go to negative
    if (DOC_conc[i+1,1]<=0){stop("Negative DOC concentration!")}
  }
  
  # Final output
  ConcOutputTimeSeries <- c(InputData$datetime,InputData$datetime[length(InputData$datetime)]+86400)
  return(data.frame('datetime' = ConcOutputTimeSeries, 'DOC_conc' = DOC_conc[,1]))
}