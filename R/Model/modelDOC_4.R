modelDOC <- function (BurialFactor_init,RespParam_init,R_auto_init) {
  
  RespParam_init = RespParam_init
  for (i in 1:(steps)){
    #Prevent negative parameter guesses from blowing model up
    # if (BurialFactor_init<=0){BurialFactor_init<-10^-5}
    # if (RespParam_init<=0){RespParam_init<-10^-5}
    # if (R_auto_init<=0){R_auto_init<-10^-5}
    if (R_auto_init > 1){R_auto_init = 1}
    
    Rainfall <- InputData$Rain[i]/TimeStep #mm/day
    Q_out <- InputData$FlowOut[i] #m3/s: total outflow. Assume steady state pending dynamic output
    if(LakeName=="Annie" && Rainfall>10){
      PropGW <- 0
    }
    Q_sw <- InputData$FlowIn[i] #m3/s surface water flowrate at i
    Q_gw <- Q_sw/(1-PropGW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
    
    #Call NPP Function
    PhoticDepth <- log(100)/(1.7/InputData$Secchi[i]) #Calc photic depth as function of Secchi depth
    if (PhoticDepth>LakeDepth){PhoticDepth<-LakeDepth} #QC - If photic depth calc'ed as greater than lake depth, photic depth = lake depth
    RawProduction <- NPP(InputData$Chla[i],InputData$TP[i],PhoticDepth,InputData$EpiTemp[i],yday(InputData$datetime[i])) #mg C/m^2/d
    NPPdata$DOC_rate[i] = RawProduction$NPP_DOC_rate
    NPPdata$POC_rate[i] = RawProduction$NPP_POC_rate
    
    #Call SWGW Function
    SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,AerialLoad, PropCanopy, LakePerimeter, WetlandLoad, PropWetlands, DOC_gw,
                         InputData$SW_DOC[i], DOC_precip, LakeArea) #change these inputs to iterative [i] values when inputs are dynamic
    SWGWData[i,2:10] <- SWGW
    
    #Call Sedimentation Function
    POC_mass <- POC_df$POC_conc_gm3[i]*LakeVolume #g
    SedOutput <- SedimentationFunction(BurialFactor_init,TimeStep,POC_mass,LakeArea) #MARC_oc units (g OC/m2/yr), POC_burial (g/day)
    SedData[i,2:4] = SedOutput
    SedData$POC_sedOut[i] <- SedData$POC_burial[i] #g #WHY IS THIS REPEATED?
    
    #Call respiration function
    DOC_resp_rate <- Resp(DOC_df$DOC_conc_gm3[i],InputData$EpiTemp[i],RespParam_init) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
    NPPdata$DOC_resp_mass[i] = DOC_resp_rate*LakeVolume*TimeStep #g C
    # Calculations that do not have to be in the loop
    NPPdata$DOC_mass[i] <- NPPdata$DOC_rate[i]*(1-R_auto_init)*LakeArea*TimeStep/1000 #g
    NPPdata$POC_mass[i] <- NPPdata$POC_rate[i]*(1-R_auto_init)*LakeArea*TimeStep/1000 #g
    
    #Calc metabolism (DO) estimates for NPP validation
    Metabolism$NEP[i] <- (NPPdata$DOC_mass[i] + NPPdata$POC_mass[i] - NPPdata$DOC_resp_mass[i]*(PhoticDepth/LakeDepth))/(LakeVolume*PhoticDepth/LakeDepth)/TimeStep #g/m3/d
    Metabolism$Oxygen[i] <- (Metabolism$NEP[i])*(32/12) #g/m3/d Molar conversion of C flux to O2 flux (lake metabolism)
    
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
      if (POC_df$POC_conc_gm3[i+1] < 0){
        stop("Negative POC concentration!")
        POC_df$POC_conc_gm3[i+1] = 0
        }
      if (DOC_df$DOC_conc_gm3[i+1] < 0){
        stop("Negative DOC concentration!")
        DOC_df$DOC_conc_gm3[i+1] = 0
        }
    }
  }

  # Final output
  return(data.frame('datetime' = as.Date(InputData$datetime), 'DOC_conc' = DOC_df$DOC_conc_gm3,
                    'MetabOxygen' = Metabolism$Oxygen,'SedData_MAR' = SedData$MAR_oc))
}