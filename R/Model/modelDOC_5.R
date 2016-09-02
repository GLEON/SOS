modelDOC <- function (BurialFactorR_init,BurialFactorL_init,RespParamR_init,RespParamL_init,R_auto_init) {
  
  for (i in 1:(steps)){
    #Prevent negative parameter guesses from blowing model up
    # if (BurialFactor_init<=0){BurialFactor_init<-10^-5}
    # if (RespParam_init<=0){RespParam_init<-10^-5}
    # if (R_auto_init<=0){R_auto_init<-10^-5}
    if (R_auto_init > 1){R_auto_init = 1}
    
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
    PPdata$NPP_DOCL_mass[i] <- PPdata$GPP_DOCL_rate[i]*(1-R_auto_init)*LakeArea*TimeStep/1000 #g
    PPdata$NPP_POCL_mass[i] <- PPdata$GPP_POCL_rate[i]*(1-R_auto_init)*LakeArea*TimeStep/1000 #g
    
    #Call heterotrophic respiration function for recalitrant DOC pool (DOCR) and labile DOC pool (DOCL)
    DOCR_resp_rate <- Resp(DOC_df$DOCR_conc_gm3[i],InputData$EpiTemp[i],RespParamR_init) #g C/m3/d
    DOCL_resp_rate <- Resp(DOC_df$DOCL_conc_gm3[i],InputData$EpiTemp[i],RespParamL_init) #g C/m3/d ##CHANGE TO AVERAGE OR LAYER TEMP WHEN AVAILABLE IN TIME SERIES
    
    PPdata$DOCR_massRespired[i] = DOCR_resp_rate*LakeVolume*TimeStep #g C
    PPdata$DOCL_massRespired[i] = DOCL_resp_rate*LakeVolume*TimeStep #g C
    
    #Calc metabolism (DO) estimates for PP validation
    Metabolism$NEP[i] <- (PPdata$NPP_DOCL_mass[i] + PPdata$NPP_POCL_mass[i] - PPdata$DOCR_massRespired[i]*(PhoticDepth/LakeDepth) - PPdata$DOCL_massRespired[i]*(PhoticDepth/LakeDepth))/
      (LakeVolume*PhoticDepth/LakeDepth)/TimeStep #g/m3/d #volume of photic zone
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
    SedOutput_R <- SedimentationFunction(BurialFactorR_init,TimeStep,POCR_mass,LakeArea)
    SedOutput_L <- SedimentationFunction(BurialFactorL_init,TimeStep,POCL_mass,LakeArea)
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
      POC_df$POCR_conc_gm3[i+1] <-  POC_df$POCR_conc_gm3[i] + ((SWGWData$POCR_massIn_g[i] - LeachData$POCR_leachOut[i] - SWGWData$POCR_outflow[i] - SedData$POC_burial_L[i])/LakeVolume)
      POC_df$POCtotal_conc_gm3[i+1] = POC_df$POCR_conc_gm3[i+1] + POC_df$POCL_conc_gm3[i+1]
      
      DOC_df$DOCL_conc_gm3[i+1] <- DOC_df$DOCL_conc_gm3[i] + ((PPdata$NPP_DOCL_mass[i] + LeachData$DOCL_leachIn[i] - SWGWData$DOCL_outflow[i] - PPdata$DOCL_massRespired[i])/LakeVolume) #g/m3
      DOC_df$DOCR_conc_gm3[i+1] <- DOC_df$DOCR_conc_gm3[i] + ((SWGWData$DOCR_massIn_g[i] + LeachData$DOCR_leachIn[i] - SWGWData$DOCR_outflow[i] - PPdata$DOCR_massRespired[i])/LakeVolume) #g/m3
      DOC_df$DOCtotal_conc_gm3[i+1] = DOC_df$DOCR_conc_gm3[i+1] + DOC_df$DOCL_conc_gm3[i+1]
      
      #Stop code and output error if concentrations go to negative
      # if (POC_df$POCtotal_conc_gm3[i+1]<=0){stop("Negative POC concentration!")}
      # if (DOC_df$DOCtotal_conc_gm3[i+1]<=0){stop("Negative DOC concentration!")}
    }
  }

  # Final output
  return(data.frame('datetime' = as.Date(InputData$datetime), 'DOC_conc' = DOC_df$DOCtotal_conc_gm3,
                    'MetabOxygen' = Metabolism$Oxygen,'SedData_MAR' = SedData$MAR_oc_total))
}