SedimentationFunction <- function(lakeArea,lakeVol,DOC_avg,MAR_sed_avg,Sed_oc_avg,POC_conc){

  #Setup output data frame for returning scalars to central code from function
  FunData = data.frame(BurialScalingFactor=NA,MAR_oc=NA,POC_burial=NA,POC_grazed_DIC=NA)
                     
  #Burial
  FunData$BurialScalingFactor <-  POC_conc/POC_conc_avg  #current POC divided by average POC to scale accumulation rate
  #using varied POC inputs (alloch and autoch) -- (unitless)
  
  #Scale accumulation rate based on Burial Scaling Factor
  FunData$MAR_oc <- MAR_sed_avg*(Sed_oc_avg/100)*FunData$BurialScalingFactor #g OC/m^2/yr
  
  #Allochthonous POC inputs
  #FunData$LeafParameter <- 0.01 #g/m of shoreline/d   #Need more research for dynamic input
  #FunData$LeafLitter <- lakePerim*FunData$LeafParameter #g/d
  
  #FunData$DOC_SWGW <-  DOC_SWGW   #g/d Input from SW/GW subroutine.
  #FunData$POC_SWGW <- FunData$DOC_SWGW*0.1  #g/d POC roughly estimated as 0.1*DOC.
  
  #FunData$POC_in_alloch <- FunData$LeafLitter + FunData$POC_SWGW  #g/d Total allochthonous inputs from the air and SW and GW.
  
  #Autochthonous POC inputs
  #Included in GPP code -- GPP_to_DIC <-  # = f(GPP) #Mineralization of autotrorophic products to DIC
  #Include this? NPP_to_DOC <-  # = f(GPP)? #Extracellular release of DC
  #FunData$POC_in_autoch <- NPP  #g/d Input from GPP subroutine
  
  #Total POC input
  #FunData$POC_in <- FunData$POC_in_alloch + FunData$POC_in_autoch #g/d
  
  ##OUTPUTS############################
  FunData$POC_burial <- FunData$MAR_oc*(1/365)*TimeStep*lakeArea #g/d; Timestep with conversion from years to timestep units - days)
  FunData$POC_grazed_DIC <- POC_conc*lakeVol*0.105 #g (Consumption, respiration, etc. See Connolly and COffin 1995)
  #Assume 0.105 consumed per day (thus TimeStepConversion)
  
  return(FunData)
}

