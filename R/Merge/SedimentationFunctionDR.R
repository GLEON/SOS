SedimentationFunction <- function(TimeStep,lakeArea,lakeVol,DOC_avg,MAR_sed_avg,Sed_oc_avg,POC_conc){

  #Setup output data frame for returning scalars to central code from function
  FunData = data.frame(BurialScalingFactor=NA,MAR_oc=NA,POC_burial=NA,POC_to_DIC=NA)
                     
  #Burial
  FunData$BurialScalingFactor <-  POC_conc/POC_conc_avg  #current POC divided by average POC to scale accumulation rate
  #using varied POC inputs (alloch and autoch) -- (unitless)
  
  #Scale accumulation rate based on Burial Scaling Factor
  FunData$MAR_oc <- MAR_sed_avg*(Sed_oc_avg/100)*FunData$BurialScalingFactor #g OC/m^2/yr
  
  ##OUTPUTS############################
  FunData$POC_burial <- FunData$MAR_oc*(1/365)*TimeStep*lakeArea #g/d; Timestep with conversion from years to timestep units - days)
  FunData$POC_to_DIC <- POC_conc*lakeVol*0.105*(TimeStep/1) #g (Consumption, respiration, etc. See Connolly and COffin 1995)
  
  return(FunData)
}

