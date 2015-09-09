SedimentationFunction <- function(BurialFactor,TimeStep,POC_mass){

  #Setup output data frame for returning scalars to central code from function
  FunData = data.frame(BurialScalingFactor=NA,MAR_oc=NA,POC_burial=NA)
                     
  #Burial
  FunData$MAR_oc <- POC_mass*BurialFactor #g OC/d
  
  ##OUTPUTS############################
  FunData$POC_burial <- FunData$MAR_oc*(TimeStep/1) #g; Timestep with conversion from years to timestep units - days)

  #FunData$POC_to_DIC <- POC_mass*0.01*(TimeStep/1) #g (Consumption, respiration, etc. See Connolly and COffin 1995)

  #! Not sure what the POC to DIC represents?  Is it mineralization of POC in the water column? If so, should not be in the
  #! Sedimentation function
  
  return(FunData)
}

