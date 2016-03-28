SedimentationFunction <- function(BurialFactor,TimeStep,POC_mass,lakeArea){

  #Setup output data frame for returning scalars to central code from function
  FunData = data.frame(BurialScalingFactor=NA,MAR_oc=NA,POC_burial=NA)
  
  FunData$BurialScalingFactor <- BurialFactor #1/d
                     
  #Burial
  FunData$MAR_oc <- POC_mass*BurialFactor*365/lakeArea #g OC/(m^2 * yr)
  
  ##OUTPUTS############################
  if (BurialFactor > 0) {
    #Burial
    FunData$MAR_oc <- POC_mass*BurialFactor*365/lakeArea #g OC/(m^2 * yr)
    FunData$POC_burial <- FunData$MAR_oc*(TimeStep/365)*lakeArea #g; Timestep with conversion from years to timestep units - days)
  } else {
    FunData$MAR_oc <- 0
    FunData$POC_burial <- 0 # If no burial
  }
  
  return(FunData)
}

