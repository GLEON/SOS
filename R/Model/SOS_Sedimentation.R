## SOS_Sedimentation.R
# This is a sub-fuction called by the SOS_CentralFunction.R, SOS_CentralFunction_optim4.R, and modelDOC_4.R scripts as part of
# the GLEON Fellowship Cohort 2 SOS working group project on organic carbon modelling. 

##Purpose:
#Model net burial rate of organic carbon

## Inputs: 
# BurialFactor - calibration parameter representing fraction of POC permanently buried per day (1/d)
# TimeStep - model timestep in days
# POC_mass - total mass of POC in lake at given time step (g)
# lakeArea - lake area in m2

##Output:
#SediData dataframe that includes:
# BurialScalingFactor - proportion of water-column POC buried at given time step
# MAR_oc - mass accumulation rate of OC in sediments in g/m2/yr
# POC_burial - rate of POC burial in sediments (g/d)


SedimentationFunction <- function(BurialFactor,TimeStep,POC_mass,lakeArea){

  SediData = data.frame(BurialScalingFactor=BurialFactor,MAR_oc=NA,POC_burial=NA) # Burial Factor units (d-1)
  
  ##OUTPUTS############################
  # if (BurialFactor > 0) {
    #Burial
    SediData$MAR_oc <- POC_mass*BurialFactor*365/lakeArea #g OC/(m^2 * yr)
    SediData$POC_burial <- SediData$MAR_oc*(TimeStep/365)*lakeArea #g/d; Timestep with conversion from years to timestep units - days
# } else {
#     SediData$MAR_oc <- 0
#     SediData$POC_burial <- 0 # If no burial
#   }
#   
  return(SediData)
}

