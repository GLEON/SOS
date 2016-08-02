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
# POC_aerial - deposition rate (g/d) of aerial POC load
# POC_SW - input rate of POC (g/d) in inflow water (0.1*(SW_DOC+Wetland_DOC))
# DOC_Wetland - input rate of DOC (g/d) due to wetlands
# DOC_GW - input rate of DOC (g/d) from groundwater
# DailyRain - volume rate of rainfall (m3/d)
# DOC_precip - input rate of DOC (g/d) due to rainfall
# Load_DOC - cumulative input rate of DOC in this function (g/d)
# Load_POC - cumulative input rate of POC in this function (g/d)


SedimentationFunction <- function(BurialFactor,TimeStep,POC_mass,lakeArea){

  #Setup output data frame for returning scalars to central code from function
  SediData = data.frame(BurialScalingFactor=BurialFactor,MAR_oc=NA,POC_burial=NA) # Burial Factor units (d-1)
  
  ##OUTPUTS############################
  if (BurialFactor > 0) {
    #Burial
    SediData$MAR_oc <- POC_mass*BurialFactor*365/lakeArea #g OC/(m^2 * yr)
    SediData$POC_burial <- SediData$MAR_oc*(TimeStep/365)*lakeArea #g/d; Timestep with conversion from years to timestep units - days)
} else {
    SediData$MAR_oc <- 0
    SediData$POC_burial <- 0 # If no burial
  }
  
  return(SediData)
}

