## SOS_Resp.R
# This is a sub-fuction called by the SOS_CentralFunction.R, SOS_CentralFunction_optim4.R, and modelDOC_4.R scripts as part of
# the GLEON Fellowship Cohort 2 SOS working group project on organic carbon modelling. 

##Purpose:
#Model heterotrophic respiration.  

## Inputs: 
# DOC - modelled DOC concentration (g/m3) in water column
# temp - temp of "photic zone layer" (estimated as epilimnion temp, deg C)
# RespParam - calibrated parameter heterotrophic respiration

##Output:
#DOC metabolized into inorganic C and O2 (g/m3)

Resp <- function(DOC,temp,RespParam){
  
  TempCorr <- 1.08^(temp-20) #Temperature correction factor for DOC-respiration relationship (Paul Hanson, personal correspondence)
  
  # if (RespParam > 0) {
    R <- DOC*RespParam*TempCorr #g/m3
  # } else {
  #   R <- 0
  # }
  
  return(R)  
}



