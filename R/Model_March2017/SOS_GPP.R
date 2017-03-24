## SOS_NPP.R
# This is a sub-fuction called by the SOS_CentralFunction.R, SOS_CentralFunction_optim4.R, and modelDOC_4.R scripts as part of
# the GLEON Fellowship Cohort 2 SOS working group project on organic carbon modelling. 

##Purpose:
#Model primary productivity

## Inputs: 
# CHL - water column chl-a concentration (micrograms/L) time-series input
# P - water column phosphorous concentration (micrograms/L) time-series input
# WT - water temp (deg C)
# JulianDay - day of year
# ProdStarDay - day to toggle on production (as needed, currently unused)
# ProdEndDay - day to toggle of production (as needed, currently unused)

## Output:
# GPP data frame
# GPP_DOC_rate - rate of DOC input due to GPP (yes, GPP) in mg/m2/d
# GPP_POC_rate - rate of POC input due to GPP (yes, GPP) in mg/m2/d
# These values are changed to GPP in lines following the sub-function call in the main code. 


GPP <- function(CHL,P,PhoticDepth,WT,JulianDay) {
  
  GPPdata <- data.frame(GPP_DOC_rate=NA,GPP_POC_rate=NA)
  
  #coefficients to predict chla from TP
  a0=-0.390   #! 
  a1=0.874
  
  #coefficients to predict GPP from chl and WT from Table 1: Morin et al 1999
  b0=1.18 # intercept
  b1=0.92 # log10 chla
  b2=0.014 # temperature
  
  #If no chl-a data, use P data
  if(missing(CHL)||is.na(CHL))
  {
    if(missing(P)||is.na(P))
      return(NA)
    else
    {
      P<-log10(P)
      CHL<- a0+a1*P
      CHL<-10^CHL
    }
  }
  
  #Scale areal value to volume
  CHL <- CHL*PhoticDepth #KF: need to include conversion from L to m3 for this (i.e., *0.001)
  P <- P*PhoticDepth #KF: need to include conversion from L to m3 for this (i.e., *0.001)
  
  #M Morin et al. 1999
  # Empirical Models Predicting Primary Productivity from Chlorophyll a and Water Temperature for Stream Periphyton and Lake and Ocean Phytoplankton
  # Antoine Morin, William Lamoureux, and Judith Busnarda
  # Journal of the North American Benthological Society 1999 18:3, 299-307 
  #M Areal chl a : mg chl-a m-2
  #M Authors converted chl-a mass per volume to areal values by integrating 
  #M over photic zone depth defined in original papers 
  #M GPP output in g m-2 d-1

  if (WT>=4) {
    GPP_rate <- 10^(b0+(b1*log10(CHL))+(b2*WT)) #mg C/m2/d
  } else {
    GPP_rate = 0 
  }
  
  GPP_Percent_DOC <- 71.4*CHL^(-0.22) #GPP as DOC, estimated as equal to 
  #%POC respired because POC most be converted to DOC to eventually be respired.
  #M Pace and Prairie 2005, Ch. 7: Respiration in Lakes 
 
  GPPdata$GPP_DOC_rate <- GPP_rate*(GPP_Percent_DOC/100)  #mg C/m2/d
  GPPdata$GPP_POC_rate <- GPP_rate*(1-(GPP_Percent_DOC/100))  #mg C/m2/d

  return(GPPdata)
}