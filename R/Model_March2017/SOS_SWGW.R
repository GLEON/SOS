## SOS_SWGW.R
# This is a sub-fuction called by the SOS_CentralFunction.R, SOS_CentralFunction_optim4.R, and modelDOC_4.R scripts as part of
# the GLEON Fellowship Cohort 2 SOS working group project on organic carbon modelling. 

##Purpose:
#Model watershed and aerial loads of organic carbon

## Inputs: 
# Q_sw - surface water inflow rate (m3/s)
# Q_gw - groundwater inflow rate (m3/s)
# Aoc_day - aerial POC load (g POC/m shoreline/d)
# PC - proportion of lake perimeter that has tree canopy (unitless)
# lakePerim - lake perimeter (meters)
# Woc_day - wetland DOC load (g DOC/m shoreline/d)
# PW - proportion of lake perimeter that has wetlands (unitless)
# DOC_GW - DOC concentration in groundwater (g/m3) static parameter
# DOC_SW - DOC concentration in surface water (g/m3) dynamic input from time-series
# DOC_precip - DOC concentration in rainfall (g/m3) static parameter
# lakeArea - lake area (m2)

##Output:
#Inflowdata dataframe that includes:
# POC_aerial - deposition rate (g/d) of aerial POC load
# POC_SW - input rate of POC (g/d) in inflow water (0.1*(SW_DOC+Wetland_DOC))
# DOC_Wetland - input rate of DOC (g/d) due to wetlands
# DOC_GW - input rate of DOC (g/d) from groundwater
# DailyRain - volume rate of rainfall (m3/d)
# DOC_precip - input rate of DOC (g/d) due to rainfall
# Load_DOC - cumulative input rate of DOC in this function (g/d)
# Load_POC - cumulative input rate of POC in this function (g/d)


SWGWFunction <- function(Q_sw,Q_gw,Rainfall,Aoc_day, PC, lakePerim, Woc_day, PW, DOC_GW,
                         DOC_SW, DOC_Precip,lakeArea) {
  
  InflowData = data.frame(POC_Aerial=NA, POC_SW=NA, DOC_Wetland=NA, 
                          DOC_GW=NA, DOC_SW=NA, DailyRain=NA, 
                          DOC_Precip=NA, Load_DOC=NA, Load_POC=NA)
  
  # Aerial POC (g/d)
  InflowData$POC_Aerial <- (PC*Aoc_day*lakePerim) #g/d
  
  # Wetland DOC (g/d)
  InflowData$DOC_Wetland <- PW * Woc_day * lakePerim #g/d
  
  # Gw DOC (g/d)
  InflowData$DOC_GW <- DOC_GW * Q_gw * 86400 #g/d
  
  # Sw DOC (g/d)
  InflowData$DOC_SW <- DOC_SW * Q_sw * 86400 #g/d
  
  # Precipitation DOC (g/d)
  InflowData$DailyRain <- (Rainfall * 0.001) * lakeArea #m3/d: daily precip.
  InflowData$DOC_Precip <- DOC_Precip * InflowData$DailyRain #g/d
  
  # LOAD DOC (g/d)
  InflowData$Load_DOC <- InflowData$DOC_Wetland + InflowData$DOC_GW + InflowData$DOC_SW + InflowData$DOC_Precip # g/d DOC
  
  # Internal POC (g/d)
  POCDOC = runif(1, 0.01, 0.2) # Randomly select POC:DOC ratio from uniform distribution
  InflowData$POC_SW <- (InflowData$DOC_Wetland + InflowData$DOC_SW) * POCDOC #inflow POC load is 10% of inflow DOC load from wetlands and surface flows
  
  # LOAD POC (g/d)
  InflowData$Load_POC <- InflowData$POC_Aerial + InflowData$POC_SW # g/d POC roughly estimated as (0.1 * DOC)
  
  return(InflowData)
}
  
  