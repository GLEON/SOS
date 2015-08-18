# DOC/POC Surface & Groundwater Sub-Routine for SOS Carbon Flux Code
# Last Update: 7/24/2015, KF
# Based on Load equation from Hanson et al 2014 'Quantifying lake allochthonous organic carbon budgets using a simple equilibrium model'

#Inputs from main program
StartDay <- 1  #Julian day
EndDay <- 1 #Julian day

lakePerim <- 1000 #m
lakeArea <- 62500 #m2

TimeStep <- 1 #days
TimeStepConversion <- 1/TimeStep #days/days
steps <- (EndDay-StartDay+1)/TimeStep

#Setup data frame for SwGw function
InflowData <- data.frame(Q_in=numeric(steps), Aoc=numeric(steps), 
                         DOC_Aerial=numeric(steps), DOC_Wetland=numeric(steps), 
                         DOC_GW=numeric(steps), DOC_SW=numeric(steps), 
                         DailyRain=numeric(steps), DOC_Precip=numeric(steps), 
                         Load_DOC=numeric(steps), Load_POC=numeric(steps))

#Inputs- Lake Specific
# Calculated inputs from GIS data
PC <- 0.7 #unitless: proportion of lake shore with canopy
PW <- 0.2 #unitless: proportion of lake shore with wetlands

prop_GW <- .3 # Unitless: proportion of Q_in that is from groundwater
DOC_GW <- 10 # g/m3: DOC concentration in groundwater. 2-40 g/m3 per Hanson et al 2014
DOC_SW <- 1 # g/m3: DOC concentration in surface water
DOC_Precip <- 2 #g/m3: DOC concentration in precipitation

Aoc_year <- 1 #g/m/yr: aerial loading factor
Woc_year <- 1 #g/m/yr: adjacent wetland loading factor

#############Break for function




######LOOP START##########
for (i in 1:(EndDay-StartDay)){
  
  # Flow Data: Currently constant but could be dynamic
  InflowData$Q_in[i] <- 5 #m3/s: total inflow (Sw + Gw)
  
  # Aerial DOC (g/d)
  Aoc = Aoc_year/365 #g/m/d: aerial loading factor
  InflowData$DOC_Aerial[i] <- (PC*Aoc*lakePerim) + ((1-PC) *0.2* Aoc * lakePerim)
  
  # Wetland DOC (g/d)
  Woc = Woc_year/365 #g/m/d: adjacent wetland loading factor
  InflowData$DOC_Wetland[i] <- PW * Woc * lakePerim
  
  # Gw DOC (g/d)
  InflowData$DOC_GW[i] <- DOC_GW * (prop_GW * InflowData$Q_in[i]) * 86400 #g/d
  
  # Sw DOC (g/d)
  InflowData$DOC_SW[i] <- DOC_SW * ((1-prop_GW)*InflowData$Q_in[i]) * 86400 #g/d
  
  # Precipitation DOC (g/d)
  Rainfall <- 2 #mm/d: precipitation input from meteorological timeseries(?)
  InflowData$DailyRain[i] <- (Rainfall * 0.001) * lakeArea #m3/d: daily precip.
  InflowData$DOC_Precip[i] <- DOC_Precip * InflowData$DailyRain[i] #g/d
  
  # MAIN OUTPUTS: LOAD DOC & POC (g/d)
  InflowData$Load_DOC[i] <- InflowData$DOC_Aerial[i] + InflowData$DOC_Wetland[i] + InflowData$DOC_GW[i] + InflowData$DOC_SW[i] + InflowData$DOC_Precip[i] # g/d DOC
  InflowData$Load_POC[i] <- InflowData$Load_DOC[i] * 0.1 # g/d POC roughly estimated as (0.1 * DOC)
  
}