# DOC/POC Surface & Groundwater Sub-Routine for SOS Carbon Flux Code
# Last Update: 7/22/2015, KF

#Inputs from main program
StartDay <- 1  #Julian day
EndDay <- 1 #Julian day

TimeStep <- 1 #days
TimeStepConversion <- 1/TimeStep #days/days
steps <- (EndDay-StartDay+1)/TimeStep

#Setup data frame for SwGw function
InflowData <- data.frame(Q_in=numeric(steps), DOC_SW=numeric(steps), DOC_GW=numeric(steps), DOC_SWGW=numeric(steps),POC_SWGW=numeric(steps))

#Inputs- Lake Specific
prop_GW <- .3 # Unitless: proportion of Q_in that is from groundwater
DOC_GW <- 10 # g/m3: DOC concentration in groundwater. 2-40 g/m3 per Hanson et al 2014
DOC_SW <- 1 # g/m3: DOC concentration in surface water

#############Break for function




######LOOP START##########
for (i in 1:(EndDay-StartDay)){
  
  # Flow Data: Currently constant but could be dynamic
  InflowData$Q_in[i] <- 10 # m3/s: total inflow (Sw + Gw)
  
  # DOC_SwGw == (Gw + Sw) * time_conv
  InflowData$DOC_SW[i] <- InflowData$Q_in[i] * (1-prop_GW) * DOC_SW * 86400 #g/d
  InflowData$DOC_GW[i] <- InflowData$Q_in[i] * prop_GW * DOC_GW * 86400 #g/d
 
  InflowData$DOC_SWGW[i] <- (InflowData$DOC_SW[i] + InflowData$DOC_GW[i]) # DOC inflow g/d
  
  # POC_SwGw
  InflowData$POC_SWGW[i] <- InflowData$DOC_SWGW[i] * 0.1  #g/d POC 
  # roughly estimated as 0.1 * DOC
}