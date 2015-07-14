#POC Sedimentation Sub-Routine for SOS Carbon Flux Code
#Last Update: 6/20/2015, DR

#Inputs from main program
lakeDepth <- 25 #m
lakePerim <- 1000 #m
lakeArea <- 62500 #m^2
lakeVol <- lakeDepth*lakeArea #m^3
resTime <-  3 #days
g <- 9.81 #m/s^2
StartDay <- 1  #Julian day
EndDay <- 10 #Julian day
rho_H2O <- 999.9720 #kg/m^3, 

TimeStep <- 1 #days
TimeStepConversion <- 1/TimeStep #days/days
steps <- (EndDay-StartDay+1)/TimeStep

#Setup data frame for sedimentation process
SedData <- data.frame(BurialScalingFactor=numeric(steps),MAR_oc=numeric(steps),LeafLitter=numeric(steps),
                      LeafParameter=numeric(steps),DOC_SWGW=numeric(steps),POC_SWGW=numeric(steps),
                      POC_in_alloch=numeric(steps),POC_in_autoch=numeric(steps),POC_in=numeric(steps),
                      POC_burial=numeric(steps),POC_grazed_DIC=numeric(steps),POC_conc=numeric(steps))

#Input Constants - Lake Specific
DOC_avg <- 0.01   #g/L or kg/m3 Average DOC value in lake to initialize estimate of average POC
MAR_sed_avg <- 1000 #Mass accumulation rate of sediment (g sed/m^2/yr) ##REQUIRES DATA##
Sed_oc_avg <-  4.5 #Percent of sediment estimated to be OC (%) ##REQUIRES DATA##


#############Break for function
POC_conc_avg <- 0.1*DOC_avg  #Average POC concentration in water column (g/L or kg/m3) ##HOW DO WE DETERMINE THIS PRE-MODEL-RUN?##
  
#Initialize POC concentration as baseline average
SedData$POC_conc[1] <- POC_conc_avg
  
######LOOP START##########
for (i in 1:(EndDay-StartDay)){
  
  #BURIAL (OUT) 
  SedData$BurialScalingFactor[i] <-  SedData$POC_conc[i]/POC_conc_avg  #current POC divided by average POC to scale accumulation rate
  #using varied POC inputs (alloch and autoch) -- (unitless)
  
  SedData$MAR_oc[i] <- MAR_sed_avg*(Sed_oc_avg/100)*SedData$BurialScalingFactor[i] #g OC/m^2/yr
  
  
  #Allochthonous POC inputs
  SedData$LeafParameter[i] <- 0.01 #g/m of shoreline/d   #Need more research for dynamic input
  SedData$LeafLitter[i] <- lakePerim*SedData$LeafParameter[i] #g/d

  SedData$DOC_SWGW[i] <-  10000   #g/d Input from SW/GW subroutine.
  SedData$POC_SWGW[i] <- SedData$DOC_SWGW[i]*0.1  #g/d POC roughly estimated as 0.1*DOC.

  SedData$POC_in_alloch[i] <- SedData$LeafLitter[i] + SedData$POC_SWGW[i]  #g/d Total allochthonous inputs from the air and SW and GW.
  
  #Autochthonous POC inputs
  NPP <-  200 #g/d Input from GPP subroutine
  #Included in GPP code -- GPP_to_DIC[i] <-  # = f(GPP) #Mineralization of autotrorophic products to DIC
  #Include this? NPP_to_DOC[i] <-  # = f(GPP)? #Extracellular release of DC
  SedData$POC_in_autoch[i] <- NPP  ##g/d
  
  #Total POC input
  SedData$POC_in[i] <- SedData$POC_in_alloch[i] + SedData$POC_in_autoch[i] #g/d
   
  ##OUTPUTS############################
  SedData$POC_burial[i] <- SedData$MAR_oc[i]*(1/365)*lakeArea #g/d; Timestep with conversion from years to timestep units - days)
  SedData$POC_grazed_DIC[i] <- SedData$POC_conc[i]*0.105*lakeVol/TimeStepConversion #g (Consumption, respiration, etc. See Connolly and COffin 1995)
  #Updated POC concentration in water column. Assume 0.105 consumed per day (thus TimeStepConversion)
  SedData$POC_conc[i+1] <-  SedData$POC_conc[i] - (SedData$POC_grazed_DIC[i])/(lakeVol*1000) + (SedData$POC_in[i] - SedData$POC_burial[i])*TimeStep/(lakeVol*1000)  #g/L
}


