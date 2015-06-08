#POC Sedimentation Sub-Routine for SOS Carbon Flux Code
#Last Update: 5/13/2015, DR

#Inputs from main program
lakeDepth <- 25 #m
lakePerim <- 1000 #m
lakeArea <- 62500 #m^2
lakeVol <- lakeDepth*lakeArea #m^3
resTime <-  3 #days
g <- 9.81 #m/s^2
StartDay <- 1  #Julian day
EndDay <- 365 #Julian day

TimeStep <- 1 #days
TimeStepConversion <- 1/TimeStep #days/days

#Inputs from other sub-process routines
#NPP[i]  #Accounts for respiration subtraction from GPP

#Constants
rho_POC <- 1050   #kg/m^3, this value is roughly estimated ##FIND IT!
rho_H2O <- 999.9720 #kg/m^3, 

#Burial rate algorithms
  POC_conc_avg <- 0.001  #Average POC concentration (g/L) ##HOW DO WE DETERMINE THIS PRE-MODEL-RUN?##
  MAR_sed_avg <- 50 #Mass accumulation rate of sediment (g sed/m^2/yr) ##REQUIRES DATA##
  Sed_oc_avg <-  50 #Percent of sediment estimated to be OC (%) ##REQUIRES DATA##
  
  #Initialize POC concentration as baseline average
  POC_conc[StartDay] <- POC_conc_avg
  
  #BURIAL (OUT) 
  BurialScalingFactor[i] <-  POC_conc[i]/POC_conc_avg  #current POC divided by average POC to scale accumulation rate
  #using varied POC inputs (alloch and autoch) -- (unitless)
  
  MAR_oc[i] <- MAR_sed_avg*(Sed_oc_avg/100)*BurialScalingFactor[i] #g OC/m^2/yr
  
  
  #Allochthonous POC inputs
  LeafParameter <-  0.3 #g/m/d   #Need more research for dynamic input
  LeafLitter[i] <- (lakePerim/lakeArea)*LeafParameter[i] #g/m^2/d
  POC_in_alloch[i] <- LeafLitter[i]*lakeArea #+ other potential inputs #g/d
  
  #Autochthonous POC inputs
  NPP[i] <-  #g/d Input from GPP subroutine
  #Included in GPP code -- GPP_to_DIC[i] <-  # = f(GPP) #Mineralization of autotrorophic products to DIC
  #Include this? NPP_to_DOC[i] <-  # = f(GPP)? #Extracellular release of DC
  POC_in_autoch[i] <- NPP[i]  ##g/d
  
  #Total POC input
  POC_in[i] <- POC_in_alloch[i] + POC_in_autoch[i] #g/d
   
  ##OUTPUTS############################
  POC_burial[i] <- MAR_oc[i]*(365/1)*lakeArea #g/d Timestep with conversion from years to timestep units - days)
  POC_grazed_DIC[i] <- POC_conc[i]*0.105*lakeVol/TimeStepConversion #g (Consumption, respiration, etc. See Connolly and COffin 1995)
  #Updated POC concentration in water column
  POC_conc[i+1] <-  POC_conc[i] - (POC_grazed_DIC[i])/lakeVol + (POC_in[i] - POC_burial[i])*TimeStep/(lakeVol/1000)  #g/L



