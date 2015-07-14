

##### General Lake Inputs ##################
lakeDepth <- 25 #m
lakePerim <- 1000 #m
lakeArea <- 62500 #m^2
lakeVol <- lakeDepth*lakeArea #m^3
resTime <-  3 #days
g <- 9.81 #m/s^2
rho_H2O <- 999.9720 #kg/m^3, 
############################################

###### Run Period and Time Step Inputs #####
TimeStep <- 1 #days
StartDay <- 1  #Julian day
EndDay <- 10 #Julian day
TimeStepConversion <- 1/TimeStep #days/days
steps <- (EndDay-StartDay+1)/TimeStep
############################################


##### Sub-Topic Inputs: Sedimentation ######
DOC_avg <- 0.01   #g/L or kg/m3 Average DOC value in lake to initialize estimate of average POC
MAR_sed_avg <- 1000 #Mass accumulation rate of sediment (g sed/m^2/yr) ##REQUIRES DATA##
Sed_oc_avg <-  4.5 #Percent of sediment estimated to be OC (%) ##REQUIRES DATA##
############################################

##### Sub-Topic Inputs: GPP ################

############################################

##### Sub-Topic Inputs: sw/GW ##############

############################################


##### Declare Data Storage - Sed ###########
SedData <- data.frame(BurialScalingFactor=numeric(steps),MAR_oc=numeric(steps),LeafLitter=numeric(steps),
                      LeafParameter=numeric(steps),DOC_SWGW=numeric(steps),POC_SWGW=numeric(steps),
                      POC_in_alloch=numeric(steps),POC_in_autoch=numeric(steps),POC_in=numeric(steps),
                      POC_burial=numeric(steps),POC_grazed_DIC=numeric(steps),POC_conc=numeric(steps))
POC_conc <- data.frame(numeric(steps))
############################################

##### Declare Data Storage - GPP ###########

############################################

##### Declare Data Storage - SW/GW #########

############################################

#####Variable Initialization - Sed #########
POC_conc_avg <- 0.1*DOC_avg  #Average POC concentration in water column (g/L or kg/m3) ##HOW DO WE DETERMINE THIS PRE-MODEL-RUN?##
POC_conc[1,1] = POC_conc_avg #SedData$POC_conc[1] <- POC_conc_avg #Initialize POC concentration as baseline average
############################################

####################### MAIN PROGRAM LOOP #############################################
#######################################################################################
for (i in 1:(EndDay-StartDay)){
  NPP<-200 #g/d        #TESTING PURPOSES ONLY!
  DOC_SWGW<-10000 #g/d #TESTING PURPOSES ONLY!
  
  #Call SWGW
  
  #Call GPP
  
  #Call Sed Function
  SedOutput <- SedimentationFunction(TimeStepConversion,lAkePerim,lakeArea,lakeVol,DOC_avg,MAR_sed_avg,Sed_oc_avg,NPP,DOC_SWGW,POC_conc[i,1])
  SedData[i,1:11] = SedOutput
  #Update POC concentration in water column. 
  POC_conc[i+1,1] <-  POC_conc[i,1] - (output$POC_grazed_DIC)/(lakeVol*1000) + (output$POC_in - output$POC_burial)*TimeStep/(lakeVol*1000)  #g/L
}
#######################################################################################
#######################################################################################

##### Combine Output Data
SedData$POC_conc = POC_conc[,1]
