
##### LOAD PACKAGES ########################
library(LakeMetabolizer)
library(signal)
library(zoo)
############################################

##### LOAD FUNCTIONS #######################
source("SedimentationFunctionDR.R")
source("SwGwComplexFunctionDR.R")
source("NPP_08182015.R")
############################################

##### READ MAIN INPUT FILE #################
RawData <- read.csv("TestData1.csv",header=T) #Read main data file with GLM outputs (physical input) and NPP input
RawData$datetime <- as.POSIXct(strptime(RawData$datetime,"%m/%d/%Y %H:%M")) #Convert time to POSIX

#Fill time-series gaps
ts_new <- data.frame(datetime = seq(RawData$datetime[1],RawData$datetime[nrow(RawData)],by="day")) #Interpolate gapless time-series
InputData <- merge(RawData,ts_new,all=T)
InputData <- as.data.frame(InputData)
for (col in 2:ncol(InputData)){
  InputData[,col] <- na.approx(InputData[,col])}
############################################

##### General Lake Inputs and Parameters ###
lakePerim <- 1000 #m
Days2Seconds <- 1*24*60*60 #s/d
DOC_conc_init <- 2.9  #g/m3
############################################

###### Run Period and Time Step Setup #####
TimeStep <- as.numeric(InputData$datetime[2]-InputData$datetime[1]) #days
steps <- nrow(InputData)
############################################


##### Sub-Topic Inputs: Sedimentation ######
DOC_avg <- DOC_conc_init   #g/m3  Average DOC value in lake to initialize estimate of average POC
POC_conc_avg <- 0.1*DOC_avg  #Average POC concentration in water column (g/m^3) ##HOW DO WE DETERMINE THIS PRE-MODEL-RUN?##
MAR_sed_avg <- 72 #Mass accumulation rate of sediment (g sed/m^2/yr) ##REQUIRES DATA##
Sed_oc_avg <-  4.5 #Percent of sediment estimated to be OC (%) ##REQUIRES DATA##
############################################

##### Sub-Topic Inputs: GPP/NPP ############
#Currently all inputs are time-series data in main input file
############################################

##### Sub-Topic Inputs: sw/GW ##############
PC <- 0.76 #unitless: proportion of lake shore with canopy
PW <- 0.0 #unitless: proportion of lake shore with wetlands

Aoc_year <- 1 #g/m/yr: aerial loading factor
Woc_year <- 1 #g/m/yr: adjacent wetland loading factor

prop_GW <- 0 # Unitless: proportion of Q_in that is from groundwater
DOC_GW <- 10 # g/m3: DOC concentration in groundwater. 2-40 g/m3 per Hanson et al 2014
DOC_SW <- 5.8 # g/m3: DOC concentration in surface water
DOC_Precip <- 2 #g/m3: DOC concentration in precipitation
############################################

##### Declare Output Data Storage ##########
POC_conc <- data.frame(numeric(steps))
DOC_conc <- data.frame(numeric(steps))
############################################

##### Declare Data Storage - Sed ###########
SedData <- data.frame(BurialScalingFactor=numeric(steps),MAR_oc=numeric(steps),
                      POC_burial=numeric(steps),POC_to_DIC=numeric(steps))
POC_sed_out <- data.frame(numeric(steps))
############################################

##### Declare Data Storage - GPP ###########
NPPoutput <- data.frame(NPP=numeric(steps),NPP_mass=numeric(steps))
############################################

##### Declare Data Storage - SW/GW #########
SWGWData = data.frame(DOC_Aerial=numeric(steps), DOC_Wetland=numeric(steps), 
                        DOC_GW=numeric(steps), DOC_SW=numeric(steps), DailyRain=numeric(steps), 
                        DOC_Precip=numeric(steps), Load_DOC=numeric(steps), Load_POC=numeric(steps))
SWGW_mass_in <- data.frame(POC=numeric(steps),DOC=numeric(steps))
POC_outflow <- data.frame(numeric(steps))
DOC_outflow <- data.frame(numeric(steps))
############################################

##### General Lake Variable Initialization ###############
POC_conc[1,1] <- POC_conc_avg # #Initialize POC concentration as baseline average
DOC_conc[1,1] <- DOC_conc_init #Initialize DOC concentration g/m3
############################################

####################### MAIN PROGRAM #############################################
##################################################################################

for (i in 1:(steps)){
  
  lakeDepth <- InputData$LakeLevel[i] #m
  lakeArea <- InputData$SurfaceArea[i] #m^2
  lakeVol <- InputData$Volume[i] #m^3
  Q_in <- InputData$TotInflow[i] #m3/s
  Q_out <- InputData$TotOutflow[i] #m3/s: total outflow. Assume steady state pending dynamic output
  Rainfall <- InputData$Rain[i]
  
  #Call NPP Function
  NPPoutput$NPP[i] <- NPP(InputData$Chla[i],InputData$TP[i],InputData$SurfaceTemp[i]) #mg C/m^2/d
  NPPoutput$NPP_mass[i] <- NPPoutput$NPP[i]*lakeArea*TimeStep*1000 #g

  #Call SWGW Function
  SWGWoutput <- SWGWFunction(Q_in,Rainfall,Aoc_year, PC, lakePerim, Woc_year, PW, DOC_GW, prop_GW, 
                             DOC_SW, lakeArea) #change these inputs to iterative [i] values when inputs are dynamic
  SWGWData[i,1:8] <- SWGWoutput
  
  #Calculate load from SWGW_in
  SWGW_mass_in$DOC[i] <- SWGWData$Load_DOC[i]*TimeStep #g
  SWGW_mass_in$POC[i] <- SWGWData$Load_POC[i]*TimeStep #g
  
  #Call Sedimentation Function
  SedOutput <- SedimentationFunction(TimeStep,lakeArea,lakeVol,DOC_avg,MAR_sed_avg,Sed_oc_avg,POC_conc[i,1])
  SedData[i,1:4] = SedOutput
  POC_sed_out[i,1] <- SedData$POC_burial[i]*TimeStep + SedData$POC_to_DIC[i] #g
  
  #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
  POC_outflow[i,1] <- POC_conc[i,1]*Q_out*60*60*24*TimeStep #g
  DOC_outflow[i,1] <- DOC_conc[i,1]*Q_out*60*60*24*TimeStep #g
  
  
  #Update POC and DOC concentration values for whole lake
  POC_conc[i+1,1] <-  POC_conc[i,1] + ((NPPoutput$NPP_mass[i] + SWGW_mass_in$POC[i] - POC_outflow[i,1] - POC_sed_out[i,1])/lakeVol) #g/m3
  DOC_conc[i+1,1] <-  DOC_conc[i,1] + ((SWGW_mass_in$DOC[i] - DOC_outflow[i,1])/lakeVol) #g/m3
  
}

#######################################################################################
#######################################################################################

plot(InputData$datetime,NPPoutput$NPP_mass,type="l")
plot(InputData$datetime,SWGW_mass_in$POC,type="l")
plot(InputData$datetime,POC_outflow[,1],type="l")
plot(InputData$datetime,POC_sed_out[,1],type="l")
plot(InputData$datetime,SWGW_mass_in$DOC,type="l")
plot(InputData$datetime,DOC_outflow[,1],type="l")





