
##### LOAD PACKAGES ########################
library(signal)
library(zoo)
############################################

##### LOAD FUNCTIONS #######################
source("SedimentationFunctionDR.R")
source("SwGwComplexFunctionDR.R")
source("NPP_08182015.R")
############################################

##### READ MAIN INPUT FILE #################
RawData <- read.csv("TestData3.csv",header=T) #Read main data file with GLM outputs (physical input) and NPP input
RawData$datetime <- as.POSIXct(strptime(RawData$datetime,"%m/%d/%Y %H:%M")) #Convert time to POSIX

#Fill time-series gaps (linear interpolation)
ts_new <- data.frame(datetime = seq(RawData$datetime[1],RawData$datetime[nrow(RawData)],by="day")) #Interpolate gapless time-series
InputData <- merge(RawData,ts_new,all=T)
InputData <- as.data.frame(InputData)
for (col in 2:ncol(InputData)){
  InputData[,col] <- na.approx(InputData[,col])}
############################################

##### General Lake Inputs and Parameters ###
lakePerim <- 32448 #m
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
BurialFactor <- 0.01 #(1/days) Parameter estimation of OC burial in sediments
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
POC_flux <- data.frame(NPP_in=numeric(steps),Flow_in=numeric(steps),Flow_out=numeric(steps),Sed_out=numeric(steps),MinResp_out=numeric(steps))
DOC_flux <- data.frame(Flow_in=numeric(steps),Flow_out=numeric(steps))
DOC_load <- data.frame(mass=numeric(steps),alloch=numeric(steps),autoch=numeric(steps))
POC_load <- data.frame(mass=numeric(steps),alloch=numeric(steps),autoch=numeric(steps))
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
  NPPoutput$NPP_mass[i] <- NPPoutput$NPP[i]*lakeArea*TimeStep/1000 #g

  #Call SWGW Function
  SWGWoutput <- SWGWFunction(Q_in,Rainfall,Aoc_year, PC, lakePerim, Woc_year, PW, DOC_GW, prop_GW, 
                             DOC_SW, lakeArea) #change these inputs to iterative [i] values when inputs are dynamic
  SWGWData[i,1:8] <- SWGWoutput
  
  #Calculate load from SWGW_in
  SWGW_mass_in$DOC[i] <- SWGWData$Load_DOC[i]*TimeStep #g
  SWGW_mass_in$POC[i] <- SWGWData$Load_POC[i]*TimeStep #g
  
  #Call Sedimentation Function
  POC_mass <- POC_conc[i,1]*lakeVol
  SedOutput <- SedimentationFunction(BurialFactor,TimeStep,lakeArea,lakeVol,DOC_avg,MAR_sed_avg,Sed_oc_avg,POC_mass)
  SedData[i,1:4] = SedOutput
  POC_sed_out[i,1] <- SedData$POC_burial[i] #g
  
  #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
  POC_outflow[i,1] <- POC_conc[i,1]*Q_out*60*60*24*TimeStep #g
  DOC_outflow[i,1] <- DOC_conc[i,1]*Q_out*60*60*24*TimeStep #g
  
  #Store POC and DOC fluxes as mass/area/time (g/m2/yr)
  POC_flux$NPP_in[i] <- NPPoutput$NPP_mass[i]/lakeArea/(TimeStep/365)
  POC_flux$Flow_in[i] <- SWGW_mass_in$POC[i]/lakeArea/(TimeStep/365)
  POC_flux$Flow_out[i] <- POC_outflow[i,1]/lakeArea/(TimeStep/365)
  POC_flux$Sed_out[i] <- POC_sed_out[i,1]/lakeArea/(TimeStep/365)
  POC_flux$MinResp_out[i] <- SedData$POC_to_DIC[i]/lakeArea/(TimeStep/365)
  
  DOC_flux$Flow_in[i] <- SWGW_mass_in$DOC[i]/lakeArea/(TimeStep/365)
  DOC_flux$Flow_out[i] <- DOC_outflow[i,1]/lakeArea/(TimeStep/365)                    
  
  #Update POC and DOC concentration values for whole lake
  POC_conc[i+1,1] <-  POC_conc[i,1] + ((NPPoutput$NPP_mass[i] + SWGW_mass_in$POC[i] - POC_outflow[i,1] - POC_sed_out[i,1] - SedData$POC_to_DIC[i])/lakeVol) #g/m3
  DOC_conc[i+1,1] <-  DOC_conc[i,1] + ((SWGW_mass_in$DOC[i] - DOC_outflow[i,1])/lakeVol) #g/m3
  
  #POC and DOC load output
  #POC_load$mass[i] <- NPPoutput$NPP_mass[i] + SWGW_mass_in$POC[i] #- POC_outflow[i,1] - POC_sed_out[i,1] - SedData$POC_to_DIC[i] #g
  #POC_load$alloch[i] <- SWGW_mass_in$POC[i]
  #POC_load$autoch[i] <- NPPoutput$NPP_mass[i]
  #DOC_load$mass[i] <- SWGW_mass_in$DOC[i] - DOC_outflow[i,1] #g
  #DOC_load$alloch[i] <- SWGW_mass_in$DOC[i]
  #DOC_load$autoch[i] <- 
  
  #Stop code and output error if concentrations go to negative
  if (POC_conc[i+1,1]<=0){stop("Negative POC concentration!")}
  if (DOC_conc[i+1,1]<=0){stop("Negative DOC concentration!")}
}

ConcOutputTimeSeries <- c(InputData$datetime,InputData$datetime[length(InputData$datetime)]+86400)
OutputTimeSeries <- InputData$datetime

#######################################################################################
#######################################################################################

#Plot POC and DOC fluxes in standardized units (g/m2/yr)
xlabel <- "Date/Time"
ylabelPOC <- c("NPP POC In (g/m2/yr)","Flow POC In (g/m2/yr)","Flow POC Out (g/m2/yr)","Sed POC Out (g/m2/yr)","Min/Resp POC Out (g/m2/yr)")
ylabelDOC <- c("Flow DOC In (g/m2/yr)","Flow  DOC Out (g/m2/yr)")

for (n in 1:ncol(POC_flux)){
  plot(OutputTimeSeries,POC_flux[,n],xlab=xlabel,ylab=ylabelPOC[n],type='l')
}

for (n in 1:ncol(DOC_flux)){
  plot(OutputTimeSeries,DOC_flux[,n],xlab=xlabel,ylab=ylabelDOC[n],type='l')
}

#POC and DOC concentration in time (g/m3)
plot(ConcOutputTimeSeries,POC_conc[,1],xlab=xlabel,ylab="POC Conc (g/m3)",type="l")
plot(ConcOutputTimeSeries,DOC_conc[,1],xlab=xlabel,ylab="DOC Conc (g/m3)",type="l")






