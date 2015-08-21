##### USER INPUT FILE NAMES ################
TimeSeriesFile <- "./TroutLake/TestDataTrout.csv"
ParameterFile <- "./TroutLake/ParameterInputsTrout.txt"
############################################

##### LOAD PACKAGES ########################
library(signal)
library(zoo)
############################################

##### LOAD FUNCTIONS #######################
source("./R/Model/SOS_Sedimentation.R")
source("./R/Model/SOS_SWGW.R")
source("./R/Model/SOS_NPP.R")
############################################

##### READ MAIN INPUT FILE #################
RawData <- read.csv(TimeSeriesFile,header=T) #Read main data file with GLM outputs (physical input) and NPP input
RawData$datetime <- as.POSIXct(strptime(RawData$datetime,"%m/%d/%Y %H:%M")) #Convert time to POSIX
#Fill time-series gaps (linear interpolation)
ts_new <- data.frame(datetime = seq(RawData$datetime[1],RawData$datetime[nrow(RawData)],by="day")) #Interpolate gapless time-series
InputData <- merge(RawData,ts_new,all=T)
InputData <- as.data.frame(InputData)
for (col in 2:ncol(InputData)){
  InputData[,col] <- na.approx(InputData[,col])}
############################################

##### READ PARAMETER FILE ##################
parameters <- read.table(file = ParameterFile,row.names=1,header=TRUE,comment.char="#")
############################################

##### General Lake Inputs and Parameters ###
lakePerim <- parameters[row.names(parameters)=="LakePerimeter",1] #m
lakeDepth <- parameters[row.names(parameters)=="LakeDepth",1] #m
lakeArea <- parameters[row.names(parameters)=="LakeArea",1] #m^2
lakeVol <- parameters[row.names(parameters)=="LakeVolume",1] #m^3
DOC_conc_init <- parameters[row.names(parameters)=="DOC_init",1]  #g/m3
POC_conc_init <- parameters[row.names(parameters)=="POC_init",1]  #Average POC concentration in water column (g/m^3)
############################################

###### Run Period and Time Step Setup #####
TimeStep <- as.numeric(InputData$datetime[2]-InputData$datetime[1]) #days
steps <- nrow(InputData)
############################################

##### Sub-Topic Inputs: Sedimentation ######
BurialFactor <- parameters[row.names(parameters)=="BurialFactor",1] #(1/days) Parameter estimation of OC burial in sediments
############################################

##### Sub-Topic Inputs: GPP/NPP ############
#Currently all inputs are time-series data in main input file
############################################

##### Sub-Topic Inputs: sw/GW ##############
PC <- parameters[row.names(parameters)=="PropCanopy",1] #unitless: proportion of lake shore with canopy
PW <- parameters[row.names(parameters)=="PropWetlands",1] #unitless: proportion of lake shore with wetlands

Aoc_year <- parameters[row.names(parameters)=="AerialLoad",1] #g/m/yr: aerial loading factor
Woc_year <- parameters[row.names(parameters)=="WetlandLoad",1] #g/m/yr: adjacent wetland loading factor

prop_GW <- parameters[row.names(parameters)=="PropGW",1] # Unitless: proportion of Q_in that is from groundwater
DOC_GW <- parameters[row.names(parameters)=="DOC_gw",1] # g/m3: DOC concentration in groundwater. 2-40 g/m3 per Hanson et al 2014
DOC_SW <- parameters[row.names(parameters)=="DOC_sw",1] # g/m3: DOC concentration in surface water
DOC_Precip <- parameters[row.names(parameters)=="DOC_precip",1] #g/m3: DOC concentration in precipitation
############################################

##### Declare Output Data Storage ##########
POC_conc <- data.frame(numeric(steps)) #Record running g/m3 POC concentration of mixed lake
DOC_conc <- data.frame(numeric(steps)) #Record running g/m3 DOC concentration of mixed lake
POC_flux <- data.frame(NPP_in=numeric(steps),Flow_in=numeric(steps),Flow_out=numeric(steps),Sed_out=numeric(steps),MinResp_out=numeric(steps)) #Record POC flux (g/m2/yr) at each time step
DOC_flux <- data.frame(Flow_in=numeric(steps),Flow_out=numeric(steps)) #Record DOC flux (g/m2/yr) at each time step
DOC_load <- data.frame(total=numeric(steps),alloch=numeric(steps),autoch=numeric(steps)) #Record DOC load (g) at each time step
POC_load <- data.frame(total=numeric(steps),alloch=numeric(steps),autoch=numeric(steps)) #Record POC load (g) at each time step
DOC_out <- data.frame(total=numeric(steps)) #Record DOC removal (g) from system at each time step
POC_out <- data.frame(total=numeric(steps)) #Record POC removal (g) from system at each time step
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

##### Carbon Concentration Initialization ################
POC_conc[1,1] <- POC_conc_init # #Initialize POC concentration as baseline average
DOC_conc[1,1] <- DOC_conc_init #Initialize DOC concentration g/m3
##########################################################

####################### MAIN PROGRAM #############################################
##################################################################################

for (i in 1:(steps)){
  
  Q_sw <- InputData$TotInflow[i] #m3/s
  Q_gw <- Q_sw/(1-prop_GW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
  Q_out <- InputData$TotOutflow[i] #m3/s: total outflow. Assume steady state pending dynamic output
  Rainfall <- InputData$Rain[i]/TimeStep #mm/day
  
  #Call NPP Function
  NPPoutput$NPP[i] <- NPP(InputData$Chla[i],InputData$TP[i],InputData$SurfaceTemp[i]) #mg C/m^2/d
  NPPoutput$NPP_mass[i] <- NPPoutput$NPP[i]*lakeArea*TimeStep/1000 #g

  #Call SWGW Function
  SWGWoutput <- SWGWFunction(Q_sw,Q_gw,Rainfall,Aoc_year, PC, lakePerim, Woc_year, PW, DOC_GW, prop_GW, 
                             DOC_SW, lakeArea) #change these inputs to iterative [i] values when inputs are dynamic
  SWGWData[i,1:8] <- SWGWoutput
  
  #Calculate load from SWGW_in
  SWGW_mass_in$DOC[i] <- SWGWData$Load_DOC[i]*TimeStep #g
  SWGW_mass_in$POC[i] <- SWGWData$Load_POC[i]*TimeStep #g
  
  #Call Sedimentation Function
  POC_mass <- POC_conc[i,1]*lakeVol
  SedOutput <- SedimentationFunction(BurialFactor,TimeStep,POC_mass)
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
  
  #Update POC and DOC concentration values (g/m3) for whole lake
  POC_conc[i+1,1] <-  POC_conc[i,1] + ((NPPoutput$NPP_mass[i] + SWGW_mass_in$POC[i] - POC_outflow[i,1] - POC_sed_out[i,1] - SedData$POC_to_DIC[i])/lakeVol) #g/m3
  DOC_conc[i+1,1] <-  DOC_conc[i,1] + ((SWGW_mass_in$DOC[i] - DOC_outflow[i,1])/lakeVol) #g/m3
  
  #POC and DOC load (in) and fate (out) (g)
  POC_load$total[i] <- NPPoutput$NPP_mass[i] + SWGW_mass_in$POC[i] #g 
  POC_load$alloch[i] <- SWGW_mass_in$POC[i] #g
  POC_load$autoch[i] <- NPPoutput$NPP_mass[i] #g
  DOC_load$total[i] <- SWGW_mass_in$DOC[i] #g
  DOC_load$alloch[i] <- SWGW_mass_in$DOC[i] #g
  DOC_load$autoch[i] <- 0 #g
  POC_out$total[i] <- POC_outflow[i,1] + POC_sed_out[i,1] + SedData$POC_to_DIC[i] #g
  DOC_out$total[i] <- DOC_outflow[i,1] #g
  
  #Stop code and output error if concentrations go to negative
  if (POC_conc[i+1,1]<=0){stop("Negative POC concentration!")}
  if (DOC_conc[i+1,1]<=0){stop("Negative DOC concentration!")}
}

#Total carbon mass additions
TotalPOCAllochIn <- sum(POC_load$alloch) #g
TotalPOCAutochIn <- sum(POC_load$autoch) #g
TotalDOCAllochIn <- sum(DOC_load$alloch) #g
TotalDOCAutochIn <- sum(DOC_load$autoch) #g
#Total carbon mass subtractions
TotalPOCout <- sum(POC_out$total)
TotalDOCout <- sum(DOC_out$total)
#Change to total carbon stocks
DeltaPOC <- POC_conc[steps+1,1]*lakeVol - POC_conc[1,1]*lakeVol
DeltaDOC <- DOC_conc[steps+1,1]*lakeVol - DOC_conc[1,1]*lakeVol
#Mass balance check (should be near zero)
POCcheck <- (TotalPOCAllochIn + TotalPOCAutochIn - TotalPOCout) - DeltaPOC
DOCcheck <- (TotalDOCAllochIn + TotalDOCAutochIn - TotalDOCout) - DeltaDOC
#Return mass balance checks
print(c("POC Balance:",POCcheck))
print(c("DOC Balance:",DOCcheck))

######################## END MAIN PROGRAM #############################################
#######################################################################################

#Define plotting time-series
ConcOutputTimeSeries <- c(InputData$datetime,InputData$datetime[length(InputData$datetime)]+86400)
OutputTimeSeries <- InputData$datetime

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






