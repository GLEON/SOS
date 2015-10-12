##### USER INPUT FILE NAMES ################
TimeSeriesFile <- "./TroutLake/TestDataTrout.csv"
ParameterFile <- "./TroutLake/ParameterInputsTrout.txt"
############################################

##### LOAD PACKAGES ########################
library(signal)
library(zoo)
library(lubridate)
############################################

##### LOAD FUNCTIONS #######################
source("./R/Model/SOS_Sedimentation.R")
source("./R/Model/SOS_SWGW.R")
source("./R/Model/SOS_GPP.R")
source("./R/Model/SOS_Resp.R")
############################################

##### READ MAIN INPUT FILE #################
RawData <- read.csv(TimeSeriesFile,header=T) #Read main data file with GLM outputs (physical input) and GPP input
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

##### Sub-Topic Parameters: Sedimentation ######
BurialFactor <- parameters[row.names(parameters)=="BurialFactor",1] #(1/days) Parameter estimation of OC burial in sediments
############################################

##### Sub-Topic Parameters: GPP ############
#Currently built into GPP sub code.
############################################

##### Sub-Topic Parameters: sw/GW ##############
PC <- parameters[row.names(parameters)=="PropCanopy",1] #unitless: proportion of lake shore with canopy
PW <- parameters[row.names(parameters)=="PropWetlands",1] #unitless: proportion of lake shore with wetlands

Aoc_day <- parameters[row.names(parameters)=="AerialLoad",1] #g/m/yr: aerial loading factor
Woc_day <- parameters[row.names(parameters)=="WetlandLoad",1] #g/m/yr: adjacent wetland loading factor

prop_GW <- parameters[row.names(parameters)=="PropGW",1] # Unitless: proportion of Q_in that is from groundwater
DOC_GW <- parameters[row.names(parameters)=="DOC_gw",1] # g/m3: DOC concentration in groundwater. 2-40 g/m3 per Hanson et al 2014
DOC_SW <- parameters[row.names(parameters)=="DOC_sw",1] # g/m3: DOC concentration in surface water
DOC_Precip <- parameters[row.names(parameters)=="DOC_precip",1] #g/m3: DOC concentration in precipitation
############################################

##### Sub-Topic Parameters: Min/Resp #######
DOC_miner_const <- parameters[row.names(parameters)=="DOC_miner_const",1] #(1/days)
############################################

##### Declare Output Data Storage ##########
POC_conc <- data.frame(numeric(steps)) #Record running g/m3 POC concentration of mixed lake
DOC_conc <- data.frame(numeric(steps)) #Record running g/m3 DOC concentration of mixed lake
POC_flux <- data.frame(GPP_in=numeric(steps),Flow_in=numeric(steps),Flow_out=numeric(steps),Sed_out=numeric(steps)) #Record POC flux (g/m2/yr) at each time step
DOC_flux <- data.frame(Flow_in=numeric(steps),GPP_in=numeric(steps),Flow_out=numeric(steps),Resp_out=numeric(steps),Miner_out=numeric(steps)) #Record DOC flux (g/m2/yr) at each time step
DOC_load <- data.frame(total=numeric(steps),alloch=numeric(steps),autoch=numeric(steps)) #Record DOC load (g) at each time step
POC_load <- data.frame(total=numeric(steps),alloch=numeric(steps),autoch=numeric(steps)) #Record POC load (g) at each time step
DOC_out <- data.frame(total=numeric(steps)) #Record DOC removal (g) from system at each time step
POC_out <- data.frame(total=numeric(steps)) #Record POC removal (g) from system at each time step
############################################

##### Declare Data Storage - Sed ###########
SedData <- data.frame(BurialScalingFactor=numeric(steps),MAR_oc=numeric(steps),
                      POC_burial=numeric(steps))
POC_sed_out <- data.frame(numeric(steps))
############################################

##### Declare Data Storage - GPP ###########
GPPdata <- data.frame(DOC_rate=numeric(steps),POC_rate=numeric(steps),DOC_mass=numeric(steps),POC_mass=numeric(steps))
############################################

##### Declare Data Storage - SW/GW #########
SWGWData = data.frame(POC_Aerial=numeric(steps), POC_SW=numeric(steps), DOC_Wetland=numeric(steps), 
                        DOC_GW=numeric(steps), DOC_SW=numeric(steps), DailyRain=numeric(steps), 
                        DOC_Precip=numeric(steps), Load_DOC=numeric(steps), Load_POC=numeric(steps))
SWGW_mass_in <- data.frame(POC=numeric(steps),DOC=numeric(steps))
POC_outflow <- data.frame(numeric(steps))
DOC_outflow <- data.frame(numeric(steps))
############################################

##### Declare Data Storage - Mineralization/Respiration ###########
MineralRespData <- data.frame(DOC_miner_mass=numeric(steps),DOC_resp_mass=numeric(steps))
############################################

##### Carbon Concentration Initialization ################
POC_conc[1,1] <- POC_conc_init # #Initialize POC concentration as baseline average
DOC_conc[1,1] <- DOC_conc_init #Initialize DOC concentration g/m3
##########################################################

####################### MAIN PROGRAM #############################################
##################################################################################

for (i in 1:(steps)){
  
  Q_sw <- InputData$TotInflow[i] #m3/s surface water flowrate at i
  Q_gw <- Q_sw/(1-prop_GW) - Q_sw #m3/s; as a function of proportion of inflow that is GW
  Q_out <- InputData$TotOutflow[i] #m3/s: total outflow. Assume steady state pending dynamic output
  Rainfall <- InputData$Rain[i]/TimeStep #mm/day
  
  #Call GPP Function
  RawProduction <- GPP(InputData$Chla[i],InputData$TP[i],InputData$SurfaceTemp[i]) #mg C/m^2/d
  GPPdata[i,1:2] <- RawProduction
  GPPdata$DOC_mass[i] <- GPPdata$DOC_rate[i]*lakeArea*TimeStep/1000 #g
  GPPdata$POC_mass[i] <- GPPdata$POC_rate[i]*lakeArea*TimeStep/1000 #g

  #Call SWGW Function
  SWGW <- SWGWFunction(Q_sw,Q_gw,Rainfall,Aoc_day, PC, lakePerim, Woc_day, PW, DOC_GW, prop_GW, 
                             DOC_SW, DOC_Precip, lakeArea) #change these inputs to iterative [i] values when inputs are dynamic
  SWGWData[i,1:9] <- SWGW
  
  #Calculate load from SWGW_in
  SWGW_mass_in$DOC[i] <- SWGWData$Load_DOC[i]*TimeStep #g
  SWGW_mass_in$POC[i] <- SWGWData$Load_POC[i]*TimeStep #g
  
  #Call Sedimentation Function
  POC_mass <- POC_conc[i,1]*lakeVol
  SedOutput <- SedimentationFunction(BurialFactor,TimeStep,POC_mass)
  SedData[i,1:3] = SedOutput
  POC_sed_out[i,1] <- SedData$POC_burial[i] #g
  
  #Call respiration function
  DOC_resp_rate <- Resp(DOC_conc[i,1],InputData$Chla[i]) #g C/m3/d
  MineralRespData$DOC_resp_mass[i] <- DOC_resp_rate*lakeVol*TimeStep #g C
  
  #Calc DOC mineralization out #! Hilary and Paul's DOC mineralization klug

  MineralRespData$DOC_miner_mass[i] = DOC_conc[i,1]*lakeVol*DOC_miner_const # Current concentration multiplied by lakevolume and a mineralization constant in units of 1/d

  MinRespData$DOC_miner_mass[i] = DOC_conc[i,1]*lakeVol*DOC_miner_const #Current concentration multiplied by lakevolume and a mineralization constant in units of 1/d
  
  #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
  POC_outflow[i,1] <- POC_conc[i,1]*Q_out*60*60*24*TimeStep #g
  DOC_outflow[i,1] <- DOC_conc[i,1]*Q_out*60*60*24*TimeStep #g
  
  #Store POC and DOC fluxes as mass/area/time (g/m2/yr)
  POC_flux$GPP_in[i] <- GPPdata$POC_mass[i]/lakeArea/(TimeStep/365)
  POC_flux$Flow_in[i] <- SWGW_mass_in$POC[i]/lakeArea/(TimeStep/365)
  POC_flux$Flow_out[i] <- POC_outflow[i,1]/lakeArea/(TimeStep/365)
  POC_flux$Sed_out[i] <- POC_sed_out[i,1]/lakeArea/(TimeStep/365)
  
  DOC_flux$Flow_in[i] <- SWGW_mass_in$DOC[i]/lakeArea/(TimeStep/365)
  DOC_flux$GPP_in[i] <- GPPdata$DOC_mass[i]/lakeArea/(TimeStep/365)
  DOC_flux$Flow_out[i] <- DOC_outflow[i,1]/lakeArea/(TimeStep/365) 
  DOC_flux$Resp_out[i] <- MineralRespData$DOC_resp_mass[i]/lakeArea/(TimeStep/365) 
  DOC_flux$Miner_out[i] <- MineralRespData$DOC_miner_mass[i]/lakeArea/(TimeStep/365)  
  
  
  #Update POC and DOC concentration values (g/m3) for whole lake
  #POC_conc[i+1,1] <-  POC_conc[i,1] + ((GPPdata$GPP_mass[i] + SWGW_mass_in$POC[i] - POC_outflow[i,1] - POC_sed_out[i,1] - SedData$POC_to_DIC[i])/lakeVol) #g/m3
  POC_conc[i+1,1] <-  POC_conc[i,1] + ((GPPdata$POC_mass[i] + SWGW_mass_in$POC[i] - POC_outflow[i,1] - POC_sed_out[i,1])/lakeVol) #g/m3
  DOC_conc[i+1,1] <-  DOC_conc[i,1] + ((GPPdata$DOC_mass[i] + SWGW_mass_in$DOC[i] - DOC_outflow[i,1] - MineralRespData$DOC_resp_mass[i] - MineralRespData$DOC_miner_mass[i])/lakeVol) #g/m3
  
  #POC and DOC load (in) and fate (out) (g)
  POC_load$total[i] <- GPPdata$POC_mass[i] + SWGW_mass_in$POC[i] #g 
  POC_load$alloch[i] <- SWGW_mass_in$POC[i] #g
  POC_load$autoch[i] <- GPPdata$POC_mass[i] #g
  DOC_load$total[i] <- GPPdata$DOC_mass[i] + SWGW_mass_in$DOC[i] #g
  DOC_load$alloch[i] <- SWGW_mass_in$DOC[i] #g
  DOC_load$autoch[i] <- GPPdata$DOC_mass[i] #g

  POC_out$total[i] <- POC_outflow[i,1] + POC_sed_out[i,1] #g
  DOC_out$total[i] <- DOC_outflow[i,1] + MineralRespData$DOC_resp_mass[i] + MineralRespData$DOC_miner_mass[i]  #g
  
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
DeltaPOC <- POC_conc[steps+1,1]*lakeVol - POC_conc[1,1]*lakeVol #g
DeltaDOC <- DOC_conc[steps+1,1]*lakeVol - DOC_conc[1,1]*lakeVol #g
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
ylabelPOC <- c("GPP POC In (g/m2/yr)","Flow POC In (g/m2/yr)","Flow POC Out (g/m2/yr)","Sed POC Out (g/m2/yr)")
ylabelDOC <- c("Flow DOC In (g/m2/yr)","GPP DOC In (g/m2/yr)","Flow  DOC Out (g/m2/yr)","Respiration DOC Out (g/m2/yr)","Mineralization DOC Out (g/m2/yr)")

for (n in 1:ncol(POC_flux)){
  plot(OutputTimeSeries,POC_flux[,n],xlab=xlabel,ylab=ylabelPOC[n],type='l')
}

for (n in 1:ncol(DOC_flux)){
  plot(OutputTimeSeries,DOC_flux[,n],xlab=xlabel,ylab=ylabelDOC[n],type='l')
}

#POC and DOC concentration in time (g/m3)
plot(ConcOutputTimeSeries,POC_conc[,1],xlab=xlabel,ylab="POC Conc (g/m3)",type="l")
plot(ConcOutputTimeSeries,DOC_conc[,1],xlab=xlabel,ylab="DOC Conc (g/m3)",type="l")






