
##### LOAD PACKAGES ########################
library(LakeMetabolizer)
############################################

##### READ MAIN INPUT FILE #################
InputData <- read.csv("ExampleInput.csv",header=T)
InputData$datetime<-as.POSIXct(InputData$datetime,tz="GMT")
############################################

##### General Lake Inputs ##################
#lakeDepth <- In Input file
lakePerim <- 1000 #m
#lakeArea <- 62500 #m^2 In Input file
#lakeVol <- lakeDepth*lakeArea #m^3 In Input file
#resTime <-  3 #days Not needed
g <- 9.81 #m/s^2
rho_H2O <- 999.9720 #kg/m^3
Days2Seconds <- 1*24*60*60 #s/d
DOC_conc_init <- 10  #g/m3
############################################

###### Run Period and Time Step Inputs #####
TimeStep <- InputData$Datenum[2]-InputData$Datenum[1] #days
StartDay <- InputData$Datenum[1]  #Julian day
EndDay <- InputData$Datenum[length(InputData)] #Julian day
steps <- length(InputData$Datenum)
############################################


##### Sub-Topic Inputs: Sedimentation ######
DOC_avg <- 10   #g/m3  Average DOC value in lake to initialize estimate of average POC
MAR_sed_avg <- 1000 #Mass accumulation rate of sediment (g sed/m^2/yr) ##REQUIRES DATA##
Sed_oc_avg <-  4.5 #Percent of sediment estimated to be OC (%) ##REQUIRES DATA##
############################################

##### Sub-Topic Inputs: GPP/NPP ############
#NEP.data<-read.csv("NPP_Test_Data.csv",header=T) #Included in general InputData file
#NEP.data$datetime<-as.POSIXct(NEP.data$datetime,tz="GMT")
############################################

##### Sub-Topic Inputs: sw/GW ##############
#Q_in <- 5 #m3/s: total inflow (Sw + Gw) Will be changed to dynamic input
#Q_out <- 5 #m3/s: total outflow. Assume steady state pending dynamic output

PC <- 0.7 #unitless: proportion of lake shore with canopy
PW <- 0.2 #unitless: proportion of lake shore with wetlands

AOC_year <- 1 #g/m/yr: aerial loading factor
Woc_year <- 1 #g/m/yr: adjacent wetland loading factor

prop_GW <- .3 # Unitless: proportion of Q_in that is from groundwater
DOC_GW <- 10 # g/m3: DOC concentration in groundwater. 2-40 g/m3 per Hanson et al 2014
DOC_SW <- 1 # g/m3: DOC concentration in surface water
DOC_Precip <- 2 #g/m3: DOC concentration in precipitation

Rainfall <- 2 #mm/d: precipitation input from meteorological timeseries(?)
############################################

##### Declare Output Data Storage ##########
POC_conc <- data.frame(numeric(steps))
DOC_conc <- data.frame(numeric(steps))
############################################

##### Declare Data Storage - Sed ###########
SedData <- data.frame(BurialScalingFactor=numeric(steps),MAR_oc=numeric(steps),LeafLitter=numeric(steps),
                      LeafParameter=numeric(steps),DOC_SWGW=numeric(steps),POC_SWGW=numeric(steps),
                      POC_in_alloch=numeric(steps),POC_in_autoch=numeric(steps),POC_in=numeric(steps),
                      POC_burial=numeric(steps),POC_grazed_DIC=numeric(steps))
POC_sed_out <- data.frame(numeric(steps))
############################################

##### Declare Data Storage - GPP ###########
#Included in input declaration
############################################

##### Declare Data Storage - SW/GW #########
SWGWData = data.frame(Q_in=numeric(steps), Aoc=numeric(steps), DOC_Aerial=numeric(steps), DOC_Wetland=numeric(steps), 
                        DOC_GW=numeric(steps), DOC_SW=numeric(steps), DailyRain=numeric(steps), 
                        DOC_Precip=numeric(steps), Load_DOC=numeric(steps), Load_POC=numeric(steps))
POC_outflow <- data.frame(numeric(steps))
DOC_outflow <- data.frame(numeric(steps))
############################################

##### Genearl Lake Variable Initialization ###############
POC_conc_avg <- 0.1*DOC_avg  #Average POC concentration in water column (g/m^3) ##HOW DO WE DETERMINE THIS PRE-MODEL-RUN?##
POC_conc[1,1] <- POC_conc_avg # #Initialize POC concentration as baseline average
DOC_conc[1,1] <- DOC_conc_init #Initialize DOC concentration g/m3
############################################

####################### MAIN PROGRAM #############################################
##################################################################################

##### GPP/NPP Sub Function #################
#Run lake metabolizer code for GPP/NPP contribution to OC using GLM data. This function runs outside of the the primary
#program loop because it does not require feedback from other OC flux sub-functions.
NEP <- metab(InputData,"ols",wtr.name="wtr",irr.name="irr",do.obs.name="do.obs") #Calculate metabolism for sample data with simple OLS model; 4 other model options: bayesian, bookkeep, kalman, mle
#Calculate mass outputs and populate new columns
### unit housekeeping ###
#OC_GPP = O2 (mg/L/day) * (1L/0.001 m3) * (1g/1000mg) * mlVol (m3)
#       = OC g/day
NEP$OC_GPP <- NEP$GPP * lakeVOl #g/day
NEP$OC_R   <- NEP$R * lakeVOl #g/day
NEP$OC_NEP <- NEP$NEP * lakeVOl #g/day


for (i in 1:(steps-1)){
  
  lakeDepth <- InputData$LakeLevel[i] #m
  lakeArea <- InputData$SurfaceArea[i] #m^2
  lakeVol <- InputData$Volume[i] #m^3
  Q_in <- InputData$TotInflowVol[i]/(TimeStep*Days2Seconds) #m3/s: 
  Q_out <- InputData$TotOutflowVol[i]/(TimeStep*Days2Seconds) #m3/s: total outflow. Assume steady state pending dynamic output
  
  #Call SWGW
  SWGWoutput <- SWGWFunction(Q_in,rainfall,Aoc_year, PC, lakePerim, Woc_year, PW, DOC_GW, prop_GW, 
                             DOC_SW, lakeArea) #change these inputs to iterative [i] values when inputs are dynamic
  SWGWData[i,1:10] <- SWGWoutput
  
  #Calculate load from SWGW_in
  DOC_SWGW_in <- SWGWData$Load_DOC[i]*TimeStep #g
  POC_SWGW_in <- SWGWData$Load_POC[i]*TimeStep #g
  
  #Call Sed Function
  SedOutput <- SedimentationFunction(TimeStepConversion,lAkePerim,lakeArea,lakeVol,DOC_avg,MAR_sed_avg,Sed_oc_avg,POC_conc[i,1])
  SedData[i,1:11] = SedOutput
  POC_sed_out[i,1] <- SedData$POC_grazed_DIC[i] + SedData$POC_in[i] - SedData$POC_burial[i]*TimeStep  #g
  
  #Calc outflow subtractions (assuming outflow concentrations = mixed lake concentrations)
  POC_outflow[i,1] <- POC_conc[i,1]*Q_out*60*60*24*TimeStep #g
  DOC_outflow[i,1] <- DOC_conc[i,1]*Q_out*60*60*24*TimeStep #g
  
  
  #Update POC and DOC concentration values for whole lake
  POC_conc[i+1,1] <-  POC_conc[i,1] + (NEP$OC_NEP[i] + POC_SWGW_in - POC_outflow[i,1] - POC_sed_out[i,1])/lakeVol #g/m3
  DOC_conc[i+1,1] <-  DOC_conc[i,1] + (DOC_SWGW_in - DOC_outflow[i,1])lakeVol #g/m3
  
}

#######################################################################################
#######################################################################################


