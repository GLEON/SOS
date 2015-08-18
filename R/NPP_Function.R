#NPP Function for SOS Central Code
#Last update 7/16/15, MM

#Apparently I've been trying to reinvent the wheel. 
#LakeMetabolizer does everything I was trying to do
#except convert to OC mass

#metab function returns GPP, R, NEP as (mg O2 L-1 day-1)
#Fixed C:O2 1:1 Because CO2+H2O -> (CH2O) + O2
#Multiply by lakeVol (m3)--> C in g/day

########################################################################

library(LakeMetabolizer)


#Inputs

##Inputs from main program

lakeArea <- 100 * 10000  #m2
lakeDepth <- 25 #m mean depth
lakeVol <- lakeArea * lakeDepth #m3


##Sample data frame generated from metab function documentation 
##Depending on type of actual data used, will need to include
##   additional functions for o2 saturation (o2.at.sat) and
##   gas exchange coefficient (k.cole, k600.2.kGAS)

  NEP.data<-read.csv("NPP_Test_Data",header=T) 

##Convert date to POSIXct
  
  NEP.data$datetime<-as.POSIXct(NEP.data$datetime,tz="GMT")

##Calculate metabolism for sample data with simple OLS model
 #4 other model options: bayesian, bookkeep, kalman, mle

  NEP <- metab(NEP.data,"ols",wtr.name="wtr",irr.name="irr",do.obs.name="do.obs")


#Outputs

##Calculate mass outputs and populate new columns
  ### unit housekeeping ###
  #OC_GPP = O2 (mg/L/day) * (1L/0.001 m3) * (1g/1000mg) * mlVol (m3)
  #       = OC g/day
 
  NEP$OC_GPP <- NEP$GPP * lakeVol
  NEP$OC_R   <- NEP$R * lakeVol
  NEP$OC_NEP <- NEP$NEP * lakeVol

#Runs, but numbers are huge. Something amiss. Will update. 








  
  
  