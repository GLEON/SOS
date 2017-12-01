############## Calculate RMSE and NSE from DOC and DO results ###############
# Date: 12-1-17
# Author: Ian McCullough, immccull@gmail.com
# Updated:
#############################################################################

library(hydroGOF)
setwd('C:/Users/immcc/Desktop/SOS')
#LakeName = 'Monona'
LakeNames = c('Harp','Monona','Trout','Toolik','Vanern')
timestampFormat =	'%Y-%m-%d'

# D-fine functions

rmse_DO = function(LakeName) {
  # read in modeled and observed data
  modeled_DO = read.csv(paste('./',LakeName,'Lake/Results/',LakeName,'_DO_Results.csv',sep=''))
  observed_DO = read.csv(paste('./',LakeName,'Lake/Results/',LakeName,'_DOvalidation.csv',sep=''))
  # reformat date column and keep only complete cases
  modeled_DO$Date <- as.Date(as.POSIXct(strptime(modeled_DO$Date,timestampFormat),tz="GMT")) #Convert time to POSIX
  modeled_DO = modeled_DO[complete.cases(modeled_DO),]
  observed_DO$datetime <- as.Date(as.POSIXct(strptime(observed_DO$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
  observed_DO = observed_DO[complete.cases(observed_DO),]
  joined_DO = merge(modeled_DO, observed_DO, by.x='Date', by.y='datetime')
  error = joined_DO$DO_con - joined_DO$Conc_Modelled
  return(sqrt(mean(error^2)))
}

rmse_DOC = function(LakeName) {
  # read in modeled and observed data
  modeled_DOC = read.csv(paste('./',LakeName,'Lake/Results/',LakeName,'_DOC_Results.csv',sep=''))
  observed_DOC = read.csv(paste('./',LakeName,'Lake/Results/',LakeName,'_DOCvalidation.csv',sep=''))
  # reformat date column and keep only complete cases
  modeled_DOC$Date <- as.Date(as.POSIXct(strptime(modeled_DOC$Date,timestampFormat),tz="GMT")) #Convert time to POSIX
  modeled_DOC = modeled_DOC[complete.cases(modeled_DOC),]
  observed_DOC$datetime <- as.Date(as.POSIXct(strptime(observed_DOC$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
  observed_DOC = observed_DOC[complete.cases(observed_DOC),]
  joined_DOC = merge(modeled_DOC, observed_DOC, by.x='Date', by.y='datetime')
  error = joined_DOC$DOC - joined_DOC$Modelled
  return(sqrt(mean(error^2)))
}

NSE_DO = function(LakeName) {
  # read in modeled and observed data
  modeled_DO = read.csv(paste('./',LakeName,'Lake/Results/',LakeName,'_DO_Results.csv',sep=''))
  observed_DO = read.csv(paste('./',LakeName,'Lake/Results/',LakeName,'_DOvalidation.csv',sep=''))
  # reformat date column and keep only complete cases
  modeled_DO$Date <- as.Date(as.POSIXct(strptime(modeled_DO$Date,timestampFormat),tz="GMT")) #Convert time to POSIX
  modeled_DO = modeled_DO[complete.cases(modeled_DO),]
  observed_DO$datetime <- as.Date(as.POSIXct(strptime(observed_DO$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
  observed_DO = observed_DO[complete.cases(observed_DO),]
  joined_DO = merge(modeled_DO, observed_DO, by.x='Date', by.y='datetime')
  NashSutcliffe = NSE(sim=joined_DO$Conc_Modelled, obs=joined_DO$DO_con, na.rm=T, FUN=NULL)
  return(NashSutcliffe)
}

NSE_DOC = function(LakeName) {
  # read in modeled and observed data
  modeled_DOC = read.csv(paste('./',LakeName,'Lake/Results/',LakeName,'_DOC_Results.csv',sep=''))
  observed_DOC = read.csv(paste('./',LakeName,'Lake/Results/',LakeName,'_DOCvalidation.csv',sep=''))
  # reformat date column and keep only complete cases
  modeled_DOC$Date <- as.Date(as.POSIXct(strptime(modeled_DOC$Date,timestampFormat),tz="GMT")) #Convert time to POSIX
  modeled_DOC = modeled_DOC[complete.cases(modeled_DOC),]
  observed_DOC$datetime <- as.Date(as.POSIXct(strptime(observed_DOC$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
  observed_DOC = observed_DOC[complete.cases(observed_DOC),]
  joined_DOC = merge(modeled_DOC, observed_DOC, by.x='Date', by.y='datetime')
  NashSutcliffe = NSE(sim=joined_DOC$Modelled, obs=joined_DOC$DOC, na.rm=T, FUN=NULL)
  return(NashSutcliffe)
}

####### Main program #################

# calculate RMSE for DO
rmse_DO_list = list()
for (i in 1:length(LakeNames)) {
  x = rmse_DO(LakeNames[i])
  rmse_DO_list[i] = x
}

rmse_DO_summary = data.frame(LakeName = LakeNames, RMSE = do.call(rbind, rmse_DO_list))

# calculate RMSE for DOC
rmse_DOC_list = list()
for (i in 1:length(LakeNames)) {
  x = rmse_DOC(LakeNames[i])
  rmse_DOC_list[i] = x
}

rmse_DOC_summary = data.frame(LakeName = LakeNames, RMSE = do.call(rbind, rmse_DOC_list))

# calculate NSE for DO
NSE_DO_list = list()
for (i in 1:length(LakeNames)) {
  x = NSE_DO(LakeNames[i])
  NSE_DO_list[i] = x
}

NSE_DO_summary = data.frame(LakeName = LakeNames, NSE = do.call(rbind, NSE_DO_list))

# calculate NSE for DOC
NSE_DOC_list = list()
for (i in 1:length(LakeNames)) {
  x = NSE_DOC(LakeNames[i])
  NSE_DOC_list[i] = x
}

NSE_DOC_summary = data.frame(LakeName = LakeNames, NSE = do.call(rbind, NSE_DOC_list))

### final summary table
final_summary = data.frame(LakeName = rmse_DOC_summary$LakeName,
                           RMSE_DOC = rmse_DOC_summary$RMSE,
                           RMSE_DO = rmse_DO_summary$RMSE,
                           NSE_DOC = NSE_DOC_summary$NSE,
                           NSE_DO = NSE_DO_summary$NSE)
