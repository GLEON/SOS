###### SOS Net Sunk Plots #####
# Date: 3-19-16
# Last modified: 3-19-16
# Author: Ian McCullough

#### R packages ####

library(dplyr)
### Plot Net Sunk Results for all lakes ###

#### Set your own working directory #####
setwd("C:/Users/Ian/Desktop/GLEON/SOS/")

# read in SOS results files for each lake
Vanern_SOS = read.csv('VanernLake/Results/Vanern_SOS_Results.csv')
Toolik_SOS = read.csv('ToolikLake/Results/Toolik_SOS_Results.csv')
Trout_SOS = read.csv('TroutLake/Results/Trout_SOS_Results.csv')
Mendota_SOS = read.csv('MendotaLake/Results/Mendota_SOS_Results.csv')
Harp_SOS = read.csv('HarpLake/Results/Harp_SOS_Results.csv')
#Annie_SOS = read.csv('AnnieLake/Results/Annie_SOS_Results.csv')

# Date conversions
Vanern_SOS$Date = as.POSIXct(strptime(Vanern_SOS$Date,"%Y-%m-%d"),tz="GMT")
Toolik_SOS$Date = as.POSIXct(strptime(Toolik_SOS$Date,"%Y-%m-%d"),tz="GMT")
Trout_SOS$Date = as.POSIXct(strptime(Trout_SOS$Date,"%Y-%m-%d"),tz="GMT")
Mendota_SOS$Date = as.POSIXct(strptime(Mendota_SOS$Date,"%Y-%m-%d"),tz="GMT")
Harp_SOS$Date = as.POSIXct(strptime(Harp_SOS$Date,"%Y-%m-%d"),tz="GMT")
#Annie_SOS$Date = as.POSIXct(strptime(Annie_SOS$Date,"%Y-%m-%d"),tz="GMT")

# The plots thicken
par(mfrow=c(1,1))

plot(Net ~ Date, Vanern_SOS, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Vanern', side=3)

plot(Net ~ Date, Toolik_SOS, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Toolik', side=3)

plot(Net ~ Date, Trout_SOS, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Trout', side=3)

plot(Net ~ Date, Mendota_SOS, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0)
mtext('Mendota', side=3)

plot(Net ~ Date, Harp_SOS, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Harp', side=3)

#plot(Net ~ Date, Annie_SOS, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
#abline(0,0, lty=2)
#mtext('Annie', side=3)


##### Panel plots #####

# Get min/max of net carbon column for common axes
min_harp = summary(Harp_SOS$Net)[1] # first object of summary is min
min_mendota = summary(Mendota_SOS$Net)[1]
min_toolik = summary(Toolik_SOS$Net)[1]
min_trout = summary(Trout_SOS$Net)[1]
min_vanern = summary(Vanern_SOS$Net)[1]
#min_annie = summary(Annie_SOS$Net)[1]

max_harp = summary(Harp_SOS$Net)[6] # sixth object of summary is max
max_mendota = summary(Mendota_SOS$Net)[6]
max_toolik = summary(Toolik_SOS$Net)[6]
max_trout = summary(Trout_SOS$Net)[6]
max_vanern = summary(Vanern_SOS$Net)[6]
#max_annie = summary(Annie$Net)[6]

# create data frame of min/max Net values for all lakes for standardizing axes
# omitted Vanern because it is such an outlier
min_df = cbind.data.frame(min_harp, min_mendota, min_toolik, min_trout)
max_df = cbind.data.frame(max_harp, max_mendota, max_toolik, max_trout)

# IF want same y axes for all plots
ylim = c(min(min_df), max(max_df)) # Vanern is such an outlier that this doesn't quite work

# If want unique y axes for all plots
#ylim=NULL

# plots
layout(matrix(c(1,2,3,4,5,6),2,3)) #6 plots with 2 rows x 3 columns

plot(Net ~ Date, Vanern_SOS, type='l', xlab='', ylab='OC mass (kg/d)', main='Vanern')
abline(0,0, lty=2)
#mtext('Vanern', side=3)

plot(Net ~ Date, Toolik_SOS, type='l', xlab='', ylab='OC mass (kg/d)', ylim=ylim, main='Toolik')
abline(0,0, lty=2)
#mtext('Toolik', side=3)

plot(Net ~ Date, Trout_SOS, type='l', xlab='', ylab='', ylim=ylim, main='Trout')
abline(0,0, lty=2)
#mtext('Trout', side=3)

plot(Net ~ Date, Mendota_SOS, type='l', xlab='', ylab='', ylim=ylim, main='Mendota')
abline(0,0, lty=2)
#mtext('Mendota', side=3)

plot(Net ~ Date, Harp_SOS, type='l', xlab='', ylab='', ylim=ylim, main='Harp')
abline(0,0, lty=2)

# Calculate standard deviation of net sunk for each lake (perhaps later we put them on plots?)
harp_sd = round(sd(Harp_SOS$Net)) #if don't provide number of digits, rounds to zero decimals by default
mendota_sd = round(sd(Mendota_SOS$Net))
toolik_sd = round(sd(Toolik_SOS$Net))
trout_sd = round(sd(Trout_SOS$Net))
vanern_sd = round(sd(Vanern_SOS$Net))
#annie_sd = round(sd(Annie_SOS$Net))

### plot all lakes on single plot (joining by date)
#all_lakes_df = left_join(Vanern_SOS, Toolik_SOS, by='Date')
#all_lakes_df = left_join(all_lakes_df, Trout_SOS, by='Date')
