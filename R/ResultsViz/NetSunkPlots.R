###### SOS Net Sunk Plots #####
# Date: 3-19-16
# Last modified: 3-19-16
# Author: Ian McCullough

#### R packages ####


### Plot Net Sunk Results for all lakes ###

# Read in SOS csv outputs from model runs
setwd('H:/Ian_GIS/gleon/tump')
dir()
Harp = read.csv('Harp_output.csv', header=T)
Langtjern = read.csv('Langtjern.csv', header=T)
Mendota = read.csv('Mendota_output.csv', header=T)
Toolik = read.csv('Toolik.csv', header=T)
Trout = read.csv('Trout_output.csv', header=T)
Vanern = read.csv('Vanern.csv', header=T)

# Date conversions
Harp$Date = as.POSIXct(strptime(Harp$Date,"%Y-%m-%d"),tz="GMT")
Langtjern$Date = as.POSIXct(strptime(Langtjern$Date,"%Y-%m-%d"),tz="GMT")
Mendota$Date = as.POSIXct(strptime(Mendota$Date,"%Y-%m-%d"),tz="GMT")
Toolik$Date = as.POSIXct(strptime(Toolik$Date,"%Y-%m-%d"),tz="GMT")
Trout$Date = as.POSIXct(strptime(Trout$Date,"%Y-%m-%d"),tz="GMT")
Vanern$Date = as.POSIXct(strptime(Vanern$Date,"%Y-%m-%d"),tz="GMT")


# The plots thicken
par(mfrow=c(1,1))
plot(Net ~ Date, Harp, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Harp', side=3)

plot(Net ~ Date, Langtjern, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Langtjern', side=3)

plot(Net ~ Date, Mendota, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0)
mtext('Mendota', side=3)

plot(Net ~ Date, Toolik, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Toolik', side=3)

plot(Net ~ Date, Trout, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Trout', side=3)

plot(Net ~ Date, Vanern, type='l', xlab='Date', ylab='OC mass (kg/d)', main='Net OC Mass Sunk')
abline(0,0, lty=2)
mtext('Vanern', side=3)

# Panel plots

# Get min/max of net carbon column for common axes
min_harp = summary(Harp$Net)[1]
min_langtjern = summary(Langtjern$Net)[1]
min_mendota = summary(Mendota$Net)[1]
min_toolik = summary(Toolik$Net)[1]
min_trout = summary(Trout$Net)[1]
min_vanern = summary(Vanern$Net)[1]

max_harp = summary(Harp$Net)[6]
max_langtjern = summary(Langtjern$Net)[6]
max_mendota = summary(Mendota$Net)[6]
max_toolik = summary(Toolik$Net)[6]
max_trout = summary(Trout$Net)[6]
max_vanern = summary(Vanern$Net)[6]

min_df = cbind.data.frame(min_harp, min_langtjern, min_mendota, min_toolik, min_trout, min_vanern)
max_df = cbind.data.frame(max_harp, max_langtjern, max_mendota, max_toolik, max_trout, max_vanern)

# IF want same y axes for all plots
ylim = c(min(min_df), max(max_df))

# If want unique y axes for all plots
ylim=NULL

# Calculate standard deviation of net sunk for each lake (perhaps later we put them on plots?)
harp_sd = round(sd(Harp$Net)) #if don't provide number of digits, rounds to zero decimals by default
langtjern_sd = round(sd(Langtjern$Net))
mendota_sd = round(sd(Mendota$Net))
toolik_sd = round(sd(Toolik$Net))
trout_sd = round(sd(Trout$Net))
vanern_sd = round(sd(Vanern$Net))

layout(matrix(c(1,2,3,4,5,6),2,3)) #6 plots with 2 rows x 3 columns
plot(Net ~ Date, Harp, type='l', xlab='', ylab='OC mass (kg/d)', ylim=ylim, main='Harp')
abline(0,0, lty=2)

plot(Net ~ Date, Langtjern, type='l', xlab='Date', ylab='OC mass (kg/d)', ylim=ylim, main='Langtjern')
abline(0,0, lty=2)
#mtext('Langtjern', side=3)

plot(Net ~ Date, Mendota, type='l', xlab='', ylab='', ylim=ylim, main='Mendota')
abline(0,0, lty=2)
#mtext('Mendota', side=3)

plot(Net ~ Date, Toolik, type='l', xlab='Date', ylab='', ylim=ylim, main='Toolik')
abline(0,0, lty=2)
#mtext('Toolik', side=3)

plot(Net ~ Date, Trout, type='l', xlab='', ylab='', ylim=ylim, main='Trout')
abline(0,0, lty=2)
#mtext('Trout', side=3)

plot(Net ~ Date, Vanern, type='l', xlab='Date', ylab='', ylim=ylim, main='Vanern')
abline(0,0, lty=2)
#mtext('Vanern', side=3)