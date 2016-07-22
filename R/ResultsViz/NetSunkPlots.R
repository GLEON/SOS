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
# Poster Fig 1
ylim = c(min(min_df), max(max_df)) # Vanern is such an outlier that this doesn't quite work
#ylim=NULL # If want unique y axes for all plots

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

#### Calculate annual, monthly Net Sunk means ####
Vanern_SOS$Month = format(Vanern_SOS$Date,"%m",tz = "GMT")
Vanern_SOS$Year = format(Vanern_SOS$Date,"%Y",tz = "GMT")
VanernNetMonth = aggregate(Net ~ Month, Vanern_SOS, mean)
VanernNetYear = aggregate(Net ~ Year, Vanern_SOS, mean)

Toolik_SOS$Month = format(Toolik_SOS$Date,"%m",tz = "GMT")
Toolik_SOS$Year = format(Toolik_SOS$Date,"%Y",tz = "GMT")
ToolikNetMonth = aggregate(Net ~ Month, Toolik_SOS, mean)
ToolikNetYear = aggregate(Net ~ Year, Toolik_SOS, mean)

Trout_SOS$Month = format(Trout_SOS$Date,"%m",tz = "GMT")
Trout_SOS$Year = format(Trout_SOS$Date,"%Y",tz = "GMT")
TroutNetMonth = aggregate(Net ~ Month, Trout_SOS, mean)
TroutNetYear = aggregate(Net ~ Year, Trout_SOS, mean)

Mendota_SOS$Month = format(Mendota_SOS$Date,"%m",tz = "GMT")
Mendota_SOS$Year = format(Mendota_SOS$Date,"%Y",tz = "GMT")
MendotaNetMonth = aggregate(Net ~ Month, Mendota_SOS, mean)
MendotaNetYear = aggregate(Net ~ Year, Mendota_SOS, mean)

Harp_SOS$Month = format(Harp_SOS$Date,"%m",tz = "GMT")
Harp_SOS$Year = format(Harp_SOS$Date,"%Y",tz = "GMT")
HarpNetMonth = aggregate(Net ~ Month, Harp_SOS, mean)
HarpNetYear = aggregate(Net ~ Year, Harp_SOS, mean)

#Annie_SOS$Month = format(Annie_SOS$Date,"%m",tz = "GMT")
#Annie_SOS$Year = format(Annie_SOS$Date,"%Y",tz = "GMT")
#AnnieNetMonth = aggregate(Net ~ Month, Annie_SOS, mean)
#AnnieNetYear = aggregate(Net ~ Year, Annie_SOS, mean)

#### Plot monthly Net Sunk ####
## POSTER FIG 2
#par(mfrow=c(1,1)) 
layout(matrix(c(1,2,3,4,5,6),2,3))
months = c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
mtext = 'OC Mass (kg)'
ylim = c(-4e6,200000)
#ylim=NULL

barplot(VanernNetMonth$Net, main='Vanern', col='dodgerblue',
        names.arg = months, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

barplot(ToolikNetMonth$Net, main='Toolik', col='dodgerblue', ylim=ylim,
        names.arg = months, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

barplot(TroutNetMonth$Net, main='Trout', col='dodgerblue', ylim=ylim,
        names.arg = months, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

barplot(MendotaNetMonth$Net, main='Mendota', col='dodgerblue', ylim=ylim,
        names.arg = months, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

barplot(HarpNetMonth$Net, main='Harp', col='dodgerblue', ylim=ylim,
        names.arg = months, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

#barplot(AnnieNetMonth$Net, main='Annie', col='dodgerblue', ylim=ylim,
#        names.arg = months, las=2) #las=2 rotates x axis labels
#mtext(side=3, mtext, cex=0.75)

#### Plot annual Net Sunk ####
#par(mfrow=c(1,1)) 
layout(matrix(c(1,2,3,4,5,6),2,3))
mtext = 'OC Mass (kg)'
ylim = c(-4e6,200000)
#ylim=NULL

barplot(VanernNetYear$Net, main='Vanern', col='dodgerblue',
        names.arg = VanernNetYear$Year, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

barplot(ToolikNetYear$Net, main='Toolik', col='dodgerblue', ylim=ylim,
        names.arg = ToolikNetYear$Year, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

barplot(TroutNetYear$Net, main='Trout', col='dodgerblue', ylim=ylim,
        names.arg = TroutNetYear$Year, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

barplot(MendotaNetYear$Net, main='Mendota', col='dodgerblue', ylim=ylim,
        names.arg = MendotaNetYear$Year, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

barplot(HarpNetYear$Net, main='Harp', col='dodgerblue', ylim=ylim,
        names.arg = HarpNetYear$Year, las=2) #las=2 rotates x axis labels
mtext(side=3, mtext, cex=0.75)

#barplot(AnnieNetYear$Net, main='Annie', col='dodgerblue', ylim=ylim,
#        names.arg = AnnieNetYear$Year, las=2) #las=2 rotates x axis labels
#mtext(side=3, mtext, cex=0.75)

#### Plot annual Net Sunk across years and lakes on single plot ####
as.Date(VanernNetYear$Year, format = "%Y")
as.Date(ToolikNetYear$Year, format = "%Y")
as.Date(TroutNetYear$Year, format = "%Y")
as.Date(MendotaNetYear$Year, format = "%Y")
as.Date(HarpNetYear$Year, format = "%Y")
#as.Date(AnnieNetYear$Year, format = "%Y")

par(mfrow=c(1,1)) 
#layout(matrix(c(1,2,3,4,5,6),2,3))
mtext = 'OC Mass (kg)'
ylim = c(-4e6,500000)
#ylim=NULL
lwd = 2
xlim = c(1991,2013)

plot(Net ~ Year, VanernNetYear, type='l', lwd=lwd, col='darkgreen', xlim=xlim, ylim=ylim)
par(new=T)
plot(Net ~ Year, ToolikNetYear, type='l', lwd=lwd, yaxt='n', 
     xaxt='n', xlab='', ylab='', col='navy', xlim=xlim, ylim=ylim)
par(new=T)
plot(Net ~ Year, TroutNetYear, type='l', lwd=lwd, yaxt='n', 
     xaxt='n', xlab='', ylab='', col='orange', xlim=xlim, ylim=ylim)
par(new=T)
plot(Net ~ Year, MendotaNetYear, type='l', lwd=lwd, yaxt='n', 
     xaxt='n', xlab='', ylab='', col='darkred', xlim=xlim, ylim=ylim)
par(new=T)
plot(Net ~ Year, HarpNetYear, type='l', lwd=lwd, yaxt='n', 
     xaxt='n', xlab='', ylab='', col='darkmagenta', xlim=xlim, ylim=ylim)
#par(new=T)
#plot(Net ~ Year, AnnieNetYear, type='l', lwd=lwd, yaxt='n', 
#     xaxt='n', xlab='', ylab='', col='chartreuse', xlim=xlim, ylim=ylim)
abline(1,0, lwd=2, lty=2)
legend('bottomleft', legend=c('Vanern','Toolik','Trout','Mendota','Harp'),
       col = c('darkgreen','navy','orange','darkred','darkmagenta'), lwd=lwd,
       lty=1)
