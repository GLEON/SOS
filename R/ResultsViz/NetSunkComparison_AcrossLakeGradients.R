###### SOS Net Sunk Plots #####
# Date: 3-23-16
# Last modified: 3-23-16
# Author: Ian McCullough

#### R packages ####
#library(lubridate)
library(zoo)
library(dplyr)

#### Set your own working directory #####
setwd("C:/Users/Ian/Desktop/GLEON/SOS/")

#### Plot Net Sunk Results for all lakes ####

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

# Calculate annual, monthly Net Sunk means
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

# Calculate annual means for each lake (...could be some other summary variable)
Harp_AM = mean(HarpNetYear$Net)
Toolik_AM = mean(ToolikNetYear$Net)
Trout_AM = mean(TroutNetYear$Net)
Mendota_AM = mean(MendotaNetYear$Net)
Vanern_AM = mean(VanernNetYear$Net)
#Annie_AM = mean(AnnieNetYear$Net)
Annie_AM = NA


AM_df = data.frame(Lake=c('Vanern','Toolik','Trout','Mendota','Harp','Annie'),
                   NetSunk = c(Vanern_AM,Toolik_AM,Trout_AM,Mendota_AM,Harp_AM,Annie_AM))

# read in lake attribute table for reodering of rows for plotting
laketable = read.csv('Table1_SOS_Lake_Summary.csv')
laketable$area_rank = rank(laketable$Area..ha., na.last=T, ties.method = c('first'))
laketable$depth_rank = rank(laketable$Mean.Depth..m., na.last = T, ties.method = c('first'))
laketable$restime_rank = rank(laketable$Residence.Time..yrs., na.last = T, ties.method = c('first'))
laketable$TP_rank = rank(laketable$TP, na.last = T, ties.method = c('first'))
laketable$volume_rank = rank(laketable$Volume..m³., na.last = T, ties.method = c('first'))
laketable$Secchi_rank = rank(laketable$Secchi..m., na.last = T, ties.method = c('first'))
laketable$DOC_rank = rank(laketable$DOC..g.m³., na.last = T, ties.method = c('first'))

# join ranks to variable of interest table
variable_df = left_join(AM_df, laketable, by='Lake') #vanern consistently outlier...what to do

## Ian: how are we going to deal with Vanern's big values? Some sort of NetSunk ratio?
#variable_df$NS_areaRatio = variable_df$NetSunk/variable_df$Area..ha.

#### make barplot panel ####
#par(mfrow=c(1,1)) #for single plot per window
variable = 'NetSunk'
layout(matrix(c(1,2,3,4,5,6),2,3)) #6 plots with 2 rows x 3 columns

## lake area
# reorder data frame in ascending order for variable of interest
Sunk_byLakeArea = variable_df[order(variable_df$area_rank) , ]
labels = Sunk_byLakeArea$Lake

barplot(Sunk_byLakeArea$NetSunk, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Lake Area', col='dodgerblue')
#mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## mean lake depth
Sunk_byMeanDepth = variable_df[order(variable_df$depth_rank) , ]
labels = Sunk_byMeanDepth$Lake

barplot(Sunk_byMeanDepth$NetSunk, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Mean Depth', col='dodgerblue')
#mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Residence time
Sunk_byResTime = variable_df[order(variable_df$restime_rank) , ]
labels = Sunk_byResTime$Lake

barplot(Sunk_byResTime$NetSunk, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Residence Time', col='dodgerblue')
#mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## TP
Sunk_byTP = variable_df[order(variable_df$TP_rank) , ]
labels = Sunk_byTP$Lake

barplot(Sunk_byTP$NetSunk, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='TP', col='dodgerblue')
#mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake volume
Sunk_byVolume = variable_df[order(variable_df$volume_rank) , ]
labels = Sunk_byVolume$Lake

barplot(Sunk_byVolume$NetSunk, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Volume', col='dodgerblue')
#mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake DOC
Sunk_byDOC = variable_df[order(variable_df$DOC_rank) , ]
labels = Sunk_byDOC$Lake

barplot(Sunk_byDOC$NetSunk, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='DOC', col='dodgerblue')
#mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)


## Secchi depth
Sunk_bySecchi = variable_df[order(variable_df$Secchi_rank) , ]
labels = Sunk_bySecchi$Lake

barplot(Sunk_bySecchi$NetSunk, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Secchi', col='dodgerblue')
#mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)