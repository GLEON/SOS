###### SOS Net Sunk Plots #####
# Date: 3-23-16
# Last modified: 3-23-16
# Author: Ian McCullough

#### R packages ####
#library(lubridate)
library(zoo)
library(dplyr)

### Plot Net Sunk Results for all lakes ###

# Read in SOS csv outputs from model runs
setwd('H:/Ian_GIS/gleon/tump')
dir()
Annie = read.csv('Annie_output.csv', header=T)
Center = read.csV('Center_output.csv', header=T)
Harp = read.csv('Harp_output.csv', header=T)
Langtjern = read.csv('Langtjern.csv', header=T)
Mendota = read.csv('Mendota_output.csv', header=T)
Silver = read.csv('Silver_output.csv', header=T)
Toolik = read.csv('Toolik.csv', header=T)
Trout = read.csv('Trout_output.csv', header=T)
Vanern = read.csv('Vanern.csv', header=T)
WestOkoboji = read.csv('WestOkoboji.csv', header=T)

# Date conversions
Annie$Date = as.POSIXct(strptime(Annie$Date,"%Y-%m-%d"),tz="GMT")
Center$Date = as.POSIXct(strptime(Center$Date,"%Y-%m-%d"),tz="GMT")
Harp$Date = as.POSIXct(strptime(Harp$Date,"%Y-%m-%d"),tz="GMT")
Langtjern$Date = as.POSIXct(strptime(Langtjern$Date,"%Y-%m-%d"),tz="GMT")
Mendota$Date = as.POSIXct(strptime(Mendota$Date,"%Y-%m-%d"),tz="GMT")
Silver$Date = as.POSIXct(strptime(Silver$Date,"%Y-%m-%d"),tz="GMT")
Toolik$Date = as.POSIXct(strptime(Toolik$Date,"%Y-%m-%d"),tz="GMT")
Trout$Date = as.POSIXct(strptime(Trout$Date,"%Y-%m-%d"),tz="GMT")
Vanern$Date = as.POSIXct(strptime(Vanern$Date,"%Y-%m-%d"),tz="GMT")
WestOkoboji$Date = as.POSIXct(strptime(WestOkoboji$Date,"%Y-%m-%d"),tz="GMT")

# Create date/month column for monthly data aggregation
Annie$DateMonth = zoo::as.yearmon(Annie$Date) # class 'yearmon' stored as four digit year (e.g., 1995)
Center$DateMonth = zoo::as.yearmon(Center$Date) 
Harp$DateMonth = zoo::as.yearmon(Harp$Date) 
Langtjern$DateMonth = zoo::as.yearmon(Langtjern$Date)
Mendota$DateMonth = zoo::as.yearmon(Mendota$Date)
Silver$DateMonth = zoo::as.yearmon(Silver$Date) 
Toolik$DateMonth = zoo::as.yearmon(Toolik$Date)
Trout$DateMonth = zoo::as.yearmon(Trout$Date)
Vanern$DateMonth = zoo::as.yearmon(Vanern$Date)
WestOkoboji$DateMonth = zoo::as.yearmon(WestOkoboji$Date) 

# Aggregate date column by month
Annie_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Annie, FUN=sum)
Center_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Center, FUN=sum)
Harp_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Harp, FUN=sum)
#plot(Net~DateMonth, Harp_byMonth, type='l')
Langtjern_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Langtjern, FUN=sum)
Mendota_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Mendota, FUN=sum)
Silver_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Silver, FUN=sum)
Toolik_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Toolik, FUN=sum)
Trout_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Trout, FUN=sum)
Vanern_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=Vanern, FUN=sum)
WestOkoboji_byMonth = aggregate(cbind(Source,Sink,Pipe,Net)~DateMonth, data=WestOkoboji, FUN=sum)

# Pull out variable of interest for comparison across lakes
variable = 'Jul'

Anniex = subset(Annie_byMonth, grepl(variable, DateMonth))
Centerx = subset(Center_byMonth, grepl(variable, DateMonth))
Harpx = subset(Harp_byMonth, grepl(variable, DateMonth))
Langtjernx = subset(Langtjern_byMonth, grepl(variable, DateMonth))
Mendotax = subset(Mendota_byMonth, grepl(variable, DateMonth))
Silverx = subset(Silver_byMonth, grepl(variable, DateMonth))
Toolikx = subset(Toolik_byMonth, grepl(variable, DateMonth))
Troutx = subset(Trout_byMonth, grepl(variable, DateMonth))
Vanernx = subset(Vanern_byMonth, grepl(variable, DateMonth))
WestOkobojix = subset(WestOkoboji_byMonth, grepl(variable, DateMonth))

Annie = mean(Anniex$Net)
Center = mean(Center$Net)
Harp = mean(Harpx$Net)
Langtjern = mean(Langtjernx$Net)
Mendota = mean(Mendotax$Net)
Silver = mean(Silverx$Net)
Toolik = mean(Toolikx$Net)
Trout = mean(Troutx$Net)
Vanern = mean(Vanernx$Net)
WestOkoboji = mean(WestOkobojix$Net)

# read in lake attribute table for reodering of rows for plotting
laketable = read.csv('H:/Ian_GIS/gleon/SOS/Table1_SOS_Lake_Summary.csv')
laketable$area_rank = rank(laketable$Area..ha., na.last=T, ties.method = c('first'))
laketable$depth_rank = rank(laketable$Mean.Depth..m., na.last = T, ties.method = c('first'))
laketable$restime_rank = rank(laketable$Residence.Time..yrs., na.last = T, ties.method = c('first'))
laketable$TP_rank = rank(laketable$TP, na.last = T, ties.method = c('first'))
laketable$volume_rank = rank(laketable[4], na.last = T, ties.method = c('first'))
laketable$latitude_rank = rank(laketable$N.lat...W.long, na.last = T, ties.method = c('first'))

# join ranks to variable of interest table
variable_df = rbind.data.frame(Harp,Langtjern,Mendota,Toolik,Trout) #vanern consistently outlier...what to do
colnames(variable_df) = variable
variable_df$Lake = c('Harp','Langtjern','Mendota','Toolik','Trout')
variable_ranks = left_join(variable_df, laketable, by='Lake')

# make barplots
#par(mfrow=c(1,1)) #for single plot per window
layout(matrix(c(1,2,3,4,5,6),2,3)) #6 plots with 2 rows x 3 columns

## lake area
# reorder data frame in ascending order for variable of interest
Sunk_byLakeArea = variable_ranks[order(variable_ranks$area_rank) , ]
labels = Sunk_byLakeArea$Lake

barplot(Sunk_byLakeArea$Jul, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Lake Area', col='dodgerblue')
mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=0.75)

## mean lake depth
Sunk_byMeanDepth = variable_ranks[order(variable_ranks$depth_rank) , ]
labels = Sunk_byMeanDepth$Lake

barplot(Sunk_byMeanDepth$Jul, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Mean Depth', col='dodgerblue')
mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=0.75)

## Residence time
Sunk_byResTime = variable_ranks[order(variable_ranks$restime_rank) , ]
labels = Sunk_byResTime$Lake

barplot(Sunk_byResTime$Jul, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Residence Time', col='dodgerblue')
mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=0.75)

## TP
Sunk_byTP = variable_ranks[order(variable_ranks$TP_rank) , ]
labels = Sunk_byTP$Lake

barplot(Sunk_byTP$Jul, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='TP', col='dodgerblue')
mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=0.75)

## Lake volume
Sunk_byVolume = variable_ranks[order(variable_ranks$volume_rank) , ]
labels = Sunk_byVolume$Lake

barplot(Sunk_byVolume$Jul, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Volume', col='dodgerblue')
mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=0.75)

## Lake volume
Sunk_byLatitude = variable_ranks[order(variable_ranks$latitude_rank) , ]
labels = Sunk_byLatitude$Lake

barplot(Sunk_byLatitude$Jul, xlab='', ylab='OC mass (kg)', xaxt='n', ylim=c(-4e07, 1e07),
        main='Latitude', col='dodgerblue')
mtext(paste0(variable, ' avg'), side=3)
endlabel = length(labels) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=0.75)
