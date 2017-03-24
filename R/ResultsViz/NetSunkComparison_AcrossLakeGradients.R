###### SOS Net Sunk Plots #####
# Date: 3-23-16
# Last modified: 1-5-17
# Author: Ian McCullough

#### R packages ####
library(dplyr)

#### Set your own working directory #####
#setwd("H:/Ian_GIS/GLEON/SOS/")

#### Plot mean fate Results for all lakes ####
SOS_mean = read.csv('FateOutputs_byLake.csv')
colnames(SOS_mean)[1] = 'Lake' #for joining later
laketable = read.csv('Table1_SOS_Lake_Summary.csv')

# create rank columns for lake variables for ordering within plots
laketable$area_rank = rank(laketable$Area_ha, na.last=T, ties.method = c('first'))
laketable$depth_rank = rank(laketable$MeanDepth_m, na.last = T, ties.method = c('first'))
laketable$restime_rank = rank(laketable$ResidenceTime_yrs, na.last = T, ties.method = c('first'))
laketable$TP_rank = rank(laketable$TP_gm3, na.last = T, ties.method = c('first'))
laketable$volume_rank = rank(laketable$Volume_m3, na.last = T, ties.method = c('first'))
laketable$Secchi_rank = rank(laketable$Secchi_m, na.last = T, ties.method = c('first'))
laketable$DOC_rank = rank(laketable$DOC_gm3, na.last = T, ties.method = c('first'))
laketable$perim_rank = rank(laketable$Perimeter_m, na.last = T, ties.method = c('first'))

# join ranks to variable of interest table
variable_df = left_join(SOS_mean, laketable, by='Lake')
variable_df$LakeAbbr = as.factor(c('H','M','TO','TR','V'))

######################### make barplot panel ################################

#################### Sedimentation #####################
## Static plot parameters
#par(mfrow=c(1,1)) #for single plot per window
png(paste0('R/ResultsViz/Figures/SedAlongLakeGradients.png'),width = 11,height = 9,units = 'in',res=300)
layout(matrix(c(1,2,3,4,5,6,7,8),2,4)) #8 plots with 2 rows x 4 columns
par(mar=c(2,3,2,0),mgp=c(1.5,0.3,0),tck=-0.03)
par('cex.axis'= 1) 
par('cex'=1)
par('cex.main'=1)
ylab = 'Sedimentation (g/m2/yr)'
col = 'gray'
ylim = c(-80,0)

# for x axis
endlabel = length(variable_df$LakeAbbr) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)

# reorder data frame in ascending order for variable of interest

## Lake area
byArea = variable_df[order(variable_df$area_rank) , ]
labels = byArea$LakeAbbr

barplot(byArea$S_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Lake Area', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake volume
byVolume = variable_df[order(variable_df$volume_rank) , ]
labels = byVolume$LakeAbbr

barplot(byVolume$S_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Volume', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Perimeter
byPerim = variable_df[order(variable_df$perim_rank) , ]
labels = byPerim$LakeAbbr

barplot(byPerim$S_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Perimeter', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## res time
byResTime = variable_df[order(variable_df$restime_rank) , ]
labels = byResTime$LakeAbbr

barplot(byResTime$S_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Residence Time', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2)

## mean lake depth
byMeanDepth = variable_df[order(variable_df$depth_rank) , ]
labels = byMeanDepth$LakeAbbr

barplot(byMeanDepth$S_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Mean Depth', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## TP
byTP = variable_df[order(variable_df$TP_rank) , ]
labels = byTP$LakeAbbr

barplot(byTP$S_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='TP', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Secchi depth
bySecchi = variable_df[order(variable_df$Secchi_rank) , ]
labels = bySecchi$LakeAbbr

barplot(bySecchi$S_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Secchi', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake DOC
byDOC = variable_df[order(variable_df$DOC_rank) , ]
labels = byDOC$LakeAbbr

barplot(byDOC$S_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='DOC', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)
dev.off()

#################### Respiration #####################
## Static plot parameters
#par(mfrow=c(1,1)) #for single plot per window
png(paste0('R/ResultsViz/Figures/RespAlongLakeGradients.png'),width = 11,height = 9,units = 'in',res=300)
layout(matrix(c(1,2,3,4,5,6,7,8),2,4)) #8 plots with 2 rows x 4 columns
par(mar=c(2,3,2,0),mgp=c(1.5,0.3,0),tck=-0.03)
par('cex.axis'= 1) 
par('cex'=1)
par('cex.main'=1)
ylab = 'Respiration (g/m2/yr)'
col = 'gray'
ylim=c(-100,0)

# for x axis
endlabel = length(variable_df$LakeAbbr) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)

# reorder data frame in ascending order for variable of interest

## Lake area
byArea = variable_df[order(variable_df$area_rank) , ]
labels = byArea$LakeAbbr

barplot(byArea$R_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Lake Area', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake volume
byVolume = variable_df[order(variable_df$volume_rank) , ]
labels = byVolume$LakeAbbr

barplot(byVolume$R_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Volume', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Perimeter
byPerim = variable_df[order(variable_df$perim_rank) , ]
labels = byPerim$LakeAbbr

barplot(byPerim$R_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Perimeter', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## res time
byResTime = variable_df[order(variable_df$restime_rank) , ]
labels = byResTime$LakeAbbr

barplot(byResTime$R_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Residence Time', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2)

## mean lake depth
byMeanDepth = variable_df[order(variable_df$depth_rank) , ]
labels = byMeanDepth$LakeAbbr

barplot(byMeanDepth$R_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Mean Depth', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## TP
byTP = variable_df[order(variable_df$TP_rank) , ]
labels = byTP$LakeAbbr

barplot(byTP$R_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='TP', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Secchi depth
bySecchi = variable_df[order(variable_df$Secchi_rank) , ]
labels = bySecchi$LakeAbbr

barplot(bySecchi$R_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Secchi', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake DOC
byDOC = variable_df[order(variable_df$DOC_rank) , ]
labels = byDOC$LakeAbbr

barplot(byDOC$R_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='DOC', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)
dev.off()

#################### Allochthonous #####################
## Static plot parameters
#par(mfrow=c(1,1)) #for single plot per window
png(paste0('R/ResultsViz/Figures/AllochAlongLakeGradients.png'),width = 11,height = 9,units = 'in',res=300)
layout(matrix(c(1,2,3,4,5,6,7,8),2,4)) #8 plots with 2 rows x 4 columns
par(mar=c(2,3,2,0),mgp=c(1.5,0.3,0),tck=-0.03)
par('cex.axis'= 1) 
par('cex'=1)
par('cex.main'=1)
ylab = 'Allochthonous (g/m2/yr)'
col = 'gray'
ylim=c(0,300)

# for x axis
endlabel = length(variable_df$LakeAbbr) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)

# reorder data frame in ascending order for variable of interest

## Lake area
byArea = variable_df[order(variable_df$area_rank) , ]
labels = byArea$LakeAbbr

barplot(byArea$Alloch_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Lake Area', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake volume
byVolume = variable_df[order(variable_df$volume_rank) , ]
labels = byVolume$LakeAbbr

barplot(byVolume$Alloch_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Volume', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Perimeter
byPerim = variable_df[order(variable_df$perim_rank) , ]
labels = byPerim$LakeAbbr

barplot(byPerim$Alloch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Perimeter', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## res time
byResTime = variable_df[order(variable_df$restime_rank) , ]
labels = byResTime$LakeAbbr

barplot(byResTime$Alloch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Residence Time', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2)

## mean lake depth
byMeanDepth = variable_df[order(variable_df$depth_rank) , ]
labels = byMeanDepth$LakeAbbr

barplot(byMeanDepth$Alloch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Mean Depth', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## TP
byTP = variable_df[order(variable_df$TP_rank) , ]
labels = byTP$LakeAbbr

barplot(byTP$Alloch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='TP', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Secchi depth
bySecchi = variable_df[order(variable_df$Secchi_rank) , ]
labels = bySecchi$LakeAbbr

barplot(bySecchi$Alloch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Secchi', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake DOC
byDOC = variable_df[order(variable_df$DOC_rank) , ]
labels = byDOC$LakeAbbr

barplot(byDOC$Alloch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='DOC', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)
dev.off()

#################### Autochthonous #####################
## Static plot parameters
#par(mfrow=c(1,1)) #for single plot per window
png(paste0('R/ResultsViz/Figures/AutochAlongLakeGradients.png'),width = 11,height = 9,units = 'in',res=300)
layout(matrix(c(1,2,3,4,5,6,7,8),2,4)) #8 plots with 2 rows x 4 columns
par(mar=c(2,3,2,0),mgp=c(1.5,0.3,0),tck=-0.03)
par('cex.axis'= 1) 
par('cex'=1)
par('cex.main'=1)
ylab = 'Autochthonous (g/m2/yr)'
col = 'gray'
ylim=c(0,80)

# for x axis
endlabel = length(variable_df$LakeAbbr) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)

# reorder data frame in ascending order for variable of interest

## Lake area
byArea = variable_df[order(variable_df$area_rank) , ]
labels = byArea$LakeAbbr

barplot(byArea$Autoch_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Lake Area', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake volume
byVolume = variable_df[order(variable_df$volume_rank) , ]
labels = byVolume$LakeAbbr

barplot(byVolume$Autoch_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Volume', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Perimeter
byPerim = variable_df[order(variable_df$perim_rank) , ]
labels = byPerim$LakeAbbr

barplot(byPerim$Autoch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Perimeter', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## res time
byResTime = variable_df[order(variable_df$restime_rank) , ]
labels = byResTime$LakeAbbr

barplot(byResTime$Autoch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Residence Time', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2)

## mean lake depth
byMeanDepth = variable_df[order(variable_df$depth_rank) , ]
labels = byMeanDepth$LakeAbbr

barplot(byMeanDepth$Autoch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Mean Depth', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## TP
byTP = variable_df[order(variable_df$TP_rank) , ]
labels = byTP$LakeAbbr

barplot(byTP$Autoch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='TP', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Secchi depth
bySecchi = variable_df[order(variable_df$Secchi_rank) , ]
labels = bySecchi$LakeAbbr

barplot(bySecchi$Autoch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Secchi', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake DOC
byDOC = variable_df[order(variable_df$DOC_rank) , ]
labels = byDOC$LakeAbbr

barplot(byDOC$Autoch_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='DOC', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)
dev.off()

#################### Outflow #####################
## Static plot parameters
#par(mfrow=c(1,1)) #for single plot per window
png(paste0('R/ResultsViz/Figures/fOutAlongLakeGradients.png'),width = 11,height = 9,units = 'in',res=300)
layout(matrix(c(1,2,3,4,5,6,7,8),2,4)) #8 plots with 2 rows x 4 columns
par(mar=c(2,3,2,0),mgp=c(1.5,0.3,0),tck=-0.03)
par('cex.axis'= 1) 
par('cex'=1)
par('cex.main'=1)
ylab = 'Outflow (g/m2/yr)'
col = 'gray'
ylim=c(-200,0)

# for x axis
endlabel = length(variable_df$LakeAbbr) * 1.2 
tickedoff = seq(0.7, endlabel, 1.2)

# reorder data frame in ascending order for variable of interest

## Lake area
byArea = variable_df[order(variable_df$area_rank) , ]
labels = byArea$LakeAbbr

barplot(byArea$Out_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Lake Area', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake volume
byVolume = variable_df[order(variable_df$volume_rank) , ]
labels = byVolume$LakeAbbr

barplot(byVolume$Out_gm2y, xlab='', ylab=ylab, xaxt='n', ylim=ylim,
        main='Volume', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Perimeter
byPerim = variable_df[order(variable_df$perim_rank) , ]
labels = byPerim$LakeAbbr

barplot(byPerim$Out_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Perimeter', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## res time
byResTime = variable_df[order(variable_df$restime_rank) , ]
labels = byResTime$LakeAbbr

barplot(byResTime$Out_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Residence Time', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2)

## mean lake depth
byMeanDepth = variable_df[order(variable_df$depth_rank) , ]
labels = byMeanDepth$LakeAbbr

barplot(byMeanDepth$Out_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Mean Depth', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## TP
byTP = variable_df[order(variable_df$TP_rank) , ]
labels = byTP$LakeAbbr

barplot(byTP$Out_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='TP', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Secchi depth
bySecchi = variable_df[order(variable_df$Secchi_rank) , ]
labels = bySecchi$LakeAbbr

barplot(bySecchi$Out_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='Secchi', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)

## Lake DOC
byDOC = variable_df[order(variable_df$DOC_rank) , ]
labels = byDOC$LakeAbbr

barplot(byDOC$Out_gm2y, xlab='', ylab='', xaxt='n', ylim=ylim,
        main='DOC', col=col)
axis(side=1,at=c(tickedoff), labels=labels, las=2,cex.axis=1)
dev.off()
