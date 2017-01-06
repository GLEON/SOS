################################################################
# Time series plots of SOS fates 
# Date: 1-3-17
# Updated: 1-5-17
# Author: Ian McC, adapted from Hilary's SOS_mean.R code (HD makes some mean R code)
################################################################

library(lubridate)

#### define function ####
SOS_fate = function(LakeName){
  #Lakename = lake name in quotes
  #Read in results data from SOS Carbon Flux Model
  DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
  POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
  #SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')
  
  DOC_df <- read.csv(DOC_results_filename)
  POC_df <- read.csv(POC_results_filename)
  #SOS <- read.csv(SOS_results_filename)
  
  ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
  parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  for (i in 1:nrow(parameters)){ # assign parameters
    assign(parameters[i,1],parameters[i,2])
  }
  
  volume = LakeVolume
  area = LakeArea
  
  #alloch<-POC_df$POCalloch_g+DOC_df$DOCalloch_g
  #autoch<-POC_df$POCautoch_g+DOC_df$DOCautoch_g
  
  R<-DOC_df$respOut_gm2y
  
  S<-POC_df$sedOut_gm2y
  
  fOut<-(POC_df$leachOut_gm2y + POC_df$FlowOut_gm2y+ DOC_df$FlowOut_gm2y)
  
  alloch<-DOC_df$FlowIn_gm2y+DOC_df$leachIn_gm2y+POC_df$FlowIn_gm2y
  autoch<-POC_df$NPPin_gm2y+DOC_df$NPPin_gm2y
  #Sink = S
  # this is the landscape ecologist's guess on what SOS is
  # intuitively, the budget is inputs + internal processing = outputs (net)
  # if net exceeds inputs + internal processing, the lake is a source
  Inputs = alloch
  InternalProc = autoch - S #may be negative if S exceeds autoch
  Net = Inputs + InternalProc
  Date = as.Date(DOC_df$Date)
  Year = as.factor(year(Date))
  summary = data.frame(Date=Date,Year=Year,Alloch_gm2y=alloch,Autoch_gm2y=autoch,
                       R_gm2y=R,S_gm2y=S,Out_gm2y=fOut,Inputs=Inputs,
                       InternalProc=InternalProc,Net=Net)
  return(summary)
}

#### run function over lakes ####
Harp = SOS_fate('Harp')
Monona = SOS_fate('Monona')
Toolik = SOS_fate('Toolik')
Trout = SOS_fate('Trout')
Vanern = SOS_fate('Vanern')

#### plotting time series ####
png(paste0('R/ResultsViz/Figures/SOSfates.png'),width = 11,height = 9,units = 'in',res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1)) #bot, left, top, right, def=c(5.1, 4.1, 4.1, 2.1)
#par(mfrow=c(1,1))
par(mfrow=c(3,2))
lty = 2
lwd = 2
ylab = 'OC (g/m2/yr)'
xlab = 'Date'
ylim = c(-100,400)
cex.main = 2
cex.axis = 2
cex.lab = 2

plot(Net ~ Date, Harp, type='l',main='Harp',ylim=ylim,xlab=xlab,ylab=ylab,cex.main=cex.main,cex.axis=cex.axis,cex.lab=cex.lab)
abline(0,0,lty=lty, lwd=lwd)

plot(Net ~ Date, Monona, type='l',main='Monona',ylim=ylim,xlab=xlab,ylab=ylab,cex.main=cex.main,cex.axis=cex.axis,cex.lab=cex.lab)
abline(0,0,lty=lty, lwd=lwd)

plot(Net ~ Date, Toolik, type='l',main='Toolik',ylim=ylim,xlab=xlab,ylab=ylab,cex.main=cex.main,cex.axis=cex.axis,cex.lab=cex.lab)
abline(0,0,lty=lty, lwd=lwd)

plot(Net ~ Date, Trout, type='l',main='Trout',ylim=ylim,xlab=xlab,ylab=ylab,cex.main=cex.main,cex.axis=cex.axis,cex.lab=cex.lab)
abline(0,0,lty=lty, lwd=lwd)

plot(Net ~ Date, Vanern, type='l',main='Vanern',ylim=ylim,xlab=xlab,ylab=ylab,cex.main=cex.main,cex.axis=cex.axis,cex.lab=cex.lab)
abline(0,0,lty=lty, lwd=lwd)
dev.off()

#### calculate annual statistics ####
Vanern_annual = aggregate(Vanern$Net, by=list(Vanern$Year), FUN='mean')
colnames(Vanern_annual) = c('Year','mean')
Vanern_annual$Lake = rep('Vanern',nrow(Vanern_annual))
Harp_annual = aggregate(Harp$Net, by=list(Harp$Year), FUN='mean')
colnames(Harp_annual) = c('Year','mean')
Harp_annual$Lake = rep('Harp',nrow(Harp_annual))
Trout_annual = aggregate(Trout$Net, by=list(Trout$Year), FUN='mean')
colnames(Trout_annual) = c('Year','mean')
Trout_annual$Lake = rep('Trout',nrow(Trout_annual))
Toolik_annual = aggregate(Toolik$Net, by=list(Toolik$Year), FUN='mean')
colnames(Toolik_annual) = c('Year','mean')
Toolik_annual$Lake = rep('Toolik',nrow(Toolik_annual))
Monona_annual = aggregate(Monona$Net, by=list(Monona$Year), FUN='mean')
colnames(Monona_annual) = c('Year','mean')
Monona_annual$Lake = rep('Monona',nrow(Monona_annual))

# merge into single data frame
All_lakes_annual = rbind.data.frame(Harp_annual, Monona_annual, Toolik_annual, Trout_annual, Vanern_annual)

#### boxplot of annual mean OC by lake ####
png(paste0('R/ResultsViz/Figures/AnnualNetOCBoxplot.png'),width = 11,height = 9,units = 'in',res=300)
par(mfrow=c(1,1))
tick_seq = seq(0,150, by=25)
cex.axis = 1.5

boxplot(mean ~ Lake, data=All_lakes_annual, axes=F, ann=F, main='Annual Net OC (g/m2/yr)')
axis(1, at = 1:5, labels = levels(as.factor(All_lakes_annual$Lake)), cex.axis = cex.axis, tick=F)
axis(2, at=tick_seq, label=rep('',length(tick_seq),cex.axis = cex.axis, tick=F))
axis(2, at=tick_seq, line=0.5, lwd=0, cex.axis=1.5, las=2) #las=2 for horizontal y axis label
box()
abline(0,0, lty=2, lwd=1.5)
dev.off()

