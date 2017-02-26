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
  
  # From Paul Hanson, Jedi master, 1-6-17
  # Pipe: fOut > R + S
  # Processor: fOut < R + S
  # Source: R > S
  # Sink: R < S
  # Total Load: alloch + autoch = R - S + fOut + dStorage
  # dStorage = change in storage in water column
  
  # Calculate Change to total carbon stocks (pulled code from optim6)
  FinalPOC <- POC_df$POCtotal_conc_gm3 + (POC_df$POCload_g - POC_df$POCout_g)/volume #g/m3
  FinalDOC <- DOC_df$DOCtotal_conc_gm3 + (DOC_df$DOCload_g - DOC_df$DOCout_g)/volume #g/m3
 
  DeltaPOC <- FinalPOC*volume - POC_df$POCtotal_conc_gm3[1]*volume #g
  DeltaDOC <- FinalDOC*volume - DOC_df$DOCtotal_conc_gm3[1]*volume #g
  #Mass balance check (should be near zero)
  POCcheck <- sum(POC_df$POCload_g - POC_df$POCout_g) - DeltaPOC #g
  DOCcheck <- sum(DOC_df$DOCload_g - DOC_df$DOCout_g) - DeltaDOC #g
  
  dStorage = (DeltaPOC/area/(nrow(POC_df)/365)) + (DeltaDOC/area/(nrow(DOC_df)/365)) #g/m2/yr
  
  PipeProc = R - S
  Budget_left = alloch + autoch
  Budget_right = R - S + fOut + dStorage # should equal Budget_left

  Date = as.Date(DOC_df$Date)
  Year = as.factor(year(Date))
  summary = data.frame(Date=Date,Year=Year,Alloch_gm2y=alloch,Autoch_gm2y=autoch,
                       R_gm2y=R,S_gm2y=S,Out_gm2y=fOut,PipeProc_gm2y=PipeProc,dStorage_gm2y=dStorage,
                       Budget_left_gm2y=Budget_left,Budget_right_gm2y=Budget_right)
  return(summary)
}

#### run function over lakes ####
Harp = SOS_fate('Harp')
Monona = SOS_fate('Monona')
Toolik = SOS_fate('Toolik')
Trout = SOS_fate('Trout')
Vanern = SOS_fate('Vanern')

############# plotting time series ##########
# compare R to S
# Source: R > S, Sink: R < S
png(paste0('R/ResultsViz/Figures/SOSfates.png'),width = 11,height = 9,units = 'in',res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1)) #bot, left, top, right, def=c(5.1, 4.1, 4.1, 2.1)
#par(mfrow=c(1,1))
par(mfrow=c(3,2))
lty = 2
lwd = 2
ylab = 'OC (g/m2/yr)'
xlab = 'Date'
ylim = c(-50,400)
cex.main = 2
cex.axis = 2
cex.lab = 2
colors=c('gray','black','gold')

# Harp
plot(Budget_left_gm2y ~ Date, Harp, type='h', main='Harp', col=colors[1], ylim=ylim, ylab=ylab, cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
par(new=T)
plot(S_gm2y ~ Date, Harp, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[2], lwd=4)
par(new=T)
plot(R_gm2y ~ Date, Harp, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[3], lwd=4)
legend('topright', lwd=lwd, legend=c('Total Load','Sed','Resp'), col=colors)
abline(0,0,lty=lty, lwd=lwd)

# Monona
plot(Budget_left_gm2y ~ Date, Monona, type='h', main='Monona', col=colors[1], ylim=ylim, ylab=ylab, cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
par(new=T)
plot(S_gm2y ~ Date, Monona, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[2], lwd=4)
par(new=T)
plot(R_gm2y ~ Date, Monona, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[3], lwd=4)
#legend('topright', lwd=lwd, legend=c('Total Load','Sed','Resp'), col=colors)
abline(0,0,lty=lty, lwd=lwd)

# Toolik
plot(Budget_left_gm2y ~ Date, Toolik, type='h', main='Toolik', col=colors[1], ylim=ylim, ylab=ylab, cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
par(new=T)
plot(S_gm2y ~ Date, Toolik, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[2], lwd=4)
par(new=T)
plot(R_gm2y ~ Date, Toolik, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[3], lwd=4)
#legend('topright', lwd=lwd, legend=c('Total Load','Sed','Resp'), col=colors)
abline(0,0,lty=lty, lwd=lwd)

# Trout
plot(Budget_left_gm2y ~ Date, Trout, type='h', main='Trout', col=colors[1], ylim=ylim, ylab=ylab, cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
par(new=T)
plot(S_gm2y ~ Date, Trout, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[2], lwd=4)
par(new=T)
plot(R_gm2y ~ Date, Trout, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[3], lwd=4)
#legend('topright', lwd=lwd, legend=c('Total Load','Sed','Resp'), col=colors)
abline(0,0,lty=lty, lwd=lwd)

# Vanern
plot(Budget_left_gm2y ~ Date, Vanern, type='h', main='Vanern', col=colors[1], ylim=ylim, ylab=ylab, cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
par(new=T)
plot(S_gm2y ~ Date, Vanern, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[2], lwd=4)
abline(0,0,lty=lty, lwd=lwd)
par(new=T)
plot(R_gm2y ~ Date, Vanern, type='h', xaxt='n',yaxt='n', xlab='',ylab='', ylim=ylim, col=colors[3], lwd=4)
#legend('topright', lwd=lwd, legend=c('Total Load','Sed','Resp'), col=colors)
dev.off()

# compare export to difference between R and S to determine pipe or processor of OC
# Pipe: Export > R + S | Processor: Export < R + S
png(paste0('R/ResultsViz/Figures/POPfates.png'),width = 11,height = 9,units = 'in',res=300)
par(mar=c(5.1, 5.1, 4.1, 2.1)) #bot, left, top, right, def=c(5.1, 4.1, 4.1, 2.1)
#par(mfrow=c(1,1))
par(mfrow=c(3,2))
lty = 2
lwd = 2
ylab = 'OC (g/m2/yr)'
xlab = 'Date'
ylim = c(-50,200)
cex.main = 2
cex.axis = 2
cex.lab = 2
colors=c('black','gold')

# Harp
plot(PipeProc_gm2y ~ Date, Harp, type='l', main='Harp', ylim=ylim, ylab=ylab, col=colors[1], cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
#abline(0,0,lty=lty, lwd=lwd)
par(new=T)
plot(Out_gm2y ~ Date, Harp, type='l', yaxt='n', xaxt='n', xlab='', ylab='', col=colors[2])
legend('topright', lwd=lwd, legend=c('R + S','Export'), col=colors)
#mtext(side=3, 'Pipe: Export > R + S | Processor: Export < R + S')

# Monona
plot(PipeProc_gm2y ~ Date, Monona, type='l', main='Monona', ylim=ylim, ylab=ylab, col=colors[1], cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
#abline(0,0,lty=lty, lwd=lwd)
par(new=T)
plot(Out_gm2y ~ Date, Monona, type='l', yaxt='n', xaxt='n', xlab='', ylab='', col=colors[2])
legend('topright', lwd=lwd, legend=c('R + S','Export'), col=colors)
#mtext(side=3, 'Pipe: Export > R + S | Processor: Export < R + S')

# Toolik
plot(PipeProc_gm2y ~ Date, Toolik, type='l', main='Toolik', ylim=ylim, ylab=ylab, col=colors[1], cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
#abline(0,0,lty=lty, lwd=lwd)
par(new=T)
plot(Out_gm2y ~ Date, Toolik, type='l', yaxt='n', xaxt='n', xlab='', ylab='', col=colors[2])
legend('topright', lwd=lwd, legend=c('R + S','Export'), col=colors)
#mtext(side=3, 'Pipe: Export > R + S | Processor: Export < R + S')

# Trout
plot(PipeProc_gm2y ~ Date, Trout, type='l', main='Trout', ylim=ylim, ylab=ylab, col=colors[1], cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
#abline(0,0,lty=lty, lwd=lwd)
par(new=T)
plot(Out_gm2y ~ Date, Trout, type='l', yaxt='n', xaxt='n', xlab='', ylab='', col=colors[2])
legend('topright', lwd=lwd, legend=c('R + S','Export'), col=colors)
#mtext(side=3, 'Pipe: Export > R + S | Processor: Export < R + S')

# Vanern
plot(PipeProc_gm2y ~ Date, Vanern, type='l', main='Vanern', ylim=ylim, ylab=ylab, col=colors[1], cex.main=cex.main, cex.axis=cex.axis, cex.lab=cex.lab)
#abline(0,0,lty=lty, lwd=lwd)
par(new=T)
plot(Out_gm2y ~ Date, Vanern, type='l', yaxt='n', xaxt='n', xlab='', ylab='', col=colors[2])
legend('topright', lwd=lwd, legend=c('R + S','Export'), col=colors)
#mtext(side=3, 'Pipe: Export > R + S | Processor: Export < R + S')
dev.off()


#### calculate annual statistics ####
Vanern_annual = aggregate(Vanern$PipeProc_gm2y, by=list(Vanern$Year), FUN='mean')
colnames(Vanern_annual) = c('Year','mean')
Vanern_annual$Lake = rep('Vanern',nrow(Vanern_annual))
Harp_annual = aggregate(Harp$PipeProc_gm2y, by=list(Harp$Year), FUN='mean')
colnames(Harp_annual) = c('Year','mean')
Harp_annual$Lake = rep('Harp',nrow(Harp_annual))
Trout_annual = aggregate(Trout$PipeProc_gm2y, by=list(Trout$Year), FUN='mean')
colnames(Trout_annual) = c('Year','mean')
Trout_annual$Lake = rep('Trout',nrow(Trout_annual))
Toolik_annual = aggregate(Toolik$PipeProc_gm2y, by=list(Toolik$Year), FUN='mean')
colnames(Toolik_annual) = c('Year','mean')
Toolik_annual$Lake = rep('Toolik',nrow(Toolik_annual))
Monona_annual = aggregate(Monona$PipeProc_gm2y, by=list(Monona$Year), FUN='mean')
colnames(Monona_annual) = c('Year','mean')
Monona_annual$Lake = rep('Monona',nrow(Monona_annual))

# merge into single data frame
All_lakes_annual = rbind.data.frame(Harp_annual, Monona_annual, Toolik_annual, Trout_annual, Vanern_annual)

#### boxplot of annual mean OC by lake ####
png(paste0('R/ResultsViz/Figures/AnnualNetOCBoxplot.png'),width = 11,height = 9,units = 'in',res=300)
par(mfrow=c(1,1))
tick_seq = seq(-150,75, by=25)
cex.axis = 1.5

boxplot(mean ~ Lake, data=All_lakes_annual, axes=F, ann=F, main='Annual Net OC (g/m2/yr)', ylim=c(-75,75))
mtext(side=3, 'Net = R - S')
axis(1, at = 1:5, labels = levels(as.factor(All_lakes_annual$Lake)), cex.axis = cex.axis, tick=F)
axis(2, at=tick_seq, label=rep('',length(tick_seq),cex.axis = cex.axis, tick=F))
axis(2, at=tick_seq, line=0.5, lwd=0, cex.axis=1.5, las=2) #las=2 for horizontal y axis label
box()
abline(0,0, lty=2, lwd=1.5)
dev.off()

