#Read in results data from SOS_CentralFunction.R and visualize output. 

#User input lake name
LakeName = 'Vanern'

#Read in results data from SOS Carbon Flux Model
DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')
DOC_df <- read.csv(DOC_results_filename)
POC_df <- read.csv(POC_results_filename)
SOS <- read.csv(SOS_results_filename)

#Plot POC and DOC fluxes in standardized units (g/m2/yr)
ylabelPOC <- c("NPP POC In (g/m2/yr)","Flow POC In (g/m2/yr)","Flow POC Out (g/m2/yr)","Sed POC Out (g/m2/yr)")
ylabelDOC <- c("NPP DOC In (g/m2/yr)","Flow DOC In (g/m2/yr)","Flow  DOC Out (g/m2/yr)","Respiration DOC Out (g/m2/yr)","Leach In (g/m2/yr)")

par(mfrow=c(2,2),mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02)
for (n in 1:4){
  plot(POC_df[,1],POC_df[,n+2],xlab='Date',ylab=ylabelPOC[n],type='l')
}

par(mfrow=c(3,2),mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02)
for (n in 1:5){
  plot(DOC_df[,1],DOC_df[,n+2],xlab='Date',ylab=ylabelDOC[n],type='l')
}

#POC and DOC concentration in time (g/m3)
par(mfrow=c(2,1),mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02,cex=0.8)
plot(POC_df[,1],POC_df$POC_conc_gm3,xlab='Date',ylab="POC Conc (g/m3)",type="l")
# Better axes tick marks 
#   plotDates = seq(OutputTimeSeries[1],tail(OutputTimeSeries,1), by="year")
#   axis.Date(1,at=plotDates,labels=format(plotDates,"%m/%y"),las=1,cex.axis = 0.8)
plot(DOC_df[,1],DOC_df$DOC_conc_gm3,xlab='Date',ylab="DOC Conc (g/m3)",type="l")

#Plot cumulative fates
par(mfrow=c(2,2),mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02,cex=0.8)
plot(POC_df[,1],POC_df$POC_flowOut_gm2y,xlab='Date',ylab="Cumulative POC Outflow (g)",type='l')
plot(POC_df[,1],POC_df$POC_sedOut_gm2y,xlab='Date',ylab="Cumulative POC Sed Burial (g)",type='l')
plot(DOC_df[,1],DOC_df$DOC_flowOut_gm2y,xlab='Date',ylab="Cumulative DOC Outflow (g)",type='l')
plot(DOC_df[,1],DOC_df$DOC_respOut_gm2y,xlab='Date',ylab="Cumulative DOC Respired (g)",type='l')

#Plot net SOS
par(mfrow=c(1,1),mar=c(3,3,2,1),mgp=c(1.5,0.3,0),tck=-0.01,cex=0.8)
plot(POC_df[,1],SOS$Net/1000,xlab='date/time',ylab='OC mass (kg/d)',
     main='Net OC Mass Sunk per Day',type='l')
#   plotDates = seq(OutputTimeSeries[1],tail(OutputTimeSeries,1), by="year")
#   axis.Date(1,at=plotDates,labels=format(plotDates,"%m/%y"),las=1,cex.axis = 0.8)


######## Stacked histrogram of DOC fluxes in and out of the system ######## 
par(mar=c(3,3,2,3),mgp=c(1.5,0.5,0),tck=-0.02,cex=0.8)
# Fluxes in
plot(DOC_df$Date,DOC_df$FlowIn_gm2y+DOC_df$NPPin_gm2y+DOC_df$leachIn_gm2y,type='h',col='goldenrod',
     ylim=c(-150,150),ylab='DOC flux (gm2/y)',xlab='',main=LakeName) # Leaching 
lines(DOC_df$Date,DOC_df$FlowIn_gm2y+DOC_df$NPPin_gm2y,type='h',col='darkblue') # NPP
lines(DOC_df$Date,DOC_df$NPPin_gm2y,type='h',col='cyan4') # Flow
# Fluxes out 
lines(DOC_df$Date,-DOC_df$FlowOut_gm2y-DOC_df$respOut_gm2y,type='h',col='green4') # Flow
lines(DOC_df$Date,-DOC_df$respOut_gm2y,type='h',col='red4') # Respiration

par(new=T) # Plot DOC concentration on separate yaxis 
plot(DOC_df$Date,DOC_df$DOC_conc_gm3,type='l',ylim=c(2,5),yaxt='n',ylab='',xlab='')
axis(side = 4)
mtext('DOC concentration (g/m3)',side = 4,line = 1.5,cex=0.8)

legend('topleft',legend = c('Leaching In','NPP','Flow In','Respiration','Flow Out'),
       fill = c('goldenrod','darkblue','cyan4','green4','red4'),ncol=2,cex=0.8)
