#Read in results data from SOS_CentralFunction.R and visualize output. 
# Updated 12-16-16 by Ian McC (included new column names from input data frames)

#User input lakename
LakeName = 'Harp'

#Read in results data from SOS Carbon Flux Model
DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')
DOC_df <- read.csv(DOC_results_filename)
POC_df <- read.csv(POC_results_filename)
#SOS <- read.csv(SOS_results_filename) #file no longer exists
#ParameterFile <- read.csv(paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep=''),sep='\t')
ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
for (i in 1:nrow(parameters)){ # assign parameters
  assign(parameters[i,1],parameters[i,2])
} # reads in unnecessary parameters, but not a big deal

volume = LakeVolume
area = LakeArea

# 
# #Plot POC and DOC fluxes in standardized units (g/m2/yr)
# ylabelPOC <- c("GPP POC In (g/m2/yr)","Flow POC In (g/m2/yr)","Flow POC Out (g/m2/yr)","Sed POC Out (g/m2/yr)")
# ylabelDOC <- c("GPP DOC In (g/m2/yr)","Flow DOC In (g/m2/yr)","Flow  DOC Out (g/m2/yr)","Respiration DOC Out (g/m2/yr)","Leach In (g/m2/yr)")
# 
# par(mfrow=c(2,2),mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02)
# for (n in 1:4){
#   plot(POC_df[,1],POC_df[,n+2],xlab='Date',ylab=ylabelPOC[n],type='l')
# }
# 
# par(mfrow=c(3,2),mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02)
# for (n in 1:5){
#   plot(DOC_df[,1],DOC_df[,n+2],xlab='Date',ylab=ylabelDOC[n],type='l')
# }
# 
# #POC and DOC concentration in time (g/m3)
# par(mfrow=c(2,1),mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02,cex=0.8)
# plot(POC_df[,1],POC_df$POC_conc_gm3,xlab='Date',ylab="POC Conc (g/m3)",type="l")
# # Better axes tick marks 
# #   plotDates = seq(OutputTimeSeries[1],tail(OutputTimeSeries,1), by="year")
# #   axis.Date(1,at=plotDates,labels=format(plotDates,"%m/%y"),las=1,cex.axis = 0.8)
# plot(DOC_df[,1],DOC_df$DOC_conc_gm3,xlab='Date',ylab="DOC Conc (g/m3)",type="l")
# 
# #Plot cumulative fates
# par(mfrow=c(2,2),mar=c(2.5,3,1,1),mgp=c(1.5,0.3,0),tck=-0.02,cex=0.8)
# plot(POC_df[,1],POC_df$POC_flowOut_gm2y,xlab='Date',ylab="Cumulative POC Outflow (g)",type='l')
# plot(POC_df[,1],POC_df$POC_sedOut_gm2y,xlab='Date',ylab="Cumulative POC Sed Burial (g)",type='l')
# plot(DOC_df[,1],DOC_df$DOC_flowOut_gm2y,xlab='Date',ylab="Cumulative DOC Outflow (g)",type='l')
# plot(DOC_df[,1],DOC_df$DOC_respOut_gm2y,xlab='Date',ylab="Cumulative DOC Respired (g)",type='l')
# 
# #Plot net SOS
# par(mfrow=c(1,1),mar=c(3,3,2,1),mgp=c(1.5,0.3,0),tck=-0.01,cex=0.8)
# plot(POC_df[,1],SOS$Net/1000,xlab='date/time',ylab='OC mass (kg/d)',
#      main='Net OC Mass Sunk per Day',type='l')
# #   plotDates = seq(OutputTimeSeries[1],tail(OutputTimeSeries,1), by="year")
# #   axis.Date(1,at=plotDates,labels=format(plotDates,"%m/%y"),las=1,cex.axis = 0.8)

# 
# ######## Stacked histrogram of DOC fluxes in and out of the system ######## 
# png(paste0('R/ResultsViz/Figures/DOCfates_',LakeName,'.png'),width = 7,height = 5,units = 'in',res=300)
# par(mar=c(3,3,2,3),mgp=c(1.5,0.5,0),tck=-0.02,cex=0.8)
# # Fluxes in
# plot(DOC_df$Date,DOC_df$FlowIn_gm2y+DOC_df$GPPin_gm2y+DOC_df$leachIn_gm2y,type='h',col='goldenrod',
#      ylim=c(-150,150),ylab='DOC flux (gm2/y)',xlab='',main=LakeName) # Leaching 
# lines(DOC_df$Date,DOC_df$FlowIn_gm2y+DOC_df$GPPin_gm2y,type='h',col='darkblue') # FLow
# lines(DOC_df$Date,DOC_df$GPPin_gm2y,type='h',col='cyan4') # GPP
# # Fluxes out 
# lines(DOC_df$Date,-DOC_df$FlowOut_gm2y-DOC_df$respOut_gm2y,type='h',col='green4') # Flow
# lines(DOC_df$Date,-DOC_df$respOut_gm2y,type='h',col='red4') # Respiration
# 
# par(new=T) # Plot DOC concentration on separate yaxis 
# plot(DOC_df$Date,DOC_df$DOC_conc_gm3,type='l',yaxt='n',ylab='',xlab='',lwd=1.5)
# axis(side = 4)
# mtext('DOC concentration (g/m3)',side = 4,line = 1.5,cex=0.8)
# 
# legend('topleft',legend = c('Leaching In','Flow In','GPP','Respiration','Flow Out','DOC Conc'),
#        col = c('goldenrod','darkblue','cyan4','red4','green4','black'),pch=c(15,15,15,15,15,NA),
#        lty=c(0,0,0,0,0,1),lwd=2,ncol=2,cex=1,pt.cex=2,seg.len=1,
#        inset=0.01)
# dev.off()
# ############################################################################
# 
# 
# # ######## Stacked histrogram of POC fluxes in and out of the system ######## 
# png(paste0('R/ResultsViz/Figures/POCfates_',LakeName,'.png'),width = 7,height = 5,units = 'in',res=300)
# par(mar=c(3,3,2,3),mgp=c(1.5,0.5,0),tck=-0.02,cex=0.8)
# # Fluxes in
# plot(POC_df$Date,POC_df$FlowIn_gm2y+POC_df$GPPin_gm2y,type='h',col='darkblue',
#      ylim=c(-60,60),ylab='POC flux (gm2/y)',xlab='',main=LakeName) # Flow 
# lines(POC_df$Date,POC_df$GPPin_gm2y,type='h',col='cyan4') # GPP IN
# # Fluxes out 
# lines(POC_df$Date,-POC_df$FlowOut_gm2y-POC_df$sedOut_gm2y-POC_df$leachOut_gm2y,type='h',col='goldenrod') # Leach
# lines(POC_df$Date,-POC_df$FlowOut_gm2y-POC_df$sedOut_gm2y,type='h',col='brown') # Sedimentation
# lines(POC_df$Date,-POC_df$FlowOut_gm2y,type='h',col='green4') # Flow Out
# 
# par(new=T) # Plot POC concentration on separate yaxis 
# plot(POC_df$Date,POC_df$POC_conc_gm3,type='l',ylim=c(-0.7,0.25),yaxt='n',xaxt='n',ylab='',xlab='',lwd=1.5)
# axis(side = 4)
# mtext('POC concentration (g/m3)',side = 4,line = 1.5,cex=0.8)
# 
# legend('topleft',legend = c('Flow In','GPP In','Leaching Out','Sedimentation','Flow Out','POC Conc'),
#        col = c('darkblue','cyan4','goldenrod','brown','green4','black'),pch=c(15,15,15,15,15,NA),
#        lty=c(0,0,0,0,0,1),lwd=2,ncol=2,cex=1,pt.cex=2,seg.len=1,
#        inset=0.01)
# dev.off()
############################################################################

# ######## Stacked histrogram of OC fluxes in and out of the system ######## 
png(paste0('R/ResultsViz/Figures/OCfates_',LakeName,'.png'),width = 7,height = 5,units = 'in',res=300)
par(mar=c(3,3,2,3),mgp=c(1.5,0.5,0),tck=-0.02,cex=0.8)

# Fluxes in
ocInflow = POC_df$FlowIn_gm2y + DOC_df$FlowIn_gm2y
ocGPP = POC_df$NPPin_gm2y + DOC_df$NPPin_gm2y

plot(POC_df$Date,ocInflow + ocGPP,col='darkblue',type='l',
     ylim=c(-200,200),ylab='OC flux (gm2/y)',xlab='',main=LakeName) # Inflow 
# Flow 
lines(POC_df$Date,ocInflow + ocGPP,col='darkblue',type='h') #inflow
lines(POC_df$Date,ocGPP,type='h',col='cyan4') # GPP in

# Fluxes out 
ocOutflow = -POC_df$FlowOut_gm2y -DOC_df$FlowOut_gm2y
ocSed = -POC_df$sedOut_gm2y
ocResp = -DOC_df$respOut_gm2y

  
lines(DOC_df$Date,ocOutflow + ocSed + ocResp ,type='h',col='green4') # Flow
lines(DOC_df$Date,ocSed + ocResp ,type='h',col='brown') # Sedimentation
lines(DOC_df$Date,ocResp ,type='h',col='grey50') # Respiration

#par(new=T) # Plot OC concentration on separate yaxis 
#plot(DOC_df$Date,DOC_df$DOCtotal_conc_gm3+POC_df$POCtotal_conc_gm3,type='l',yaxt='n',ylab='',xlab='',lwd=1.5)
#axis(side = 4)
#mtext('OC concentration (g/m3)',side = 4,line = 1.5,cex=0.8)

legend('topleft',legend = c('Inflow','Autoch','Outflow','Burial','Respiration'),#,'DOC Conc'),
       col = c('darkblue','cyan4','green4','brown','grey50'),#,'black'),
       pch=c(15,15,15,15,15,NA),
       lty=c(0,0,0,0,0,1),lwd=2,ncol=2,cex=1,pt.cex=2,seg.len=1,
       inset=0.01)
dev.off()

# balance fates
ocLeachOut = -POC_df$leachOut_gm2y
ocLake = (DOC_df$DOCtotal_conc_gm3+POC_df$POCtotal_conc_gm3) * volume/area
ocDelta = tail(ocLake,1) - ocLake[1]

# Fluxes out of the lake are already negative 
sum(ocInflow + ocGPP +  ocOutflow + ocResp + ocSed )/365 - ocDelta

