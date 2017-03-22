#Read in results data from SOS_CentralFunction.R and visualize output. 
# Updated 3-22-17 by Ian McC 

setwd('C:/Users/immcc/Desktop/SOS/')

library(dplyr)

#User input lakename
#LakeName = 'Harp'

#### define function ####
flux_plot = function(LakeName, ylim1, ylim2, legend){
  #LakeName = quoted lake name
  #ylim1 = bottom y axis bound
  #ylim2 = high y axis bound
  #legend = 1 if want legend
  
  #Read in results data from SOS Carbon Flux Model
  DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
  POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
  SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')
  DOC_df <- read.csv(DOC_results_filename)
  POC_df <- read.csv(POC_results_filename)
  
  mean(DOC_df$DOCalloch_g) + mean(POC_df$POCalloch_g)
  mean(DOC_df$DOCautoch_g) + mean(POC_df$POCautoch_g)
  
  ### aggregate daily to monthly
  DOC_df$Date = as.Date(DOC_df$Date)
  POC_df$Date = as.Date(POC_df$Date)
  
  #poc
  Month_POC = POC_df %>%
    mutate(month = as.numeric(format(Date, "%m")), year = as.numeric(format(Date, "%Y")), day = 1) %>%
    group_by(month, year) %>%
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    ungroup()
    
  month_day_year = as.Date(paste0(Month_POC$month,'/',Month_POC$day,'/',Month_POC$year), format = "%m/%d/%Y")
  POC_FlowIn_gm2y = Month_POC$FlowIn_gm2y
  POC_NPPin_gm2y = Month_POC$NPPin_gm2y
  POC_FlowOut_gm2y = Month_POC$FlowOut_gm2y
  POC_sedOut_gm2y = Month_POC$sedOut_gm2y
 
  POC_df = data.frame(Date=month_day_year, FlowIn_gm2y=POC_FlowIn_gm2y, NPPin_gm2y=POC_NPPin_gm2y,
                      FlowOut_gm2y=POC_FlowOut_gm2y, sedOut_gm2y=POC_sedOut_gm2y)
  POC_df = dplyr::arrange(POC_df, Date)
  
  #DOC
  #poc
  Month_DOC = DOC_df %>%
    mutate(month = as.numeric(format(Date, "%m")), year = as.numeric(format(Date, "%Y")), day = 1) %>%
    group_by(month, year) %>%
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    ungroup()
  
  month_day_year = as.Date(paste0(Month_DOC$month,'/',Month_DOC$day,'/',Month_DOC$year), format = "%m/%d/%Y")
  DOC_FlowIn_gm2y = Month_DOC$FlowIn_gm2y
  DOC_NPPin_gm2y = Month_DOC$NPPin_gm2y
  DOC_FlowOut_gm2y = Month_DOC$FlowOut_gm2y
  DOC_respOut_gm2y= Month_DOC$respOut_gm2y
  
  DOC_df = data.frame(Date=month_day_year, FlowIn_gm2y=DOC_FlowIn_gm2y, NPPin_gm2y=DOC_NPPin_gm2y,
                      FlowOut_gm2y=DOC_FlowOut_gm2y, respOut_gm2y=DOC_respOut_gm2y)
  DOC_df = dplyr::arrange(DOC_df, Date)
  
  #SOS <- read.csv(SOS_results_filename) #file no longer exists
  #ParameterFile <- read.csv(paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep=''),sep='\t')
  ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
  parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  for (i in 1:nrow(parameters)){ # assign parameters
    assign(parameters[i,1],parameters[i,2])
  } # reads in unnecessary parameters, but not a big deal
  
  # Fluxes in
  ocInflow = POC_df$FlowIn_gm2y + DOC_df$FlowIn_gm2y
  ocGPP = POC_df$NPPin_gm2y + DOC_df$NPPin_gm2y
  
  plot(POC_df$Date,ocInflow + ocGPP,col='darkblue',type='l',
       ylim=c(ylim1,ylim2),ylab='OC flux (gm2/y)',xlab='',main=LakeName) # Inflow 
  # Flow 
  # lines(POC_df$Date,ocInflow + ocGPP,col='darkblue',type='l') #inflow
  # lines(POC_df$Date,ocGPP,type='l',col='cyan4') # GPP in
  polygon(x = c(POC_df$Date,rev(POC_df$Date)),y = c(ocGPP,rep(0,length(ocGPP))),col = 'cyan4')
  polygon(x = c(POC_df$Date,rev(POC_df$Date)),y = c(ocInflow + ocGPP,rev(ocGPP)),col = 'darkblue')
  
  # Fluxes out 
  ocOutflow = -POC_df$FlowOut_gm2y -DOC_df$FlowOut_gm2y
  ocSed = -POC_df$sedOut_gm2y
  ocResp = -DOC_df$respOut_gm2y
  
  polygon(x = c(POC_df$Date,rev(POC_df$Date)),y = c(ocResp,rep(0,length(ocResp))),col = 'grey50')
  polygon(x = c(POC_df$Date,rev(POC_df$Date)),y = c(ocSed + ocResp,rev(ocResp)),col = 'brown')
  polygon(x = c(POC_df$Date,rev(POC_df$Date)),y = c(ocOutflow + ocSed + ocResp,rev(ocSed + ocResp)),col = 'green4')
  
  # lines(DOC_df$Date,ocOutflow + ocSed + ocResp ,type='h',col='green4') # Flow
  # lines(DOC_df$Date,ocSed + ocResp ,type='h',col='brown') # Sedimentation
  # lines(DOC_df$Date,ocResp ,type='h',col='grey50') # Respiration
  
  #par(new=T) # Plot OC concentration on separate yaxis 
  #plot(DOC_df$Date,DOC_df$DOCtotal_conc_gm3+POC_df$POCtotal_conc_gm3,type='l',yaxt='n',ylab='',xlab='',lwd=1.5)
  #axis(side = 4)
  #mtext('OC concentration (g/m3)',side = 4,line = 1.5,cex=0.8)
  if (legend==1){
  legend('bottomleft',legend = c('Inflow','Autoch','Outflow','Burial','Respiration'),#,'DOC Conc'),
         col = c('darkblue','cyan4','green4','brown','grey50'),#,'black'),
         pch=c(15,15,15,15,15,NA),
         lty=c(0,0,0,0,0,1),lwd=2,ncol=2,cex=1,pt.cex=2,seg.len=1,
         inset=0.01) }
  
}

#### run function over lakes ####
png(paste0('R/ResultsViz/Figures/OCfates_allLakes.png'),width = 11,height = 9,units = 'in',res=300)
par(mfrow=c(3,2))
    #par(mar=c(3,3,3,1),mgp=c(1.5,0.4,0),mfrow=c(3,2),tck=-0.02,cex=1.2) 
  ylim1= -400
  ylim2= 400
  
  flux_plot('Monona',ylim1=ylim1,ylim2=ylim2,legend=1)
  flux_plot('Trout',ylim1=ylim1,ylim2=ylim2,legend=0)
  flux_plot('Toolik',ylim1=ylim1,ylim2=ylim2,legend=0)
  flux_plot('Vanern',ylim1=ylim1,ylim2=ylim2,legend=0)
  flux_plot('Harp',ylim1=ylim1,ylim2=ylim2,legend=0)
dev.off()

png(paste0('R/ResultsViz/Figures/OCfates_',LakeName,'.png'),width = 11,height = 9,units = 'in',res=300)
  par(mar=c(3,3,3,1),mgp=c(1.5,0.4,0),mfrow=c(1,1),tck=-0.02,cex=1.2) 
  ylim1= -400
  ylim2= 400
  flux_plot(LakeName,ylim1=ylim1,ylim2=ylim2,legend=1)
dev.off()

# #Read in results data from SOS Carbon Flux Model
# DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
# POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
# SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')
# DOC_df <- read.csv(DOC_results_filename)
# POC_df <- read.csv(POC_results_filename)
# #SOS <- read.csv(SOS_results_filename) #file no longer exists
# #ParameterFile <- read.csv(paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep=''),sep='\t')
# ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
# parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
# for (i in 1:nrow(parameters)){ # assign parameters
#   assign(parameters[i,1],parameters[i,2])
# } # reads in unnecessary parameters, but not a big deal
# 
# volume = LakeVolume
# area = LakeArea

################## 
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
#png(paste0('R/ResultsViz/Figures/OCfates_',LakeName,'.png'),width = 7,height = 5,units = 'in',res=300)
#par(mar=c(3,3,2,3),mgp=c(1.5,0.5,0),tck=-0.02,cex=0.8)

# # Fluxes in
# ocInflow = POC_df$FlowIn_gm2y + DOC_df$FlowIn_gm2y
# ocGPP = POC_df$NPPin_gm2y + DOC_df$NPPin_gm2y
# 
# plot(POC_df$Date,ocInflow + ocGPP,col='darkblue',type='l',
#      ylim=c(-200,200),ylab='OC flux (gm2/y)',xlab='',main=LakeName) # Inflow 
# # Flow 
# lines(POC_df$Date,ocInflow + ocGPP,col='darkblue',type='h') #inflow
# lines(POC_df$Date,ocGPP,type='h',col='cyan4') # GPP in
# 
# # Fluxes out 
# ocOutflow = -POC_df$FlowOut_gm2y -DOC_df$FlowOut_gm2y
# ocSed = -POC_df$sedOut_gm2y
# ocResp = -DOC_df$respOut_gm2y
# 
#   
# lines(DOC_df$Date,ocOutflow + ocSed + ocResp ,type='h',col='green4') # Flow
# lines(DOC_df$Date,ocSed + ocResp ,type='h',col='brown') # Sedimentation
# lines(DOC_df$Date,ocResp ,type='h',col='grey50') # Respiration
# 
# #par(new=T) # Plot OC concentration on separate yaxis 
# #plot(DOC_df$Date,DOC_df$DOCtotal_conc_gm3+POC_df$POCtotal_conc_gm3,type='l',yaxt='n',ylab='',xlab='',lwd=1.5)
# #axis(side = 4)
# #mtext('OC concentration (g/m3)',side = 4,line = 1.5,cex=0.8)
# 
# legend('topleft',legend = c('Inflow','Autoch','Outflow','Burial','Respiration'),#,'DOC Conc'),
#        col = c('darkblue','cyan4','green4','brown','grey50'),#,'black'),
#        pch=c(15,15,15,15,15,NA),
#        lty=c(0,0,0,0,0,1),lwd=2,ncol=2,cex=1,pt.cex=2,seg.len=1,
#        inset=0.01)
# dev.off()
# 
# # balance fates
# ocLeachOut = -POC_df$leachOut_gm2y
# ocLake = (DOC_df$DOCtotal_conc_gm3+POC_df$POCtotal_conc_gm3) * volume/area
# ocDelta = tail(ocLake,1) - ocLake[1]
# 
# # Fluxes out of the lake are already negative 
# sum(ocInflow + ocGPP +  ocOutflow + ocResp + ocSed )/365 - ocDelta

