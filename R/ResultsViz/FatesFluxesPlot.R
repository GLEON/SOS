##################################################################################
# Single plot of fluxes and fates over time 
# Combo of the colored flux plot and the black/gold "steeler" plot showing SOS
# Date: 3-25-17 by Ian McC
# Updated:
##################################################################################

setwd('C:/Users/immcc/Desktop/SOS/')

library(lubridate)
library(dplyr)

#### define functions ####
# plot fluxes over time
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
  
  ### aggregate daily to monthly
  DOC_df$Date = as.Date(DOC_df$Date)
  POC_df$Date = as.Date(POC_df$Date)
  
  #poc
  Month_POC = POC_df %>%
    mutate(month = as.numeric(format(Date, "%m")), year = as.numeric(format(Date, "%Y")), day = 1) %>%
    group_by(month, year) %>%
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    ungroup() %>% mutate(month_day_year = as.Date(paste0(month,'/',day,'/',year), format = "%m/%d/%Y")) %>%
    select(Date,FlowIn_gm2y,NPPin_gm2y,FlowOut_gm2y,sedOut_gm2y) %>%
    arrange(Date)
  
  #DOC
  Month_DOC = DOC_df %>%
    mutate(month = as.numeric(format(Date, "%m")), year = as.numeric(format(Date, "%Y")), day = 1) %>%
    group_by(month, year) %>%
    summarise_each(funs(mean(., na.rm = TRUE))) %>%
    ungroup() %>% mutate(month_day_year = as.Date(paste0(month,'/',day,'/',year), format = "%m/%d/%Y")) %>%
    select(Date,FlowIn_gm2y,NPPin_gm2y,FlowOut_gm2y,respOut_gm2y) %>%
    arrange(Date)
  
  #SOS <- read.csv(SOS_results_filename) #file no longer exists
  #ParameterFile <- read.csv(paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep=''),sep='\t')
  ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
  parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  for (i in 1:nrow(parameters)){ # assign parameters
    assign(parameters[i,1],parameters[i,2])
  } # reads in unnecessary parameters, but not a big deal
  
  # Fluxes in
  ocInflow = Month_POC$FlowIn_gm2y + Month_DOC$FlowIn_gm2y
  ocGPP = Month_POC$NPPin_gm2y + Month_DOC$NPPin_gm2y
  
  
  plot(Month_POC$Date,ocInflow + ocGPP,col='dodgerblue',type='l',
       ylim=c(ylim1,ylim2),ylab='OC flux (gm2/y)',xlab='', main=LakeName, las=1) # Inflow
  #tick_seq = seq(ylim1,ylim2, by=75), if want manual y axis, set yaxt='n'
  #axis(side=2, at=tick_seq, line=0.5, lwd=0, cex.axis=1, las=1, tick = T, labels=T)
  
  # Flow 
  # lines(POC_df$Date,ocInflow + ocGPP,col='dodgerblue',type='l') #inflow
  # lines(POC_df$Date,ocGPP,type='l',col='firebrick') # GPP in
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocGPP,rep(0,length(ocGPP))),col = 'firebrick')
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocInflow + ocGPP,rev(ocGPP)),col = 'dodgerblue')
  
  # Fluxes out 
  ocOutflow = -Month_POC$FlowOut_gm2y -Month_DOC$FlowOut_gm2y
  ocSed = -Month_POC$sedOut_gm2y
  ocResp = -Month_DOC$respOut_gm2y
  
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocResp,rep(0,length(ocResp))),col = 'gold')
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocSed + ocResp,rev(ocResp)),col = 'black')
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocOutflow + ocSed + ocResp,rev(ocSed + ocResp)),col = 'green4')
  abline(0,0, lwd=2, lty=2)
  
  #par(new=T) # Plot OC concentration on separate yaxis 
  #plot(DOC_df$Date,DOC_df$DOCtotal_conc_gm3+POC_df$POCtotal_conc_gm3,type='l',yaxt='n',ylab='',xlab='',lwd=1.5)
  #axis(side = 4)
  #mtext('OC concentration (g/m3)',side = 4,line = 1.5,cex=0.8)
  if (legend==1){
    legend('bottomleft',legend = c('Alloch','Autoch  ','Export','Burial','Resp'),
           col = c('dodgerblue','firebrick','green4','black','gold'), horiz=T,
           pch=c(15,15,15,15,15),
           lty=c(0,0,0,0,0,0),lwd=2,cex=1,pt.cex=2,seg.len=1)} #ncol=2
  
}

# calculate fates
SOS_fate = function(LakeName, monthlyFlag){
  #Lakename = lake name in quotes
  #monthlyFlag = 1 if want to aggregate to monthly data,0 for daily
  #Read in results data from SOS Carbon Flux Model
  DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
  POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
  #SOS_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_SOS_Results.csv',sep='')
  
  DOC_df <- read.csv(DOC_results_filename)
  POC_df <- read.csv(POC_results_filename)
  #SOS <- read.csv(SOS_results_filename)
  
  ### aggregate daily to monthly
  DOC_df$Date = as.Date(DOC_df$Date)
  POC_df$Date = as.Date(POC_df$Date)
  
  ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
  parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  for (i in 1:nrow(parameters)){ # assign parameters
    assign(parameters[i,1],parameters[i,2])
  }
  
  volume = LakeVolume
  area = LakeArea
  
  if (monthlyFlag==1){
    #poc
    POC_df = POC_df %>%
      mutate(month = as.numeric(format(Date, "%m")), year = as.numeric(format(Date, "%Y")), day = 1) %>%
      group_by(month, year) %>%
      summarise_each(funs(mean(., na.rm = TRUE))) %>%
      ungroup() %>% mutate(month_day_year = as.Date(paste0(month,'/',day,'/',year), format = "%m/%d/%Y")) %>%
      select(Date,FlowIn_gm2y,NPPin_gm2y,FlowOut_gm2y,sedOut_gm2y,POCtotal_conc_gm3,POCload_g,POCout_g) %>%
      arrange(Date)
    
    ## doc
    DOC_df = DOC_df %>%
      mutate(month = as.numeric(format(Date, "%m")), year = as.numeric(format(Date, "%Y")), day = 1) %>%
      group_by(month, year) %>%
      summarise_each(funs(mean(., na.rm = TRUE))) %>%
      ungroup() %>% mutate(month_day_year = as.Date(paste0(month,'/',day,'/',year), format = "%m/%d/%Y")) %>%
      select(Date,FlowIn_gm2y,NPPin_gm2y,FlowOut_gm2y,respOut_gm2y,DOCtotal_conc_gm3,DOCload_g,DOCout_g) %>%
      arrange(Date)
  }
  
  # Fluxes in
  alloch<-DOC_df$FlowIn_gm2y+POC_df$FlowIn_gm2y
  autoch<-POC_df$NPPin_gm2y+DOC_df$NPPin_gm2y
  total_load = alloch + autoch
  
  # Fluxes out 
  ocExport = -POC_df$FlowOut_gm2y -DOC_df$FlowOut_gm2y
  ocSed = -POC_df$sedOut_gm2y
  ocResp = -DOC_df$respOut_gm2y
  
  # plot(POC_df$Date,alloch + autoch,col='dodgerblue',type='n',
  #      ylim=c(ylim1,ylim2),ylab='OC flux (gm2/y)',xlab='',main=LakeName) # Inflow (alloch) 
  # 
  # polygon(x = c(POC_df$Date,rev(POC_df$Date)),y = c(ocResp,rep(0,length(ocResp))),col = 'black')
  # polygon(x = c(POC_df$Date,rev(POC_df$Date)),y = c(ocSed + ocResp,rev(ocResp)),col = 'gold')
  # polygon(x = c(POC_df$Date,rev(POC_df$Date)),y = c(total_load,rep(0,length(total_load))),col = 'grey70')
  
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
  
  PipeProc = ocResp - ocSed
  Budget_left = total_load
  Budget_right = -ocResp - ocSed - ocExport + dStorage # should equal Budget_left
  
  Date = as.Date(DOC_df$Date)
  Year = as.factor(year(Date))
  summary = data.frame(Date=Date,Year=Year,Alloch_gm2y=alloch,Autoch_gm2y=autoch,
                       R_gm2y=ocResp,S_gm2y=ocSed,Out_gm2y=ocExport,PipeProc_gm2y=PipeProc,dStorage_gm2y=dStorage,
                       Budget_left_gm2y=Budget_left,Budget_right_gm2y=Budget_right)
  return(summary)
}

#### run function over lakes ####
monthlyFlag = 1 #1=monthly, 0=daily
Harp = SOS_fate('Harp',monthlyFlag = monthlyFlag) 
Monona = SOS_fate('Monona',monthlyFlag = monthlyFlag)
Toolik = SOS_fate('Toolik', monthlyFlag = monthlyFlag)
Trout = SOS_fate('Trout',monthlyFlag = monthlyFlag)
Vanern = SOS_fate('Vanern',monthlyFlag = monthlyFlag)

#generate summary table of fates by lake
# recommend using daily (monthlyFlag=0)
Harp_means = colMeans(Harp[,3:11],na.rm=T)
Monona_means = colMeans(Monona[,3:11],na.rm=T)
Toolik_means = colMeans(Toolik[,3:11],na.rm=T)
Trout_means = colMeans(Trout[,3:11],na.rm=T)
Vanern_means = colMeans(Vanern[,3:11],na.rm=T)

table_heads = names(Harp_means)
fate_means = rbind.data.frame(Harp_means,Monona_means,Toolik_means,Trout_means,Vanern_means)
colnames(fate_means) = table_heads
rownames(fate_means) = c('Harp','Monona','Toolik','Trout','Vanern')
#write.csv(fate_means,'FateOutputs_byLake.csv')

##### plotting ######
# static plot parameters
lty = 2
lwd = 2
ylab = 'OC (g/m2/yr)'
xlab = 'Date'
ylim_fate = c(0,400)
cex.main = 2
cex.axis = 2
cex.lab = 2

png(paste0('R/ResultsViz/Figures/OCfates_fluxesAllLakes.png'),width = 9,height = 11,units = 'in',res=300)
par(mfrow=c(5,2))
par(mar=c(3,4.5,3,1))
ylim1= -250
ylim2= 250

# Harp
flux_plot('Harp',ylim1=ylim1,ylim2=ylim2,legend=1)
plot(Harp$Budget_left_gm2y~Harp$Date,col='dodgerblue',type='n',
     ylim=ylim_fate,ylab='',xlab='',main='Harp',las=1) # Inflow (alloch) 

polygon(x = c(Harp$Date,rev(Harp$Date)),y = c(-Harp$R_gm2y,rep(0,length(-Harp$R_gm2y))),col = 'gold')
polygon(x = c(Harp$Date,rev(Harp$Date)),y = c(-Harp$S_gm2y - Harp$R_gm2y,rev(-Harp$R_gm2y)),col = 'black')
polygon(x = c(Harp$Date,rev(Harp$Date)),y = c(Harp$Budget_left_gm2y - Harp$S_gm2y - Harp$R_gm2y,rev(-Harp$S_gm2y - Harp$R_gm2y)),col = 'grey70')

legend('topright',legend=c('Alloch + Autoch'), col=c('grey70'), pch=15, pt.cex = 2)

# Monona
flux_plot('Monona',ylim1=ylim1,ylim2=ylim2,legend=0)
plot(Monona$Budget_left_gm2y~Monona$Date,col='dodgerblue',type='n',
     ylim=ylim_fate,ylab='',xlab='',main='Monona',las=1) # Inflow (alloch) 

polygon(x = c(Monona$Date,rev(Monona$Date)),y = c(-Monona$R_gm2y,rep(0,length(-Monona$R_gm2y))),col = 'gold')
polygon(x = c(Monona$Date,rev(Monona$Date)),y = c(-Monona$S_gm2y - Monona$R_gm2y,rev(-Monona$R_gm2y)),col = 'black')
polygon(x = c(Monona$Date,rev(Monona$Date)),y = c(Monona$Budget_left_gm2y - Monona$S_gm2y - Monona$R_gm2y,rev(-Monona$S_gm2y - Monona$R_gm2y)),col = 'grey70')

# Trout
flux_plot('Trout',ylim1=ylim1,ylim2=ylim2,legend=0)
plot(Trout$Budget_left_gm2y~Trout$Date,col='dodgerblue',type='n',
     ylim=ylim_fate,ylab='',xlab='',main='Trout',las=1) # Inflow (alloch) 

polygon(x = c(Trout$Date,rev(Trout$Date)),y = c(-Trout$R_gm2y,rep(0,length(-Trout$R_gm2y))),col = 'gold')
polygon(x = c(Trout$Date,rev(Trout$Date)),y = c(-Trout$S_gm2y - Trout$R_gm2y,rev(-Trout$R_gm2y)),col = 'black')
polygon(x = c(Trout$Date,rev(Trout$Date)),y = c(Trout$Budget_left_gm2y - Trout$S_gm2y - Trout$R_gm2y,rev(-Trout$S_gm2y - Trout$R_gm2y)),col = 'grey70')

#legend('topleft',legend=c('Total Load','Burial','Respiration'), col=c('grey70','black','gold'),lwd=2)

# Vanern
flux_plot('Vanern',ylim1=ylim1,ylim2=ylim2,legend=0)
plot(Vanern$Budget_left_gm2y~Vanern$Date,col='dodgerblue',type='n',
     ylim=ylim_fate,ylab='',xlab='',main='Vanern',las=1) # Inflow (alloch) 

polygon(x = c(Vanern$Date,rev(Vanern$Date)),y = c(-Vanern$R_gm2y,rep(0,length(-Vanern$R_gm2y))),col = 'gold')
polygon(x = c(Vanern$Date,rev(Vanern$Date)),y = c(-Vanern$S_gm2y - Vanern$R_gm2y,rev(-Vanern$R_gm2y)),col = 'black')
polygon(x = c(Vanern$Date,rev(Vanern$Date)),y = c(Vanern$Budget_left_gm2y - Vanern$S_gm2y - Vanern$R_gm2y,rev(-Vanern$S_gm2y - Vanern$R_gm2y)),col = 'grey70')

#legend('topleft',lecgend=c('Total Load','Burial','Respiration'), col=c('grey70','black','gold'),lwd=2)

# Toolik
flux_plot('Toolik',ylim1=-800,ylim2=800,legend=0)
plot(Toolik$Budget_left_gm2y~Toolik$Date,col='dodgerblue',type='n',
     ylim=c(),ylab='',xlab='',main='Toolik',las=1) # Inflow (alloch) 

polygon(x = c(Toolik$Date,rev(Toolik$Date)),y = c(-Toolik$R_gm2y,rep(0,length(-Toolik$R_gm2y))),col = 'gold')
polygon(x = c(Toolik$Date,rev(Toolik$Date)),y = c(-Toolik$S_gm2y - Toolik$R_gm2y,rev(-Toolik$R_gm2y)),col = 'black')
polygon(x = c(Toolik$Date,rev(Toolik$Date)),y = c(Toolik$Budget_left_gm2y - Toolik$S_gm2y - Toolik$R_gm2y,rev(-Toolik$S_gm2y - Toolik$R_gm2y)),col = 'grey70')

#legend('topleft',legend=c('Total Load','Burial','Respiration'), col=c('grey70','black','gold'),lwd=2)
dev.off()

####################################333
### boxplot of SOS status across years

# calculate annual statistics
Vanern_annual = aggregate(-Vanern$PipeProc_gm2y, by=list(Vanern$Year), FUN='mean')
colnames(Vanern_annual) = c('Year','mean')
Vanern_annual$Lake = rep('Vanern',nrow(Vanern_annual))
Harp_annual = aggregate(-Harp$PipeProc_gm2y, by=list(Harp$Year), FUN='mean')
colnames(Harp_annual) = c('Year','mean')
Harp_annual$Lake = rep('Harp',nrow(Harp_annual))
Trout_annual = aggregate(-Trout$PipeProc_gm2y, by=list(Trout$Year), FUN='mean')
colnames(Trout_annual) = c('Year','mean')
Trout_annual$Lake = rep('Trout',nrow(Trout_annual))
Toolik_annual = aggregate(-Toolik$PipeProc_gm2y, by=list(Toolik$Year), FUN='mean')
colnames(Toolik_annual) = c('Year','mean')
Toolik_annual$Lake = rep('Toolik',nrow(Toolik_annual))
Monona_annual = aggregate(-Monona$PipeProc_gm2y, by=list(Monona$Year), FUN='mean')
colnames(Monona_annual) = c('Year','mean')
Monona_annual$Lake = rep('Monona',nrow(Monona_annual))

# merge into single data frame
All_lakes_annual = rbind.data.frame(Harp_annual, Monona_annual, Toolik_annual, Trout_annual, Vanern_annual)

#boxplot of annual mean OC by lake
png(paste0('R/ResultsViz/Figures/AnnualNetOCBoxplot.png'),width = 11,height = 9,units = 'in',res=300)
par(mfrow=c(1,1))
par(mar=c(2.1, 5.1, 3.1, 2.1)) #bot, left, top, right, def=c(5.1, 4.1, 4.1, 2.1)
tick_seq = seq(-50,100, by=25)
cex.axis = 1.5

boxplot(mean ~ Lake, data=All_lakes_annual, axes=F, ann=F, main='Annual Net Lake Function', ylim=c(-50,100))
mtext(side=3, '(g/m2/yr OC)')
#mtext(side=2, 'OC (g/m2/yr)')
axis(1, at = 1:5, labels = levels(as.factor(All_lakes_annual$Lake)), cex.axis = cex.axis, tick=F)
axis(2, at=tick_seq, label=rep('',length(tick_seq),cex.axis = cex.axis, tick=F))
axis(2, at=tick_seq, line=0.5, lwd=0, cex.axis=1.5, las=1) #las=1 for horizontal y axis label
box()
abline(0,0, lty=2, lwd=1.5)
dev.off()

