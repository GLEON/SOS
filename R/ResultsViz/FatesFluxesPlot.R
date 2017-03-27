##################################################################################
# Single plot of fluxes and fates over time 
# Combo of the colored flux plot and the black/gold "steeler" plot showing SOS
# Date: 3-25-17 by Ian McC
# Updated: 3-27-2017 by HD
##################################################################################

setwd('C:/Users/immcc/Desktop/SOS/')

library(lubridate)
library(dplyr)

#### define functions ####
# calculate fates
SOS_fate = function(LakeName, monthlyFlag){
  #Lakename = lake name in quotes
  #monthlyFlag = 1 if want to aggregate to monthly data,0 for daily
  #Read in results data from SOS Carbon Flux Model
  DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
  POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')

  DOC_df <- read.csv(DOC_results_filename)
  POC_df <- read.csv(POC_results_filename)

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
fate_plot <- function(LakeName){
  lake = get(LakeName)
  plot(lake$Budget_left_gm2y~lake$Date,col='dodgerblue',type='n',
       ylim=ylim_fate,ylab=ylabs,xlab='',main= LakeName,las=1) # Inflow (alloch) 
  
  polygon(x = c(lake$Date,rev(lake$Date)),y = c(-lake$R_gm2y,rep(0,length(-lake$R_gm2y))),col = 'gold')
  polygon(x = c(lake$Date,rev(lake$Date)),y = c(-lake$S_gm2y - lake$R_gm2y,rev(-lake$R_gm2y)),col = 'black')
  polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$Budget_left_gm2y - lake$S_gm2y - lake$R_gm2y,rev(-lake$S_gm2y - lake$R_gm2y)),col = 'grey70')
}

flux_plot <- function(LakeName,ylim1=NULL,ylim2=NULL,legend=1){
    lake = get(LakeName)
    plot(lake$Date,lake$Alloch_gm2y + lake$Autoch_gm2y,col='dodgerblue',type='l',
         ylim=c(ylim1,ylim2),ylab='OC flux (gm2/y)',xlab='', main=LakeName, las=1) # Inflow
    # Fluxes in 
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$Autoch_gm2y,rep(0,length(lake$Autoch_gm2y))),col = 'firebrick')
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$Alloch_gm2y + lake$Autoch_gm2y,rev(lake$Autoch_gm2y)),col = 'dodgerblue')
    # Fluxes Out
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$R_gm2y,rep(0,length(lake$R_gm2y))),col = 'gold')
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$S_gm2y + lake$R_gm2y,rev(lake$R_gm2y)),col = 'black')
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$Out_gm2y + lake$S_gm2y + lake$R_gm2y,rev(lake$S_gm2y + lake$R_gm2y)),col = 'green4')
    abline(0,0, lwd=2, lty=2)
    
    if (legend==1){
      legend('bottomleft',legend = c('Export','Burial','Resp'),fill = c('green4','black','gold'), horiz=T,
             seg.len=0.3,bty = 'n',x.intersp = 0.5)
      legend('topleft',legend = c('Alloch','Autoch'),fill = c('dodgerblue','firebrick'), horiz=T,
             seg.len=0.3,bty = 'n',x.intersp = 0.5)}
}

# static plot parameters
ylabs = 'OC (g/m2/yr)'
xlabs = 'Date'
ylim_fate = c(0,400)
png(paste0('R/ResultsViz/Figures/OCfates_fluxesAllLakes2.png'),width = 8,height = 11,units = 'in',res=300)
  par(mar=c(1.5,3,2,1),mgp=c(2,0.3,0),mfrow=c(5,2),cex=1,tck=-0.03)
  ylim1= -250
  ylim2= 250
  
  # Harp
  flux_plot('Harp',ylim1=ylim1,ylim2=ylim2,legend=1)
  fate_plot('Harp')
  legend('topright',legend=c('Alloch + Autoch'), fill=c('grey70'), bty = 'n')
  
  # Monona
  flux_plot('Monona',ylim1=ylim1,ylim2=ylim2,legend=0)
  fate_plot('Monona')
  
  # Trout
  flux_plot('Trout',ylim1=ylim1,ylim2=ylim2,legend=0)
  fate_plot('Trout')

  # Vanern
  flux_plot('Vanern',ylim1=ylim1,ylim2=ylim2,legend=0)
  fate_plot('Vanern')
  
   # Toolik
  flux_plot('Toolik',ylim1=-800,ylim2=800,legend=0)
  fate_plot('Toolik')
  
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

