##################################################################################
# Single plot of fluxes and fates over time 
# Combo of the colored flux plot and the black/gold "steeler" plot showing SOS
# Date: 3-25-17 by Ian McC
# Updated: 11-14-2017 by Ian McC
##################################################################################

## NOTE: summarise_each seems to have been deprecated, so switched to summarise_all
# This even happened when I switched to r 3.3.2, which we had been using for the SOS project

# setwd('C:/Users/immcc/Desktop/SOS/')
# setwd("C:/Users/FWL/Documents/SOS")
library(lubridate)
library(dplyr)
#library(viridis)

alloch_col = 'red3'
autoch_col = 'royalblue3'
resp_col = 'black'
export_col = 'green4'
burial_col = 'gold'

#cols = viridis(5)
#cols[1] = 'red4'
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
      summarise_all(funs(mean(., na.rm = TRUE))) %>%
      ungroup() %>% mutate(month_day_year = as.Date(paste0(month,'/',day,'/',year), format = "%m/%d/%Y")) %>%
      dplyr::select(Date,FlowIn_gm2y,NPPin_gm2y,FlowOut_gm2y,sedOut_gm2y,POCtotal_conc_gm3,POCload_g,POCout_g) %>%
      arrange(Date)
    
    ## doc
    DOC_df = DOC_df %>%
      mutate(month = as.numeric(format(Date, "%m")), year = as.numeric(format(Date, "%Y")), day = 1) %>%
      group_by(month, year) %>%
      summarise_all(funs(mean(., na.rm = TRUE))) %>%
      ungroup() %>% mutate(month_day_year = as.Date(paste0(month,'/',day,'/',year), format = "%m/%d/%Y")) %>%
      dplyr::select(Date,FlowIn_gm2y,NPPin_gm2y,FlowOut_gm2y,respOut_gm2y,DOCtotal_conc_gm3,DOCload_g,DOCout_g) %>%
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
  Net = PipeProc
  TotalLoad = alloch + autoch
  Processed = TotalLoad - abs(ocExport)
  
  Date = as.Date(DOC_df$Date)
  Year = as.factor(year(Date))
  summary = data.frame(Date=Date,Year=Year,Alloch_gm2y=alloch,Autoch_gm2y=autoch,
                       R_gm2y=ocResp,S_gm2y=ocSed,Out_gm2y=ocExport,PipeProc_gm2y=PipeProc,dStorage_gm2y=dStorage,
                       Budget_left_gm2y=Budget_left,Budget_right_gm2y=Budget_right,Net_gm2y=Net,TotalLoad_gm2y=TotalLoad,
                       Processed_gm2y=Processed)
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

# get rid of first year; artifact of excluding cold season burial (emphasizes summer respiration)
Harp$Year = as.numeric(as.character(Harp$Year))
Harp_fullyears = subset(Harp, Year > Harp$Year[1]) #remove first year, which is incomplete
Harp_fullyears = subset(Harp_fullyears, Year <  Harp_fullyears$Year[nrow(Harp_fullyears)]) #remove last year, which is incomplete

Monona$Year = as.numeric(as.character(Monona$Year))
Monona_fullyears = subset(Monona, Year > Monona$Year[1])
Monona_fullyears = subset(Monona_fullyears, Year <  Monona_fullyears$Year[nrow(Monona_fullyears)])

Trout$Year = as.numeric(as.character(Trout$Year))
Trout_fullyears = subset(Trout, Year > Trout$Year[1])
Trout_fullyears = subset(Trout_fullyears, Year <  Trout_fullyears$Year[nrow(Trout_fullyears)])

Toolik$Year = as.numeric(as.character(Toolik$Year))
Toolik_fullyears = subset(Toolik, Year > Toolik$Year[1])
Toolik_fullyears = subset(Toolik_fullyears, Year <  Toolik_fullyears$Year[nrow(Toolik_fullyears)])

Vanern$Year = as.numeric(as.character(Vanern$Year))
Vanern_fullyears = subset(Vanern, Year > Vanern$Year[1])
Vanern_fullyears = subset(Vanern_fullyears, Year <  Vanern_fullyears$Year[nrow(Vanern_fullyears)])

Harp_means = colMeans(Harp_fullyears[,3:11],na.rm=T)
Monona_means = colMeans(Monona_fullyears[,3:11],na.rm=T)
Toolik_means = colMeans(Toolik_fullyears[,3:11],na.rm=T)
Trout_means = colMeans(Trout_fullyears[,3:11],na.rm=T)
Vanern_means = colMeans(Vanern_fullyears[,3:11],na.rm=T)

table_heads = names(Harp_means)
fate_means = rbind.data.frame(Harp_means,Monona_means,Toolik_means,Trout_means,Vanern_means)
colnames(fate_means) = table_heads
rownames(fate_means) = c('Harp','Monona','Toolik','Trout','Vanern')
#write.csv(fate_means,'FateOutputs_byLake.csv')
#write.csv(fate_means,'FateOutputs_byLake_full_years_only.csv')

# calculate SD of annual means for each lake
Harp_annual_means <- aggregate(. ~ Year, Harp_fullyears[,2:11], mean)
Monona_annual_means <- aggregate(. ~ Year, Monona_fullyears[,2:11], mean)
Toolik_annual_means <- aggregate(. ~ Year, Toolik_fullyears[,2:11], mean)
Trout_annual_means <- aggregate(. ~ Year, Trout_fullyears[,2:11], mean)
Vanern_annual_means <- aggregate(. ~ Year, Vanern_fullyears[,2:11], mean)

Harp_SD <- sapply(Harp_annual_means, sd)
Monona_SD <- sapply(Monona_annual_means, sd)
Toolik_SD <- sapply(Toolik_annual_means, sd)
Trout_SD <- sapply(Trout_annual_means, sd)
Vanern_SD <- sapply(Vanern_annual_means, sd)

##### plotting ######
fate_plot <- function(LakeName){
  lake = get(LakeName)
  plot(lake$Budget_left_gm2y~lake$Date,col=alloch_col,type='n',
       ylim=ylim_fate,ylab=ylabs,xlab='',main= LakeName,las=1) # Inflow (alloch) 
  
  polygon(x = c(lake$Date,rev(lake$Date)),y = c(-lake$R_gm2y,rep(0,length(-lake$R_gm2y))),col = resp_col)
  polygon(x = c(lake$Date,rev(lake$Date)),y = c(-lake$S_gm2y - lake$R_gm2y,rev(-lake$R_gm2y)),col = burial_col)
  polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$Budget_left_gm2y - lake$S_gm2y - lake$R_gm2y,rev(-lake$S_gm2y - lake$R_gm2y)),col = 'grey70')
}

flux_plot <- function(LakeName,ylim1=NULL,ylim2=NULL,legend=1){
    lake = get(LakeName)
    plot(lake$Date,lake$Alloch_gm2y + lake$Autoch_gm2y,col=alloch_col,type='l',
         ylim=c(ylim1,ylim2),ylab=ylabs,xlab='', main=LakeName, las=1) # Inflow
    # Fluxes in 
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$Autoch_gm2y,rep(0,length(lake$Autoch_gm2y))),col = autoch_col)
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$Alloch_gm2y + lake$Autoch_gm2y,rev(lake$Autoch_gm2y)),col = alloch_col)
    # Fluxes Out
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$R_gm2y,rep(0,length(lake$R_gm2y))),col = resp_col)
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$S_gm2y + lake$R_gm2y,rev(lake$R_gm2y)),col = burial_col)
    polygon(x = c(lake$Date,rev(lake$Date)),y = c(lake$Out_gm2y + lake$S_gm2y + lake$R_gm2y,rev(lake$S_gm2y + lake$R_gm2y)),col = export_col)
    abline(0,0, lwd=2, lty=2)
    
    if (legend==1){
      legend('bottomleft',legend = c('Export','Burial','Resp'),fill = c(export_col,burial_col,resp_col), horiz=T,
             seg.len=0.3,bty = 'n',x.intersp = 0.5)
      legend('topleft',legend = c('Alloch','Autoch'),fill = c(alloch_col,autoch_col), horiz=T,
             seg.len=0.3,bty = 'n',x.intersp = 0.5)}
}

# static plot parameters
ylabs = expression(paste("g m"^"-2"," yr"^"-1"))
#ylab = expression(paste("DO (mg L"^"-1",")"))
xlabs = 'Date'
ylim_fate = c(0,400)

png(paste0('R/ResultsViz/Figures/OCfates_fluxesAllLakes2.png'),width = 8,height = 11,units = 'in',res=300)
  par(mar=c(1.5,4,2,1),mgp=c(2,0.3,0),mfrow=c(5,2),cex=1,tck=-0.03)
  ylim1= -300
  ylim2= 300
  
  # Harp
  flux_plot('Harp',ylim1=ylim1,ylim2=ylim2,legend=1)
  mtext('a)',side = 3,line = 0.5,adj=0,cex=1)
  fate_plot('Harp')
  mtext('b)',side = 3,line = 0.5,adj=0,cex=1)
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
  flux_plot('Toolik',ylim1=ylim1,ylim2=ylim2,legend=0)
  fate_plot('Toolik')
  
dev.off()

####################################
#### boxplot of SOS status across years ####

# # calculate annual statistics
# Vanern_annual = aggregate(-Vanern_fullyears$PipeProc_gm2y, by=list(Vanern_fullyears$Year), FUN='mean')
# colnames(Vanern_annual) = c('Year','mean')
# Vanern_annual$Lake = rep('Vanern',nrow(Vanern_annual))
# Harp_annual = aggregate(-Harp_fullyears$PipeProc_gm2y, by=list(Harp_fullyears$Year), FUN='mean')
# colnames(Harp_annual) = c('Year','mean')
# Harp_annual$Lake = rep('Harp',nrow(Harp_annual))
# Trout_annual = aggregate(-Trout_fullyears$PipeProc_gm2y, by=list(Trout_fullyears$Year), FUN='mean')
# colnames(Trout_annual) = c('Year','mean')
# Trout_annual$Lake = rep('Trout',nrow(Trout_annual))
# Toolik_annual = aggregate(-Toolik_fullyears$PipeProc_gm2y, by=list(Toolik_fullyears$Year), FUN='mean')
# colnames(Toolik_annual) = c('Year','mean')
# Toolik_annual$Lake = rep('Toolik',nrow(Toolik_annual))
# Monona_annual = aggregate(-Monona_fullyears$PipeProc_gm2y, by=list(Monona_fullyears$Year), FUN='mean')
# colnames(Monona_annual) = c('Year','mean')
# Monona_annual$Lake = rep('Monona',nrow(Monona_annual))
# 
# # merge into single data frame
# All_lakes_annual = rbind.data.frame(Harp_annual, Monona_annual, Toolik_annual, Trout_annual, Vanern_annual)
# 
# # get standard deviations for all fates
# Harp_sd = describe(aggregate(Harp_fullyears, by=list(Harp_fullyears$Year), FUN='mean'))
# Monona_sd = describe(aggregate(Monona_fullyears, by=list(Monona_fullyears$Year), FUN='mean'))
# Toolik_sd = describe(aggregate(Toolik_fullyears, by=list(Toolik_fullyears$Year), FUN='mean'))
# Trout_sd = describe(aggregate(Trout_fullyears, by=list(Trout_fullyears$Year), FUN='mean'))
# Vanern_sd = describe(aggregate(Vanern_fullyears, by=list(Vanern_fullyears$Year), FUN='mean'))
# 
# All_lakes_sd = cbind.data.frame(Harp_sd$sd, Monona_sd$sd, Toolik_sd$sd, Trout_sd$sd, Vanern_sd$sd)
# colnames(All_lakes_sd) = c('Harp','Monona','Toolik','Trout','Vanern')
# rownames(All_lakes_sd) = rownames(Harp_sd)
# All_lakes_sd = as.data.frame(t(All_lakes_sd))
# All_lakes_sd = round(All_lakes_sd, 2)
# 
# # #boxplot of annual mean OC by lake
# png(paste0('R/ResultsViz/Figures/AnnualNetOCBoxplot.png'),width = 5,height = 5,units = 'in',res=300)
#   par(mar=c(2.1, 4.1, 3.1, 1.1)) #bot, left, top, right, def=c(5.1, 4.1, 4.1, 2.1)
#   boxplot(mean ~ Lake, data=All_lakes_annual, las=1, ylab='Respiration - Burial, OC (g/m2/yr)',main='Lake function across years', ylim=c(-50,100))
#   #mtext(side=3, 'Burial-Respiration')
#   abline(0,0, lty=2, lwd=2)
#   text(x=1,y=4,'Source',font=2)
#   text(x=1,y=-4,'Sink',font=2)
# dev.off()

#### boxplot of sub-annual mean OC by lake ####
# 
# # subset to specific months
# first_month = 5 #e.g., 5=May
# last_month = 8
# Vanern_subannual = subset(Vanern_fullyears, as.numeric(format(Date, "%m")) %in% first_month:last_month) #e.g., 5:8 is May-Aug
# Trout_subannual = subset(Trout_fullyears, as.numeric(format(Date, "%m")) %in% first_month:last_month)
# Harp_subannual = subset(Harp_fullyears, as.numeric(format(Date, "%m")) %in% first_month:last_month)
# Toolik_subannual = subset(Toolik_fullyears, as.numeric(format(Date, "%m")) %in% first_month:last_month)
# Monona_subannual = subset(Monona_fullyears, as.numeric(format(Date, "%m")) %in% first_month:last_month)
# 
# # calculate sub-annual statistics
# Vanern_subannual = aggregate(-Vanern_subannual$PipeProc_gm2y, by=list(Vanern_subannual$Year), FUN='mean')
# colnames(Vanern_subannual) = c('Year','mean')
# Vanern_subannual$Lake = rep('Vanern',nrow(Vanern_subannual))
# Harp_subannual = aggregate(-Harp_subannual$PipeProc_gm2y, by=list(Harp_subannual$Year), FUN='mean')
# colnames(Harp_subannual) = c('Year','mean')
# Harp_subannual$Lake = rep('Harp',nrow(Harp_subannual))
# Trout_subannual = aggregate(-Trout_subannual$PipeProc_gm2y, by=list(Trout_subannual$Year), FUN='mean')
# colnames(Trout_subannual) = c('Year','mean')
# Trout_subannual$Lake = rep('Trout',nrow(Trout_subannual))
# Toolik_subannual = aggregate(-Toolik_subannual$PipeProc_gm2y, by=list(Toolik_subannual$Year), FUN='mean')
# colnames(Toolik_subannual) = c('Year','mean')
# Toolik_subannual$Lake = rep('Toolik',nrow(Toolik_subannual))
# Monona_subannual = aggregate(-Monona_subannual$PipeProc_gm2y, by=list(Monona_subannual$Year), FUN='mean')
# colnames(Monona_subannual) = c('Year','mean')
# Monona_subannual$Lake = rep('Monona',nrow(Monona_subannual))
# 
# # merge into single data frame
# All_lakes_subannual = rbind.data.frame(Harp_subannual, Monona_subannual, Toolik_subannual, Trout_subannual, Vanern_subannual)

# png(paste0('R/ResultsViz/Figures/SubAnnualNetOCBoxplot.png'),width = 11,height = 9,units = 'in',res=300)
#   par(mfrow=c(1,1))
#   par(mar=c(2.1, 5.1, 3.1, 2.1)) #bot, left, top, right, def=c(5.1, 4.1, 4.1, 2.1)
#   tick_seq = seq(-50,100, by=25)
#   cex.axis = 1.5
# 
#   boxplot(mean ~ Lake, data=All_lakes_subannual, axes=F, ann=F, main='Subannual Net Lake Function: May-Aug', ylim=c(-50,100))
#   mtext(side=3, '(g/m2/yr OC)')
#   #mtext(side=2, 'OC (g/m2/yr)')
#   axis(1, at = 1:5, labels = levels(as.factor(All_lakes_subannual$Lake)), cex.axis = cex.axis, tick=F)
#   axis(2, at=tick_seq, label=rep('',length(tick_seq),cex.axis = cex.axis, tick=F))
#   axis(2, at=tick_seq, line=0.5, lwd=0, cex.axis=1.5, las=1) #las=1 for horizontal y axis label
#   box()
#   abline(0,0, lty=2, lwd=1.5)
# dev.off()


# #### calculate differences in net source/sink by annual vs. subannual
# full_year_sampling = aggregate(All_lakes_annual$mean~All_lakes_annual$Lake, FUN=mean)
# part_year_sampling = aggregate(All_lakes_subannual$mean~All_lakes_subannual$Lake, FUN=mean)
# comparison = data.frame(Lake=full_year_sampling$`All_lakes_annual$Lake`,
#                         full_year=full_year_sampling$`All_lakes_annual$mean`,
#                         part_year=part_year_sampling$`All_lakes_subannual$mean`)
# comparison$Diff = comparison$part_year - comparison$full_year
# comparison$PctDiff = 100- ((comparison$full_year/comparison$part_year)*100)
# 
# png(paste0('R/ResultsViz/Figures/AnnualNetOCBoxplot_panel.png'),width = 11,height = 9,units = 'in',res=300)
#   par(mfrow=c(1,2))
#   par(mar=c(2.1, 4.1, 3.1, 1.1)) #bot, left, top, right, def=c(5.1, 4.1, 4.1, 2.1)
#   boxplot(mean ~ Lake, data=All_lakes_annual, las=1, ylab='Respiration - Burial, OC (g/m2/yr)',main='a) Annual', ylim=c(-50,100))
#   #mtext(side=3, 'Burial-Respiration')
#   abline(0,0, lty=2, lwd=2)
#   text(x=1,y=4,'Source',font=2)
#   text(x=1,y=-4,'Sink',font=2)
# 
#   boxplot(mean ~ Lake, data=All_lakes_subannual, las=1, ylab='Respiration - Burial, OC (g/m2/yr)', main='b) May-Aug', ylim=c(-50,100))
#   #mtext(side=2, 'OC (g/m2/yr)')
#   abline(0,0, lty=2, lwd=2)
#   text(x=1,y=4,'Source',font=2)
#   text(x=1,y=-4,'Sink',font=2)
#   
# dev.off()
