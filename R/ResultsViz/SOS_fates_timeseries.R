################################################################
# Time series plots of SOS fates 
# Date: 1-3-17
# Updated: 3-22-17 to include monthly aggregations
# Author: Ian McC, adapted from Hilary's SOS_mean.R code (HD makes some mean R code)
################################################################

library(lubridate)
library(dplyr)

#### define function ####
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
    FlowIn_gm2y=POC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    FlowIn_gm2y$day = rep(1,1,nrow(FlowIn_gm2y))
    FlowIn_gm2y$month = as.numeric(FlowIn_gm2y$month)
    FlowIn_gm2y$year = as.numeric(FlowIn_gm2y$year)
    colnames(FlowIn_gm2y) = c('month','year','FlowIn_gm2y','day')
    month_day_year = paste0(FlowIn_gm2y$month,'/',FlowIn_gm2y$day,'/',FlowIn_gm2y$year)
    FlowIn_gm2y = FlowIn_gm2y[3]
    
    sedOut_gm2y=POC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    sedOut_gm2y$day = rep(1,1,nrow(sedOut_gm2y))
    sedOut_gm2y$month = as.numeric(sedOut_gm2y$month)
    sedOut_gm2y$year = as.numeric(sedOut_gm2y$year)
    colnames(sedOut_gm2y) = c('month','year','sedOut_gm2y','day')
    sedOut_gm2y = sedOut_gm2y[3]
    
    leachOut_gm2y=POC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    leachOut_gm2y$day = rep(1,1,nrow(leachOut_gm2y))
    leachOut_gm2y$month = as.numeric(leachOut_gm2y$month)
    leachOut_gm2y$year = as.numeric(leachOut_gm2y$year)
    colnames(leachOut_gm2y) = c('month','year','leachOut_gm2y','day')
    leachOut_gm2y = leachOut_gm2y[3]
    
    FlowOut_gm2y=POC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    FlowOut_gm2y$day = rep(1,1,nrow(FlowOut_gm2y))
    FlowOut_gm2y$month = as.numeric(FlowOut_gm2y$month)
    FlowOut_gm2y$year = as.numeric(FlowOut_gm2y$year)
    colnames(FlowOut_gm2y) = c('month','year','FlowOut_gm2y','day')
    FlowOut_gm2y = FlowOut_gm2y[3]
    
    NPPin_gm2y=POC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    NPPin_gm2y$day = rep(1,1,nrow(NPPin_gm2y))
    NPPin_gm2y$month = as.numeric(NPPin_gm2y$month)
    NPPin_gm2y$year = as.numeric(NPPin_gm2y$year)
    colnames(NPPin_gm2y) = c('month','year','NPPin_gm2y','day')
    NPPin_gm2y = NPPin_gm2y[3]
    
    POCtotal_conc_gm3=POC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    POCtotal_conc_gm3$day = rep(1,1,nrow(POCtotal_conc_gm3))
    POCtotal_conc_gm3$month = as.numeric(POCtotal_conc_gm3$month)
    POCtotal_conc_gm3$year = as.numeric(POCtotal_conc_gm3$year)
    colnames(POCtotal_conc_gm3) = c('month','year','POCtotal_conc_gm3','day')
    POCtotal_conc_gm3 = POCtotal_conc_gm3[3]
    
    POCload_g=POC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    POCload_g$day = rep(1,1,nrow(POCload_g))
    POCload_g$month = as.numeric(POCload_g$month)
    POCload_g$year = as.numeric(POCload_g$year)
    colnames(POCload_g) = c('month','year','POCload_g','day')
    POCload_g = POCload_g[3]
    
    POCout_g=POC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    POCout_g$day = rep(1,1,nrow(POCout_g))
    POCout_g$month = as.numeric(POCout_g$month)
    POCout_g$year = as.numeric(POCout_g$year)
    colnames(POCout_g) = c('month','year','POCout_g','day')
    POCout_g = POCout_g[3]
    
    POC_df = data.frame(Date=month_day_year, POCtotal_conc_gm3=POCtotal_conc_gm3,
                        NPPin_gm2y=NPPin_gm2y,FlowIn_gm2y=FlowIn_gm2y,FlowOut_gm2y=FlowOut_gm2y,
                        sedOut_gm2y=sedOut_gm2y,leachOut_gm2y=leachOut_gm2y,POCload_g=POCload_g,
                        POCout_g=POCout_g)
    POC_df$Date = as.Date(POC_df$Date, format = "%m/%d/%Y")
    POC_df = dplyr::arrange(POC_df, Date)
    
    ## doc
    FlowIn_gm2y=DOC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    FlowIn_gm2y$day = rep(1,1,nrow(FlowIn_gm2y))
    FlowIn_gm2y$month = as.numeric(FlowIn_gm2y$month)
    FlowIn_gm2y$year = as.numeric(FlowIn_gm2y$year)
    colnames(FlowIn_gm2y) = c('month','year','FlowIn_gm2y','day')
    month_day_year = paste0(FlowIn_gm2y$month,'/',FlowIn_gm2y$day,'/',FlowIn_gm2y$year)
    FlowIn_gm2y = FlowIn_gm2y[3]
    
    respOut_gm2y=DOC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    respOut_gm2y$day = rep(1,1,nrow(respOut_gm2y))
    respOut_gm2y$month = as.numeric(respOut_gm2y$month)
    respOut_gm2y$year = as.numeric(respOut_gm2y$year)
    colnames(respOut_gm2y) = c('month','year','respOut_gm2y','day')
    respOut_gm2y = respOut_gm2y[3]
    
    leachIn_gm2y=DOC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    leachIn_gm2y$day = rep(1,1,nrow(leachIn_gm2y))
    leachIn_gm2y$month = as.numeric(leachIn_gm2y$month)
    leachIn_gm2y$year = as.numeric(leachIn_gm2y$year)
    colnames(leachIn_gm2y) = c('month','year','leachIn_gm2y','day')
    leachIn_gm2y = leachIn_gm2y[3]
    
    FlowOut_gm2y=DOC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    FlowOut_gm2y$day = rep(1,1,nrow(FlowOut_gm2y))
    FlowOut_gm2y$month = as.numeric(FlowOut_gm2y$month)
    FlowOut_gm2y$year = as.numeric(FlowOut_gm2y$year)
    colnames(FlowOut_gm2y) = c('month','year','FlowOut_gm2y','day')
    FlowOut_gm2y = FlowOut_gm2y[3]
    
    NPPin_gm2y=DOC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    NPPin_gm2y$day = rep(1,1,nrow(NPPin_gm2y))
    NPPin_gm2y$month = as.numeric(NPPin_gm2y$month)
    NPPin_gm2y$year = as.numeric(NPPin_gm2y$year)
    colnames(NPPin_gm2y) = c('month','year','NPPin_gm2y','day')
    NPPin_gm2y = NPPin_gm2y[3]
    
    DOCtotal_conc_gm3=DOC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    DOCtotal_conc_gm3$day = rep(1,1,nrow(DOCtotal_conc_gm3))
    DOCtotal_conc_gm3$month = as.numeric(DOCtotal_conc_gm3$month)
    DOCtotal_conc_gm3$year = as.numeric(DOCtotal_conc_gm3$year)
    colnames(DOCtotal_conc_gm3) = c('month','year','DOCtotal_conc_gm3','day')
    DOCtotal_conc_gm3 = DOCtotal_conc_gm3[3]
    
    DOCload_g=DOC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    DOCload_g$day = rep(1,1,nrow(DOCload_g))
    DOCload_g$month = as.numeric(DOCload_g$month)
    DOCload_g$year = as.numeric(DOCload_g$year)
    colnames(DOCload_g) = c('month','year','DOCload_g','day')
    DOCload_g = DOCload_g[3]
    
    DOCout_g=DOC_df %>%
      mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
      group_by(month, year) %>%
      summarise(mean = mean(FlowIn_gm2y))
    DOCout_g$day = rep(1,1,nrow(DOCout_g))
    DOCout_g$month = as.numeric(DOCout_g$month)
    DOCout_g$year = as.numeric(DOCout_g$year)
    colnames(DOCout_g) = c('month','year','DOCout_g','day')
    DOCout_g = DOCout_g[3]
    
    DOC_df = data.frame(Date=month_day_year, DOCtotal_conc_gm3=DOCtotal_conc_gm3,
                        NPPin_gm2y=NPPin_gm2y,FlowIn_gm2y=FlowIn_gm2y,FlowOut_gm2y=FlowOut_gm2y,
                        respOut_gm2y=respOut_gm2y,leachIn_gm2y=leachIn_gm2y,DOCload_g=DOCload_g,
                        DOCout_g=DOCout_g)
    DOC_df$Date = as.Date(DOC_df$Date, format = "%m/%d/%Y")
    DOC_df = dplyr::arrange(DOC_df, Date)
  }
 
  R<-DOC_df$respOut_gm2y
  
  S<-POC_df$sedOut_gm2y
  
  #fOut<-(POC_df$leachOut_gm2y + POC_df$FlowOut_gm2y+ DOC_df$FlowOut_gm2y)
  fOut<-(POC_df$FlowOut_gm2y+ DOC_df$FlowOut_gm2y)
  
  #alloch<-DOC_df$FlowIn_gm2y+DOC_df$leachIn_gm2y+POC_df$FlowIn_gm2y
  #autoch<-POC_df$NPPin_gm2y+DOC_df$NPPin_gm2y
  
  alloch<-DOC_df$FlowIn_gm2y+POC_df$FlowIn_gm2y
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
monthlyFlag = 1 #1=monthly, 0=daily
Harp = SOS_fate('Harp',monthlyFlag = monthlyFlag) 
Monona = SOS_fate('Monona',monthlyFlag = monthlyFlag)
Toolik = SOS_fate('Toolik', monthlyFlag = monthlyFlag)
Trout = SOS_fate('Trout',monthlyFlag = monthlyFlag)
Vanern = SOS_fate('Vanern',monthlyFlag = monthlyFlag)

#### generate summary table of fates by lake ####
# recommend using daily
Harp_means = colMeans(Harp[,3:11])
Monona_means = colMeans(Monona[,3:11])
Toolik_means = colMeans(Toolik[,3:11])
Trout_means = colMeans(Trout[,3:11])
Vanern_means = colMeans(Vanern[,3:11])

table_heads = names(Harp_means)
fate_means = rbind.data.frame(Harp_means,Monona_means,Toolik_means,Trout_means,Vanern_means)
colnames(fate_means) = table_heads
rownames(fate_means) = c('Harp','Monona','Toolik','Trout','Vanern')
#write.csv(fate_means,'FateOutputs_byLake.csv')
  
############# plotting time series ##########
# compare R to S
# Source: R > S, Sink: R < S

lty = 2
lwd = 2
ylab = 'OC (g/m2/yr)'
xlab = 'Date'
ylim = c(-50,600)
cex.main = 2
cex.axis = 2
cex.lab = 2

png(paste0('R/ResultsViz/Figures/SOSfates.png'),width = 11,height = 9,units = 'in',res=300)
par(mar=c(3,3,3,1),mgp=c(1.5,0.4,0),mfrow=c(3,2),tck=-0.02,cex=1.2) 

  # Harp
  plot(Harp$Date,Harp$Budget_left_gm2y,type = 'h',col = 'grey70',xlab = 'Date',ylab = 'OC (g/m2/yr)',ylim = ylim,main='Harp')
  lines(Harp$Date,Harp$R_gm2y,type='h',col='gold')
  lines(Harp$Date,Harp$S_gm2y,type='h',col='black')
  legend('topright',legend=c('Total Load','Burial','Respiration'), col=c('grey70','black','gold'),lwd=2)
  abline(0,0,lty=2, lwd=2)

# Monona
  plot(Monona$Date,Monona$Budget_left_gm2y,type = 'h',col = 'grey70',xlab = 'Date',ylab = 'OC (g/m2/yr)',ylim = ylim,main='Monona')
  lines(Monona$Date,Monona$S_gm2y,type='h',col='black')
  lines(Monona$Date,Monona$R_gm2y,type='h',col='gold')
  abline(0,0,lty=2, lwd=2)

# Toolik
  plot(Toolik$Date,Toolik$Budget_left_gm2y,type = 'h',col = 'grey70',xlab = 'Date',ylab = 'OC (g/m2/yr)',ylim = ylim,main='Toolik')
  lines(Toolik$Date,Toolik$S_gm2y,type='h',col='black')
  lines(Toolik$Date,Toolik$R_gm2y,type='h',col='gold')
  abline(0,0,lty=2, lwd=2)

# Trout
  plot(Trout$Date,Trout$Budget_left_gm2y,type = 'h',col = 'grey70',xlab = 'Date',ylab = 'OC (g/m2/yr)',ylim = ylim,main='Trout')
  lines(Trout$Date,Trout$S_gm2y,type='h',col='black')
  lines(Trout$Date,Trout$R_gm2y,type='h',col='gold')
  abline(0,0,lty=2, lwd=2)

# Vanern
  plot(Vanern$Date,Vanern$Budget_left_gm2y,type = 'h',col = 'grey70',xlab = 'Date',ylab = 'OC (g/m2/yr)',ylim = ylim,main='Vanern')
  lines(Vanern$Date,Vanern$R_gm2y,type='h',col='gold')
  lines(Vanern$Date,Vanern$S_gm2y,type='h',col='black')
  abline(0,0,lty=2, lwd=2)
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

