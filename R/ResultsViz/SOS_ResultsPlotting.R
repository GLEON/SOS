#Read in results data from SOS_CentralFunction.R and visualize output. 
# Updated 3-22-17 by Ian McC 

setwd('C:/Users/immcc/Desktop/SOS/')

library(dplyr)

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
  
  plot(Month_POC$Date,ocInflow + ocGPP,col='darkblue',type='l',
       ylim=c(ylim1,ylim2),ylab='OC flux (gm2/y)',xlab='',main=LakeName) # Inflow 
  # Flow 
  # lines(POC_df$Date,ocInflow + ocGPP,col='darkblue',type='l') #inflow
  # lines(POC_df$Date,ocGPP,type='l',col='cyan4') # GPP in
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocGPP,rep(0,length(ocGPP))),col = 'cyan4')
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocInflow + ocGPP,rev(ocGPP)),col = 'darkblue')
  
  # Fluxes out 
  ocOutflow = -Month_POC$FlowOut_gm2y -Month_DOC$FlowOut_gm2y
  ocSed = -Month_POC$sedOut_gm2y
  ocResp = -Month_DOC$respOut_gm2y
  
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocResp,rep(0,length(ocResp))),col = 'grey50')
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocSed + ocResp,rev(ocResp)),col = 'brown')
  polygon(x = c(Month_POC$Date,rev(Month_POC$Date)),y = c(ocOutflow + ocSed + ocResp,rev(ocSed + ocResp)),col = 'green4')
  
  #par(new=T) # Plot OC concentration on separate yaxis 
  #plot(DOC_df$Date,DOC_df$DOCtotal_conc_gm3+POC_df$POCtotal_conc_gm3,type='l',yaxt='n',ylab='',xlab='',lwd=1.5)
  #axis(side = 4)
  #mtext('OC concentration (g/m3)',side = 4,line = 1.5,cex=0.8)
  if (legend==1){
  legend('topright',legend = c('Alloch','Autoch','Export','Burial','Respiration'),#,'DOC Conc'),
         col = c('darkblue','cyan4','green4','brown','grey50'),#,'black'),
         pch=c(15,15,15,15,15,NA),
         lty=c(0,0,0,0,0,1),lwd=2,ncol=2,cex=1,pt.cex=2,seg.len=1,
         inset=0.01) }
  
}

#### run function over lakes ####
png(paste0('R/ResultsViz/Figures/OCfates_allLakes.png'),width = 11,height = 9,units = 'in',res=300)
par(mfrow=c(3,2))
  par(mar=c(3,4.5,3,1))#,mgp=c(1.5,0.4,0),mfrow=c(3,2),tck=-0.02,cex=1.2) 
  ylim1= -250
  ylim2= 250
  
  flux_plot('Monona',ylim1=ylim1,ylim2=ylim2,legend=0)
  flux_plot('Trout',ylim1=ylim1,ylim2=ylim2,legend=1)
  flux_plot('Toolik',ylim1=ylim1,ylim2=ylim2,legend=0)
  flux_plot('Vanern',ylim1=ylim1,ylim2=ylim2,legend=0)
  flux_plot('Harp',ylim1=ylim1,ylim2=ylim2,legend=0)
dev.off()

#User input lakename
LakeName = 'Harp'
png(paste0('R/ResultsViz/Figures/OCfates_',LakeName,'.png'),width = 11,height = 9,units = 'in',res=300)
  par(mar=c(3,3,3,1),mgp=c(1.5,0.4,0),mfrow=c(1,1),tck=-0.02,cex=1.2) 
  ylim1= -400
  ylim2= 400
  flux_plot(LakeName,ylim1=ylim1,ylim2=ylim2,legend=1)
dev.off()

