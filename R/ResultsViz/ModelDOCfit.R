##### TEST RESIDUALS #####
setwd('C:/Users/hdugan/Documents/Rpackages/SOS/')

##### LOAD PACKAGES ########################
library(readr)
lakeNames = c('Monona','Harp','Trout','Vanern','Toolik')
plotFile  = T


par(mfrow=c(2,3),mar=c(2,3,2,1),mgp=c(1.3,0.3,0),tck=-0.02)
if (plotFile == T){
  png(paste0('R/ResultsViz/Figures/ModelDOCfit.png'),units = 'in',width = 8,height = 5, res = 300)
  par(mfrow=c(2,3),mar=c(2,3,2,1),mgp=c(1.3,0.3,0),tck=-0.02)
}
for (l in lakeNames){
  LakeName = l
  ##### READ FILES ########################
  DOC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOC_Results.csv',sep='')
  POC_results_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_POC_Results.csv',sep='')
  Input_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_InputData.csv',sep='')
  DOC_validation_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOCvalidation.csv',sep='')
  DO_validation_filename = paste('./',LakeName,'Lake/','Results/',LakeName,'_DOvalidation.csv',sep='')
  
  DOCfile = read_csv(file = DOC_results_filename)
  POCfile = read_csv(file = POC_results_filename)
  InputData = read_csv(file = Input_filename)
  DOCval = read_csv(file = DOC_validation_filename)
  DOval = read_csv(file = DO_validation_filename)
  
  
  plot(DOCval$datetime,DOCval$Measured,pch=16,col='black',xlab='',
       ylab = 'DOC mg/L',type='o',main=LakeName)
  abline(v = as.Date(paste0(unique(year(DOCval$datetime)),'-01-01')),lty=2,col='grey50') #lines at Jan 1
  abline(v = as.Date(paste0(unique(year(DOCval$datetime)),'-06-01')),lty=3,col='grey80') #lines at Jul 1
  lines(DOCval$datetime,DOCval$Modelled,type='o',col='darkgreen',pch=16)
  legend('bottomleft',legend = c('Observed','Modeled'),fill=c('black','darkgreen'),bty='n')
  
}
if (plotFile == T){
  dev.off()
}







