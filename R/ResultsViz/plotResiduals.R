##### TEST RESIDUALS #####
setwd('C:/Users/hdugan/Documents/Rpackages/SOS/')

##### LOAD PACKAGES ########################
library(readr)
library(dplyr)
plotResiduals('aMendota',T)
plotResiduals('Monona',T)
plotResiduals('Harp',T)
plotResiduals('Trout',T)
plotResiduals('Vanern',T)


plotResiduals <- function(LakeName,plotFile = T) {
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
  
  ######## Calculate residuals ##########
  DOCval$ResidSurf = round(DOCval$Modelled - DOCval$Measured,3)
  DOCval$ResidWC =  round(DOCval$Modelled - DOCval$MeasuredWC,3)
  DOval$Resid = round(DOval$Modelled - DOval$Measured,3)
  
  ######## Compare dataframes ###########
  InputVal = InputData %>% dplyr::select(-datetime,-Volume)
  InputVal = InputVal[InputData$datetime %in% DOCval$datetime,]
  
  InputVal = InputVal[,order(names(InputVal))] #put in alphabetical order so all plots are the same
  if (plotFile == T){
    png(paste0('R/ResultsViz/Figures/ModelResidual_',LakeName,'.png'),units = 'in',width = 6,height = 6, res = 300)
    par(mfrow=c(3,3),mar=c(3,3,1,1),mgp=c(1.3,0.3,0),tck=-0.02)
    for (c in 1:ncol(InputVal)){
      plot(unlist(InputVal[,c]),DOCval$ResidSurf,pch=16,col=rainbow(9)[c],xlab=names(InputVal)[c],
           ylab = 'DOC residual')
      mtext(LakeName,3,line = -1.5,adj = 1,cex=1)
      abline(h=0,lty=2,col='grey70')
    }
    dev.off()
  } else {
    par(mfrow=c(3,3),mar=c(3,3,1,1),mgp=c(1.3,0.5,0),tck=-0.02)
    for (c in 1:ncol(InputVal)){
      plot(unlist(InputVal[,c]),DOCval$ResidSurf,pch=16,col=rainbow(9)[c],xlab=names(InputVal)[c],
           ylab = 'DOC residual')
      mtext(LakeName,3,line = -1.5,adj = 1,cex=1)
      abline(h=0,lty=2,col='grey70')
    }
  }
}


