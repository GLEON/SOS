# Updated 3-22-17 by Ian McC
# Note from Ian: some of the various input files still seem to have inconsistent date formatting,
# which was causing some lakes to return errors. I created some fixes based on current data/date
# structure

library(LakeMetabolizer)


#### Compare DOC observations vs. model ####
docComp = function(lakename,timestamp = "%Y-%m-%d",ylim=NULL) {
  #read in results csv files of DOC 
  #lakename = lake name
  DOCpath = paste0(lakename,'Lake','/Results/',lakename,'_DOC_Results.csv')
  DOC = read.csv(DOCpath,stringsAsFactors = F)
  DOC$Date = as.Date(strptime(DOC$Date,'%Y-%m-%d'))
  
  ValidationFileDOC <- paste('./',lakename,'Lake/',lakename,'ValidationDOC.csv',sep='')
  ValidationDataDOC <- read.csv(ValidationFileDOC,header=T)
  ValidationDataDOC$datetime <- as.Date(as.POSIXct(strptime(ValidationDataDOC$datetime,timestamp),tz="GMT")) #Convert time to POSIX
  
  plot(DOC$Date,DOC$DOCtotal_conc_gm3,type='l',xlab = '', ylab = 'DOC (mg/L)',main=deparse(lakename),
       ylim = ylim)
  points(ValidationDataDOC$datetime,ValidationDataDOC$DOC,col='red3',pch=19,cex=0.7)
  lines(ValidationDataDOC$datetime,ValidationDataDOC$DOC,lty=2,col='red3',pch=19,cex=0.7)
}

png(paste0('R/ResultsViz/Figures/compareDOC.png'),width = 7,height = 10,units = 'in',res=300)
  par(mfrow=c(5,1))
  par(mar=c(1.5,3,2,1),mgp=c(1.5,0.5,0),tck=-0.03,cex=0.8)
  # run over the lakes
  Vanern = docComp('Vanern', ylim=c(3,5),timestamp = '%Y-%m-%d')
  Toolik = docComp('Toolik',timestamp = '%Y-%m-%d')
  Trout = docComp('Trout',timestamp = '%Y-%m-%d',ylim=c(0.5,3.5))
  Monona = docComp('Monona',timestamp = '%Y-%m-%d',ylim=c(4.5,7))
  Harp = docComp('Harp', ylim=c(3,5.5),timestamp = '%Y-%m-%d')
dev.off()

#### Compare DO observations vs. model ####
doComp = function(lakename,timestamp = "%Y-%m-%d",ylim=NULL) {
  #read in results csv files of DOC 
  #lakename = lake name
  DOpath = paste0(lakename,'Lake','/Results/',lakename,'_DO_Results.csv')
  DO = read.csv(DOpath,stringsAsFactors = F)
  DO$Date = as.Date(strptime(DO$Date,'%Y-%m-%d'))
  
  ValidationFileDO <- paste('./',lakename,'Lake/',lakename,'ValidationDO.csv',sep='')
  ValidationDataDO <- read.csv(ValidationFileDO,header=T)
  ValidationDataDO$datetime <- as.Date(strptime(ValidationDataDO$datetime,timestamp))
  
  DO_sat <- o2.at.sat(ValidationDataDO[,1:2])  
  k = 0.5
  ValidationDataDO$DOflux <- k*(ValidationDataDO$DO_con - DO_sat$do.sat) #Should be divided by photic depth
  
  plot(DO$Date,DO$Oxygen,type='l',xlab = '', ylab = 'DO (flux/m2)',main=deparse(lakename),
       ylim = ylim)
  points(ValidationDataDO$datetime,ValidationDataDO$DOflux,col='red3',pch=19,cex=0.7)
  lines(ValidationDataDO$datetime,ValidationDataDO$DOflux,lty=2,col='red3',pch=19,cex=0.7)
  abline(h=0,lty=2,col='grey50')
  abline(v = as.Date(paste0(unique(year(DO$Date)),'-01-01')),lty=2,col='grey50') #lines at Jan 1
  abline(v = as.Date(paste0(unique(year(DO$Date)),'-06-01')),lty=3,col='grey80') #lines at Jul 1
}
 
png(paste0('R/ResultsViz/Figures/compareDO.png'),width = 7,height = 10,units = 'in',res=300)
  par(mfrow=c(5,1))
  par(mar=c(1.5,3,2,1),mgp=c(1.5,0.5,0),tck=-0.03,cex=0.8)
  # run over the lakes
  Vanern = doComp('Vanern', ylim=c(-2,2))
  #Toolik = doComp('Toolik')
  Trout = doComp('Trout',timestamp = '%Y-%m-%d',ylim=c(-2,2))
  Monona = doComp('Monona',timestamp = '%Y-%m-%d',ylim=c(-2,2))
  Harp = doComp('Harp', ylim=c(-2,2))
dev.off()
