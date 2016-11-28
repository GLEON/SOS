#### Compare DOC observations vs. model ####
docComp = function(lakename,timestamp = "%m/%d/%Y",ylim=NULL) {
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
  Vanern = docComp('Vanern')
  Toolik = docComp('Toolik')
  Trout = docComp('Trout',ylim=c(2,3.5))
  Monona = docComp('Monona',timestamp = '%Y-%m-%d',ylim=c(4,6.5))
  Harp = docComp('Harp')

dev.off()
