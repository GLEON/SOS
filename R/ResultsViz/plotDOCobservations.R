#CarbonFluxModel <- function(LakeName,PlotFlag,ValidationFlag){
library(lubridate)
library(zoo)
names = c('Mendota','Toolik','Vanern','Harp','Trout')

var = 'DOC'
annual = FALSE
par(mfrow=c(6,1))
for (LakeName in names){
  ##### INPUT FILE NAMES ################
  TimeSeriesFile <- paste('./',LakeName,'Lake/',LakeName,'TS.csv',sep='')
  RainFile <- paste('./',LakeName,'Lake/',LakeName,'Rain.csv',sep='')
  ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
  ValidationFileDOC <- paste('./',LakeName,'Lake/',LakeName,'ValidationDOC.csv',sep='')
  ValidationFileDO <- paste('./',LakeName,'Lake/',LakeName,'ValidationDO.csv',sep='')
  
  if (var == 'DO'){
    # DO PLOTTING
    DO <- read.csv(ValidationFileDO,header=T)
    DO = DO[complete.cases(DO),]
    DO$datetime <- as.Date(as.POSIXct(strptime(DO$datetime,"%m/%d/%Y %H:%M"),tz="GMT")) #Convert time to POSIX
    DO$year = year(DO$datetime)
    DO$yday = DO$datetime
    year(DO$yday) = 2000
    
    years = unique(DO$year)
    
    par(mar=c(2,3,1.5,1),mgp=c(1.5,0.5,0))
    df = DO[DO$year == years[1],]
    plot(df$yday,df$DO,type='o',col='red',xlim=c(as.Date('2000-01-01'),as.Date('2000-12-31')),
         ylim=c(min(DO$DO,na.rm = T),max(DO$DO,na.rm = T)),main=LakeName,ylab='DO (mg/L)',xlab = '',pch=16)
    for (y in years) {
      df = DO[DO$year == y,]
      lines(df$yday,df$DO,type='o',col=rainbow(10)[runif(1,min=1,max=10)],pch=16)
    }
  }
  
  if (var == 'DOC'){
    #DOC Validation Output Setup
    DOC <- read.csv(ValidationFileDOC,header=T)
    DOC$datetime <- as.Date(as.POSIXct(strptime(DOC$datetime,"%m/%d/%Y %H:%M"),tz="GMT")) #Convert time to POSIX
    DOC$year = year(DOC$datetime)
    
    ########## Following filter leaves some nans
    myData = DOC$DOC
    n = round(length(myData) * 0.2)
    LTma = stats::filter(myData,rep(1/n,n), method = c("convolution"), sides=2)
    iGood = !is.na(LTma)
    myData = myData - LTma
    myData = na.omit(myData)
    myData = myData[1:length(myData)]
    myTime = DOC$datetime[iGood]
    print('Removing trend...')     
    ###########
    
    # sampFreq = round(nrow(DOC)/length(years),0)
    # DOC$runMean = rollmean(x = DOC$DOC,k = sampFreq,fill = NA)
    # DOC$resid = DOC$DOC - DOC$runMean
    new = data.frame(date = myTime, DOC = myData)
    new$year = year(new$date)
    new$yday = new$date
    year(new$yday) = 2000
  
    years = unique(new$year)
    par(mar=c(2,3,1.5,1),mgp=c(1.5,0.5,0))
    
    if (annual == T) {
      df = new[new$year == years[1],]
      plot(df$yday,df$DOC,type='o',col='red',xlim=c(as.Date('2000-01-01'),as.Date('2000-12-31')),
        ylim=c(min(new$DOC,na.rm = T),max(new$DOC,na.rm = T)),main=LakeName,ylab='DOC (mg/L)',xlab = '')
      for (y in years) {
        df = new[new$year == y,]
        lines(df$yday,df$DOC,type='o',col=rainbow(20)[runif(1,min=1,max=20)])
      }
    }
    plot(DOC$datetime,DOC$DOC,type='o',col='red',main=LakeName,ylab='DOC (mg/L)',xlab = '',pch=16)
    
  }
}

