setwd("~/Documents/SOS")


LakeName = 'Monona'
lakenames = c('Monona','Trout','Harp','Vanern','Toolik')
for (LakeName in lakenames) {
  ##### LOAD PACKAGES ########################
  library(lubridate)
  library(LakeMetabolizer)
  library(dplyr)
  library(FME)
  library(zoo)
  library(plyr)
  ##### INPUT FILE NAMES ################
  TimeSeriesFile <- paste('./',LakeName,'Lake/',LakeName,'TS.csv',sep='')
  RainFile <- paste('./',LakeName,'Lake/',LakeName,'Rain.csv',sep='')
  ParameterFile <- paste('./',LakeName,'Lake/','ConfigurationInputs',LakeName,'.txt',sep='')
  ValidationFileDOC <- paste('./',LakeName,'Lake/',LakeName,'ValidationDOC.csv',sep='')
  ValidationFileDO <- paste('./',LakeName,'Lake/',LakeName,'ValidationDO.csv',sep='')
  timestampFormat =	'%Y-%m-%d'
  
  ##### READ MAIN INPUT FILE #################
  RawData <- read.csv(TimeSeriesFile,header=T) #Read main data file with GLM outputs (physical input) and NPP input
  RawData$datetime <- as.POSIXct(strptime(RawData$datetime,timestampFormat),tz="GMT") #Convert time to POSIX
  cc = which(complete.cases(RawData))
  RawData = RawData[cc[1]:tail(cc,1),]
  
  # Fill time-series gaps (linear interpolation)
  ts_new <- data.frame(datetime = seq(RawData$datetime[1],RawData$datetime[nrow(RawData)],by="day")) #Interpolate gapless time-series
  InputData <- merge(RawData,ts_new,all=T)
  for (col in 2:ncol(InputData)){
    InputData[,col] <- na.approx(InputData[,col],na.rm = T)}
  InputData$Chla[InputData$Chla == 0] = 0.0001
  
  ##### READ RAIN FILE #######################
  RainData <- read.csv(RainFile,header=T,stringsAsFactors = F) #Read daily rain file (units=mm) Read separately and added back to main file to avoid issues of linear interpolation with rain data in length units
  RainData$datetime <- as.POSIXct(strptime(RainData$datetime,timestampFormat,tz='GMT'))
  InputData$Rain <- RainData$Rain[RainData$datetime %in% InputData$datetime] #Plug daily rain data into InputData file to integrate with original code.
  
  #DOC Validation Output Setup
  ValidationDataDOC <- read.csv(ValidationFileDOC,header=T,stringsAsFactors = F)
  ValidationDataDOC$datetime <- as.Date(as.POSIXct(strptime(ValidationDataDOC$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
  ValidationDataDOC = ValidationDataDOC[complete.cases(ValidationDataDOC),]
  outlier.limit = (mean(ValidationDataDOC$DOC) + 3*(sd(ValidationDataDOC$DOC))) # Calculate mean + 3 SD of DOC column
  ValidationDataDOC = ValidationDataDOC[ValidationDataDOC$DOC <= outlier.limit,] # Remove rows where DOC > outlier.limit
  ValidationDataDOC = ddply(ValidationDataDOC,'datetime',summarize,DOC=mean(DOC),DOCwc=mean(DOCwc))
  
  #DO Validation 
  #DO Validation Output Setup
  ValidationDataDO <- read.csv(ValidationFileDO,header=T)
  ValidationDataDO$datetime <- as.Date(as.POSIXct(strptime(ValidationDataDO$datetime,timestampFormat),tz="GMT")) #Convert time to POSIX
  ValidationDataDO = ValidationDataDO[complete.cases(ValidationDataDO),]
  k <- 0.5 #m/d
  PhoticDepth <- data.frame(datetime = InputData$datetime,PhoticDepth = log(100)/(1.7/InputData$Secchi))
  IndxVal = ValidationDataDO$datetime %in% as.Date(PhoticDepth$datetime)
  IndxPhotic = as.Date(PhoticDepth$datetime) %in% ValidationDataDO$datetime
  
  ValidationDataDO = ValidationDataDO[IndxVal,]
  ValidationDataDO$DO_sat <- o2.at.sat(ValidationDataDO[,1:2])[,2]  
  ValidationDataDO$Flux <- k*(ValidationDataDO$DO_con - ValidationDataDO$DO_sat)/(PhoticDepth$PhoticDepth[IndxPhotic]) #g/m3/d
  
  
  ##### LOAD FUNCTIONS #######################
  source("./R/Model_March2017/SOS_Sedimentation.R")
  source("./R/Model_March2017/SOS_SWGW.R")
  source("./R/Model_March2017/SOS_GPP.R")
  source("./R/Model_March2017/SOS_Resp.R")
  source("./R/Model_March2017/modelDOC_7.R")
  
  ##### READ PARAMETER FILE ##################
  parameters <- read.table(file = ParameterFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  for (i in 1:nrow(parameters)){ # assign parameters
    assign(parameters[i,1],parameters[i,2])
  }
  ###### Run Period and Time Step Setup #####
  TimeStep <- as.numeric(InputData$datetime[2]-InputData$datetime[1]) #days
  steps <- nrow(InputData)
  #################### OPTIMIZATION ROUTINE ############################################
  
  pars = c(DOCR_RespParam,DOCL_RespParam,BurialFactor_R,BurialFactor_L)
  names(pars) = c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
  
  ##------------------------------------------------------------------------------
  ##   Sensitivity range
  ##------------------------------------------------------------------------------
  lowerBound = c(0.00003,0.03,0,0)
  upperBound = c(0.03,0.3,1,1)
  pRange <- data.frame(min = lowerBound, max = upperBound)
  rownames(pRange) = c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
  ## 2. Calculate sensitivity: model is solved 10 times, uniform parameter distribution (default)
  DOCsens <- function(p){
    names(pars) = c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
    pars[names(pars) %in% names(p)] = p
    # DOC model 
    modeled = modelDOC(pars[1],pars[2],pars[3],pars[4])
    
    joinMod = inner_join(ValidationDataDOC,modeled,by='datetime')
    joinMod2 <- joinMod %>% select(datetime,DOC,DOC_conc)
    return(joinMod2)
  }
  

  Sens <- list()
  for (i in 1:4){
    print(i)
    
      Sens[[i]] <- sensRange(parms=pars[i], func=DOCsens, 
                             num=20, parRange=pRange[i,])
    }
    assign(x = paste0(LakeName,'_Sens'),value = Sens)
}

plot.sensRange.HD <- function (x,Select = 2,cols){
  npar <- attr(x, "npar")
  nx <- attr(x, "nx")
  varnames <- attr(x, "var")
  X <- attr(x, "x")
  X <- as.Date(strptime(X,'%Y-%m-%d'))
  sens <- x[, -(1:npar)]
  
  ii <- ((Select - 1) * nx + 1):(Select * nx)
  y = t(sens[,ii])
  lines(X, rowMeans(y), col = add.alpha(cols,0), lwd = 2,type='l') 
  min = apply(y,1,min)
  max = apply(y,1,max)
  polygon(x = c(X,rev(X)),y = c(min,rev(max)),col= add.alpha(cols,0.6),border = add.alpha(cols,0.6))
}

###### PLOTTING #############
png(paste0('R/FMEresults/',LakeName,'_sensitivity_all.png'),height = 8,width = 7,units = 'in',res=300)
  par(mar = c(3,3,1,1),mgp=c(1.5,0.5,0),mfrow=c(1,1))

  plot(joinDOC$datetime,joinDOC$DOC,type='o',ylab='DOC',xlab='Date',pch=16,cex=0.7,ylim=c(1,8))

  cols = c('navy','red3','darkgreen','gold')
  for (i in 1:4){
    plot.sensRange.HD(Sens[[i]],Select = 2,cols = cols[i])
    lines(as.POSIXlt(ValidationDataDOC$datetime),ValidationDataDOC$DOC,lty=2,pch=16,cex=0.7,type='o')
  }
  
  legend('topleft',legend = c('Observed','Respiration_Alloch','Respiration_Auto','Burial_Alloch','Burial_Auto'),
         fill=c('black',cols),bty='n')
dev.off()