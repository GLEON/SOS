setwd("~/Documents/SOS")
LakeName = 'Trout'

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

DOC_DO_diff <- function(pars){
  # DOC model 
  modeled = modelDOC(pars[1],pars[2],pars[3],pars[4])
  joinDOC = inner_join(ValidationDataDOC,modeled,by='datetime')
  resDOC = joinDOC$DOC - joinDOC$DOC_conc
  joinDO = inner_join(ValidationDataDO,modeled,by='datetime')
  resDO = joinDO$DO_con - joinDO$MetabOxygen.oxy_conc
  lengthScale = length(resDO)/length(resDOC)
  return(c(resDOC,resDO/lengthScale))
}

# # PLOTTING and GOF
# par(mfrow=c(2,1),mar=c(3,3,1,1),mgp=c(1.5,0.5,0))
# plot(joinDOC$datetime,joinDOC$DOC,type='o',xlab='',ylab='DOC')
# lines(joinDOC$datetime,joinDOC$DOCwc,type='o',col='grey50')
# lines(joinDOC$datetime,joinDOC$DOC_conc,type='o',col='red3')
# 
# plot(joinDO$datetime,joinDO$DO_con,type='o',xlab='',ylab='DO')
# lines(joinDO$datetime,joinDO$DOC_conc,type='o',col='red3')

# #Goodness of fit
# library(hydroGOF)
# rmse(c(joinDOC$DOC,joinDO$DO_con), c(joinDOC$DOC_conc,joinDO$MetabOxygen.oxy_conc)) #Harp 0.43 Trout 0.427 Monona 0.62 Vanern 0.32
# NSE(c(joinDOC$DOC,joinDO$DO_con), c(joinDOC$DOC_conc,joinDO$MetabOxygen.oxy_conc)) #Harp 0.09 Trtou -0.015 Monona 0.279 Vanern -0.03

# Starting parameters cannot be negative, because of bounds we set 
parStart = pars
lowerBound = c(0,0,0,0)
upperBound = c(0.005,0.1,1,1)
parStart[(parStart - lowerBound) < 0] = lowerBound[(parStart - lowerBound) < 0]
parStart[(upperBound - parStart) < 0] = upperBound[(upperBound - parStart) < 0]
names(parStart) = c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')

# Fit <- modFit(f = DOC_DO_diff, p=parStart,method = 'BFGS',
#                lower= lowerBound,
#                upper= upperBound)

# For difficult problems it may be efficient to perform some iterations with Pseudo, which will bring the algorithm 
# near the vicinity of a (the) minimum, after which the default algorithm (Marq) is used to locate the minimum more precisely.
Fit2 <- modFit(f = DOC_DO_diff, p=parStart,method = 'Pseudo',
               lower= lowerBound,
               upper= upperBound)

Fit2par = Fit2$par

# Constrain burial factors == 1
Fit3 <- modFit(f = DOC_DO_diff, p=c(Fit2par[1:2],1,1),method = 'Pseudo',
               lower= c(0,0,1,1),
               upper= c(0.005,0.1,1,1))

#Test Fits
fitTest <- function(pars){
  # DOC model 
  modeled = modelDOC(pars[1],pars[2],pars[3],pars[4])
  joinDOC = inner_join(ValidationDataDOC,modeled,by='datetime')
  joinDO = inner_join(ValidationDataDO,modeled,by='datetime')

  # PLOTTING and GOF
  png(paste0('R/FMEresults/',LakeName,'FMEfit.png'),width = 6,height = 8,units = 'in',res = 300)
  par(mar=c(3,3,3,1),mgp=c(1.5,0.5,0),mfrow=c(2,1),cex=0.8)
    plot(joinDOC$datetime,joinDOC$DOC,type='o',xlab='Date',ylab = 'DOC (mg/L)',pch=16,main=LakeName)
    lines(joinDOC$datetime,joinDOC$DOCwc,type='o',col='grey50',pch=16)
    lines(joinDOC$datetime,joinDOC$DOC_conc,type='o',col='red3',pch=16)
    legend('bottomleft',legend = c('ObsSurf','ObsWC','Mod'),col=c('black','grey50','red3'),pch=16)
    
    plot(joinDO$datetime,joinDO$DO_con,xlab='Date',type='o',ylab = 'DO (mg/L)',pch=16,main=LakeName)
    lines(joinDO$datetime,joinDO$MetabOxygen.oxy_conc,type='o',col='red3',pch=16)
    
  dev.off()
  #Goodness of fit
  library(hydroGOF)
  print(paste('RMSE = ',rmse(c(joinDOC$DOC,joinDO$DO_con), c(joinDOC$DOC_conc,joinDO$MetabOxygen.oxy_conc))))
  print(paste('NSE = ',NSE(c(joinDOC$DOC,joinDO$DO_con), c(joinDOC$DOC_conc,joinDO$MetabOxygen.oxy_conc))))
}

fitTest(Fit2$par)
fitTest(Fit3$par)

summary(Fit2)
summary(Fit3)
Fit2$par
Fit3$par
# Save Fit data
save(Fit2,file=paste0('R/FMEresults/',LakeName,'_fitresults.RData'))
write.csv(Fit2$par,paste0('R/FMEresults/',LakeName,'_fitpars.csv'),row.names = F)

# New parameters
newPars = Fit2$par
names(newPars) = c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
covar <- solve(0.5 * Fit2$hessian)

# Sensitivity of parameters
sF <- sensFun(DOC_DO_diff,parms = newPars,tiny = 1e-2)
plot(sF)
summary(sF)
plot(summary(sF))
collin(sF)
plot(collin(sF), log="y")
write.csv(collin(sF),paste0('R/FMEresults/',LakeName,'_collinearity.csv'),row.names = F)
##------------------------------------------------------------------------------
##   Sensitivity range
##------------------------------------------------------------------------------

pRange <- data.frame(min = lowerBound, max = upperBound)
rownames(pRange) = c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
## 2. Calculate sensitivity: model is solved 10 times, uniform parameter distribution (default)
DOCsens <- function(p){
  pars = newPars
  names(pars) = c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
  pars[names(pars) %in% names(p)] = p
  # DOC model 
  modeled = modelDOC(pars[1],pars[2],pars[3],pars[4])
  
  joinMod = inner_join(ValidationDataDOC,modeled,by='datetime')
  joinMod2 <- joinMod %>% select(datetime,DOC,DOC_conc)
  return(joinMod2)
}

startPars = newPars
Sens <- list()
for (i in 1:4){
  print(i)
  
  Sens[[i]] <- sensRange(parms=startPars[i], func=DOCsens, 
                         num=100, parRange=pRange[i,])
}
##------------------------------------------------------------------------------
##   Functions ####
##------------------------------------------------------------------------------
plot.sensRange.HD <- function (x,Select = 2,ylabs = NULL,main=NULL,ylims=NULL){
  npar <- attr(x, "npar")
  nx <- attr(x, "nx")
  varnames <- attr(x, "var")
  X <- attr(x, "x")
  X <- strptime(X,'%Y-%m-%d')
  sens <- x[, -(1:npar)]
  
  ii <- ((Select - 1) * nx + 1):(Select * nx)
  y = t(sens[,ii])
  plot(X, rowMeans(y), col = add.alpha('red3',0), lwd = 2,type='l',ylab=ylabs,xlab = 'Date',main=main,ylim=ylims) 
  min = apply(y,1,min)
  max = apply(y,1,max)
  polygon(x = c(X,rev(X)),y = c(min,rev(max)),col= add.alpha("red3",0.6),border = add.alpha("red3",0.6))
  legend('bottomright',legend = c('Mean','Max-Min'),fill=c('navy', add.alpha("red3",0.6)),bty='n')
}
add.alpha <- function(col, alpha=1){
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
##------------------------------------------------------------------------------
## Plotting ##
pars = startPars
modeled = modelDOC(pars[1],pars[2],pars[3],pars[4],pars[5],pars[6],pars[7])
joinMod = inner_join(ValidationDataDOC,modeled,by='datetime')

png(paste0('R/FMEresults/',LakeName,'_sensitivity.png'),height = 8,width = 7,units = 'in',res=300)
  par(mar = c(3,3,1,1),mgp=c(1.5,0.5,0),mfrow=c(4,2))
  # PLOTTING
  plot(joinMod$datetime,joinMod$DOC,type='o',ylab='DOC',xlab='Date',pch=16,cex=0.7,ylim=c(2,8))
  # lines(joinMod$datetime,joinMod$DOCwc,type='o',col='grey50')
  lines(joinMod$datetime,joinMod$DOC_conc,type='o',col='red3',pch=16,cex=0.7)
  legend('topleft',legend = c('Observed','Modeled'),fill=c('black','red3'),bty='n')
  for (i in 1:7){
    plot.sensRange.HD(Sens[[i]],Select = 2,ylabs = "DOC",ylims = c(3,8),
                      main=paste(names(startPars)[i],' ',pRange[i,1],'-',pRange[i,2],sep=''))
    lines(as.POSIXlt(ValidationDataDOC$datetime),ValidationDataDOC$DOC,lty=2,pch=16,cex=0.7,type='o')
  }
dev.off()


##------------------------------------------------------------------------------
##  Run MCMC
##------------------------------------------------------------------------------

## 1. use parameter covariances of fit to update parameters
Covar   <- summary(Fit2)$cov.scaled * 2.4^2/4

## mean squared residuals of fit = prior for model variance
s2prior <- Fit2$ms

## adaptive Metropolis
MCMC <- modMCMC(f=DOCdiff, p=Fit2$par,  niter=5000, ntrydr=3, #jump=Covar,
                var0=s2prior, wvar0=1, updatecov=100, lower= c(0,0,0.5,0,0,0,0),
                upper= c(0.005,0.01,1,1,1,0.1,0.5))
plot(MCMC, Full=TRUE)
hist(MCMC, Full=TRUE)
MCMC$bestpar

pairs(MCMC, Full=TRUE)
summary(MCMC)
cor(MCMC$pars)
plot(summary(sensRange(parms=pars, parInput=MCMC$par,
                       f=modelDOC, num=100)), xyswap=TRUE)




DOCR_RespParam <- optimOut$par[1]
DOCL_RespParam <- optimOut$par[2]
R_auto <- optimOut$par[3]
BurialFactor_R <- optimOut$par[4]
BurialFactor_L <- optimOut$par[5] 
POC_lcR <- optimOut$par[6]
POC_lcL <- optimOut$par[7]
}

if (updateParameters == 1){
  parameters[parameters$Parameter == 'DOCR_RespParam',2] = DOCR_RespParam
  parameters[parameters$Parameter == 'DOCL_RespParam',2] = DOCL_RespParam
  parameters[parameters$Parameter == 'R_auto',2] = R_auto
  parameters[parameters$Parameter == 'BurialFactor_R',2] = BurialFactor_R
  parameters[parameters$Parameter == 'BurialFactor_L',2] = BurialFactor_L
  parameters[parameters$Parameter == 'POC_lcR',2] = POC_lcR
  parameters[parameters$Parameter == 'POC_lcL',2] = POC_lcL
  write.table(parameters,file = ParameterFile,quote = F,row.names = F)
}





