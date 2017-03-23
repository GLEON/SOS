
#################### OPTIMIZATION ROUTINE ############################################
library(dplyr)
library(FME)

pars = c(DOCR_RespParam,DOCL_RespParam,R_auto,BurialFactor_R,BurialFactor_L,POC_lcR,POC_lcL)

DOCdiff <- function(p){
  # pars = c(DOCR_RespParam,DOCL_RespParam,R_auto,BurialFactor_R,BurialFactor_L,POC_lcR,POC_lcL)
  pars = p
  # DOC model 
  modeled = modelDOC(pars[1],pars[2],pars[3],pars[4],pars[5],pars[6],pars[7])
  
  joinMod = inner_join(ValidationDataDOC,modeled,by='datetime')
  resDOC = joinMod$DOC - joinMod$DOC_conc

  return(resDOC)
}

# PLOTTING and GOF
plot(joinMod$datetime,joinMod$DOC,type='o')
lines(joinMod$datetime,joinMod$DOCwc,type='o',col='grey50')
lines(joinMod$datetime,joinMod$DOC_conc,type='o',col='red3')
#Goodness of fit
library(hydroGOF)
rmse(joinMod$DOC_conc, joinMod$DOC) #Harp 0.43 Trout 0.427 Monona 0.62 Vanern 0.32
NSE(joinMod$DOC_conc, joinMod$DOC) #Harp 0.09 Trtou -0.015 Monona 0.279 Vanern -0.03

# Test1: BFGS
# parStart = c(0.0022,0.0027,0.9,0.33,0.1,0.05,0.05) #Harp
# parStart = c(0.022288,0.00495,0.707381,0.479661,0.01,0.01,0.480108) #Trout
parStart = c(0.01,0.01,0.9,0.000507,0.012573,0.046772,0.46913) # Cannonsville
parStart = c(0.00079,0.00223,0.815567,0.195992,0.13656,0.01,0.043347) # Monona
parStart = c(0.00153205993800365,0.000695425284613213,0.554185248724029,0.0111865174956529,0.217093487420212,0.001,0.0204770652079501) #Vanern

names(parStart) = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L','POC_lcR','POC_lcL')
Fit2 <- modFit(f = DOCdiff, p=parStart,method = 'BFGS',
               lower= c(0,0,0.5,0,0,0,0),
               upper= c(0.01,0.01,1,1,1,0.1,0.5))

#0.0021797,0.0004051,0.9746785,0.9668259,0.0107793,0.0042922,0.2927224 #HARP
#0.005345,0.00267,0.8558,0.904096,0.010085,0.008615,0.483261 #Trout
#0.02376539391  0.00002679875  0.96910872072  0.95797337438  0.99179312975  0.00001154516  0.41626287998 #Cannonsville
#0.001572,0.001184,0.514193,0.892303,0.973353,0.009416,0.121458 #Monona
#0.023765,0.000027,0.969109,0.957973,0.991793,0.000012,0.416263 #Vanern

Fit2 <- modFit(f = DOCdiff, p=parStart,method = 'Pseudo',
               lower= c(0,0,0.5,0,0,0,0),
               upper= c(0.005,0.01,1,1,1,0.1,0.5))

summary(Fit2)
pars1 = Fit2$par
names(pars1) = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L','POC_lcR','POC_lcL')

covar <- solve(0.5 * Fit2$hessian)

# Sensitivity of parameters
sF <- sensFun(DOCdiff,parms = pars1,tiny = 1e-2)
plot(sF)
summary(sF)
plot(summary(sF))
collin(sF)
plot(collin(sF), log="y")

##------------------------------------------------------------------------------
##   Sensitivity range
##------------------------------------------------------------------------------
lower= c(0,0,0.5,0,0,0,0)
upper= c(0.005,0.01,1,1,1,0.1,0.5)
pRange <- data.frame(min = lower, max = upper)
rownames(pRange) = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L','POC_lcR','POC_lcL')
## 2. Calculate sensitivity: model is solved 10 times, uniform parameter distribution (default)
DOCsens <- function(p){
  pars = pars1
  names(pars) = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L','POC_lcR','POC_lcL')
  
  pars[names(pars) %in% names(p)] = p

  # DOC model 
  modeled = modelDOC(pars[1],pars[2],pars[3],pars[4],pars[5],pars[6],pars[7])
  
  joinMod = inner_join(ValidationDataDOC,modeled,by='datetime')
  joinMod2 <- joinMod %>% select(datetime,DOC,DOC_conc)
  return(joinMod2)
}

startPars = pars1
Sens <- list()
for (i in 1:7){
  print(i)
  
  Sens[[i]] <- sensRange(parms=startPars[i], func=DOCsens, 
                         num=10, parRange=pRange[i,])
}

## Plotting ##
source('~/Dropbox/2017MadisonGLM_TestFolder/GLMtest/plot.sensRange.HD.R')
pars = startPars
modeled = modelDOC(pars[1],pars[2],pars[3],pars[4],pars[5],pars[6],pars[7])
joinMod = inner_join(ValidationDataDOC,modeled,by='datetime')

png(paste0('R/ResultsViz/SensitivityAnalyses/par',LakeName,'.png'),height = 8,width = 7,units = 'in',res=300)
  par(mar = c(3,3,1,1),mgp=c(1.5,0.5,0),mfrow=c(4,2))
  # PLOTTING
  plot(joinMod$datetime,joinMod$DOC,type='o',ylab='DOC',xlab='Date',pch=16,cex=0.7,ylim=c(2,6))
  # lines(joinMod$datetime,joinMod$DOCwc,type='o',col='grey50')
  lines(joinMod$datetime,joinMod$DOC_conc,type='o',col='red3',pch=16,cex=0.7)
  legend('topleft',legend = c('Observed','Modeled'),fill=c('black','red3'),bty='n')
  for (i in 1:7){
    # png(paste0('Figures/Sensitivity_',names(startPars)[i],'.png'),width = 7,height = 7,units = 'in',res=300)
    plot.sensRange.HD(Sens[[i]],Select = 2,ylabs = "DOC",ylims = c(3,8),
                      main=paste(names(startPars)[i],' ',pRange[i,1],'-',pRange[i,2],sep=''))
    lines(as.POSIXlt(ValidationDataDOC$datetime),ValidationDataDOC$DOC,lty=2,pch=16,cex=0.7,type='o')
    # dev.off()
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
  plot(X, rowMeans(y), col = "navy", lwd = 2,type='l',ylab=ylabs,xlab = 'Date',main=main,ylim=ylims) 
  min = apply(y,1,min)
  max = apply(y,1,max)
  polygon(x = c(X,rev(X)),y = c(min,rev(max)),col= add.alpha("red3",0.6),border = add.alpha("red3",0.6))
  legend('bottomright',legend = c('Mean','Max-Min'),fill=c('navy', add.alpha("red3",0.6)),bty='n')
}


