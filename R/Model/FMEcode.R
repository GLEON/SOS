
#################### OPTIMIZATION ROUTINE ############################################
library(dplyr)
library(FME)

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
rmse(joinMod$DOC_conc, joinMod$DOC) #0.43
NSE(joinMod$DOC_conc, joinMod$DOC) #0.09



# Test1: Nelder-Mead
parStart = c(0.0022,0.0027,0.9,0.33,0.1,0.05,0.05)
names(parStart) = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L','POC_lcR','POC_lcL')
Fit1 <- modFit(f = DOCdiff, p=parStart,method = 'Marq',
               lower= c(0,0,0.5,0,0,0,0),
               upper= c(0.01,0.2,1,1,1,0.3,0.3))

pars1 = Fit1$par
summary(Fit1)
# DOCR_RespParam DOCL_RespParam         R_auto BurialFactor_R BurialFactor_L        POC_lcR        POC_lcL 
# 2.308337e-03   7.680111e-09   9.771564e-01   9.999419e-01   1.539747e-06   9.173109e-08   2.999994e-01 


# Test1: Nelder-Mead
parStart = c(0.0022,0.0027,0.9,0.33,0.1,0.05,0.05)
Fit2 <- modFit(f = DOCdiff, p=parStart,method = 'BFGS',
               lower= c(0,0,0.5,0,0,0,0),
               upper= c(0.01,0.2,1,1,1,0.3,0.3))

#0.0021797,0.0004051,0.9746785,0.9668259,0.0107793,0.0042922,0.2927224
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
upper= c(0.01,0.2,1,1,1,0.3,0.3)
pRange <- data.frame(min = lower, max = upper)
rownames(pRange) = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L','POC_lcR','POC_lcL')
## 2. Calculate sensitivity: model is solved 10 times, uniform parameter distribution (default)
DOCsens <- function(p){
  pars = c(0.0021797,0.0004051,0.9746785,0.9668259,0.0107793,0.0042922,0.2927224)
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

par(mar = c(3,3,1,1),mgp=c(1.5,0.5,0),mfrow=c(4,2))
# PLOTTING
plot(joinMod$datetime,joinMod$DOC,type='o',ylab='DOC',xlab='Date',pch=16,cex=0.7,ylim=c(2,6))
# lines(joinMod$datetime,joinMod$DOCwc,type='o',col='grey50')
lines(joinMod$datetime,joinMod$DOC_conc,type='o',col='red3',pch=16,cex=0.7)
legend('topleft',legend = c('Observed','Modeled'),fill=c('black','red3'),bty='n')
for (i in 1:7){
  # png(paste0('Figures/Sensitivity_',names(startPars)[i],'.png'),width = 7,height = 7,units = 'in',res=300)
  plot.sensRange.HD(Sens[[i]],Select = 2,ylabs = "DOC",ylims = c(2,6),
                    main=paste(names(startPars)[i],' ',pRange[i,1],'-',pRange[i,2],sep=''))
  lines(as.POSIXlt(ValidationDataDOC$datetime),ValidationDataDOC$DOC,lty=2,pch=16,cex=0.7,type='o')
  # dev.off()
}


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


