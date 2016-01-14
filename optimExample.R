## Setup the function to be optimized because it can only take in one parameter
## that contains values being optimized (here: param1, param2, param3)
toOptim <- function(param){
  calcModelNLL(param[1], param[2], param[3], lake$depth, lake$perWetlands,day$DO)
}
## Optimization of GPP and R using optim
optimOut = optim(par = c(p1,p2,p3), fn = toOptim)


## Calculate negative log likelihood 
calcModelNLL <- function(p1, p2, p3, depth, %wetlands, obsDOC){
  modeled = modelDOC(p1, p2, p3, depth, %wetlands)
  res 	= obsDOC - modeled
  
  nRes 	= length(res)
  SSE 	= sum(res^2)
  sigma2 	= SSE/nRes
  NLL 	= 0.5*((SSE/sigma2) + nRes*log(2*pi*sigma2))
  return(NLL)
}
