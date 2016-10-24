setwd('C:/Users/hdugan/Documents/Rpackages/SOS/')
setwd("~/Documents/SOS")

##### READ IN BOOTSTRAPPING RESULTS ################
readFile <- function(LakeName){
  Results <- paste('./',LakeName,'Lake/Results/',LakeName,'_boostrapResults.csv',sep='')
  df = read.csv(Results,stringsAsFactors = F)
  return(df)
}
vanern = readFile('Vanern')
mon = readFile('Monona')
trout = readFile('Trout')
toolik = readFile('Toolik')



png('R/ResultsViz/Figures/bootstrappingResults2.png',width=6,height = 6,units='in',res=300)
  names = c('DOCR_RespParam','DOCL_RespParam','R_auto','BurialFactor_R','BurialFactor_L','POC_lcR','POC_lcL','NLL')
  par(mfrow=c(3,2),mar=c(2,2,2,1),mgp=c(1.3,0.4,0),tck=-0.02)
  #bootstrapping results
  for (i in 1:6){
    df = data.frame(a = t(trout[i,]),b = t(vanern[i,]),c = t(mon[i,]), d=t(toolik[i,]))
    boxplot(df,names = c('Trout','Vanern','Monona','Toolik'), main = names[i])
  }
dev.off()