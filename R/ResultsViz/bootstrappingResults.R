setwd("~/Rpackages/SOS/")
library(viridis)

trout = read.csv('TroutLake/Results/Trout_boostrapResults.csv',stringsAsFactors = F)
harp = read.csv('HarpLake//Results/Harp_boostrapResults.csv',stringsAsFactors = F)
mon = read.csv('MononaLake/Results/Monona_boostrapResults.csv',stringsAsFactors = F)
too = read.csv('ToolikLake/Results/Toolik_boostrapResults.csv',stringsAsFactors = F)
van = read.csv('VanernLake/Results/Vanern_boostrapResults.csv',stringsAsFactors = F)

cols = viridis(5)
ylabs = colnames(trout)
png('r/ResultsViz/Figures/bootstrapping.png',width = 5,height = 5,units = 'in',res = 300)
  par(mfrow = c(2,2),mar=c(3,3,0.5,0.5),mgp=c(1.5,0.5,0),tck=-0.03)
  for (i in 1:4) {
    boxplot(cbind(harp[,i],mon[,i],trout[,i],van[,i],too[,i]),col=adjustcolor(cols,0.7),
            names = c('Ha','Mo','Tr','Va','To'),
            ylab = ylabs[i],pch=21,outbg=adjustcolor(cols,0.7),outcol='black')
  }
dev.off()


getResults <- function(LakeName){
  FreeParFile <- paste('./R/FMEresults/',LakeName,'_fitpars.csv',sep='')
  freeParams <- read.table(file = FreeParFile,header=TRUE,comment.char="#",stringsAsFactors = F)
  freeParamsNames <- c('DOCR_RespParam','DOCL_RespParam','BurialFactor_R','BurialFactor_L')
  for (i in 1:4){ # assign parameters
    assign(freeParamsNames[i],freeParams[i,1])
  }
  bs = read.csv(paste0(LakeName,'Lake/Results/',LakeName,'_boostrapResults.csv'),stringsAsFactors = F)
  
  par(mfrow = c(2,2),mar=c(3,3,0.5,0.5),mgp=c(1.5,0.5,0),tck=-0.03)
  
  for (i in 1:4) {
    print(freeParamsNames[i])
    print(paste0('fit parameter = ',freeParams[i,1]))
    print(paste0('mean = ',mean(bs[,i])))
    print(paste0('sd = ',sd((bs[,i]))/sqrt(length((bs[,i])))))
    boxplot(bs[,i],col='slategrey')
    points(mean(bs[,i]),pch=16,col='red3',cex=2)
  }
}

getResults('Harp')
getResults('Monona')
getResults('Toolik')
getResults('Trout')
getResults('Vanern')


for (i in 1:4) {
  print(names(harp[i]))
  print(mean(harp[,i]))
  print(mean(mon[,i]))
  print(mean(too[,i]))
  print(mean(trout[,i]))
  print(mean(van[,i]))
}
