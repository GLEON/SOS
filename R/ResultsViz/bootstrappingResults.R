setwd("~/Rpackages/SOS/")

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