setwd('~/Rpackages/SOS/R/FMEresults/')
library(dplyr)


png('plotCollinearity.png',width = 8,height = 8,units = 'in',res = 300)
par(mfrow=c(2,2))
  plotCollinearity('Trout')
  plotCollinearity('Vanern')
  plotCollinearity('Monona')
  plotCollinearity('Harp')
dev.off()

plotCollinearity <- function(lakeName) {

  col = read.csv(paste0(lakeName,'_collinearity.csv'),stringsAsFactors = F)
  col <- col %>% filter(N == 2)
  m = matrix(data = NA, ncol=7,nrow=7)
  colnames(m) = names(col[,1:7])
  m[1,] = c(NA,col$collinearity[1:6])
  m[2,] = c(NA,NA,col$collinearity[7:11])
  m[3,] = c(NA,NA,NA,col$collinearity[12:15])
  m[4,] = c(NA,NA,NA,NA,col$collinearity[16:18])
  m[5,] = c(NA,NA,NA,NA,NA,col$collinearity[19:20])
  m[6,] = c(NA,NA,NA,NA,NA,NA,col$collinearity[21])
  m[7,] = c(NA,NA,NA,NA,NA,NA,NA)
  # m[t(upper.tri(m))] = m[upper.tri(m)]
  
  par(mar=c(3,6,2,1))
  image(1:7,1:7,m,breaks=c(0,100,10000),col=c('tomato','red4'),
        axes = F,xlab='',ylab='',main = lakeName)
  axis(1,at = 1:7,labels = colnames(m),cex.axis=0.7)
  axis(2,at = 1:7,labels = colnames(m),las=2,cex.axis=0.7)
  grid(7,7, col = 'grey80', lty = "dotted") #add.grid
}

text(x = c(1:7,1:7,1:7,1:7,1:7,1:7,1:7),
     y =c(rep(1,7),rep(2,7),rep(3,7),rep(4,7),rep(5,7),rep(6,7),rep(7,7)),
     labels=round(as.vector(m),), cex= 0.7)
