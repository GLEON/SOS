setwd('~/Rpackages/SOS/R/FMEresults/')
library(dplyr)

par(mfrow=c(2,2))
png('plotCollinearity.png',width = 8,height = 8,units = 'in',res = 300)
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
  m[2,] = c(0,NA,col$collinearity[7:11])
  m[3,] = c(0,0,NA,col$collinearity[12:15])
  m[4,] = c(0,0,0,NA,col$collinearity[16:18])
  m[5,] = c(0,0,0,0,NA,col$collinearity[19:20])
  m[6,] = c(0,0,0,0,0,NA,col$collinearity[21])
  m[7,] = c(0,0,0,0,0,0,NA)
  m[lower.tri(m)] = m[upper.tri(m)]
  
  par(mar=c(3,6,2,1))
  image(1:7,1:7,m,breaks=c(0,100,10000),col=c('red1','red4'),
        axes = F,xlab='',ylab='',main = lakeName)
  axis(1,at = 1:7,labels = colnames(m),cex.axis=0.7)
  axis(2,at = 1:7,labels = colnames(m),las=2,cex.axis=0.7)
  grid(7,7, col = "lightgray", lty = "dotted") #add.grid
}