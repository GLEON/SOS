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
  m = matrix(data = NA, ncol=4,nrow=4)
  colnames(m) = names(col[,1:4])
  m[1,] = c(NA,col$collinearity[1:3])
  m[2,] = c(NA,NA,col$collinearity[4:5])
  m[3,] = c(NA,NA,NA,col$collinearity[6])
  # m[t(upper.tri(m))] = m[upper.tri(m)]
  
  par(mar=c(3,6,2,1))
  image(1:4,1:4,m,breaks=c(0,100,10000),col=c('tomato','red4'),
        axes = F,xlab='',ylab='',main = lakeName)
  axis(1,at = 1:4,labels = colnames(m),cex.axis=0.7)
  axis(2,at = 1:4,labels = colnames(m),las=2,cex.axis=0.7)
  grid(4,4, col = 'grey80', lty = "dotted") #add.grid
  
  text(x = c(1:4,1:4,1:4,1:4,1:4,1:4,1:4),
       y =c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4),rep(6,4),rep(7,4)),
       labels=round(as.vector(m),3), cex= 0.7)
}


