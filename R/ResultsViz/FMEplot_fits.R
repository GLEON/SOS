# update 4-16-17 by Ian to change colors

lakenames = c('Harp','Monona','Trout','Vanern','Toolik')
library(viridis)
cols = viridis(3)

png(paste0('R/ResultsViz/Figures/FMEfit_all2.png'),width = 8,height = 11,units = 'in',res = 300)
  
  par(mar=c(2,3,2,1),mgp=c(1.5,0.5,0),mfrow=c(5,2),cex=1,tck=-0.03)
  for (l in 1:5) {
    modelDOC = read.csv(paste0('./',lakenames[l],'Lake','/Results/',lakenames[l],'_DOCvalidation.csv'),stringsAsFactors = F)
    modelDOC$datetime = strptime(modelDOC$datetime,'%Y-%m-%d')
    modelDO =  read.csv(paste0('./',lakenames[l],'Lake','/Results/',lakenames[l],'_DOvalidation.csv'),stringsAsFactors = F)
    modelDO$datetime = strptime(modelDO$datetime,'%Y-%m-%d')
    
    ylims = extendrange(modelDOC[,c(2,4)],f = 0.1)
    plot(modelDOC$datetime,modelDOC$Modelled,type='o',xlab='',ylab = expression(paste("DOC (mg L"^"-1",")")),pch=21,
         bg=cols[1],col=cols[1],main=lakenames[l], ylim = ylims,yaxt='n')
    # lines(modelDOC$datetime,modelDOC$DOCwc,type='o',col='grey50',pch=16)
    axis(2,at = pretty(range(modelDOC[,c(2,4)], na.rm=T),n = 4),
         labels =  pretty(range(modelDOC[,c(2,4)], na.rm=T),n = 4))
    lines(modelDOC$datetime,modelDOC$DOC,type='o',bg=cols[2],pch=22,col=cols[2])
    
    abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
    if (l == 1){
      legend('bottomright', legend = c('Observed','Modeled'),pt.bg=c(cols[2],cols[1]),pch=c(22,21),
             cex=0.9,lwd = 1,col = c(cols[2],cols[1]), bty='n',horiz = T,pt.cex=1.2)
    }
    
    #ylim = range(modelDO[,c(3,7)], na.rm=T)
    plot(modelDO$datetime,modelDO$Conc_Modelled,xlab='',type='o',ylab = expression(paste("DO (mg L"^"-1",")")),pch=21,main=lakenames[l],
         ylim = c(6,16),bg=cols[1],col=cols[1],yaxt='n')
    axis(2,at = c(6,8,10,12,14,16),labels = c('',8,'',12,'',16))
    lines(modelDO$datetime,modelDO$DO_con,type='o',bg=cols[2],pch=22,col=cols[2])
    abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
    
  }
dev.off()
