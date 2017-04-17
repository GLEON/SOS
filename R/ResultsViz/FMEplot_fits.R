# update 4-16-17 by Ian to change colors

lakenames = c('Harp','Monona','Trout','Vanern','Toolik')

png(paste0('R/ResultsViz/Figures/FMEfit_all3.png'),width = 8,height = 11,units = 'in',res = 300)
  
  par(mar=c(2,3,2,1),mgp=c(1.5,0.5,0),mfrow=c(5,2),cex=1,tck=-0.03)
  for (l in 1:5) {
    modelDOC = read.csv(paste0('./',lakenames[l],'Lake','/Results/',lakenames[l],'_DOCvalidation.csv'),stringsAsFactors = F)
    modelDOC$datetime = strptime(modelDOC$datetime,'%Y-%m-%d')
    modelDO =  read.csv(paste0('./',lakenames[l],'Lake','/Results/',lakenames[l],'_DOvalidation.csv'),stringsAsFactors = F)
    modelDO$datetime = strptime(modelDO$datetime,'%Y-%m-%d')
    
    plot(modelDOC$datetime,modelDOC$Modelled,type='o',xlab='',ylab = expression(paste("DOC (mg L"^"-1",")")),pch=21,
         bg='black',main=lakenames[l], ylim = range(modelDOC[,c(2,4)], na.rm=T))
    # lines(modelDOC$datetime,modelDOC$DOCwc,type='o',col='grey50',pch=16)
    lines(modelDOC$datetime,modelDOC$DOC,type='o',bg='black',pch=0)

    abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
    if (l == 1){
      legend('bottomright', legend = c('Observed','Modeled'),pt.bg=c('white','black'),pch=c(0,21),cex=0.8,lwd = 1,bg = 'white', bty='n',horiz = T)
    }
    
    plot(modelDO$datetime,modelDO$Conc_Modelled,xlab='',type='o',ylab = expression(paste("DO (mg L"^"-1",")")),pch=21,main=lakenames[l],
         ylim = range(modelDO[,c(3,7)], na.rm=T),bg='black')
    lines(modelDO$datetime,modelDO$DO_con,type='o',bg='grey50',pch=0)
    abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
    
  }
dev.off()
