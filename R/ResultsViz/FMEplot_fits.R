
lakenames = c('Harp','Monona','Trout','Vanern','Toolik')

png(paste0('R/ResultsViz/Figures/FMEfit_all.png'),width = 8,height = 11,units = 'in',res = 300)
  
  par(mar=c(2,3,2,1),mgp=c(1.5,0.5,0),mfrow=c(5,2),cex=1,tck=-0.03)
  for (l in 1:5) {
    modelDOC = read.csv(paste0('./',lakenames[l],'Lake','/Results/',lakenames[l],'_DOCvalidation.csv'),stringsAsFactors = F)
    modelDOC$datetime = strptime(modelDOC$datetime,'%Y-%m-%d')
    modelDO =  read.csv(paste0('./',lakenames[l],'Lake','/Results/',lakenames[l],'_DOvalidation.csv'),stringsAsFactors = F)
    modelDO$datetime = strptime(modelDO$datetime,'%Y-%m-%d')
    
    plot(modelDOC$datetime,modelDOC$DOC,type='o',xlab='',ylab = expression(paste("DOC (mg L"^"-1",")")),pch=16,
         col='slategray4',main=lakenames[l])
    # lines(modelDOC$datetime,modelDOC$DOCwc,type='o',col='grey50',pch=16)
    lines(modelDOC$datetime,modelDOC$Modelled,type='o',col='red4',pch=16)

    abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
    if (l == 1){
      legend('topleft',legend = c('Observed','Modeled'),col=c('slategray4','red4'),pch=16,cex=0.8,lwd = 1,bg = 'white')
    }
    
    plot(modelDO$datetime,modelDO$DO_con,xlab='',type='o',ylab = expression(paste("DO (mg L"^"-1",")")),pch=16,main=lakenames[l],
         ylim = range(modelDO[,c(3,7)]),col='slategray4')
    lines(modelDO$datetime,modelDO$Conc_Modelled,type='o',col='red4',pch=16)
    abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
    
  }
dev.off()
