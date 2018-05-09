# update 5-9-18 by Ian
# changed loop to Harp, monona and Trout
# Vanern and Toolik manually plotted to give manual control of axes
# Vanern straightened out, but Toolik left as is due to year differences between DO/DOC

lakenames = c('Harp','Monona','Trout')#,'Vanern','Toolik')
library(viridis)
cols = viridis(3)

png(paste0('R/ResultsViz/Figures/FMEfit_all3.png'),width = 8,height = 11,units = 'in',res = 300)
  
  par(mar=c(2,3,2,1),mgp=c(1.5,0.5,0),mfrow=c(5,2),cex=1,tck=-0.03)
  for (l in 1:3) {
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
         ylim = c(4,18),bg=cols[1],col=cols[1],yaxt='n')
    axis(2,at = c(4,6,8,10,12,14,16,18))#,labels = c(4,'',8,'',12,'',16,''))
    lines(modelDO$datetime,modelDO$DO_con,type='o',bg=cols[2],pch=22,col=cols[2])
    abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
    
  }
  
  ### add Vanern and Toolik manually so axes can be customized
  # Vanern
  modelDOC = read.csv(paste0('./','VanernLake','/Results/','Vanern_DOCvalidation.csv'),stringsAsFactors = F)
  modelDOC$datetime = strptime(modelDOC$datetime,'%Y-%m-%d')
  modelDO =  read.csv(paste0('./','VanernLake','/Results/','Vanern_DOvalidation.csv'),stringsAsFactors = F)
  modelDO$datetime = strptime(modelDO$datetime,'%Y-%m-%d')
  
  ylims = extendrange(modelDOC[,c(2,4)],f = 0.1)
  plot(modelDOC$datetime,modelDOC$Modelled,type='o',xlab='',ylab = expression(paste("DOC (mg L"^"-1",")")),pch=21,
       bg=cols[1],col=cols[1],main='Vanern', ylim = ylims,yaxt='n', xlim=c(as.POSIXct('2001-06-01'), as.POSIXct('2014-01-01')))
  axis(2,at = pretty(range(modelDOC[,c(2,4)], na.rm=T),n = 4),
       labels =  pretty(range(modelDOC[,c(2,4)], na.rm=T),n = 4))
  #axis(1, at=seq(2002,2014,4), labels=seq(2002,2014,4))
  lines(modelDOC$datetime,modelDOC$DOC,type='o',bg=cols[2],pch=22,col=cols[2])
  
  abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
  
  #ylim = range(modelDO[,c(3,7)], na.rm=T)
  plot(modelDO$datetime,modelDO$Conc_Modelled,xlab='',type='o',ylab = expression(paste("DO (mg L"^"-1",")")),pch=21,main='Vanern',
       ylim = c(4,18),bg=cols[1],col=cols[1],yaxt='n', xlim=c(as.POSIXct('2001-06-01'), as.POSIXct('2014-01-01')))
  axis(2,at = c(4,6,8,10,12,14,16,18))#,labels = c(4,'',8,'',12,'',16,''))
  lines(modelDO$datetime,modelDO$DO_con,type='o',bg=cols[2],pch=22,col=cols[2])
  abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
  
  # Toolik
  modelDOC = read.csv(paste0('./','ToolikLake','/Results/','Toolik_DOCvalidation.csv'),stringsAsFactors = F)
  modelDOC$datetime = strptime(modelDOC$datetime,'%Y-%m-%d')
  modelDO =  read.csv(paste0('./','ToolikLake','/Results/','Toolik_DOvalidation.csv'),stringsAsFactors = F)
  modelDO$datetime = strptime(modelDO$datetime,'%Y-%m-%d')
  
  ylims = extendrange(modelDOC[,c(2,4)],f = 0.1)
  plot(modelDOC$datetime,modelDOC$Modelled,type='o',xlab='',ylab = expression(paste("DOC (mg L"^"-1",")")),pch=21,
       bg=cols[1],col=cols[1],main='Toolik', ylim = ylims,yaxt='n')
  axis(2,at = pretty(range(modelDOC[,c(2,4)], na.rm=T),n = 4),
       labels =  pretty(range(modelDOC[,c(2,4)], na.rm=T),n = 4))
  lines(modelDOC$datetime,modelDOC$DOC,type='o',bg=cols[2],pch=22,col=cols[2])
  
  abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
  
  #ylim = range(modelDO[,c(3,7)], na.rm=T)
  plot(modelDO$datetime,modelDO$Conc_Modelled,xlab='',type='o',ylab = expression(paste("DO (mg L"^"-1",")")),pch=21,main='Toolik',
       ylim = c(4,18),bg=cols[1],col=cols[1],yaxt='n')
  axis(2,at = c(4,6,8,10,12,14,16,18))#,labels = c(4,'',8,'',12,'',16,''))
  lines(modelDO$datetime,modelDO$DO_con,type='o',bg=cols[2],pch=22,col=cols[2])
  abline(v = seq.POSIXt(as.POSIXct('1990-01-01'),as.POSIXct('2015-01-01'),by='year'),col='grey80',lty=2)
  
  
dev.off()
