#### The legendary beard plot ####
# Warning: things could get hairy

library(lubridate)
library(fields)

#### Set your own working directory #####
#setwd("C:/Users/Ian/Desktop/GLEON/SOS/")

#lake_list = c('Vanern','Toolik','Trout','Mendota','Harp')

#### define (dis)function ####
clean_shave = function(lakename) {
  
  #read in results csv files of DOC and POC
  wd = getwd()
  DOCpath = paste0(wd,'/',lakename,'Lake','/Results/',lakename,'_DOC_Results.csv')
  DOC = read.csv(DOCpath)
  POCpath = paste0(wd,'/',lakename,'Lake','/Results/',lakename,'_POC_Results.csv')
  POC = read.csv(POCpath)
  Inputpath = paste0(wd,'/',lakename,'Lake','/Results/',lakename,'_InputData.csv')
  InputData = read.csv(Inputpath)

  #IM: redid above lines because had been using cumulative totals for some fluxes; didn't make sense
  lake_df = data.frame(Date = DOC$Date, Resp = DOC$respOut_gm2y,
                       Sed = POC$sedOut_gm2y, Alloch = DOC$FlowIn_gm2y+DOC$leachIn_gm2y+POC$FlowIn_gm2y,
                       Autoch = POC$NPPin_gm2y+DOC$NPPin_gm2y,Temp = InputData$EpiTemp)

  # retain only complete cases
  cc = which(complete.cases(lake_df))
  lake_df = lake_df[cc[1]:tail(cc,1),]
  
  # get rid of 0 values and replace with NA, then omit those rows
  lake_df[lake_df == 0] <- NA
  lake_df = na.omit(lake_df)
  
  # Calculate respiration-sedimentation and autochthonous/allochthonous ratio
  # and natural log transform them
  lake_df$RS = lake_df$Resp / lake_df$Sed
  lake_df$logRS = log10(lake_df$RS)
  lake_df$AA = lake_df$Alloch/lake_df$Autoch
  lake_df$logAA = log10(lake_df$AA)

  # Output each lake as a dataframe
  data.frame(Date = lake_df$Date, RS = lake_df$RS, logRS = lake_df$logRS, AA = lake_df$AA, logAA = lake_df$logAA,
             temp = lake_df$Temp)
}

# run over the lakes
Vanern = clean_shave('Vanern')
Toolik = clean_shave('Toolik')
Trout = clean_shave('Trout')
Monona = clean_shave('Monona')
Harp = clean_shave('Harp')
#Annie = clean_shave('Annie')

# plot
png(paste0('R/ResultsViz/Figures/beardplot2.png'),width = 8,height = 4,units = 'in',res=300)
  par(mfrow=c(2,3))
  par(mar=c(2.5,2.5,2.5,1),mgp=c(1.5,0.5,0),tck=-0.03,cex=0.8)
  xlab = 'log(Resp/Burial)'
  ylab = 'log(Alloch/Autoch)'
  pch = 20
  
  cr = colorRampPalette(c('slategray1','firebrick1','firebrick4'))
  cols = rev(heat.colors(27))[round(Harp$temp)]

  plot(logAA ~ logRS, Harp, xlab = xlab, ylab = ylab, main='Harp', pch=pch, col= cols)
  mtext(side=3, paste0('n=',nrow(Harp)), cex=0.6)
  abline(h=0,v=0,lty=2,col='navy',lwd=1.5)
  
  cols = rev(heat.colors(27))[round(Monona$temp)]
  plot(logAA ~ logRS, Monona, xlab = xlab, ylab = ylab, main='Monona', pch=pch, col= cols)
  mtext(side=3, paste0('n=',nrow(Monona)), cex=0.6)
  abline(h=0,v=0,lty=2,col='navy',lwd=1.5)
  
  cols = rev(heat.colors(27))[round(Trout$temp)]
  plot(logAA ~ logRS, Trout, xlab = xlab, ylab = ylab, main='Trout', pch=pch, col= cols,xlim=c(-0.1,2.2))
  mtext(side=3, paste0('n=',nrow(Trout)), cex=0.6)
  abline(h=0,v=0,lty=2,col='navy',lwd=1.5)
  
  cols = rev(heat.colors(27))[round(Vanern$temp)]
  plot(logAA ~ logRS, Vanern, xlab = xlab, ylab = ylab, main='Vanern', pch=pch, col=cols)
  mtext(side=3, paste0('n=',nrow(Vanern)), cex=0.6)
  abline(h=0,v=0,lty=2,col='navy',lwd=1.5)
  
  cols = rev(heat.colors(27))[round(Toolik$temp)]
  plot(logAA ~ logRS, Toolik, xlab = xlab, ylab = ylab, main='Toolik', pch=pch, col= cols)
  mtext(side=3, paste0('n=',nrow(Toolik)), cex=0.6)
  abline(h=0,v=0,lty=2,col='navy',lwd=1.5)
  
  # Plot legend
  plot.new()
    # legend('topleft',legend = c(1:30),ncol=1,fill=rev(heat.colors(30)),bty='n')
    colorbar.plot(0,0.5, strip = c(1:27), strip.width = 0.2, strip.length = 0.5,
                  adj.x = 0.5, adj.y = 0.5, col = heat.colors(27), 
                  horizontal = F)
    text(0.15,0,labels = '27')
    text(0.15,1,labels = '0')
    text(0.2,0.5,labels = 'Temperature °C',srt=90)
    
  par(new=T)
  par(mar=c(2.5,7,2.5,1),mgp=c(0.5,0,0))
  plot(0:10,0:10,xaxt='n',yaxt='n',pch='',ylab='',xlab='Dominant Process')
  abline(h=5,v=5,lty=2,col=adjustcolor('navy',0.7),lwd=1.5)
  mtext(text = 'Burial',side = 4,line = -1.5,srt=90,cex=0.8,font = 2)
  mtext(text = 'Respiration',side = 2,line = -1.5,srt=90,cex=0.8,font = 2)
  mtext(text = 'Autochthony',side = 1,line = -2,srt=90,cex=0.8,font = 2)
  mtext(text = 'Allochthony',side = 3,line = -2,srt=90,cex=0.8,font = 2)
dev.off()


############################################################
#################### Months ##############################
library(dplyr)
library(plotrix)

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}

plotEllipse <- function(lakedata) {
ylab = 'Respiration/Sedimentation'
xlab = 'log(Alloch/Autoch)'
cols = add.alpha(rainbow(12),0.6)

library(dplyr)
eData <- lakedata %>% mutate(quarter = quarter(Date), month = month(Date)) %>%
  group_by(month) %>%
  dplyr::summarise(medRS = median(RS),sdRS = sd(RS),medlogAA = median(logAA),
                   sdlogAA = sd(logAA))

plot(c(-4,3), c(0,6), type="n", main=deparse(substitute(lakedata)),
     ylab = ylab, xlab = xlab)

draw.ellipse(eData$medlogAA,eData$medRS, 1*eData$sdlogAA, 1*eData$sdRS,col=cols)
abline(h=1,v=0,lty=2,col='red4',lwd=1.5)
}

par(mfrow=c(2,3))
par(mar=c(3,3,2,3),mgp=c(1.5,0.5,0),tck=-0.03,cex=0.8)

plotEllipse(Vanern)
plotEllipse(Toolik)
plotEllipse(Trout)
plotEllipse(Monona)
plotEllipse(Harp)
# Plot legend
plot.new()
legend('topleft',legend = month.abb[1:12],ncol=1,fill=rainbow(12),bty='n')

