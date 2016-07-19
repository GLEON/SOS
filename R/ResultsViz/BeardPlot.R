#### The legendary beard plot ####
# Warning: things could get hairy

#### Set your own working directory #####
setwd("C:/Users/Ian/Desktop/GLEON/SOS/")

#lake_list = c('Vanern','Toolik','Trout','Mendota','Harp')

#### define (dis)function ####
clean_shave = function(lakename) {
  
  #read in results csv files of DOC and POC
  #lakename = lake name
  wd = getwd()
  DOCpath = paste0(wd,'/',lakename,'Lake','/Results/',lakename,'_DOC_Results.csv')
  DOC = read.csv(DOCpath)
  POCpath = paste0(wd,'/',lakename,'Lake','/Results/',lakename,'_POC_Results.csv')
  POC = read.csv(POCpath)
  
  # create new data frame with select columns
  lake_df = data.frame(Date = DOC$Date, Resp = DOC$respOut_gm2y,
                         Sed = POC$sedOut_gm2y, Alloch = DOC$DOC_in_alloch_g,
                         Autoch = DOC$DOC_in_autoch_g)
  
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
  
  # get rid of NAs produced by log transformation
  # KF: I am #-ing this command for now; NaNs will automatically be removed when we plot, 
  #     but if we keep this line here, it will remove lines that are complete cases if using non-logged plotting
  # IM: OK, thank you KF 
  #lake_df = na.omit(lake_df)
  
  # Output each lake as a dataframe
  data.frame(Date = lake_df$Date, RS = lake_df$RS, logRS = lake_df$logRS, AA = lake_df$AA, logAA = lake_df$logAA)
}

# run over the lakes
Vanern = clean_shave('Vanern')
Toolik = clean_shave('Toolik')
Trout = clean_shave('Trout')
Mendota = clean_shave('Mendota')
Harp = clean_shave('Harp')
#Annie = clean_shave('Annie')

## F***in loop doesn't work...it is probably a simple thing but I am too tired
#for (i in 1:length(lake_list)) {
#  x = clean_shave(lake_list[i])
#  assign(x, lake_list[i])
#}

# plot
par(mfrow=c(2,3))
ylab = 'Respiration/Sedimentation'
xlab = 'log(Allochthonous/Autochthonous Ratio)'
xlim = c(-4,4)
ylim = c(0,10)

cols = rainbow(12)[month(Vanern$Date)]
plot(RS ~ logAA, Vanern, xlab = xlab, ylab = ylab, main='Vanern', pch=19, col=cols,xlim=xlim,ylim=ylim)
mtext(side=3, paste0('n=',nrow(Vanern)), cex=0.75)
abline(h=1,v=0,lty=2,col='red4',lwd=1.5)

cols = rainbow(12)[month(Toolik$Date)]
plot(RS ~ logAA, Toolik, xlab = xlab, ylab = ylab, main='Toolik', pch=19, col= cols,xlim=xlim,ylim=ylim)
mtext(side=3, paste0('n=',nrow(Toolik)), cex=0.75)
abline(h=1,v=0,lty=2,col='red4',lwd=1.5)

cols = rainbow(12)[month(Trout$Date)]
plot(RS ~ logAA, Trout, xlab = xlab, ylab = ylab, main='Trout', pch=19, col= cols,xlim=xlim,ylim=ylim)
mtext(side=3, paste0('n=',nrow(Trout)), cex=0.75)
abline(h=1,v=0,lty=2,col='red4',lwd=1.5)

cols = rainbow(12)[month(Mendota$Date)]
plot(RS ~ logAA, Mendota, xlab = xlab, ylab = ylab, main='Mendota', pch=19, col= cols,xlim=xlim,ylim=ylim)
mtext(side=3, paste0('n=',nrow(Mendota)), cex=0.75)
abline(h=1,v=0,lty=2,col='red4',lwd=1.5)

cols = rainbow(12)[month(Harp$Date)]
plot(RS ~ logAA, Harp, xlab = xlab, ylab = ylab, main='Harp', pch=19, col= cols,xlim=xlim,ylim=ylim)
mtext(side=3, paste0('n=',nrow(Harp)), cex=0.75)
abline(h=1,v=0,lty=2,col='red4',lwd=1.5)
#plot(RS ~ logAA, Annie, xlab = xlab, ylab = ylab, xlim=xlim, ylim=ylim, main='Annie')
#mtext(side=3, paste0('n=',nrow(Annie)), cex=0.75)
