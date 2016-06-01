#### The legendary beard plot ####
# Warning: things could get hairy

#### Set your own working directory #####
setwd("C:/Users/Ian/Desktop/GLEON/SOS/")

lake_list = c('Vanern','Toolik','Trout','Mendota','Harp')

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
  lake_df$RS = lake_df$Resp - lake_df$Sed
  #lake_df$logRS = log(lake_df$RS)
  lake_df$AA = lake_df$Autoch/lake_df$Alloch
  lake_df$logAA = log(lake_df$AA)
  
  # get rid of NAs produced by log transformation
  lake_df = na.omit(lake_df)
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
ylab = 'Respiration - Sedimentation'
xlab = 'ln(Autochthonous/Allochthonous Ratio)'

plot(RS ~ logAA, Vanern, xlab = xlab, ylab = ylab, main='Vanern')
plot(RS ~ logAA, Toolik, xlab = xlab, ylab = ylab, main='Toolik')
plot(RS ~ logAA, Trout, xlab = xlab, ylab = ylab, main='Trout')
plot(RS ~ logAA, Mendota, xlab = xlab, ylab = ylab, main='Mendota')
plot(RS ~ logAA, Harp, xlab = xlab, ylab = ylab, main='Harp')
plot(RS ~ logAA, Annie, xlab = xlab, ylab = ylab, main='Annie')

