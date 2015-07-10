### Create R function to predict lake DOC from Secchi depth ###

# Created: Jul 10, 2015
# Author: Ian McCullough, immccull@gmail.com

# setwd("H:/Ian_GIS/gleon/SOS/data")
setwd('/Users/FarrellKJ/Desktop/R/SOS/Data') # Kait's working directory

# test data
test_data2 = read.csv('DOCfunction_test_data2.csv')

## Very simple function ##

# function doc.lake
#This function uses pre-determined regression coefficient and intercept
#User supplies data frame (eg, csv) of Secchi values from which to predict DOC
#Initial paramters are based on Wisconsin model calibration only

# Initial parameters
doc.slope <- -0.6468 # Slope of log(DOC)~log(Secchi)
doc.intercept <- 2.35075 # Intercept of log(DOC)~log(Secchi)

doc.lake = function(df) {
  logDOC = apply(df, 2, function(x) (doc.slope*df$Secchi)+doc.intercept)# in g/m3
  DOC.mgL = exp(logDOC)
  DOC.gm3 = DOC.mgL * 0.001 * 0.001 # transform DOC into g/m3
  DOCdf = data.frame(Lake=df$Lake, Secchi=df$Secchi, DOC=DOC.gm3)
  colnames(DOCdf) = c('Lake ID', 'Secchi (m)', 'DOC (g/m3)')
  return(DOCdf)
}

doc = doc.lake(test_data2)

## Function with built-in regression ##

#function doc.lake.regress
#This function takes data frame of DOC and Secchi 
#Performs regression to predict DOC of lakes with unknown DOC

doc.lake.regress = function(df) {
  DOClm = lm(log(DOC)~log(Secchi), data=df)
  logunknownDOC = predict(DOClm)
  unknownDOC = exp(logunknownDOC)
  unknownDOC_df = data.frame(DOC=unknownDOC)
  return(unknownDOC_df)
}

doc2 = doc.lake.regress(test_data2)
