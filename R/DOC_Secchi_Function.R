### Create R function to predict lake DOC from Secchi depth ###

# Created: Jul 10, 2015
# Author: Ian McCullough, immccull@gmail.com

setwd("H:/Ian_GIS/gleon/SOS/data")

# test data
test_data = read.csv("DOCfunction_test_data.csv")

## Very simple function ##

# function doc.lake
doc.lake = function(df) {
  #This function uses pre-determined regression coefficient and intercept
  #User supplies data frame (eg, csv) of Secchi values from which to predict DOC
  #Initial paramters are based on Wisconsin model calibration only
  #Possible issue: only works with csv of just Secchi, nothing else
  DOC = apply(df, 2, function(x) 2.35075 + (-0.6468*df$Secchi))# in g/m3
  DOCdf = data.frame(Secchi=df$Secchi, DOC=DOC)
  colnames(DOCdf) = c('Secchi', 'DOC')
  return(DOCdf)
}

doc = doc.lake(test_data)

## Function with built-in regression ##

# test data
test_data2 = read.csv('DOCfunction_test_data2.csv')

#function doc.lake.regress
doc.lake.regress = function(df) {
  #This function takes data frame of DOC and Secchi 
  #Performs regression to predict DOC of lakes with unknown DOC
  DOClm = lm(log(DOC)~log(Secchi), data=df)
  unknownDOC = predict(DOClm)
  unknownDOC_df = data.frame(DOC=unknownDOC)
  return(unknownDOC_df)
}

doc2 = doc.lake.regress(test_data2)
