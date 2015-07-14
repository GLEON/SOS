### Create R function to predict lake DOC from Secchi depth ###

# Created: Jul 10, 2015
# Updated: Jul 14, 2015
# Author: Ian McCullough (immccull@gmail.com) & Kait Farrell (farrellkj2@gmail.com)

# setwd("H:/Ian_GIS/gleon/SOS/data") # Ian's working directory
setwd('/Users/FarrellKJ/Desktop/R/SOS/Data') # Kait's working directory

# test data
test_data2 = read.csv('DOCfunction_test_data2.csv')
RLS_data = read.csv('./randomWIlakes_DOC.csv', header=T) # N. WI dataset from SurfaceGroundwater_SecchiApproach file

## SwGw DOC function in 2 parts ##

#Part 1: function calc.doc.regress
#This function calculates slope and intercept of the linear model log(DOC)~log(Secchi depth).
#User supplies data frame (eg, csv) of Secchi depth values (m) and in-lake DOC (mg/L).
#Output is slope and intercept of log-log linear model that can be used to estimate DOC values for lakes where only Secchi is available

# Regress DOC vs Secchi to calculate dataframe-specific slope & intercept
calc.doc.regress = function(df) {
  DOClm = lm(log(DOC)~log(Secchi), data=df, na.action=na.omit)
  doc.slope <- summary(DOClm)$coefficients[2]
  doc.intercept <- summary(DOClm)$coefficients[1]
  DOC_regress_df = data.frame(Slope=doc.slope, Intercept=doc.intercept)
  return(DOC_regress_df)
}

regress = calc.doc.regress(RLS_data)

# Part 2: function doc.lake
#This function uses pre-determined regression coefficient and intercept (calculated above or manually input)
#User supplies data frame (eg, csv) of Secchi values from which to estimate DOC

# Initial parameters 
# Pulls from output calculated in calc.doc.regress above. If calc.doc.regress not used or yields NA value,
# defaults to slope & intercept from N. Wisc. dataset
default.slope <- -0.6468028 
default.intercept <- 2.350748

doc.slope <- ifelse(regress$Slope!='NA', regress$Slope,
                    ifelse(default.slope))
doc.intercept <- ifelse(regress$Intercept!='NA', regress$Intercept,
                        ifelse(default.intercept))

doc.lake = function(df) {
  logDOC = apply(df, 2, function(x) (doc.slope*df$Secchi)+doc.intercept)# in mg/L
  DOC.mgL = exp(logDOC)
  DOC.gm3 = DOC.mgL * 0.001 * 0.001 # transform DOC into g/m3
  DOCdf = data.frame(Lake=df$Lake, Secchi=df$Secchi, DOC=DOC.gm3)
  colnames(DOCdf) = c('Lake ID', 'Secchi (m)', 'DOC (g/m3)')
  return(DOCdf)
}

doc = doc.lake(test_data2)

### Not sure we want/need to keep code below this line(?) ###
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
