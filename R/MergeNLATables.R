## Merge statenames to NLA dataset and subset by state ##

# Created Jul 8, 2015
# Author: Ian McCullough, immccull@gmail.com

# Ian's working directory
#setwd('H:/Ian_GIS/gleon/SOS/Data')

# Kait's working directory
setwd('/Users/FarrellKJ/Desktop/R/SOS/Data')

# Read in water quality data file ####
wq = read.csv('NTL water chem.csv', header=T)

# Read in sampled lake information file
si = read.csv('NLA2007_SampledLakeInformation_20091113.csv', header=T)

names(wq) # verify that SITE_ID column exists for join
names(si) # verify that SITE_ID column exists for join

# Join by common column name (by=)
merger = merge(wq,si, by='SITE_ID')

# Put statename column first
statename = merger[grep("STATE_NAME", colnames(merger))] # use pattern matching to call state_name column
merger = cbind(statename,merger)
merger = merger[, -grep("STATE_NAME", colnames(merger[2:length(merger)]))] # delete redundant state_name column in middle of table by considering all columns but first

# Output to csv
write.csv(merger, 'NTL_water_chem_allstates.csv')

# Split data by state into new list 'by_state' ####
library(plyr)
splt.by <- c('STATE_NAME')
by_state <- split(merger, merger[,splt.by] )

# Run log-log model for each state, output as dataframe 'state_slopes' ####
# function will output slope, int., and R2 value for log(DOC)~log(Secchi) for each state
# p-value indicates whether slope is different from zero
state_slopes <- ldply(by_state, function(by_state) {
  lm1 <-lm((log(DOC))~log(SECMEAN), data=by_state)
  N    = (df.residual(lm1)+2) # Number of data points per state
  r.sq <- summary(lm1)$r.squared # R-squared value for model fit
  slope <- (summary(lm1)$coefficients[2]) # Slope of log-log model
  int <- (summary(lm1)$coefficients[1]) # Intercept of log-log model
  Fv <- as.numeric((summary(lm1))$fstatistic) 
  ss <- sum(is.nan(Fv)) >0 
  P <- ifelse(!ss, 1-pf(Fv[1],Fv[2],Fv[3]),NA) #Is slope sig. diff. from zero
  data.frame(N, slope, int, r.sq, P)
})
colnames(state_slopes) <- c('State', 'N', 'Slope', 'Intercept', 'R-Squared', 'P-Value')

write.csv(state_slopes, 'NTL_water_chem_model_slopes_allstates.csv')

# Plot each state on its own panel ####
library(lattice)
xyplot(log(DOC)~log(SECMEAN)|STATE_NAME, group=STATE_NAME, data=merger,
       type=c('r','p'), pch=15, lwd=2, layout=c(4,3,4),
       scales=list(alternating=1,tck=c(1,0), cex=1))

## Subset by state name and output state-specific CSV####
Ohio = subset(merger, grepl("Ohio", merger$STATE_NAME), select=STATE_NAME:FID_1)
write.csv(Ohio, "OhioNLA.csv")

Maine = subset(merger, grepl("Maine", merger$STATE_NAME), select=STATE_NAME:FID_1)
write.csv(Maine, "MaineNLA.csv")

Michigan = subset(merger, grepl("Michigan", merger$STATE_NAME), select=STATE_NAME:FID_1)
write.csv(Michigan, "MichiganNLA.csv")

Minnesota = subset(merger, grepl("Minnesota", merger$STATE_NAME), select=STATE_NAME:FID_1)
write.csv(Minnesota, "MinnesotaNLA.csv")

NewHampshire = subset(merger, grepl("New Hampshire", merger$STATE_NAME), select=STATE_NAME:FID_1)
write.csv(NewHampshire, "NewHampshireNLA.csv")

Indiana = subset(merger, grepl("Indiana", merger$STATE_NAME), select=STATE_NAME:FID_1)
write.csv(Indiana, "IndianaNLA.csv")

NewYork = subset(merger, grepl("New York", merger$STATE_NAME), select=STATE_NAME:FID_1)
write.csv(NewYork, "NewYorkNLA.csv")

## All New England ##
Maine = subset(merger, grepl("Maine", merger$STATE_NAME), select=STATE_NAME:FID_1)
NewHampshire = subset(merger, grepl("New Hampshire", merger$STATE_NAME), select=STATE_NAME:FID_1)
Connecticut = subset(merger, grepl("Connecticut", merger$STATE_NAME), select=STATE_NAME:FID_1)
RhodeIsland = subset(merger, grepl("Rhode Island", merger$STATE_NAME), select=STATE_NAME:FID_1)
Vermont = subset(merger, grepl("Vermont", merger$STATE_NAME), select=STATE_NAME:FID_1)
Massachusetts = subset(merger, grepl("Massachusetts", merger$STATE_NAME), select=STATE_NAME:FID_1)

NewEngland = rbind(Maine, NewHampshire, Vermont, Massachusetts, RhodeIsland, Connecticut)
write.csv(NewEngland, "NewEnglandNLA.csv")
