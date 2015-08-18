## Merge statenames to NLA dataset and subset by state ##

# Created Jul 8, 2015
# Author: Kait Farrel, Ian McCullough, immccull@gmail.com

library(plyr)
library(maptools)
library(rgdal)
library(dplyr)
library(ggmap) #ggplot2 is dependency
library(RColorBrewer)

# Ian's working directory
setwd('H:/Ian_GIS/gleon/SOS/Data')

# Kait's working directory
#setwd('/Users/FarrellKJ/Desktop/R/SOS/Data')

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
splt.by <- c('STATE_NAME')
by_state <- split(merger, merger[,splt.by] )

# Run log-log model for each state, output as dataframe 'state_slopes' ####
# function will output slope, int., and R2 value for log(DOC)~log(Secchi) for each state
# p-value indicates whether slope is different from zero
state_slopes <- ldply(by_state, function(by_state) {
  lm1 <-lm((log(DOC))~log(SECMEAN), data=by_state)
  N    = (df.residual(lm1)+2) # Number of data points per state (model degrees of freedom + 2 = n)
  r.sq <- summary(lm1)$r.squared # R-squared value for model fit
  slope <- (summary(lm1)$coefficients[2]) # Slope of log-log model
  int <- (summary(lm1)$coefficients[1]) # Intercept of log-log model
  Fv <- as.numeric((summary(lm1))$fstatistic) 
  ss <- sum(is.nan(Fv)) >0 
  P <- ifelse(!ss, 1-pf(Fv[1],Fv[2],Fv[3]),NA) #Is slope sig. diff. from zero
  data.frame(N, slope, int, r.sq, P)
})
colnames(state_slopes) <- c('STATE_NAME', 'N', 'Slope', 'Intercept', 'RSquared', 'P-Value')

write.csv(state_slopes, 'NTL_water_chem_model_slopes_allstates.csv')

### Map strength of log-log DOC Secchi relationship across USA ###

# Get state shapefile
USA = readShapePoly('states_shapefile/states.shp') #from ESRI Online
USA_DOC = merge(USA, state_slopes, by='STATE_NAME', all.x=F) #all.x=F returns only matching values in join

## With help from: http://www.r-bloggers.com/basic-mapping-and-attribute-joins-in-r/ ##
# accessed Jul 10, 2015

# Basic map
colors = brewer.pal(n=4, name='Blues')
lcolors = cut(USA_DOC$RSquared, breaks=quantile(USA_DOC$RSquared), labels=colors)
plot(USA_DOC, col=as.character(lcolors), main='R Squared by State')
mtext('log-log Secchi-DOC Relationship', side=3)
legend('bottomright', c('Q1, poor fit','Q2','Q3','Q4, best fit'), col=colors, lwd=c(4,4,4,4))

# using ggmap
USA_DOCF = fortify(USA_DOC, region='STATE_NAME')
USA_DOCF = rename(USA_DOCF, STATE_NAME=id)
USA_DOCF = left_join(USA_DOCF, USA_DOC@data)

ggplot(USA_DOCF) + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=RSquared))
                                    
# Plot each state on its own panel ####
library(lattice)
xyplot(log(DOC)~log(SECMEAN)|STATE_NAME, group=STATE_NAME, data=merger,
       type=c('r','p'), pch=15, lwd=2, layout=c(4,3,4),
       scales=list(alternating=1,tck=c(1,0), cex=1))
