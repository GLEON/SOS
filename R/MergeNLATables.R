## Merge statenames to NLA dataset and subset by state ##

# Created Jul 8, 2015
# Author: Ian McCullough, immccull@gmail.com

# Ian's working directory
setwd('H:/Ian_GIS/gleon/SOS/Data')

# Read in water quality data file
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

## Subset by state name and output state-specific CSV##
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
