# Subsetting by date example
# Vanern Lake

# Set WD
setwd ('/Users/FarrellKJ/Desktop/R/SOS')

# Read in data file of flow (m3/s). Original file spans 1938-2014
flowrate <- read.csv('./VanernLake/Staging Files/Vanern_Variables/outflow.csv')
names(flowrate)

# Explicitly define format of date (if needed)
flowrate$date <- as.Date(as.character(flowrate$date), "%m/%d/%Y")

# Subset data into new dataframe that only includes values since Jan. 1, 2001
flowrate_sub <- subset(flowrate, date >= as.Date("2000/01/01"))