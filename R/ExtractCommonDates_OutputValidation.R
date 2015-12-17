### Extract SOS DOC model output and corresponding DOC validation data ###
# Date: 12-17-15
# Author: Ian McCullough, immccull@gmail.com

#install.packages('dplyr')
library(dplyr)

# Set YOUR working directory
setwd('C:/Users/Ian/Desktop/GLEON/SOS/ToolikLake')

# Read in validation data
valid = read.csv('Toolik_validation_test.csv')

# Read in SOS model output
output = read.csv('Toolik_output_test.csv')

# Intersect validation data and output
validation = inner_join(output,valid,by='datetime')

# Export to csv to current working directory
write.csv(validation,'validation.csv')



