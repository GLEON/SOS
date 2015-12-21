### Extract SOS DOC model output and corresponding DOC validation data ###
# Date: 12-17-15
# Author: Ian McCullough, immccull@gmail.com

#install.packages('dplyr')

# Set YOUR working directory
setwd('C:/Users/Ian/Desktop/GLEON/SOS/ToolikLake')

# Read in validation data and SOS model output data
# Example based on csvs with made up numbers
validation = read.csv('Toolik_validation_test.csv')
mod_output = read.csv('Toolik_output_test.csv')

dateFunction = function(validation,mod_output,by){
  #validation = validation csv
  #mod_output = model output csv
  #by = common header name for joining tables (in quotes)
  library(dplyr)
  inner_join(validation,mod_output,by)
}

# Run function with named object output
# Ignore error about coercing factor (datetime) to character; doesn't matter
test = dateFunction(validation=validation,mod_output=mod_output, by='datetime')

# Export to csv to current working directory
write.csv(test,'test_validation.csv')



