# Access USGS stream discharge data

# Read in Trout Lake surface inflow gauge ID's
trout_inflow <- c('05357239', '05357225', '05357230', '05357215')

#Omit stream without daily/hourly data (-Mann Creek)
trout_in_3 <- c('05357225', '05357230', '05357215')

# Create empty list to populate with daily discharge values
Sw_Qin <- list()

# Set start date for data grab. Set end date if desired (not done here)
startDate <- '2005-01-01'

# Indicate desired variable (here, code 00060 is for Discharge)
pcode <- '00060' # Discharge

# Create empty start values for model start and end dates
# Will be populated based on latest start date for all sites, earliest end date
mod_start <- NA
mod_end <- NA

library(dataRetrieval)

# readNWISdv to summarize daily discharge values between specified dates
for (i in 1:length(trout_in_3)){
  Sw_Qin[[trout_in_3[i]]] <- readNWISdv(trout_in_3[i], parameterCd=pcode, 
                                          startDate=startDate)
  mod_start=max(c(min(Sw_Qin[[i]]$Date, na.rm = TRUE), mod_start), na.rm=TRUE)
  mod_end=min(c(max(Sw_Qin[[i]]$Date, na.rm = TRUE), mod_end), na.rm=TRUE)
}

# create continuous set of dates between mod_start mod_end
dates <- seq(as.Date(mod_start), as.Date(mod_end), by='days')

# Create empty dataframe of continuous dates onto which you'll paste your Q data
Swmodel_input <- data.frame(Date=dates) # dataframe w/ 1st column 'date'

for(i in 1:length(trout_in_3)){
  val <- approx(x=Sw_Qin[[i]][,3], y=Sw_Qin[[i]][,4], xout= dates, method='linear')$y
  Swmodel_input = cbind(Swmodel_input, data.frame(val))
}

# Sum discharge from individual streams for each day
Swmodel_input$Sum <- rowSums(Swmodel_input[,-1])

# Write .csv into Merge file
write.csv(Swmodel_input, file='./R/Merge/Sw_Qin.csv')