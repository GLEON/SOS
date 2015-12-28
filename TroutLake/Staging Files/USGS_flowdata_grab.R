# Access USGS stream discharge data

# Read in Trout Lake surface inflow gauge ID's (Alleq., Stevenson, North... Trout River outflow)
trout_inflow <- c('05357225', '05357230', '05357215', '05357245')
# Read in gauge ID with less-continuous data (Mann Crk)
mann_in <- '05357239'

# Create empty list to populate with daily discharge values
Sw_Qin <- list()

# Set start date for data grab. Set end date if desired (not done here)
startDate <- '2004-01-01'

# Indicate desired variable (here, code 00060 is for Discharge)
pcode <- '00060' # Discharge for trout_inflow
pcode2 <- '00061' # discharge for mann_in

# Create empty start values for model start and end dates
# Will be populated based on latest start date for all sites, earliest end date
mod_start <- NA
mod_end <- NA

library(dataRetrieval)

# readNWISdv to summarize daily discharge values between specified dates
for (i in 1:length(trout_inflow)){
  Sw_Qin[[trout_inflow[i]]] <- readNWISdv(trout_inflow[i], parameterCd=pcode, 
                                          startDate=startDate)
  mod_start=max(c(min(Sw_Qin[[i]]$Date, na.rm = TRUE), mod_start), na.rm=TRUE)
  mod_end=min(c(max(Sw_Qin[[i]]$Date, na.rm = TRUE), mod_end), na.rm=TRUE)
}

# Add daily Mann Creek values 
mann_data <- readNWISmeas(mann_in, startDate=startDate)
mann_q <- mann_data[,10]
mann_q_Date <- as.Date(mann_data[,4])
mod_start=max(c(min(mann_q_Date, na.rm = TRUE), mod_start), na.rm=TRUE)
mod_end=min(c(max(mann_q_Date, na.rm = TRUE), mod_end), na.rm=TRUE)

# create continuous set of dates between mod_start mod_end
dates <- seq(as.Date(mod_start), as.Date(mod_end), by='days')

# Create empty dataframe of continuous dates onto which you'll paste your Q data
Swmodel_input <- data.frame(Date=dates) # dataframe w/ 1st column 'date'

for(i in 1:length(trout_inflow)){
  val <- approx(x=Sw_Qin[[i]][,3], y=Sw_Qin[[i]][,4], xout= dates, method='linear')$y
  Swmodel_input = cbind(Swmodel_input, data.frame(val))
}
val <- approx(x=mann_q_Date, y=mann_q, xout= dates, method='linear')$y
Swmodel_input = cbind(Swmodel_input, data.frame(val))

colnames(Swmodel_input) <- c('Date', 'Stevenson Creek (Inflow cfs)', 'North Creek (Inflow cfs)', 
                             'Allequash Creek (Inflow cfs)', 'Trout River (Outflow cfs)', 
                             'Mann Creek (Inflow cfs)')
format(Swmodel_input$Date,"%m/%d/%Y")

# Sum discharge from individual streams for each day 
Swmodel_input$SumInflow_ft3 <- rowSums(Swmodel_input[,-c(1,5)])

# Convert discharge to m3/sec
# Sum of inflow data
Swmodel_input$Inflow_m3s = Swmodel_input$SumInflow_ft3 * 0.0283168466 # Convert cfs to m3s-1
# Outflow data
Swmodel_input$Outflow_m3s = Swmodel_input[,5] * 0.0283168466 # Convert cfs to m3s-1
# Add column for ratio of outflow to known surface inflow
Swmodel_input$Out.vs.In = Swmodel_input$Outflow_m3s / Swmodel_input$Inflow_m3s

# Write .csv of all inflow/outflow data
write.csv(Swmodel_input, file='./TroutLake/Staging Files/TL_Variables/flow_data.csv')

# Isolate date and discharge (m3/s)
# Outflow data based on Trout River surface outflow
sw_inflow <- data.frame(datetime=Swmodel_input$Date, TotInflow=Swmodel_input$Inflow_m3s)
sw_outflow <- data.frame(datetime=Swmodel_input$Date, TotOutflow=Swmodel_input$Outflow_m3s)

# Write .csv for Sw Inflow
write.csv(sw_inflow, file='./TroutLake/Staging Files/TL_Variables/sw_inflow.csv')

# Write .csv for Sw Outflow
write.csv(sw_outflow, file='./TroutLake/Staging Files/TL_Variables/sw_outflow.csv')