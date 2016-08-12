# Access USGS stream discharge data

# Read in Lake Mendota surface inflow gauge ID's (Yahara in, 6-mile, Dorn/Spring, Pheasant; Yahara outflow)
mendota_flow <- c('05427850', '05427910', '05427927','05427948', '05428500')

# Create empty list to populate with daily discharge values
Sw_Qin <- list()

# Indicate desired variable (here, code 00060 is for Discharge)
pcode <- '00060' # Discharge for trout_inflow

# Create empty start values for model start and end dates
mod_start <- '2001-10-01'
mod_end <- '2016-01-01'

library(dataRetrieval)

# readNWISdv to summarize daily discharge values between specified dates
for (i in 1:length(mendota_flow)){
  Sw_Qin[[mendota_flow[i]]] <- readNWISdv(mendota_flow[i], parameterCd=pcode, 
                                          startDate=mod_start, endDate=mod_end)
}

# create continuous set of dates between mod_start mod_end
dates <- seq(as.Date(mod_start), as.Date(mod_end), by='days')

# Create empty dataframe of continuous dates onto which you'll paste your Q data
Swmodel_input <- data.frame(Date=dates) # dataframe w/ 1st column 'date'

for(i in 1:length(mendota_flow)){
  val <- approx(x=Sw_Qin[[i]][,3], y=Sw_Qin[[i]][,4], xout= dates, method='linear')$y
  Swmodel_input = cbind(Swmodel_input, data.frame(val))
}

colnames(Swmodel_input) <- c('Date', 'Yahara (Inflow cfs)', 'Six Mile (Inflow cfs)', 
                             'Dorn (Inflow cfs)', 'Pheasant (Inflow cfs)', 
                             'Yahara (Outflow cfs)')
format(Swmodel_input$Date,"%m/%d/%Y")

# Sum discharge from individual streams for each day 
Swmodel_input$SumInflow_ft3 <- rowSums(Swmodel_input[,-c(1,6)], na.rm=TRUE)

# Convert discharge to m3/sec
# Sum of inflow data
Swmodel_input$Inflow_m3s = Swmodel_input$SumInflow_ft3 * 0.0283168466 # Convert cfs to m3s-1
# Outflow data
Swmodel_input$Outflow_m3s = Swmodel_input[,6] * 0.0283168466 # Convert cfs to m3s-1
# Add column for ratio of outflow to known surface inflow
Swmodel_input$Out.vs.In = Swmodel_input$Outflow_m3s / Swmodel_input$Inflow_m3s

# Write .csv of all inflow/outflow data
write.csv(Swmodel_input, file='./MendotaLake/Staging Files/flow_data.csv')

# Isolate date and discharge (m3/s)
# Outflow data based on Yahara River surface outflow
sw_outflow <- data.frame(datetime=Swmodel_input$Date, TotOutflow=Swmodel_input$Outflow_m3s)
sw_outflow <- na.omit(sw_outflow)

# Write .csv for Sw Outflow
write.csv(sw_outflow, file='./MendotaLake/Staging Files/sw_outflow.csv')

