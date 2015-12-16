# Gather in-lake chlorophyll and total phosphorus data
# Vanern Lake

# Read in datafile including temp, TP, chla ####
data <- read.csv('./VanernLake/Staging Files/Vanern_Variables/Vanern_inlake_chem_Megrundet.csv')
chla <- data # For simplicity of reusing code, name copy of 'data' as chla
tp <- data # For simplicity of reusing code, name copy of 'data' as tp

# Chlorophyll-a in ug/L (1 mg/m3=1ug/L) ####
# Calculate mean water column chla for each available sampling date
library(xts)
chla <- subset(chla, chla$Chla.mg.m3>=0) #Remove NAs and values <0
chla.xts <- xts(x = chla$Chla.mg.m3,as.POSIXct(chla$sampledate))
ep <- endpoints(chla.xts,'days')
daily_chla <- period.apply(chla.xts,ep,mean) # ug/L

# Total Phosphorus ug/L ####
# Calculate mean water column TP for each available sampling date
tp <- subset(tp, Tot.P.µg.l>=0) #Remove NA values
tp.xts <- xts(x = tp$Tot.P.µg.l, as.POSIXct(tp$sampledate))
ep <- endpoints(tp.xts,'days')
daily_tp <- period.apply(tp.xts,ep,mean) # ug/L

# Flow m3/s ####
flow <- read.csv('./VanernLake/Staging Files/Vanern_Variables/outflow.csv')
flow$date <- as.Date( as.character(flow$date), "%m/%d/%Y")
flow <- subset(flow, date >= as.Date("2000/01/01"))
flow.xts <- xts(x= flow$flow, as.POSIXct(flow$date))
ep <- endpoints(flow.xts, 'days')
daily_flow <- period.apply(flow.xts, ep, mean)
daily_flow <- to.daily(daily_flow)
daily_flow <- subset(daily_flow[,1])

# Temperature in lake ####
epi <- subset(data, Depth..m.<=2)
hypo <- subset(data, Depth..m.>=50)

# Mean Epi and Hypo temp per date ####
library(xts)

epi.xts <- xts(x = epi$Temp...C,as.POSIXct(epi$sampledate))
ep <- endpoints(epi.xts,'days')
daily_epi <- period.apply(epi.xts,ep,mean) # C

hypo.xts <- xts(x = hypo$Temp...C,as.POSIXct(hypo$sampledate))
ep <- endpoints(hypo.xts,'days')
daily_hypo <- period.apply(hypo.xts,ep,mean) # C

# In Lake DOC #####
toc.xts <- xts(x= data$TOC.mg.l, as.POSIXct(data$sampledate))
ep <- endpoints(toc.xts, 'days')
daily_toc <- period.apply(toc.xts, ep,mean)
daily_doc <- daily_toc*.9

# Surface water DOC
sw <- read.csv ('./VanernLake/Staging Files/Vanern_Variables/Vanern_tributaries.csv')
sw$sampledate <- as.Date( as.character(sw$sampledate), "%m/%d/%Y")
sw <- subset(sw, In.Out=='Inflow')
sw.xts <- xts(x= sw$TOC.mg.L, as.POSIXct(sw$sampledate))
ep <- endpoints(sw.xts, 'days')
daily_sw <- period.apply(sw.xts, ep,mean)
daily_swdoc <- daily_sw*.9

# Merge data with continuous date series and interpolate between missing values ####
# create continuous set of dates between mod_start mod_end
mod_start <- '2000-01-01'
mod_end <- '2014-12-31'
dates <- (seq(as.Date(mod_start), as.Date(mod_end), by='days'))
dates_xts <- as.xts(dates)

# Merge xts dates, chla, tp, flow and convert to dataframe
daily_data <- merge.xts(dates_xts, daily_flow, daily_hypo, daily_epi, 
                        daily_chla, daily_tp,  daily_doc, daily_swdoc, fill='')
daily_data <- data.frame(date=index(daily_data), coredata(daily_data))

daily_data$date <- as.Date(daily_data$date, format = "%m/%d/%y")
format(daily_data$date,"%m/%d/%Y")

#Write .csv of data before interpolation
write.csv(daily_data, file='./VanernLake/Staging Files/Vanern_Variables/Vanern_inlake_TPchla.csv',
          row.names=FALSE, na="")
