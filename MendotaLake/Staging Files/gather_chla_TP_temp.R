# Gather in-lake data for chl-a, TP, temperature
# Lake Mendota

library(xts)

# Read in datafiles ####
temp <- read.csv('./MendotaLake/Staging Files/temp_high_freq.csv')
tp <- read.csv('./MendotaLake/Staging Files/TP_DOC.csv')
chla <- read.csv('./MendotaLake/Staging Files/chla.csv')

# Chlorophyll-a in ug/L (1 mg/m3=1ug/L) ####
# Calculate mean water column chla for each available sampling date
chla <- subset(chla, chla$chlor>=0) #Remove NAs and values <0
chla$chlor <- as.integer(chla$chlor)
chla.xts <- xts(x = chla$chlor,as.POSIXct(chla$sampledate))
ep <- endpoints(chla.xts,'days')
daily_chla <- period.apply(chla.xts,ep,mean) # ug/L

# Total Phosphorus ug/L ####
# Calculate mean water column TP for each available sampling date
tp <- subset(tp, totpuf>=0) #Remove NA values
tp.xts <- xts(x = tp$totpuf, as.POSIXct(tp$sampledate))
ep <- endpoints(tp.xts,'days')
daily_tp <- period.apply(tp.xts,ep,mean) # ug/L

# Temperature in lake ####
epi <- subset(temp, depth<=2)
hypo <- subset(temp, depth>=10)

# Mean Epi and Hypo temp per date ####
epi.xts <- xts(x = epi$wtemp,as.POSIXct(epi$sampledate))
ep <- endpoints(epi.xts,'days')
daily_epi <- period.apply(epi.xts,ep,mean) # C

hypo.xts <- xts(x = hypo$wtemp,as.POSIXct(hypo$sampledate))
ep <- endpoints(hypo.xts,'days')
daily_hypo <- period.apply(hypo.xts,ep,mean) # C

# Merge data with continuous date series and interpolate between missing values ####
# create continuous set of dates between mod_start mod_end
mod_start <- '2006-05-01'
mod_end <- '2016-01-01'
dates <- (seq(as.Date(mod_start), as.Date(mod_end), by='days'))
dates_xts <- as.xts(dates)

# Merge xts dates, chla, tp, flow and convert to dataframe
daily_data <- merge.xts(dates_xts, daily_hypo, daily_epi, daily_tp,  
                        daily_chla, fill='')
daily_data <- data.frame(date=index(daily_data), coredata(daily_data))

daily_data$date <- as.Date(daily_data$date, format = "%m/%d/%y")
format(daily_data$date,"%m/%d/%Y")

daily_tp_all <- as.data.frame(na.approx(daily_data$daily_tp, na.rm=F))
daily_chla_all <- as.data.frame(na.approx(daily_data$daily_chla, na.rm=F))
daily_hypo_all <- as.data.frame(na.approx(daily_data$daily_hypo, na.rm=F))
daily_epi_all <- as.data.frame(na.approx(daily_data$daily_epi, na.rm=F))

daily_data_all <- data.frame(datetime=daily_data$date, HypoTemp=daily_hypo_all,
                             EpiTemp=daily_epi_all, TP=daily_tp_all, Chla=daily_chla_all) 
colnames(daily_data_all) <- c('datetime', 'HypoTemp','EpiTemp','TP','Chla')

#Write .csv of data before interpolation
write.csv(daily_data_all, file='./TroutLake/Staging Files/TL_Variables/TL_TPchlaTemps.csv',
          row.names=FALSE, na="")
