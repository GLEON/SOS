# Gather in-lake chlorophyll, total phosphorus, secchi, temperature data
# Trout Lake

# Chlorophyll-a in ug/L ####
# Read datafile from NTL LTER (http://tinyurl.com/ob7qtks)
chla <- read.csv('./TroutLake/Staging Files/TL_Variables/TL_chla_raw.csv')
chla <- subset(chla, chlor>=0) # Remove values <0 (biologically impossible) and NAs

# Calculate mean water column chla for each available sampling date
library(xts)
chla.xts <- xts(x = chla$chlor,as.POSIXct(chla$sampledate))
ep <- endpoints(chla.xts,'days')
daily_chla <- period.apply(chla.xts,ep,mean) # ug/L

# Total Phosphorus ug/L ####
# Read datafile from NTL LTER (http://tinyurl.com/otfv7ng)
tp <- read.csv('./TroutLake/Staging Files/TL_Variables/TL_TP_raw.csv')
tp <- subset(tp, totpuf>=0) #Remove NA values

# Calculate mean water column TP for each available sampling date
tp.xts <- xts(x = tp$totpuf,as.POSIXct(tp$sampledate))
ep <- endpoints(tp.xts,'days')
daily_tp <- period.apply(tp.xts,ep,mean) # ug/L

# Merge data with continuous date series and interpolate between missing values ####
# create continuous set of dates between mod_start mod_end
mod_start <- '2010-01-01'
mod_end <- '2015-12-31'
dates <- (seq(as.Date(mod_start), as.Date(mod_end), by='days'))
dates_xts <- as.xts(dates)

# Merge xts dates, chla, tp and convert to dataframe
daily_data <- merge.xts(dates_xts, daily_chla, daily_tp, daily_secchi, all=TRUE, fill=NA)
daily_data <- data.frame(date=index(daily_data), coredata(daily_data))

daily_data$date <- as.Date(daily_data$date, format = "%m/%d/%y")
format(daily_data$date,"%m/%d/%Y")

daily_data2a <- as.data.frame(na.approx(daily_data$daily_chla))
daily_data2b <- as.data.frame(na.approx(daily_data$daily_tp))

TL_chla_tp <- data.frame(datetime=daily_data$date, TP=daily_data2b, Chla=daily_data2a) 
write.csv(TL_chla_tp, file='./TroutLake/Staging Files/chla_tp.csv')