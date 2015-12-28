# Obtain Secchi depth data for Trout Lake
# Last modified 28 Dec 2015, KJF

# Secchi depth m ####
secchi <- read.csv('./TroutLake/Staging Files/TL_Variables/TL_monthly_secchi.csv')
secchi$sampledate <- as.POSIXct(secchi$sampledate, format="%m/%d/%Y %H:%M", tz="America/Chicago")

# Extract secchi depth for each available date
library(xts)
secchi.xts <- xts(x = secchi$secnview, as.POSIXct(secchi$sampledate, format="%m/%d/%Y %H:%M"))
daily_secchi <- secchi.xts # m

# Merge data with continuous date series and interpolate between missing values ####
# create continuous set of dates between mod_start mod_end
mod_start <- '2004-01-01'
mod_end <- '2015-12-31'
dates <- (seq(as.Date(mod_start), as.Date(mod_end), by='days'))
dates_xts <- as.xts(dates)

# Merge xts dates, chla, tp and convert to dataframe
daily_data <- merge.xts(dates_xts, daily_secchi, all=TRUE, fill=NA)
daily_data <- data.frame(date=index(daily_data), coredata(daily_data))

daily_data$date <- as.Date(daily_data$date, format = "%m/%d/%y")

daily_data2c <- as.data.frame(na.approx(daily_data$daily_secchi, na.rm=F))
colnames(daily_data2c) <- c('Secchi')

TL_secchi <- data.frame(datetime=daily_data$date, Secchi=daily_data2c) 
TL_secchi <- na.omit(TL_secchi)
write.csv(TL_secchi, file='./TroutLake/Staging Files/TL_Variables/secchi.csv')