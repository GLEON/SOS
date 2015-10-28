# Gather DOC data for surface water inflows
# Trout Lake

# DOC in mg/L ####
# Read datafile from NTL LTER (http://tinyurl.com/pwybz5l)
doc <- read.csv('./TroutLake/Staging Files/TL_Variables/TL_monthly_inflow_concentrations.csv')
doc <- subset(doc, doc>=0) # Remove values <0 and NAs
doc$date <- as.Date(doc$sampledate, format = "%Y-%m-%d %H:%M")

# Subset Data by Stream
Stevenson <- subset(doc, site_name=='STEVENSON CREEK')
Allequash <- subset(doc, site_name=='ALLEQUASH CREEK')
North <- subset(doc, site_name=='NORTH CREEK')
Trout <- subset(doc, site_name=='TROUT RIVER')

# Aggregate DOC (mg/L) for each available sampling date and site ####
library(xts)
doc.xts1 <- xts(x = Stevenson$doc, as.POSIXct(Stevenson$date))
doc.xts2 <- xts(x = Allequash$doc, as.POSIXct(Allequash$date))
doc.xts3 <- xts(x = North$doc, as.POSIXct(North$date))
doc.xts4 <- xts(x = Trout$doc, as.POSIXct(Trout$date))

ep1 <- endpoints(doc.xts1,'days')
ep2 <- endpoints(doc.xts2,'days')
ep3 <- endpoints(doc.xts3,'days')
ep4 <- endpoints(doc.xts4,'days')

daily_doc1 <- period.apply(doc.xts1,ep1,mean) # mg/L
daily_doc2 <- period.apply(doc.xts2,ep2,mean) # mg/L
daily_doc3 <- period.apply(doc.xts3,ep3,mean) # mg/L
daily_doc4 <- period.apply(doc.xts4,ep4,mean) # mg/L

# Merge data with continuous date series and interpolate between missing values ####
# create continuous set of dates between mod_start mod_end
mod_start <- '1975-08-21'
mod_end <- '1999-04-06'
dates <- (seq(as.Date(mod_start), as.Date(mod_end), by='days'))
dates_xts <- as.xts(dates)

# Merge xts dates, doc and convert to dataframe
daily_doc_data <- merge.xts(dates_xts, daily_doc1, daily_doc2, daily_doc3, 
                            daily_doc4, all=TRUE, fill=NA)
daily_doc_data <- data.frame(date=index(daily_doc_data), coredata(daily_doc_data))
colnames(daily_doc_data) <- c('Date', 'Stevenson', 'Allequash', 'NorthCreek', 'TroutRiver')
daily_doc_data$Date <- as.Date(daily_doc_data$Date, format = "%m/%d/%y")
daily_doc_data$Date <- format(daily_doc_data$Date,"%m/%d/%Y")

daily_data2 <- as.data.frame(na.approx(daily_doc_data[,2:5]))

TL_Sw_doc <- data.frame(datetime=daily_doc_data[,1], DOC_sw=daily_data2) 
TL_Sw_doc <- TL_Sw_doc[c(5817:8434),]
write.csv(TL_Sw_doc, file='./TroutLake/Staging Files/TL_Variables/Sw_DOC.csv')
