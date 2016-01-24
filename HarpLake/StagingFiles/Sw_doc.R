# Gather DOC data for surface water inflows: Harp Lake

# DOC in mg/L ####
# Read datafile provided from Jim Rusak
doc <- read.csv('./HarpLake/Staging Files/DOC_Sw.csv')
doc <- subset(doc, doc>=0) # Remove values <0 and NAs
doc$date <- as.Date(as.character(doc$sampledate), "%m/%d/%Y")

# Subset Data by Stream
In3 <- subset(doc, site_name=='HARP INFLOW #3')
In3a <- subset(doc, site_name=='HARP INFLOW #3A')
In4 <- subset(doc, site_name=='HARP INFLOW #4')
In5 <- subset(doc, site_name=='HARP INFLOW #5')
In6 <- subset(doc, site_name=='HARP INFLOW #6')
In6a <- subset(doc, site_name=='HARP INFLOW #6A')

# Aggregate DOC (mg/L) for each available sampling date and site ####
library(xts)
doc.xts1 <- xts(x = In3$doc, as.POSIXct(In3$date))
doc.xts2 <- xts(x = In3a$doc, as.POSIXct(In3a$date))
doc.xts3 <- xts(x = In4$doc, as.POSIXct(In4$date))
doc.xts4 <- xts(x = In5$doc, as.POSIXct(In5$date))
doc.xts5 <- xts(x = In6$doc, as.POSIXct(In6$date))
doc.xts6 <- xts(x = In6a$doc, as.POSIXct(In6a$date))

ep1 <- endpoints(doc.xts1,'days')
ep2 <- endpoints(doc.xts2,'days')
ep3 <- endpoints(doc.xts3,'days')
ep4 <- endpoints(doc.xts4,'days')
ep5 <- endpoints(doc.xts5,'days')
ep6 <- endpoints(doc.xts6,'days')

daily_doc1 <- period.apply(doc.xts1,ep1,mean) # mg/L
daily_doc2 <- period.apply(doc.xts2,ep2,mean) # mg/L
daily_doc3 <- period.apply(doc.xts3,ep3,mean) # mg/L
daily_doc4 <- period.apply(doc.xts4,ep4,mean) # mg/L
daily_doc5 <- period.apply(doc.xts5,ep5,mean) # mg/L
daily_doc6 <- period.apply(doc.xts6,ep6,mean) # mg/L

# Merge data with continuous date series and interpolate between missing values ####
# create continuous set of dates between mod_start mod_end
mod_start <- '1991-01-01'
mod_end <- '2001-12-27'
dates <- (seq(as.Date(mod_start), as.Date(mod_end), by='days'))
dates_xts <- as.xts(dates)

# Merge xts dates, doc and convert to dataframe
daily_doc_data <- merge.xts(dates_xts, daily_doc1, daily_doc2, daily_doc3, 
                            daily_doc4, daily_doc5, daily_doc6, all=TRUE, fill=NA)
daily_doc_data <- data.frame(date=index(daily_doc_data), coredata(daily_doc_data))
colnames(daily_doc_data) <- c('Date', '3', '3a', '4', '5', '6', '6a')
daily_doc_data$Date <- as.Date(daily_doc_data$Date, format = "%m/%d/%y")
daily_doc_data$Date <- format(daily_doc_data$Date,"%m/%d/%Y")

daily_data2 <- as.data.frame(na.approx(daily_doc_data[,2:7]))
daily_doc_data <- daily_doc_data[-1,]

options(digits=3)
Harp_Sw_doc <- data.frame(datetime=daily_doc_data[,1], DOC_sw=daily_data2) 
colnames(Harp_Sw_doc) <- c('Date', 'DOC3', 'DOC3a', 'DOC4', 'DOC5', 'DOC6', 'DOC6a')
write.csv(Harp_Sw_doc, file='./HarpLake/Staging Files/Harp_interpolated_Sw_DOC.csv')
