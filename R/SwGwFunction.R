SWGWFunction <- function(TimeStepConversion, prop_GW, DOC_GW, DOC_SW) {
  
  InflowData = data.frame(Q_in=NA, Time=NA, DOC_SW=NA, DOC_GW=NA, DOC_SWGW=NA, POC_SWGW=NA)
  
  # Flow Data: Currently constant but could be dynamic
  InflowData$Q_in <- 10 # m3/s: total inflow (Sw + Gw)
  
  # DOC_SwGw == (Gw + Sw) * time_conv
  InflowData$Time <-  86400 * TimeStepConversion # convert time from seconds to days
  InflowData$DOC_SW <- InflowData$Q_in * (1-prop_GW) * DOC_SW
  InflowData$DOC_GW <- InflowData$Q_in * prop_GW * DOC_GW
  InflowData$DOC_SWGW <- (InflowData$DOC_SW + InflowData$DOC_GW) * InflowData$Time # DOC inflow
  # g/d
  
  # POC_SwGw
  InflowData$POC_SWGW <- InflowData$DOC_SWGW * 0.1  #g/d POC 
  # roughly estimated as 0.1 * DOC
  
  return(InflowData)
}