SWGWFunction <- function(Aoc_year, PC, lakePerim, Woc_year, PW, DOC_GW, prop_GW, 
                         DOC_SW, lakeArea, DOC_Precip) {
  
  InflowData = data.frame(Q_in=NA, Aoc=NA, DOC_Aerial=NA, DOC_Wetland=NA, 
                          DOC_GW=NA, DOC_SW=NA, DailyRain=NA, 
                          DOC_Precip=NA, Load_DOC=NA, Load_POC=NA)
  
  # Flow Data: Currently constant but could be dynamic
  InflowData$Q_in <- 5 #m3/s: total inflow (Sw + Gw)
  
  # Aerial DOC (g/d)
  Aoc = Aoc_year/365 #g/m/d: aerial loading factor
  InflowData$DOC_Aerial <- (PC*Aoc*lakePerim) + ((1-PC) *0.2* Aoc * lakePerim)
  
  # Wetland DOC (g/d)
  Woc = Woc_year/365 #g/m/d: adjacent wetland loading factor
  InflowData$DOC_Wetland <- PW * Woc * lakePerim
  
  # Gw DOC (g/d)
  InflowData$DOC_GW <- DOC_GW * (prop_GW * InflowData$Q_in) * 86400 #g/d
  
  # Sw DOC (g/d)
  InflowData$DOC_SW <- DOC_SW * ((1-prop_GW)*InflowData$Q_in) * 86400 #g/d

  # Precipitation DOC (g/d)
  Rainfall <- 2 #mm/d: precipitation input from meteorological timeseries(?)
  InflowData$DailyRain <- (Rainfall * 0.001) * lakeArea #m3/d: daily precip.
  InflowData$DOC_Precip <- DOC_Precip * InflowData$DailyRain #g/d
  
  # MAIN OUTPUTS: LOAD DOC & POC (g/d)
  InflowData$Load_DOC <- InflowData$Aerial + InflowData$Wetland + InflowData$DOC_GW + InflowData$DOC_SW + InflowData$Precip # g/d DOC
  InflowData$Load_POC <- InflowData$Load_DOC * 0.1 # g/d POC roughly estimated as (0.1 * DOC)
  
  return(InflowData)
}