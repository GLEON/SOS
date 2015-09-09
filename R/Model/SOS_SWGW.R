SWGWFunction <- function(Q_sw,Q_gw,rainfall,Aoc_year, PC, lakePerim, Woc_year, PW, DOC_GW, prop_GW, 
                         DOC_SW, lakeArea) {
  #! Per Table 4 of Hanson et al. 2014 (L&O), whould be ~1.15 g/m Shoreline/d (not per year)
  InflowData = data.frame(POC_Aerial=NA, POC_SW=NA, DOC_Wetland=NA, 
                          DOC_GW=NA, DOC_SW=NA, DailyRain=NA, 
                          DOC_Precip=NA, Load_DOC=NA, Load_POC=NA)
    
  # Aerial POC (g/d)
  Aoc = Aoc_year/365 #g/m/d: aerial loading factor
  InflowData$POC_Aerial <- (PC*Aoc*lakePerim) + ((1-PC) *0.2* Aoc * lakePerim)
  
  # Wetland DOC (g/d)
  Woc = Woc_year/365 #g/m/d: adjacent wetland loading factor
  InflowData$DOC_Wetland <- PW * Woc * lakePerim
  
  # Gw DOC (g/d)
  InflowData$DOC_GW <- DOC_GW * Q_gw * 86400 #g/d
  
  # Sw DOC (g/d)
  InflowData$DOC_SW <- DOC_SW * Q_sw * 86400 #g/d

  # Precipitation DOC (g/d)
  #Rainfall <- 2 #mm/d: precipitation input from meteorological timeseries(?) #commented out - met would be read in main program (DR, 7/26)
  InflowData$DailyRain <- (Rainfall * 0.001) * lakeArea #m3/d: daily precip.
  InflowData$DOC_Precip <- DOC_Precip * InflowData$DailyRain #g/d
  #! We do not see the value for DOC_Precip being passed into the function.  Typically, this is a bout 2 g/m3
  
  #! Split out from the total POC equation the portion that is POC in the surface water inflow, and then track that separately.
  #! This will allow us to play with how POC is loaded due to, e.g., storm events (plus it's hidden as currently written).
  # LOAD DOC (g/d)
  InflowData$Load_DOC <- InflowData$DOC_Wetland + InflowData$DOC_GW + InflowData$DOC_SW + InflowData$DOC_Precip # g/d DOC
  
  # Internal POC (g/d)
  InflowData$POC_SW <- InflowData$Load_DOC*0.1
  
  # LOAD POC (g/d)
  InflowData$Load_POC <- InflowData$POC_Aerial + InflowData$POC_SW # g/d POC roughly estimated as (0.1 * DOC)
  
  return(InflowData)
}