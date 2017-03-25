
fixToolik <- function(InputData,LakeName) {
  # Set FlowIn to 0 for ice-on periods for Toolik Inlet, based on historical data: 
  # http://toolik.alaska.edu/edc/journal/annual_summaries.php?summary=inlet
  # Used average ice on/off dates from 2006-2010 for 2001-2005 (no data available those years)
  
  icepath = paste0(LakeName,'Lake','/','ToolikInlet_IceOn_IceOff.csv')
  IceOnOff = read.csv(icepath)
  IceOnOff$IceOff = as.POSIXct(strptime(IceOnOff$IceOff,"%Y-%m-%d"),tz="GMT") #Convert time to POSIX
  IceOnOff$IceOn = as.POSIXct(strptime(IceOnOff$IceOn,"%Y-%m-%d"),tz="GMT") #Convert time to POSIX
  #str(IceOnOff)
  
  ice_func <- function(year,off,on, dataframe){
    ## ARGUMENTS ##
    # year: 4 digits, in quotes as character ('2002')
    # off: month-day in quotes ('05-09') = May 9th
    # on: same structure as off
    # dataframe = name of dataframe of interest
    
    day1 = paste0(year,'-01-01')
    year_num = as.numeric(year)
    year_before = as.character(year_num - 1)
    day1 = paste0(year_before, '-12-31') # there was a bug; R thought >= Jan 1 meant Jan 2 (must be something internal with date structure)
    day365 = paste0(year, '-12-31')
    iceoff = paste0(year,'-',off)
    iceon = paste0(year,'-',on)
    # create annual subset for specific year
    annual_subset = dataframe[dataframe$datetime > day1 & dataframe$datetime <= day365,]
    
    # extract data for that year before ice off
    pre_thaw = annual_subset[annual_subset$datetime < iceoff,]
    pre_thaw$FlowIn = rep(0,length(pre_thaw$FlowIn)) # set FlowIn = 0 during ice time
    pre_thaw$FlowOut = pre_thaw$FlowIn # we assume flow out = flow in
    
    # extract data for that year for between ice off and ice on (ice free season)
    ice_free_season = annual_subset[annual_subset$datetime >= iceoff & annual_subset$datetime < iceon,]
    
    # extract data for that year for after fall ice on
    post_freeze = annual_subset[annual_subset$datetime >= iceon,]
    post_freeze$FlowIn = rep(0,length(post_freeze$FlowIn))
    post_freeze$FlowOut = post_freeze$FlowIn
    
    # combine 3 annual subsets (pre thaw, ice free season, post freeze)
    annual_corrected = rbind.data.frame(pre_thaw,ice_free_season,post_freeze)
    return(annual_corrected)
  }
  
  years = as.character(IceOnOff$Year)
  iceoff_dates = IceOnOff$IceOff
  iceoff_dates = format(iceoff_dates, format='%m-%d')
  iceon_dates = IceOnOff$IceOn
  iceon_dates = format(iceon_dates, format='%m-%d')
  
  for (i in 1:length(years)){
    for (j in 1:length(iceoff_dates)) {
      for (k in 1:length(iceon_dates)) {
        x = ice_func(year = years[i], off = iceoff_dates[j], on = iceon_dates[k], dataframe = InputData)
        assign(paste0(LakeName,years[i]),x)
        x = NULL #get rid of extra output with unassigned name
      }
    }
  }
  
  ## Combine annual data frames into single for lake
  # I know this isn't the most dynamic code, but I was having trouble making the above loop output a single DF
  InputData = rbind(Toolik2001,Toolik2002,Toolik2003,Toolik2004,Toolik2005,Toolik2006,
                    Toolik2007,Toolik2008,Toolik2009,Toolik2010)
  return(InputData)
}