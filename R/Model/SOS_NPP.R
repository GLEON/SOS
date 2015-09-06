
NPP<-function(CHL,P,WT)
{
  
  #coefficients to predict chla from TP
  a0=-0.390    
  a1=0.874
  
 #coefficients to predict GPP from chl and WT
 #log(NPP)=b0+b1*log10(chl)+b2*WT
 
  b0=1.18
  b1=0.92
  b2=0.014
  ###########################################################################
  if(missing(CHL)||is.na(CHL))
  {
    if(missing(P)||is.na(P))
      return(NA)
    else
    {
      P<-log10(P)
      CHL<- a0+a1*P
      CHL<-10^CHL
    }
    
  }
  NPP<-b0+b1*log10(CHL)+b2*WT
  
  return(10^NPP)
}

#P <- rnorm(10,80,2) #ug/L phosphorus concentration 
#WT <- rnorm(10,30,1) # degrees C : averaged over photic zone depth 
#CHL <- rnorm(10,100,0.3) #chlorophyll-a concentration (ug/L)


#x<-NPP(CHL,P,WT) #mgC/ m-2/ day-1

