
NPP<-function(CHL,P,PhoticDepth,WT)
{
  
  NPPdata <- data.frame(NPP_DOC_rate=NA,NPP_POC_rate=NA)
  #! Document units on the input parameters
  #M = Mindy comments 
  
  #coefficients to predict chla from TP
  #M Prairie et al. 1989
  #M log(chl a)= -0.390 + 0.874 log(TP)
  #M chla and TP reported in ug/L
  
  #coefficients to predict chla from TP
  a0=-0.390   #! 
  a1=0.874
  
  #coefficients to predict NPP from chl and WT
  #log(NPP)=b0+b1*log10(chl)+b2*WT
  
  #M Morin et al. 1999
  #M Areal chl a : mg chl a m-2
  #M Authors converted chl a mass per volume to areal values by integrating 
  #M over photic zone depth defined in original papers 
  #M Temp degrees C
  #M NPP output in g m-2 d-1
  
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
  
  CHL <- CHL*PhoticDepth
  P <- P*PhoticDepth
  
  NPP_rate <- 10^(b0+b1*log10(CHL)+b2*WT) #mg C/m2/d
 
  NPP_Percent_DOC <- 71.4*CHL^(-0.22) #NPP as DOC, estimated as equal to 
  #%POC respired because POC most be converted to DOC to eventually be respired.
  #Citation, Mindy? This value represents fraction of NPP that becomes DOC
  #M Pace and Prairie 2005, Ch. 7: Respiration in Lakes 
 
  NPPdata$NPP_DOC_rate <- NPP_rate*(NPP_Percent_DOC/100)  #mg C/m2/d
  NPPdata$NPP_POC_rate <- NPP_rate*(1-(NPP_Percent_DOC/100))  #mg C/m2/d

  return(NPPdata)
}



