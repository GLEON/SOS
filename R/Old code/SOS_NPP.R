
GPP<-function(CHL,P,WT)
{
  #! Document units on the input parameters
  #M = Mindy comments 9/10/15
  
  #coefficients to predict chla from TP
  #M Prairie et al. 1989
  #M log(chl a)= -0.390 + 0.874 log(TP)
  #M chla and TP reported in ug/L
    
  a0=-0.390   #! Units? Citation? 
  a1=0.874
  
 #coefficients to predict GPP from chl and WT
 #log(GPP)=b0+b1*log10(chl)+b2*WT
 
 #M Morin et al. 1999
 #M Areal chl a : mg chl a m-2
 #M Authors converted chl a mass per volume to areal values by integrating 
  #M over photic zone depth defined in original papers 
 #M Temp degrees C
 #M GPP output in g m-2 d-1
 
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
  GPP<-b0+b1*log10(CHL)+b2*WT
  
  #! We changed what is returned by dividing by 10
<<<<<<< HEAD:R/SOS_NPP.R
  return(10^GPP)
=======
   return(10^NPP / 10)
  #M This should be GPP, not NPP
  #M Not making change here to not interfere with central code
>>>>>>> d5caedcad572a9d4d3852db8870880fbd656495e:R/Model/SOS_NPP.R
}

#! What is being returned by this function may be about 10x too high.
#! for a lake with Chl = 10 ug/L and WT = 20, this results in GPP of about 240 mg/m2, or 0.24 g/m2 of GPP
#! 0.24 g/m2 of GPP is about equal to the diel excursion, not the amount that makes it into the summer duration
#! pool of OC, which might be closer to 0.024 (maybe less?);  For example, over the summer, the OC in Trout might increase by 
#! 20% over baseline values.  If baseline is 2 g/m3, then increase would be 0.4 g/m3.  This would happen over about 100 days (?)
#! Converted to the daily scale, that's about 0.004 g/m3.  If Trout were a 10 m deep lake, that would be 0.04 g/m2/d.

#M Two possible sources of magnitude problem here. 
  #M 1. Output is GPP, not NPP
  #M 2. Chl a in Morin paper is reported as areal values (mg/m2); our 
    #M test input data is a volume (ug/L)

#P <- rnorm(10,80,2) #ug/L phosphorus concentration 
#WT <- rnorm(10,30,1) # degrees C : averaged over photic zone depth 
#CHL <- rnorm(10,100,0.3) #chlorophyll-a concentration (ug/L)


#x<-GPP(CHL,P,WT) #mgC/ m-2/ day-1

