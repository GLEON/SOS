#NPP Sub-Routine for SOS Carbon Flux Code
#Last Update: 6/9/2015, MM


#Inputs from main program

PAR <- 1000 # mmol/m2/sec surface light input 
lakeArea <- 100 * 10000  #m2
lakeDepth <- 25 #m mean depth
lakeVol <- lakeArea * lakeDepth #m3


StartDay <- 1  #Julian day
EndDay <- 365 #Julian day

TimeStep <- 1 #days
TimeStepConversion <- 1/TimeStep #days/days 


#NPP-Specific Inputs

Z <- 5 #m measurement depth 
Iz<- 800 #mmol / m2/ s PAR at Z
ln(Iz/PAR)<- # surface PAR remaining at Z
Zeu <- #m euphotic zone depth. = depth w/ 1% surface PAR remaining
P <- 0.000015 #g/L phosphorus concentration 
WT <- 20 # degrees C : averaged over photic zone depth 
CHL <- 0.01 #chlorophyll-a concentration (g/L)
DO <- 0.0085 #g/L dissolved oxygen concentration
DOsat <- 100 #% temperature corrected oxygen saturation
wnd <- #wind speed (m/s)


#Constants

kO2 <- 2.65 #cm / hr  #input literature value. need to account for 
                      #variability in wind speed, stratification, lake size
                      #see Cole & Caraco 1998, Cole et al 2010, Read et al 2012
Kd_par<-0.05  #extiction coefficient. slope of linear relationship between
                                  #Z and ln(Iz/PAR)
                                  #If no PAR, = 1.7 / Secchi depth
R<- 0.01 #Respiration coefficient. Literature value. Need to update.

#GPP reaction parameters
#Zutao may want to update this section before its integrated to full model
  
  GPP_react_par<-function(CHL,P,WT=20)
  {
    #######define coefficients to predict CHL from TP when CHL is not available
    ##reference: Prairie et al, Unifying Nutrient-chlorophyll relationships in Lakes
    a0=-0.390    ##########log10(CHL)=a0+a1*log10(P)
    a1=0.874
    ###########################################################################
    
    ######define coefficients to predict GPP from chl and WT
    ##reference: Morin et al, Empirical models predicting primary productivity from......
    
    ####GPP_react_par<-b0+b1*log10(chl)+b2*WT
    b0=-1.18
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
    GPP<-b0+b1*log10(chl)+b2*WT
    
  }


#Simple Mixed layer model  

GPP = PAR * (PAR*GPP_react_par) # Linear relationship between PSN, light
#OR could model as non-linear saturation function accounting for max psn & 
# light limitation
#These units need attention.

FluxAtm = kO2 (DO - DOsat)/Zeu #allows for dynamic euphotic zone depth

#Full NPP Model

O2 = (PAR*GPP_react_par) - R + (kO2 (DO-DOsat)/Z) #mg / L / day
# fixed C:O2 is 1:1 because CO2+H2O -> (CH2O) + O2 

#So, unit housekeeping:
#OC_NPP = O2 (mg/L/day) * (1L/0.001 m3) * (1g/1000mg) * lakeVol (m3)
      # = C g/day




############################################################################

###### Carbon Mass Flux Equations#############
OC_GPP = GPP * lakeVol # g/day double check unit coversion from model above
OC_NPP = O2 * lakeVol #g/day
  