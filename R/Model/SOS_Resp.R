
Resp <- function(DOC,temp,RespParam){
  #DOC in g/m3
  #CHL in mg/m3 or ug/L
  
  # M Pace and Prairie 2005, Ch. 7: Respiration in Lakes 
  # R <- 10^(-0.92 + 0.41*log10(CHL) + 0.30*log10(DOC)) # mmol O2/m3/hr 
  # R <- R*(1/1000)*(32)*(24/1) #g O2/m3/d 
  
  # logR <- -0.81 + 0.56*log(CHL) # M Pace and Prairie 2005, for CHL only 
  
  TempCorr <- 1.08^(temp-20) #Temperature correction factor for DOC-respiration relationship
  
  if (RespParam > 0) {
    R <- DOC*RespParam*TempCorr #g/m3
  } else {
    R <- 0
  }
  
  #R <- 10^logR #mmol/m3/hr
  #R <- R*(1/1000)*(16)*(24/1) #g O2/m3/d 
  #Output: R  (O2 mmol/m3/hr) --> * (1mol /1000 mmol) * (16g O2/mol) * (24h/1day)  -->  g/m3/d 

  #Assume O2 respired is one-to-one with grams of DOC removed
  #Resp_DOC_out <- R #g C/m3/d

  return(R)  
}