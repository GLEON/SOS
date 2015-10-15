
Resp <- function(DOC,temp,RespParam){
  #DOC in g/m3
  #CHL in mg/m3 or ug/L
  
  #logR <- -0.92 + 0.41*log(CHL) + 0.30*log(DOC)
  
  #logR <- -0.81 + 0.56*log(CHL)
  TempCorr <- 1.08^(T-20) #Temperature correction factor for DOC-respiration relationship
  
  R <- DOC*RespParam*TempCorr #g/m3
  
  #R <- 10^logR #mmol/m3/hr

  #R <- R*(1/1000)*(16)*(24/1) #g O2/m3/d 
  #Output: R  (O2 mmol/m3/hr) --> * (1mol /1000 mmol) * (16g O2/mol) * (24h/1day)  -->  g/m3/d 

  #Assume O2 respired is one-to-one with grams of DOC removed
  #Resp_DOC_out <- R #g C/m3/d

  return(R)  
}