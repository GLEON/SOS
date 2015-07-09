# Example code adapted from LakeMetabolizer
# This code returns the gas exchange velocity for gas of interest
# Uses both a packaged function and an internal function called 'k.cole'
# This function must be first 'sourced' for the code to run. 
# If using RStudio, loaded Functions are listed in the global environment on the 
# right side of the screen, at the bottom under all the Data and Values

library(LakeMetabolizer) #must have package installed

n	<-	0.5
schmidt	<- getSchmidt(temperature = 12,gas = "O2") #LakeMetabolizer function
Sc600	<- schmidt/600

k600 <- k.cole(wnd=4) # <<<---- Internal function below, units in m/d
kGAS <-	k600*(Sc600^-n)

###################################################################
k.cole <- function(wnd){
  #This function uses just the wind speed it is supplied in m/s 
  k600 <- 2.07 + (0.215 * (wnd^1.7)) # units in cm/h
  k600 <- k600*24/100 #units in m/d
  return(k600)
}
