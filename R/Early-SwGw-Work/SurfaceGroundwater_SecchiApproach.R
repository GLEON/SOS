### Modeling DOC inputs to lakes from surface and groundwater flow ###
### Sub-process leads: Ian McC, Kait F

# Install (if necessary) & load all packages ####
# install.packages('car')
# install.packages('drc')
# install.packages('fmsb')
# install.packages('lattice')
# install.packages('lmtest')
# install.packages('raster')
# install.packages('relaimpo')
# install.packages('rgdal')
library(car)
library(drc)
library(fmsb)
library(lattice)
library(lmtest)
library(MASS)
library(raster)
library(relaimpo)
library(rgdal)

# Set working directory: Must change each time ####
# Comment in/out your working directory for ease of transitions between group members
# setwd('H:/Ian_GIS/gleon/SOS') # Ian's WD
setwd('/Users/FarrellKJ/Desktop/R/SOS') # Kait's WD

##################################################################
### Northern Wisconsin 2004 Dataset ####
# Data file: Northern Wisconsin Temperate Lakes fluxes project, random lake survey (2004)
# Citation: Hanson PC, Carpenter S, Cardille JA, Coe MT, Winslow LA.  2007.  Small lakes dominate a random sample of regional lake characteristics. Freshwater Biology. 52:814-22

RLS_data = read.csv('./data/randomWIlakes_DOC.csv', header=T) # contains column for DOC

#Create "DOCdata" file == RLS_data 
DOCdata <- RLS_data

# Correlation matrix for all variables in dataset
cor.matrix <- cor(x=DOCdata[-1],y=NULL, use="na.or.complete") #[-1] omits objectid column
cor.matrix

# Basic plot of DOC ~ Secchi relationship ####
plot(DOC ~ Secchi, DOCdata, pch=16)
plot(log(DOC)~log(Secchi), DOCdata, pch=16)

## Linear Model of logDOC ~ logSecchi ##
logDOCSecchi <- lm(log(DOC)~log(Secchi), data=DOCdata)
summary(logDOCSecchi)
intercept <- summary(logDOCSecchi)$coefficients[1]
slope <- summary(logDOCSecchi)$coefficients[2]

# Check regression assumptions and view diagnostic plots
VIF(logDOCSecchi) # Variance Inflation Factors (multicollinearity check; won't be an issue with few predictors)
layout(matrix(c(1,2,3,4),2,2)) # view all 4 diagnostic plots at once
plot(logDOCSecchi)
shapiro.test(rstudent(logDOCSecchi)) # Shapriro-Wilks test for normality (of residuals)
bptest(logDOCSecchi) # Breusch-Pagan test for heteroskedasticity (non-constant variance)
outlierTest(logDOCSecchi) # Bonferonni outlier test
DOCdata = DOCdata[-c(39,61),] # remove outlier row by row number

# Plot linear model of logDOC ~ logSecchi
par(mfrow=c(1,1))
plot(log(DOC)~log(Secchi), data=DOCdata, pch=16, main='log-log DOC ~ Secchi',
     xlab='log Secchi Depth (m)', ylab='log DOC (mg/L)')
abline(logDOCSecchi, col='red', lwd='2') #add regression line
r.sq <- summary(logDOCSecchi)$r.squared
label = bquote(italic(R)^2 == .(format(r.sq,digits=3)))
text(x=1.5,y=3.25, labels=label)
text(x=1.5, y=3.1, 'P = <2e-16')
# outlier points previously removed above

# Calculate DOC (g/m3) from Secchi depth ####
log_DOC_est <- (intercept + (slope*log(DOCdata$Secchi)))
# gives logDOC for each Secchi depth; need to transform units of DOC

DOCdata$DOC_est <- exp(log_DOC_est)

# Plot estimate of DOC from log-log regression (black) vs data (blue)
plot(DOCdata$DOC_est~DOCdata$Secchi, pch=16, xlab='Secchi Depth (m)', 
     ylab='DOC (g/m3)', main='Dissolved Organic Carbon (DOC), \n Northern Wisconsin Lakes')
points(DOC ~ Secchi, DOCdata, pch=16, col='blue')
legend('topright', legend=c('Observed', 'Modeled'), col=c('blue', 'black'), pch=16)

# Transform DOC (mg/L) to g/m3 (based on lake volume)
DOCdata$DOC_vol_est <- DOC_est * (DOCdata$SHAPE_Area * DOCdata$Depth)
# NOTE: In this dataset, 'Depth' is not lake mean depth, so volume calculated
# not actual volume

# How off are we? Calculate mean absolute error (MAE)
AE = abs(DOCdata$DOC_est - DOCdata$DOC) 
MAE = round(mean(AE), digits=3)
MAE #g/m3

# Examine modeled vs. observed DOC
plot(DOC~DOC_est, DOCdata, pch=16, xlab='Observed DOC (g/m3)', ylab='Modeled DOC (g/m3)',
     main='Dissolved Organic Carbon (DOC), Northern Wisconsin Lakes', xlim=c(0,25), ylim=c(0,25))
abline(0,1) #1:1 fit line

predobs = lm(DOC~DOC_est, DOCdata)
r.sq <- summary(predobs)$r.squared
label = bquote(italic(R)^2 == .(format(r.sq,digits=3)))
MAElabel = bquote(italic(MAE) ==.(format(MAE,digits=3)))
text(x=24,y=1.2, labels=label)
text(x=24, y=0, labels=MAElabel)

# Test multiple regression of predictors from DOCdata to predict DOC ####
# Stepwise Regression
fit <- lm(DOC~Secchi+SHAPE_Leng+SHAPE_Area+Depth+Wshed_area+Wshed_perim,
          data=DOCdata) #Includes Secchi --> most important predictor
step <- stepAIC(fit, direction="both")
step$anova # display results

# Calculate Relative Importance for Each Predictor
calc.relimp(fit,type=c("lmg","last","first","pratt"), rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fit, b = 1000, type = c("lmg", "last", "first", "pratt"), 
                    rank = TRUE, diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

##################################################################
### Apply similar test of Secchi as predictor of in-lake DOC from 2007 NLA lakes ####
# 2007 NLA database water chem ('Water Quality Data')
# Access via http://water.epa.gov/type/lakes/NLA_data.cfm ####
nla <- read.csv('./data/NTL water chem.csv', header=T)
# column 30 is DOC (mg/L) == 'DOC', column 154 is Secchi depth (m) == 'SECMEAN'

plot(DOC ~ SECMEAN, nla, pch=16)
plot(log(DOC)~log(SECMEAN), nla, pch=16)

# Linear Model of logDOC ~ logSecchi ####
logDOCSecchi.b <- lm(log(DOC)~log(SECMEAN), data=nla)
summary(logDOCSecchi.b)
intercept <- summary(logDOCSecchi.b)$coefficients[1]
slope <- summary(logDOCSecchi.b)$coefficients[2]

# Check regression assumptions and view diagnostic plots
VIF(logDOCSecchi.b) # Variance Inflation Factors (multicollinearity check; won't be an issue with few predictors)
layout(matrix(c(1,2,3,4),2,2)) # view all 4 diagnostic plots at once
plot(logDOCSecchi.b)
shapiro.test(rstudent(logDOCSecchi.b)) # Shapriro-Wilks test for normality (of residuals)
bptest(logDOCSecchi.b) # Breusch-Pagan test for heteroskedasticity (non-constant variance)
coeftest(logDOCSecchi,vcov=hccm(logDOCSecchi.b)) # heteroscedasticity-corrected covariance matrix for summary statistics

outlierTest(logDOCSecchi.b) # Bonferonni outlier test
nla = nla[-c(1101,178,1131,1313,1149),] # remove outliers row by row number

# Plot log-log Secchi vs DOC for 2007 NLA lakes ####
par(mfrow=c(1,1))
plot(log(DOC)~log(SECMEAN), data=nla, pch=16, main='log-log DOC ~ Secchi',
     xlab='log Secchi Depth (m)', ylab='log DOC (mg/L)')
abline(logDOCSecchi.b, col='red', lwd='2') #add regression line
r.sq <- summary(logDOCSecchi.b)$r.squared
label = bquote(italic(R)^2 == .(format(r.sq,digits=3)))
text(x=3,y=4.1, labels=label)
text(x=3, y=3.8, 'P = <2e-16')

### Calculate DOC (g/m3) from Secchi depth ####
log_DOC_est.b <- (intercept + (slope*log(nla$SECMEAN)))
# gives logDOC for each Secchi depth; need to transform units of DOC

nla$DOC_est.b <- exp(log_DOC_est.b)

# Plot estimate of DOC from log-log regression (black) vs data (blue)
plot(DOC ~ SECMEAN, nla, pch=16, col='lightblue', xlab='Secchi Depth (m)', 
     ylab='DOC (g/m3)', main='DOC vs Secchi: 2007 NLA Lakes')
points(nla$DOC_est.b~nla$SECMEAN, pch=16)
legend('topright', legend=c('Observed', 'Modeled'), col=c('lightblue', 'black'), pch=16)

# How off are we? Calculate mean absolute error (MAE)
AE.b = abs(nla$DOC_est.b - nla$DOC) 
MAE.b = round(mean(AE.b, na.rm=TRUE), digits=3)
MAE.b #g/m3

# Examine modeled vs. observed DOC
plot(DOC~DOC_est.b, nla, pch=16, xlab='Observed DOC (g/m3)', ylab='Modeled DOC (g/m3)',
     main='Dissolved Organic Carbon (DOC), 2007 NLA', xlim=c(0,25), ylim=c(0,25))
abline(0,1) #1:1 fit line

predobs.b = lm(DOC~DOC_est.b, nla)
r.sq <- summary(predobs.b)$r.squared
label = bquote(italic(R)^2 == .(format(r.sq,digits=3)))
MAElabel = bquote(italic(MAE) ==.(format(MAE.b,digits=3)))
text(x=20,y=1.2, labels=label)
text(x=20, y=0, labels=MAElabel)
