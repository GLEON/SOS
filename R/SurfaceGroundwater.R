### Modeling DOC inputs to lakes from surface and groundwater flow ###
### Sub-process leads: Ian McC, Kait F

# Install (if necessary) & load all packages ####
#install.packages('car')
# install.packages('drc')
#install.packages('fmsb')
#install.packages('lattice')
#install.packages('lmtest')
#install.packages('raster')
#install.packages('relaimpo')
#install.packages('rgdal')
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
setwd('/Users/FarrellKJ/Desktop/R/SOS')

# All spatial data in projected coordinate system NAD 1983 HARN Transverse Mercator

### DATA FOR THIS SECTION NOT ATTACHED TO GITHUB: Wetland buffer approach ####
# Import lakes shapefile
#lakes = shapefile("WI_lakes/WI_NTL.shp") # NTL lakes
lakes = shapefile("randomWIlakes/randomWIlakes.shp") # random lakes from Hanson 2004 study
Wisconsin = shapefile('H:/Ian_GIS/gleon/WI_state_outline/WI_state_outline.shp')
plot(Wisconsin)
plot(lakes, col='dodgerblue', add=T)

# Buffer lakes
bufferwidth = 750 # meter (meter=linear unit of lake shapefile)
lakebuffer = buffer(lakes, width=bufferwidth, dissolve=F)

# Import wetland shapefile
wetlands_shp = shapefile("WI_shapefile_wetlands/WI_Wetlands_NTL_HARN_Clip.shp")#NTL area
wetlands_shp = shapefile("WI_shapefile_wetlands/WI_Wetlands_Hanson_HARN_Clip.shp")#Hanson 2004 study area
#wetlands from: http://dnr.wi.gov/maps/gis/datalandcover.html

## Make wetlands shapefile into raster ##
cell_size = 100 # 30 m is common spatial resolution of land cover datasets 

# Create blank raster based on extent of wetland shapefile
wetlands_raster = raster(wetlands_shp, resolution=c(cell_size,cell_size), vals=1)

# Use shapefile as mask to get wetlands raster (areas in blank raster that actually have wetlands)
wetlands_raster = mask(wetlands_raster, wetlands_shp, inverse=F)

# Extract wetlands in buffered area around lakes
buffer_wetlands = mask(wetlands_raster,lakebuffer, inverse=F)

# Map of Northern Highland lakes
#plot(buffer_wetlands, col='lightblue', main='Northern Highland Lakes', legend=F, xlim=c(540000,550000), ylim=c(610000,625000)) # adjust xlim, ylim based on extent of specific analysis
#plot(lakebuffer, add=T)
#plot(lakes, col='dodgerblue', add=T)

# Map of Madison area lakes
#plot(buffer_wetlands, col='lightblue', main='Madison Lakes', legend=F, xlim=c(548000,580000), ylim=c(285000,315000)) # adjust xlim, ylim based on extent of specific analysis
#plot(lakebuffer, add=T)
#plot(lakes, col='dodgerblue', add=T)

# Map of Hanson 2004 northern WI lakes
plot(buffer_wetlands, col='lightblue', main='Hanson 2004 Northern WI Lakes', legend=F, xlim=c(480000,610000), ylim=c(560000,650000)) # adjust xlim, ylim based on extent of specific analysis
plot(lakebuffer, add=T)
plot(lakes, col='dodgerblue', add=T)

# Calculate wetland area within lake bufferwidth by counting # of cells and compile in data frame
wetlands_cells = extract(buffer_wetlands, lakebuffer, fun=sum, na.rm=T)
wetlands_sqm = wetlands_cells * cell_size^2 # multiply # of cells by cell area to get wetlands area
wetlands_area_df = data.frame(WATERBODY_=lakes$WATERBODY_, Wetlands_sqm = wetlands_sqm, LakeArea_sqm = lakes$SHAPE_Area)
wetlands_area_df$Wetland_Lake_Ratio = wetlands_area_df$Wetlands_sqm/wetlands_area_df$LakeArea_sqm # ratio of wetland area in buffer to lake area

## Optionally, create dynamic lake buffer width weighted by lake area ##
area = lakes$SHAPE_Area
lakeid = lakes$Lake
dino_width = area / bufferwidth
#dino_width = data.frame(dino_width = dino_width, Lake=lakeid)

# apply weighted lake buffer
dino_buffers = buffer(lakes, width=dino_width, dissolve=F)
dino_buffers$dino_width = dino_width

# Extract wetlands in buffered areas around lakes using weighted buffer width
dino_wetlands = mask(wetlands_raster, dino_buffers, inverse=F)

# Calculate wetland area within dino bufferwidth by counting # of cells and compile in data frame
dino_wetlands_cells = extract(dino_wetlands, dino_buffers, fun=sum, na.rm=T)
dino_wetlands_sqm = dino_wetlands_cells * cell_size^2 # multiply # of cells by cell area to get wetlands area
dino_wetlands_area_df = data.frame(WATERBODY_=lakes$WATERBODY_, Wetlands_sqm = dino_wetlands_sqm, LakeArea_sqm = lakes$SHAPE_Area)
dino_wetlands_area_df$Wetland_Lake_Ratio = dino_wetlands_area_df$Wetlands_sqm/wetlands_area_df$LakeArea_sqm # ratio of wetland area in buffer to lake area

### Examine relationship between lake DOC and wetlands within buffer distance of lake ###

# Data file: NTL LTER focal lakes: nutrient, pH, Carbon (1981-2013)
# Metadata: http://tinyurl.com/l9wa4d5

# Import Full LTER dataset
#lake_chem <- read.csv('NTL_Water_Chem_1981_2013.csv', header=T)

# Omit DOC values that are NA
#lake_chem2 <- subset(lake_chem, DOC!='NA')

# Create dataframe of average DOC values for each lake
#DOC_means <- tapply(lake_chem2$DOC, list(lakeid=lake_chem2$lakeid), mean) 
#DOC_means <- as.data.frame(DOC_means)

# Subset for 1991-1993; year range based on acquisition of wetland data
# buffer test. Excludes rows where DOC value is missing (=NA)
#lake_chem_9193 <- subset(lake_chem2, (year==2007|year==2008|year==2009))

# Create dataframe of average DOC values for each lake
#test_lakes_DOC_mean <- tapply(lake_chem_9193$DOC, list(lakeid=lake_chem_9193$lakeid), mean) 
#test_lakes_DOC_mean <- as.data.frame(test_lakes_DOC_mean)
#test_lakes_DOC_mean$Lake = row.names(test_lakes_DOC_mean)

# Add column for POC; define POC = TOC - DOC
# Many values show as POC < 0
#lake_chem_9193$POC <- (lake_chem_9193$TOC - lake_chem_9193$DOC) 


# Plot DOC over time from 1991-1993 in each lake ###
xyplot(DOC~sampledate, group=lakeid, data=lake_chem_9193, type=c('p','r'),
       lwd=3, pch=16, cex=.6, 
       col=rainbow(5),
       xlab='Sample Date (1991-1993)', ylab='DOC (mg/L)', 
       key=list(text=list(c("AL", "BM","CB","CR", "SP", "TB", "TR")), cex=1,
                points=list(pch="-", cex=5,
                            col=rainbow(5)),
                columns=1, border=T, corner=c(.95,.98), 
                title='Lake ID'),
       scales=list(alternating=1,tck=c(1,0), cex=1.5,
                   y=list(at=seq(0,10,2))))

# Plot DOC vs TOC, DOC vs POC: Can reasonably predict TOC from DOC
DOC_TOC_lm <- lm(DOC~TOC, data=lake_chem_9193)
summary(DOC_TOC_lm)
xyplot(DOC~TOC, group=lakeid, data=lake_chem_9193, xlim=c(0,10), ylim=c(0,10),
       panel = function(x, y) {
         panel.xyplot(x, y, type='p', pch=16, lwd=2, cex=2, col=rainbow(7))
         panel.abline(0,  1, lwd=2)
       },
       key=list(text=list(c("AL", "BM","CB","CR", "SP", "TB", "TR")), cex=1,
                points=list(pch="-", cex=5,
                            col=rainbow(7)),
                columns=1, border=T, corner=c(.95,.1), 
                title='Lake ID'))

# DOC vs POC: Not predictive!  
DOC_POC_lm <- lm(DOC~POC, data=lake_chem_9193)
summary(DOC_POC_lm)
xyplot(POC~DOC, group=lakeid, data=lake_chem_9193, Xlim=c(-4,10), ylim=c(-4,10),
       panel = function(x, y) {
         panel.xyplot(x, y, type='p', pch=16, lwd=2, cex=2, col=rainbow(5))
         panel.abline(0,  1, lwd=2)
       },
       key=list(text=list(c("AL", "BM","CB","CR", "SP", "TB", "TR"), cex=1,
                          points=list(pch="-", cex=5,
                                      col=rainbow(5)),
                          columns=1, border=T, corner=c(.05,.98), 
                          title='Lake ID')))

# Merge DOC and wetland area data by lake code name
#DOCdata = merge(test_lakes_DOC_mean, wetlands_area_df, by='WATERBODY_')
#DOCdata$DOC = DOCdata$test_lakes_DOC_mean 

#DOClm = lm(DOC ~ Wetland_Lake_Ratio, DOCdata)
#summary(DOClm)
#VIF(DOClm) # Variance Inflation Factors (multicollinearity check)
#layout(matrix(c(1,2,3,4),2,2)) # view all 4 diagnostic plots at once
#plot(DOClm)
#shapiro.test(rstudent(DOClm)) # Shapriro-Wilks test for normality (of residuals)
#bptest(DOClm) # Breusch-Pagan test for heteroskedasticity (non-constant variance)

# Plot observed vs. predicted DOC
#par(mfrow=c(1,1))
#predicted = predict(DOClm)
#predvsobs = data.frame(Lake=wetlands_area_df$Lake) # create new data frame of predicted vs. obs DOC
#predvsobs$obs = DOCdata$DOC # $DOC is a placeholder for column name of DOC in DOCdata
#predvsobs$pred = predicted

#plot(obs~pred, predvsobs, xlab='Observed DOC UNIT', ylab='Modeled DOC unit', pch=16, 
#     main='DOC, Wisconsin NTL')
#abline(0,1) # add 1:1 fit line

# Add R squared to plot
#sumlm = summary(DOClm)
#r2= sumlm$r.squared
#label = bquote(italic(R)^2 == .(format(r2,digits=2)))
#text(x=1.9,y=0.5, labels=label) # will need to adjust x and y based on data distribution

### Northern Wisconsin 2004 Dataset ####
# Data file: Northern Wisconsin Temperate Lakes fluxes project, random lake survey (2004)
# Citation: Hanson PC, Carpenter S, Cardille JA, Coe MT, Winslow LA.  2007.  Small lakes dominate a random sample of regional lake characteristics. Freshwater Biology. 52:814-22

RLS_data = read.csv('./data/randomWIlakes_DOC.csv', header=T) # contains column for DOC
names(RLS_data)

# Merge DOC and wetland area data by lake code name
#DOCdata = merge(RLS_data, wetlands_area_df, by='WATERBODY_')

#Create "DOCdata" file == RLS_data if not using GIS data portion of code
DOCdata <- RLS_data

# Correlation matrix for all variables in dataset
cor.matrix <- cor(x=DOCdata[-1],y=NULL, use="na.or.complete") #[-1] omits objectid column
print(cor.matrix)

# Linear regression between lake DOC and wetlands within buffered distance (m)
DOClm = lm(DOC ~ Secchi, DOCdata)
summary(DOClm)
VIF(DOClm) # Variance Inflation Factors (multicollinearity check)
layout(matrix(c(1,2,3,4),2,2)) # view all 4 diagnostic plots at once
plot(DOClm)
shapiro.test(rstudent(DOClm)) # Shapriro-Wilks test for normality (of residuals)
bptest(DOClm) # Breusch-Pagan test for heteroskedasticity (non-constant variance)
outlierTest(DOClm)
#DOCdata = DOCdata[-39,] # remove outlier row by row number

# Plot observed vs. predicted DOC
par(mfrow=c(1,1))
predicted = predict(DOClm)
predvsobs = data.frame(WBIC=DOCdata$WATERBODY_) # create new data frame of predicted vs. obs DOC
predvsobs$obs = DOCdata$DOC # $DOC is a placeholder for column name of DOC in DOCdata
predvsobs$pred = predicted

plot(obs~pred, data=predvsobs, xlab='Observed DOC UNIT', ylab='Modeled DOC unit', pch=16, 
     main='DOC, Northern Wisconsin 2004 Lake Dataset')
#abline(0,1) # add 1:1 fit line

# Add R squared to plot
sumlm = summary(DOClm)
r2= sumlm$r.squared
label = bquote(italic(R)^2 == .(format(r2,digits=2)))
text(x=-5,y=27, labels=label) # will need to adjust x and y based on data distribution

# Basic plot of DOC ~ Secchi relationship
plot(DOC ~ Secchi, DOCdata, pch=16)

# LM  & plot of logDOC ~ logSecchi
logDOCSecchi <- lm(log(DOC)~log(Secchi), data=DOCdata)
plot(log(DOC)~log(Secchi), data=DOCdata, pch=16, main='log-log DOC ~ Secchi')
abline(logDOCSecchi, col='red', lwd='2')
r.sq <- summary(logDOCSecchi)$r.squared
label = bquote(italic(R)^2 == .(format(r.sq,digits=3)))
text(x=0,y=0.5, labels=label)
text(x=0, y=0.3, 'P = <2e-16')
# outlier points are row 39 (object ID 5703) & row 61 (object ID 6992)

## Non-linear model of DOC predicted by Secchi
# with package drc

# fixed vector: NA for unfixed, value is fixed at that value, names=names of fixed vector 
drc = drm(DOC~Secchi, data=DOCdata, fct=EXD.3(fixed =c(NA,NA,NA),names=c("init", "plateau", "k")))       
plot(drc,pch=16, main='#GroanZoan')
summary(drc)
getInitial(drc) # print model starting parameters
#identify(DOCdata$DOC, DOCdata$Secchi, labels=row.names(DOCdata), plot=T) DOES NOT WORK

RSS = sum(residuals(drc)^2)
TSS = sum((DOCdata$DOC - mean(DOCdata$DOC))^2)
a = round((1-RSS/TSS),2) # R squared
print(a)
label = bquote(italic(R)^2 == .(format(a,digits=2)))
text(x=6, y=28, labels=label)

# modeled vs observed plot
modeled = predict(drc)
modvsobs = data.frame(WBIC=DOCdata$WATERBODY_) # create new data frame of predicted vs. obs DOC
modvsobs$obs = DOCdata$DOC # $DOC is a placeholder for column name of DOC in DOCdata
modvsobs$mod = modeled

plot(obs~mod, data=modvsobs, xlab='Observed DOC UNIT', 
     ylab='Modeled DOC unit', pch=16, 
     main='DOC, Northern Wisconsin 2004 Lake Dataset',
     xlim=c(0,30), ylim=c(0,30))
abline(0,1) # add 1:1 fit line

# with nls function THIS IS NOT COMPLETE; NEED TIPS ON NEG EXPONENTIAL CURVE FITTING
#nls = nls(DOC~I(Secchi^power), data=DOCdata, start=list(power=2), trace=T)
#summary(nls)

### Test multiple regression of predictors from DOCdata to predict DOC
# Stepwise Regression
fit <- lm(DOC~SHAPE_Leng+SHAPE_Area+Depth+Wshed_area+Wshed_perim+Secchi,
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