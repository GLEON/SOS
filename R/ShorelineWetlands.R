### Calculate percent wetland, forest shoreline cover for lakes ###

# Essentially perform standard GIS procedures in uncomfortable R environment
# Date: 8-20-15
# Author: Ian McCullough, immccull@gmail.com

#install.packages(c('raster','rgdal','rgeos'))

library(raster)
library(rgdal)
library(rgeos)

# Set working directory
setwd("C:/Users/Ian/Desktop/GLEON/surfaceflow")

# Load basic GIS layers
Wisconsin = shapefile('WI_state_outline/WI_state_outline.shp')
wetlands = shapefile('WI_Wetlands_NTL_HARN_Clip.shp') 
lakes = shapefile('NTL_lakes.shp')

# Create map of study area lakes (comment out if too large/slow)
plot(lakes, col='dodgerblue', main='Lakes and Wetlands')
#plot(Wisconsin, add=T)
plot(wetlands, add=T, col='purple')

# add optional labels; polygonsLabels optimizes label locations (ie centroid)
labels = lakes$WATERBODY_
polygonsLabel(lakes,labels=labels, method=c('centroid'), cex=1)

# Create buffer around lakes based on specified distance
# 30 m is cell length of rasterized wetland layer (further below)
lakebuffer = buffer(lakes, width=30, dissolve=F)
buffer_ring = symdif(lakebuffer,lakes) # symmetrical difference vector operation
#plot(buffer_ring)

# Identify wetlands within lake buffer
near_wetlands = crop(wetlands, buffer_ring)

plot(lakes, col='dodgerblue', main='Wetlands adjacent to lakeshores')
plot(near_wetlands, col='purple', add=T)
labels = lakes$WATERBODY_
polygonsLabel(lakes,labels=labels, method=c('centroid'), cex=1)

# Convert near wetlands to raster
cell_size = 30 # 30 m is common spatial resolution of land cover datasets 

# Create blank raster based on extent of wetland shapefile
wetlands_raster = raster(near_wetlands, resolution=c(cell_size,cell_size), vals=1)

# Use shapefile as mask to get wetlands raster (areas in blank raster that actually have wetlands)
wetlands_raster = mask(wetlands_raster, near_wetlands, inverse=F)

# Count number of wetland cells along shoreline of each lake (Thank you Zutao here!)
x = vector(mode='numeric', length=nrow(lakebuffer))
for(i in 1:nrow(lakebuffer)) { 
  x[i] = freq(crop(wetlands_raster, lakebuffer[i,]), value=T, useNA='no')
}

length = cell_size*x #multiply num of cells by cell length specified above
shoreline = data.frame(lake=lakes$WATERBODY_, perim_m=lakes$SHAPE_Leng,wetland_m=length)
shoreline$area_sqm = lakes$SHAPE_Area
shoreline$non_wetland_m = shoreline$perim_m - shoreline$wetland_m
shoreline$pct_wetland = (shoreline$wetland_m/shoreline$perim_m) *100
shoreline$pct_non_wetland = 100-shoreline$pct_wetland

# export data to csv
#write.csv(shoreline, file='shoreline_cover.csv')
