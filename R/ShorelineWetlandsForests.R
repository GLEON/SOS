### Calculate percent wetland, forest shoreline cover for lakes ###

# Essentially perform standard GIS procedures in uncomfortable R environment
# Date: 11-19-15 
# Author: Ian McCullough, immccull@gmail.com

#install.packages(c('raster','rgdal','rgeos'))

library(raster)
library(rgdal)
library(rgeos)

# Set working directory
setwd("H:/Ian_GIS/gleon/SOS/GIS/TroutLake")

# Load basic GIS layers
#Wisconsin = shapefile('WI_state_outline.shp')
wetlands = shapefile('WI_Wetlands_NTL_HARN_Clip.shp')
lakes = shapefile('NTL_lakes.shp')
landcover = raster('wi_lc_NTL')

# Create map of study area lakes (comment out if too large/slow)
plot(lakes, col='dodgerblue', main='Lakes and Wetlands')
#plot(Wisconsin, add=T)
plot(wetlands, add=T, col='purple')

# add optional labels; polygonsLabels optimizes label locations (ie centroid)
labels = lakes$WATERBODY_
polygonsLabel(lakes,labels=labels, method=c('centroid'), cex=1)

# Create buffer around lakes based on specified distance
lakebuffer = buffer(lakes, width=30, dissolve=F) #meters (mapping unit of input lake layer)
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

# Count number of wetland cells along shoreline of each lake (Thank you Zutao for helping with loop!)
x = vector(mode='numeric', length=nrow(lakebuffer))
for(i in 1:nrow(lakebuffer)) { 
  x[i] = freq(crop(wetlands_raster, lakebuffer[i,]), value=T, useNA='no')
}

length = cell_size*x #multiply num of cells by cell length specified above
shoreline = data.frame(lake=lakes$WATERBODY_, perim_m=lakes$SHAPE_Leng,wetland_m=length)
shoreline$area = lakes$SHAPE_Area
shoreline$non_wetland_m = shoreline$perim_m - shoreline$wetland_m
shoreline$pct_wetland = (shoreline$wetland_m/shoreline$perim_m) *100
shoreline$pct_non_wetland = 100-shoreline$pct_wetland

##### calculate perecnt of lake shoreline covered by forest #####

# for the wetlands, I made a ring (width=cell-length=30m) around the lakes and counted the 
# wetland cells within that ring to quantify shoreline wetlands

# for forests, this approach didn't work because of the way in which the lake shapefile 
# didn't line up with the landcover raster dataset (lake raster didn't quite line up with lake shapefile)

# I ended up using a 30 m wide buffer again for forests, but the issue is that because the lineup
# isn't perfect, lake raster cells are counted as next to the buffered lake shapefile
# making a wide buffer creates problems because lake polygons end up overlapping or too many cells
# get counted in the shoreline counts

# for now, what I have below seems OK, given that it produces a similar forest value as Hanson et al. 2014

# Identify land cover cells within lake buffer
landcover_clip = crop(landcover, lakebuffer) # full state file is too big/unwieldy
#plot(landcover)
near_landcover = mask(landcover_clip, lakebuffer, inverse = F)
plot(near_landcover, main='Shoreline Land cover')
plot(lakes, col='dodgerblue', add=T)
labels = lakes$WATERBODY_
polygonsLabel(lakes,labels=labels, method=c('centroid'), cex=1)

### Reclassify forested pixels as 1, all else as no data (file is too big to run on whole state)
# values_of_forest = c(161,162,163,166,173,175,176,177,179,180,183,185,187,190,223,229,234)
# (from Wiscosin DNR land cover dataset, from Landsat imagery)
# Create 3x3 reclassification table (matrix)
# first value = to
# second value = from
# third value = new value
# ex) 0,150,1: in English: make values 0-150 as NA
forest_values = c(0,150,NA, 161,190,1, 200,220,NA, 223,234,1, 240,255,NA)
classy = matrix(forest_values, ncol=3, byrow=T)
near_landcover2 = reclassify(near_landcover, classy)
plot(near_landcover2, main='Shoreline Land cover')
plot(lakes, add=T, col='dodgerblue')
labels = lakes$WATERBODY_
polygonsLabel(lakes,labels=labels, method=c('centroid'), cex=1)

# Count number of forest cells along shoreline of each lake (Thank you Zutao for helping with loop!)
x = vector(mode='numeric', length=nrow(lakebuffer))
for(i in 1:nrow(lakebuffer)) { 
  x[i] = freq(crop(near_landcover2, lakebuffer[i,]), value=T, useNA='no')
}

length = cell_size*x #multiply num of cells by cell length specified above
shoreline$forest_m=length
shoreline$non_forest_m = shoreline$perim_m - shoreline$forest_m
shoreline$pct_forest = (shoreline$forest_m/shoreline$perim_m) *100
shoreline$pct_non_forest = 100-shoreline$pct_forest
