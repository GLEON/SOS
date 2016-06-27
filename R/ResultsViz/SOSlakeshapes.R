library(rgdal)
library(rgeos)
library(sp)
library(latticeExtra)
library(raster)
library(maptools)
setwd("~/Documents/Rpackages/SOS/R/ResultsViz")

annie = readOGR('Annie/abs_LakeAnnie_Current.shp',layer = 'abs_LakeAnnie_Current')
mendota = readOGR('Mendota/LakeMendota.shp','LakeMendota')
harp = readOGR('HarpLake/HarpLake.shp','HarpLake')
toolik = readOGR('ToolikLake/ToolikLake_NAD83.shp','ToolikLake_NAD83')
lang = readOGR('Langtjern/LangtjernLake.shp','LangtjernLake')
trout = readOGR('TroutLake/TroutLake/TroutLake_Vilas_WI_VIII.csv.shp','TroutLake_Vilas_WI_VIII.csv')
vanern = readOGR('VanernLake/Vanern_Tärnan_Sweden.csv.shp','Vanern_Tärnan_Sweden.csv')

harp = spTransform(harp,crs(mendota))
toolik = spTransform(toolik,crs(mendota))
annie = spTransform(annie,crs(mendota))
lang = spTransform(lang,crs(mendota))
trout = spTransform(trout,crs(mendota))
vanern = spTransform(vanern,crs(mendota))

centerShp <- function(shape,add) {
  a = gCentroid(shape)@coords
  b = elide(shape,shift = (-a+add))
  return(b)
}

mendota2 = centerShp(mendota,c(0,0))
trout2 = centerShp(trout,c(0.09,0))
toolik2 = centerShp(toolik,c(-0.01,-0.05))
harp2 = centerShp(harp,c(0.03,-0.05))
lang2 = centerShp(lang,c(0.06,-0.05))
annie2 = centerShp(annie,c(0.09,-0.05))

vanern2 = centerShp(vanern,c(0,0))

plot(vanern2,lwd=2,axes=T)

pdf('SOSlakeshapes.pdf',width = 6,height = 4)
  cols = 'lightsteelblue3'
  par(mar=c(0,0,0,0))
  plot(mendota2,lwd=1,axes=F,xlim=c(-0.09,0.14),ylim=c(-0.07,0.05),col=cols)
  plot(trout2,add=T,lwd=1,col=cols)
  plot(toolik2,add=T,lwd=1,col=cols)
  plot(harp2,add=T,lwd=1,col=cols)
  plot(lang2,add=T,lwd=1,col=cols)
  plot(annie2,add=T,lwd=1,col=cols)
dev.off()


