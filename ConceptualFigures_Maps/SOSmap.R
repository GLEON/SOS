library(maps)
library(mapdata)
library(mapproj)

setwd("~/Documents/Rpackages/SOS/R/ResultsViz")

#Members
#annie = c(27.990419, -81.607692)
harp = c(45.378941, -79.136019)
mendota = c(43.110658, -89.423043)
toolik = c(68.632333, -149.610966)
trout = c(46.030534, -89.679898)
vanern = c(58.952994, 13.512845)

lakes = rbind(harp,mendota,toolik,trout,vanern)#,annie)

cols1 = rgb(169,16,16,200,max=255)
cols2 = rgb(51,53,94,200,max=255)

pdf('SOSlakes.pdf',width=6,height=4)
par(mar = c(0,0,0,0),mgp=c(0,0,0))

map('world', proj='gall',par = 0,col = 'grey90', fill = T, border = 'grey90',
    bg = 'white',ylim = c(-55, 90),orient=c(90,0,0),resolution=0)
map('lakes',proj='gall',par = 0,add=T,border= 'grey50',fill=T,lwd=0.5,col='cadetblue3',lwd=0.3,bg='transparent',orient=c(90,0,0))
map('world',proj='gall',par = 0, interior = F,add=T,ylim = c(-55, 90),orient=c(90,0,0),
    col='grey70',lwd=0.5,resolution=0)

#map('rivers',projection='gall',par=0,col='grey80',add=TRUE,orient=c(90,0,0))
points(mapproject(lakes[,2],lakes[,1]),bg=cols1,col='black', pch=21,cex=1.2) 

dev.off()


pdf('SOSlakes2.pdf',width=6,height=4)
par(mar = c(0,0,0,0),mgp=c(0,0,0))

map('world', proj='gall',par = 0,col = 'grey90', fill = T, border = 'grey90',
    bg = 'white',ylim = c(40, 80),orient=c(90,0,0),resolution=0,xlim=c(-140,20))
map('world', proj='gall',par = 0,col = 'grey90', fill = T, border = 'grey90',
    bg = 'white',orient=c(90,0,0),resolution=0,add=T)
map('lakes',proj='gall',par = 0,add=T,border= 'grey50',fill=T,lwd=0.5,col='cadetblue3',lwd=0.3,bg='transparent',orient=c(90,0,0))
map('world',proj='gall',par = 0, interior = F,add=T,ylim = c(-55, 90),orient=c(90,0,0),
    col='grey70',lwd=0.5,resolution=0)

#map('rivers',projection='gall',par=0,col='grey80',add=TRUE,orient=c(90,0,0))
points(mapproject(lakes[,2],lakes[,1]),bg=cols1,col='black', pch=21,cex=1.2) 

dev.off()



