setwd("~/Dropbox/PhD_thesis/PhD_thesis/macrodistribution/")

ishodnik<-read.table("macrodistribution.csv", header=T, sep=";", dec=",")
str(ishodnik)

(lat_lim<-c(min(ishodnik$Ndec)-1, max(ishodnik$Ndec)+1))
(long_lim<-c(min(ishodnik$Edec)-1, max(ishodnik$Edec)+1))
#считаем какой должен быть радиус у кружков на карте - площадь пропорциональна
radius_mean <- sqrt( ishodnik$Nmean/ pi)

radius_max <- sqrt( ishodnik$Nmax/ pi)

#install.packages("maps")
#install.packages("mapdata")
library(maps)
library(mapdata)

pdf("Nmean_1.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="red", pch=21, cex=radius_mean/5)
legend(x="bottomright", col="red", pch=20, legend=c("1000 indd./sq.meter", "100 indd./sq.meter", "10 indd./sq.meter"), pt.cex=(sqrt(c(1000,100,10)/pi)/5), bg = "white", title = "Circle squares are proportional to Macoma balthica abundance")
dev.off()
embedFonts("Nmean_1.pdf")

pdf("Nmax_1.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="red", pch=21, cex=radius_max/5)
legend(x="bottomright", col="red", pch=20, legend=c("1000 indd./sq.meter", "100 indd./sq.meter", "10 indd./sq.meter"), pt.cex=(sqrt(c(1000,100,10)/pi)/5), bg = "white", title = "Circle squares are proportional to Macoma balthica abundance")
dev.off()
embedFonts("Nmax_1.pdf")


## попробуемкарту из shape файла
#install.packages("maptools")
#install.packages("scales")

library(maptools)  #for shapefiles
library(scales)  #for transparency
seas <- readShapeSpatial("./seas_marineregions_org/World_Seas.shp", proj4string=CRS("+proj=longlat"))   #layer of data for species range
radius <- sqrt( ishodnik$Nmean/ pi)

pdf("white_barents_Nmean.pdf", family="NimbusSan", width=190, height=280, paper="a4")
#plot(seas, xlim=c(30,44), ylim=c(65,71), lwd=2) 
plot(seas, xlim=long_lim, ylim=lat_lim, lwd=2) 

points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="red", pch=21, cex=radius/5)
legend(x="bottomright", col="red", pch=20, legend=c("1000 indd./sq.meter", "100 indd./sq.meter", "10 indd./sq.meter"), pt.cex=(sqrt(c(1000,100,10)/pi)/5), bg = "white", title = "Circle squares are proportional to Macoma balthica abundance")
dev.off()
embedFonts("white_barents_Nmean.pdf")

locator()
06	samps <- read.csv("FieldSamples.csv")   #my data for sampling sites, contains a column of "lat" and a column of "lon" with GPS points in decimal degrees
07	map("worldHires","Canada", xlim=c(-140,-110),ylim=c(48,64), col="gray90", fill=TRUE)  #plot the region of Canada I want
08	map("worldHires","usa", xlim=c(-140,-110),ylim=c(48,64), col="gray95", fill=TRUE, add=TRUE)  #add the adjacent parts of the US; can't forget my homeland
09	plot(pcontorta, add=TRUE, xlim=c(-140,-110),ylim=c(48,64), col=alpha("darkgreen", 0.6), border=FALSE)  #plot the species range
10	points(samps$lon, samps$lat, pch=19, col="red", cex=0.5)  #plot my sample sites