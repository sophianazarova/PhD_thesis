#install.packages("maps")
#install.packages("mapdata")
#install.packages("maptools")
#install.packages("ggplot2")

setwd("/home/sonya/Dropbox/PHD_material/seas")

##
library(maps)
library(mapdata)

map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)

pdf(file="White_Barents_seas_worldhires.pdf")
map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
dev.off()
embedFonts("White_Barents_seas_worldhires.pdf")

##
library(maptools)
library(ggplot2)

seas.shp<-readShapePoly("World_Seas.shp")
seas.map<-fortify(seas.shp)
str(seas.map)

ggplot(seas.map, aes(x = long, y = lat, group=group)) + geom_path()


##файл из Claus S., De Hauwere N., Vanhoorne B., Hernandez F., Mees J. (Flanders Marine Institute) (2013). Marineregions.org. Accessed at http://www.marineregions.org on 2013-11-22. 
setwd("/home/sonya/Dropbox/PHD_material/seas/seas_marineregions_org")
seas<-readShapePoly("World_Seas.shp", proj4string=CRS("+proj=longlat"))
plot(seas, xlim=c(30,44), ylim=c(63.8,70),lwd=2)

pdf(file="White_Barents_seas_plot_gis_marineregions.pdf")
plot(seas, xlim=c(30,44), ylim=c(63.8,70),lwd=2)
dev.off()
embedFonts("White_Barents_seas_plot_gis_marineregions.pdf")

#про восточный мурман этот сайт ничего не знает :(
plot(seas, xlim=c(35.776,36.395), ylim=c(69.023, 69.217),lwd=2)

## файл отсюда http://gis-lab.info/qa/osmshp.html
#рисует что-то не то по ощущению... очень подробно, но не море.
setwd("/home/sonya/Dropbox/PHD_material/seas/RU-MUR_gislab/waterline/")

murmansk.waterpoly<-readShapePoly("water-polygon.shp", proj4string=CRS("+proj=longlat"))
murmansk.waterline<-readShapeLines("water-line.shp", proj4string=CRS("+proj=longlat"))

plot(murmansk.waterline)

plot(murmansk.waterpoly, xlim=c(35.776,36.395), ylim=c(69.023, 69.217))

## файл отсюда http://download.geofabrik.de/europe.html
#
setwd("/home/sonya/Dropbox/PHD_material/seas/russia-european-part-latest.shp/waterways/")

european.russia<-readShapeLines("waterways.shp", proj4string=CRS("+proj=longlat"))
plot(european.russia, xlim=c(35.776,36.395), ylim=c(69.023, 69.217))