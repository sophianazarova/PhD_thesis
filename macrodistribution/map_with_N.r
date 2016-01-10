setwd("~/Dropbox/PhD_thesis/PhD_thesis/macrodistribution/")

detach(ishodnik)

ishodnik<-read.table("macrodistribution.csv", header=T, sep=";", dec=",")
str(ishodnik)

(lat_lim<-c(min(ishodnik$Ndec, na.rm = T)-1, max(ishodnik$Ndec, na.rm = T)+1))
(long_lim<-c(min(ishodnik$Edec, na.rm = T)-1, max(ishodnik$Edec, na.rm = T)+1))

#подгонка границ карты
(long_lim<-c(-10.5, max(ishodnik$Edec, na.rm=T)+1))

#считаем какой должен быть радиус у кружков на карте - площадь пропорциональна
radius_Nmean <- sqrt( ishodnik$Nmean/ pi)
radius_Bmean <- sqrt( ishodnik$Bmean/ pi)

radius_max <- sqrt( ishodnik$Nmax/ pi)

#install.packages("maps")
#install.packages("mapdata")
library(maps)
library(mapdata)



pdf("Nmean_1.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="red", pch=21, cex=radius_Nmean/5)
legend(horiz = T, x="bottomright", col="red", pch=20, legend=c("2000","1000", "500", "100", "50", "10"), pt.cex=(sqrt(c(2000,1000,500,100,50,10)/pi)/5), bg = "white", title = "Circle squares are proportional to Macoma balthica abundance, indd./sq.meter")
dev.off()
embedFonts("Nmean_1.pdf")

pdf("Bmean_1.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="red", pch=21, cex=radius_Bmean/2)
legend(horiz = T, x="bottomright", col="red", pch=20, legend=c("300", "100", "50", "10", "5", "1"), pt.cex=(sqrt(c(300,100,50,10,5,1)/pi)/2), bg = "white", title = "Circle squares are proportional to Macoma balthica biomass, g/sq.meter")
dev.off()
embedFonts("Bmean_1.pdf")

pdf("Nmax_1.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="red", pch=21, cex=radius_max/5)
legend(x="bottomright", col="red", pch=20, legend=c("1000 indd./sq.meter", "100 indd./sq.meter", "10 indd./sq.meter"), pt.cex=(sqrt(c(1000,100,10)/pi)/5), bg = "white", title = "Circle squares are proportional to Macoma balthica abundance")
dev.off()
embedFonts("Nmax_1.pdf")

##пробую подбрать районы чтобы нагляднее было
str(ishodnik)
region_Nmean<-tapply(ishodnik$Nmean,ishodnik$region,mean)
region_long<-tapply(ishodnik$Edec,ishodnik$region,mean)
region_lat<-tapply(ishodnik$Ndec,ishodnik$region,mean)

radius_region_Nmean <- sqrt( region_Nmean/ pi)

pdf("Nmean_region.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=region_long, y=region_lat, col="black", bg="red", pch=21, cex=radius_region_Nmean/5)
legend(horiz = T, x="bottomright", col="red", pch=20, legend=c("2000","1000", "500", "100", "50", "10"), pt.cex=(sqrt(c(2000,1000,500,100,50,10)/pi)/5), bg = "white", title = "Circle squares are proportional to Macoma balthica abundance, indd./sq.meter")
dev.off()
embedFonts("Nmean_region.pdf")


region_Bmean<-tapply(ishodnik$Bmean,ishodnik$region,mean,na.rm=T)
radius_region_Bmean <- sqrt( region_Bmean/ pi)

pdf("Bmean_region.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=region_long, y=region_lat, col="black", bg="red", pch=21, cex=radius_region_Bmean/2)
legend(horiz = T, x="bottomright", col="red", pch=20, legend=c("300", "100", "50", "10", "5", "1"), pt.cex=(sqrt(c(300,100,50,10,5,1)/pi)/2), bg = "white", title = "Circle squares are proportional to Macoma balthica biomass, g/sq.meter")
dev.off()
embedFonts("Bmean_region.pdf")



## попробуем карту из shape файла
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

#####

#####
## карта европа + Белое
pdf("seas.pdf", family="NimbusSan", width=190, height=280, paper="a4")
#plot(seas, ylim=c(54.29, 70.70), xlim=c(7.55, 45.08), lwd=2) 
map("worldHires", ylim=c(54.29, 70.70), xlim=c(7.55, 45.08), col="gray90", fill=TRUE)
dev.off()
embedFonts("seas.pdf")
#####



## ggmap
#####
library(ggplot2)
#install.packages("ggmap")
require(ggmap)

##Баренцево море
macoma_google <- get_map(location = c(35.5, 68.547651),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 7)
macoma_ggmap<-ggmap(macoma_google,
      extent = "device",
      ylab = "Latitude",
      xlab = "Longitude")


macoma_ggmap + geom_point(aes(x = (ishodnik$Edec[ ishodnik$sea=="Barents"]), y = (ishodnik$Ndec[ ishodnik$sea=="Barents"]), size = (radius_Nmean[11:22]), color="red"))

##Белое море

macoma_google <- get_map(location = c(32.8, 66.713141),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 8)
macoma_ggmap<-ggmap(macoma_google,
                    extent = "device",
                    ylab = "Latitude",
                    xlab = "Longitude")


macoma_ggmap + geom_point(aes(x = (ishodnik$Edec[ ishodnik$sea=="White"]), y = (ishodnik$Ndec[ ishodnik$sea=="White"]), size = (radius_Nmean[c(1:10,23:29,40:42)]), color="red"))

##серая карта для статьи
pdf("Macoma_Nmean_article.pdf", family="NimbusSan", bg="white", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
  points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="gray30", pch=21, cex=radius_Nmean/5)
legend(horiz = T, x="bottomright", col="gray30", pch=20, legend=c("1000", "100", "10", "1"), pt.cex=(sqrt(c(1000,100,10,1)/pi)/5), bg = "white", title = "Macoma balthica abundance, indd./m-2", cex=1.9)
dev.off()
embedFonts("Macoma_Nmean_article.pdf")

#===== русскоязычные подписи ==========
pdf("Nmean_ru.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="red", pch=21, cex=radius_Nmean/5)
legend(horiz = T, x="bottomright", col="red", pch=20, legend=c("2000","1000", "500", "100", "50", "10"), pt.cex=(sqrt(c(2000,1000,500,100,50,10)/pi)/5), bg = "white", title = "N, экз./кв.м")
dev.off()
embedFonts("Nmean_ru.pdf")


pdf("Bmean_ru.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
#map("worldHires", xlim=c(30,44), ylim=c(63.8,70), col="gray90", fill=TRUE)
points(x=ishodnik$Edec, y=ishodnik$Ndec, col="black", bg="red", pch=21, cex=radius_Bmean/2)
legend(horiz = T, x="bottomright", col="red", pch=20, legend=c("300", "100", "50", "10", "5", "1"), pt.cex=(sqrt(c(300,100,50,10,5,1)/pi)/2), bg = "white", title = "B, г/кв.м")
dev.off()
embedFonts("Bmean_ru.pdf")
str(ishodnik)
# ========== корреляция широты и среднего обилия ==============
pdf("lat_vs_Nmean.pdf", family="NimbusSan")
plot(ishodnik$Ndec, ishodnik$Nmean, xlab = "широта", ylab = "N, экз./кв.м")
dev.off()
embedFonts("lat_vs_Nmean.pdf")

abline(lm(ishodnik$Nmean ~ ishodnik$Ndec))
summary((lm(ishodnik$Nmean ~ ishodnik$Ndec)))

pdf("lat_vs_Nmean_big.pdf", family="NimbusSan")
plot(ishodnik$Ndec, ishodnik$Nmean, xlab = "широта", ylab = "N, экз./кв.м", cex=2, pch=21, bg="blue", cex.lab=1.5, cex.axis=1.5)
abline(lm(ishodnik$Nmean~ishodnik$Ndec))
dev.off()
embedFonts("lat_vs_Nmean_big.pdf")

# корреляция
cor.test(ishodnik$Ndec, ishodnik$Nmean, method = "spearman")

boxplot(ishodnik$Nmean ~ cut(ishodnik$Ndec, breaks = seq(44,71, 5)))


plot(ishodnik$Ndec, ishodnik$Bmean, xlab = "широта", ylab = "B, г/кв.м", ylim=c(0,300))
cor.test(ishodnik$Ndec, ishodnik$Bmean, method = "spearman")


# ========== корреляция широты и биомассы ==============
pdf("lat_vs_Bmean_big.pdf", family="NimbusSan")
plot(ishodnik$Ndec, ishodnik$Bmean, xlab = "широта", ylab = "B, г/кв.м", cex=2, pch=21, bg="blue", cex.lab=1.5, cex.axis=1.5)
#abline(lm(ishodnik$Nmean~ishodnik$Ndec))
dev.off()
embedFonts("lat_vs_Bmean_big.pdf")

# корреляция
cor.test(ishodnik$Ndec, ishodnik$Bmean, method = "spearman")


#============== численность наше Белое море ==============
library(ggplot2)
library(ggmap)

ishodnik <- cbind(ishodnik, radius_Nmean, radius_max)
our <- subset(ishodnik, ishodnik$source=="authors data")

Kandb_bord<-c(top=67.163484, left=32.133979, bottom=66.260781, right=34.102189)
Kandb_stamen <- get_map(location = Kandb_bord,
                        color = "color",
                        source = "stamen",
                        maptype = "toner",
                        zoom = 9)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_Kandb <- attr(Kandb_stamen, "bb")    # save attributes from original

# change color in raster
Kandb_stamen[Kandb_stamen == "#000000"] <- "#C0C0C0" #черный на белый


# correct class, attributes
class(Kandb_stamen) <- c("ggmap", "raster")
attr(Kandb_stamen, "bb") <- attr_Kandb

pdf("map_Kandalaksha_monitorings.pdf", family="NimbusSan")
ggmap(Kandb_stamen) + geom_point(data= our, aes(x = Edec, y = Ndec, size=1), pch=21, col="black", fill="red", cex=our$radius_max/3) + geom_point(data= our, aes(x = Edec, y = Ndec, size=1), pch=21, col="black", fill="blue", cex=our$radius_Nmean/3)
  guides(size=FALSE)
dev.off()
embedFonts("map_Kandalaksha_monitorings.pdf")