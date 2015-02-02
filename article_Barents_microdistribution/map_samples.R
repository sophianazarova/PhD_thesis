setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/article_Barents_microdistribution/")


site_coord <- read.csv2("sites_coord.csv", header=T)
## ggmap
#####
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggmap")
#require(ggmap)
library(ggmap)

##Areal
areal_bord<-c(top=70, left=-13.587220, bottom=25, right=69.996757)
areal_stamen <- get_map(location = areal_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 4)

#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_areal <- attr(areal_stamen, "bb")    # save attributes from original

# change color in raster
areal_stamen[areal_stamen == "#000000"] <- "white" #черный на белый
areal_stamen[areal_stamen == "#FFFFFF"] <- "gray" #белый на серый

# correct class, attributes
class(areal_stamen) <- c("ggmap", "raster")
attr(areal_stamen, "bb") <- attr_areal

pdf("map_areals.pdf", family="NimbusSan")
ggmap(areal_stamen)
dev.off()
embedFonts("map_areals.pdf")

#Ареал лучше через maps
library(maps)
library(mapdata)



lat_lim<-c(25, 72)
 long_lim<-c(-13.587220, 69.996757)

pdf("map_areal_unlab.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", xlim=long_lim, ylim=lat_lim, col="gray90", fill=TRUE)
dev.off()
embedFonts("map_areal_unlab.pdf")


##Barents sea
BS_bord<-c(top=70.041678, left=31.157318, bottom=67.8, right=40.891692)
BS_stamen <- get_map(location = BS_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 7)

#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_BS <- attr(BS_stamen, "bb")    # save attributes from original

# change color in raster
BS_stamen[BS_stamen == "#000000"] <- "white" #черный на белый
BS_stamen[BS_stamen == "#FFFFFF"] <- "gray" #белый на серый


# correct class, attributes
class(BS_stamen) <- c("ggmap", "raster")
attr(BS_stamen, "bb") <- attr_BS

pdf("map_Barents_Sea2.pdf", family="NimbusSan")
ggmap(BS_stamen) + geom_point(data=site_coord, aes(x = long, y = lat, size=1)) + guides(size=FALSE) + geom_text(data=site_coord, aes(label=site_coord$code, x = long, y = lat, hjust=-1, vjust=-1), size=5, position="jitter") + theme_gray()
embedFonts("map_Barents_Sea2.pdf")


#без подписей
pdf("map_Barents_Sea_unlab.pdf", family="NimbusSan")
ggmap(BS_stamen) + geom_point(data=site_coord, aes(x = long, y = lat, size=1)) + guides(size=FALSE)
dev.off()
embedFonts("map_Barents_Sea_unlab.pdf")


## Kola bay
 
Kb_bord<-c(top=69.511924, left=32.591034, bottom=68.9, right=33.739105)
Kb_stamen <- get_map(location = Kb_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 10)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_Kb <- attr(Kb_stamen, "bb")    # save attributes from original

# change color in raster
Kb_stamen[Kb_stamen == "#000000"] <- "white" #черный на белый
Kb_stamen[Kb_stamen == "#FFFFFF"] <- "gray" #белый на серый

# correct class, attributes
class(Kb_stamen) <- c("ggmap", "raster")
attr(Kb_stamen, "bb") <- attr_Kb

pdf("map_Kola_bay.pdf", family="NimbusSan")
ggmap(Kb_stamen) + geom_point(data=site_coord, aes(x = long, y = lat, size=1)) + geom_text(data=site_coord, aes(label=site_coord$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_Kola_bay.pdf")


## Dalnie Zelency

DZ_bord<-c(top=69.225470, left=35.694671, bottom=68.951068, right=36.592981)
DZ_stamen <- get_map(location = DZ_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 10)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_DZ <- attr(DZ_stamen, "bb")    # save attributes from original

# change color in raster
DZ_stamen[DZ_stamen == "#000000"] <- "white" #черный на белый
DZ_stamen[DZ_stamen == "#FFFFFF"] <- "gray" #белый на серый

# correct class, attributes
class(DZ_stamen) <- c("ggmap", "raster")
attr(DZ_stamen, "bb") <- attr_DZ

pdf("map_DZ.pdf", family="NimbusSan")
ggmap(DZ_stamen) + geom_point(data=site_coord, aes(x = long, y = lat, size=1)) + geom_text(data=site_coord, aes(label=site_coord$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_DZ.pdf")

