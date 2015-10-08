setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/maps/")


site_coord <- read.csv2("sites_coord.csv", header=T)
Barents_sites <- subset(site_coord, sea=="Barents")
White_sites <- subset(site_coord, sea=="White")
## ggmap
#####
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggmap")
#require(ggmap)
library(ggmap)

## =========Barents sea =====================
BS_bord<-c(top=70.041678, left=31.157318, bottom=67.8, right=40.891692)
BS_stamen <- get_map(location = BS_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 7)

#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_BS <- attr(BS_stamen, "bb")    # save attributes from original

# change color in raster
BS_stamen[BS_stamen == "#000000"] <- "#C0C0C0" #черный на серый
#BS_stamen[BS_stamen == "#FFFFFF"] <- "gray" #белый на серый


# correct class, attributes
class(BS_stamen) <- c("ggmap", "raster")
attr(BS_stamen, "bb") <- attr_BS

pdf("map_Barents_Sea.pdf", family="NimbusSan")
ggmap(BS_stamen) + geom_point(data=Barents_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + guides(size=FALSE) + geom_text(data=Barents_sites, aes(label=Barents_sites$code, x = long, y = lat, hjust=-1, vjust=-1), size=5, position="jitter") + theme_gray()
dev.off()
embedFonts("map_Barents_Sea.pdf")

#без подписей
pdf("map_Barents_Sea_unlab.pdf", family="NimbusSan")
ggmap(BS_stamen) + geom_point(data=Barents_sites, aes(x = long, y = lat, size=1)) + guides(size=FALSE)
dev.off()
embedFonts("map_Barents_Sea_unlab.pdf")


## ========== Kola bay ==========================

Kb_bord<-c(top=69.511924, left=32.591034, bottom=68.9, right=33.739105)
Kb_stamen <- get_map(location = Kb_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 10)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_Kb <- attr(Kb_stamen, "bb")    # save attributes from original

# change color in raster
Kb_stamen[Kb_stamen == "#000000"] <- "#C0C0C0" #черный на серый

# correct class, attributes
class(Kb_stamen) <- c("ggmap", "raster")
attr(Kb_stamen, "bb") <- attr_Kb

pdf("map_Kola_bay.pdf", family="NimbusSan")
ggmap(Kb_stamen) + geom_point(data=Barents_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + geom_text(data=Barents_sites, aes(label=Barents_sites$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_Kola_bay.pdf")


##  ============== Dalnie Zelency ======================

DZ_bord<-c(top=69.225470, left=35.694671, bottom=68.951068, right=36.592981)
DZ_stamen <- get_map(location = DZ_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 10)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_DZ <- attr(DZ_stamen, "bb")    # save attributes from original

# change color in raster
DZ_stamen[DZ_stamen == "#000000"] <- "#C0C0C0" #черный на белый


# correct class, attributes
class(DZ_stamen) <- c("ggmap", "raster")
attr(DZ_stamen, "bb") <- attr_DZ

pdf("map_DZ.pdf", family="NimbusSan")
ggmap(DZ_stamen) + geom_point(data=Barents_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + geom_text(data=Barents_sites, aes(label=Barents_sites$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_DZ.pdf")

## =============== Кандалакшский залив ==========================
Kandb_bord<-c(top=67.211843, left=31.528132, bottom=65.788521, right=37.032282)
Kandb_stamen <- get_map(location = Kandb_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 7)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_Kandb <- attr(Kandb_stamen, "bb")    # save attributes from original

# change color in raster
Kandb_stamen[Kandb_stamen == "#000000"] <- "#C0C0C0" #черный на белый


# correct class, attributes
class(Kandb_stamen) <- c("ggmap", "raster")
attr(Kandb_stamen, "bb") <- attr_Kandb

pdf("map_Kandalaksha_bay.pdf", family="NimbusSan")
ggmap(Kandb_stamen) + geom_point(data=White_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + geom_text(data=White_sites, aes(label=White_sites$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_Kandalaksha_bay.pdf")

# ============== Чупа =================
Ch_bord<-c(top=66.364598, left=33.517344, bottom=66.270549, right=33.977396)
Ch_stamen <- get_map(location = Ch_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 12)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_Ch <- attr(Ch_stamen, "bb")    # save attributes from original

# change color in raster
Ch_stamen[Ch_stamen == "#000000"] <- "#C0C0C0" #черный на белый


# correct class, attributes
class(Ch_stamen) <- c("ggmap", "raster")
attr(Ch_stamen, "bb") <- attr_WS

pdf("map_Chupa.pdf", family="NimbusSan")
ggmap(Ch_stamen) + geom_point(data=White_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + geom_text(data=White_sites, aes(label=White_sites$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_Chupa.pdf")


# ============== Чупа =================
Ch_bord<-c(top=66.364598, left=33.517344, bottom=66.270549, right=33.977396)
Ch_stamen <- get_map(location = Ch_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 12)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_Ch <- attr(Ch_stamen, "bb")    # save attributes from original

# change color in raster
Ch_stamen[Ch_stamen == "#000000"] <- "#C0C0C0" #черный на белый


# correct class, attributes
class(Ch_stamen) <- c("ggmap", "raster")
attr(Ch_stamen, "bb") <- attr_Ch

pdf("map_Chupa.pdf", family="NimbusSan")
ggmap(Ch_stamen) + geom_point(data=White_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + geom_text(data=White_sites, aes(label=White_sites$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_Chupa.pdf")



# ============== Кандалакша =================
Kanda_bord<-c(top=67.172727, left=32.123255, bottom=66.958136, right=32.874445)
Kanda_stamen <- get_map(location = Kanda_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 11)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_Kanda <- attr(Kanda_stamen, "bb")    # save attributes from original

# change color in raster
Kanda_stamen[Kanda_stamen == "#000000"] <- "#C0C0C0" #черный на белый


# correct class, attributes
class(Kanda_stamen) <- c("ggmap", "raster")
attr(Kanda_stamen, "bb") <- attr_Kanda

pdf("map_Kandalaksha.pdf", family="NimbusSan")
ggmap(Kanda_stamen) + geom_point(data=White_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + geom_text(data=White_sites, aes(label=White_sites$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_Kandalaksha.pdf")

## =============== White Sea ==========================
WS_bord<-c(top=68.703829, left=32.115901, bottom=63.708787, right=44.728204)
WS_stamen <- get_map(location = WS_bord,
                     color = "color",
                     source = "stamen",
                     maptype = "toner",
                     zoom = 7)
#меняем цвета на серые: stackoverflow.com/questions/18859809/how-do-you-replace-colors-in-a-ggmap-object
attr_WS <- attr(WS_stamen, "bb")    # save attributes from original

# change color in raster
WS_stamen[WS_stamen == "#000000"] <- "#C0C0C0" #черный на белый


# correct class, attributes
class(WS_stamen) <- c("ggmap", "raster")
attr(WS_stamen, "bb") <- attr_WS

pdf("map_White_sea.pdf", family="NimbusSan")
ggmap(WS_stamen) + geom_point(data=White_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + geom_text(data=White_sites, aes(label=White_sites$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_White_sea.pdf")

pdf("map_White_sea_unlab.pdf", family="NimbusSan")
ggmap(WS_stamen) + geom_point(data=White_sites, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + guides(size=FALSE)
dev.off()
embedFonts("map_White_sea_unlab.pdf")

# ========== бланковка для ареала ==============
library(maps)
library(mapdata)



pdf("world.pdf", family="NimbusSan", width=190, height=280, paper="a4")
map("worldHires", col="gray90", fill=TRUE)
dev.off()
embedFonts("world.pdf")
