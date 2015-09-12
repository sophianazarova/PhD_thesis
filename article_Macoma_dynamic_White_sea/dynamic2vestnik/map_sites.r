setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/article_Macoma_dynamic_White_sea/dynamic2vestnik/")


site_coord <- read.csv2("sites_coord.csv", header=T)
White_sites <- subset(site_coord, sea=="White")
Luvenga <- White_sites[8:10,]
## ggmap
#####
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggmap")
#require(ggmap)
library(ggmap)




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
ggmap(Kanda_stamen) + geom_point(data=Luvenga, aes(x = long, y = lat, size=1), pch=21, col="black", fill="black") + geom_text(data=Luvenga, aes(label=Luvenga$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
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
ggmap(WS_stamen) # + geom_point(data=Luvenga, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + geom_text(data=Luvenga, aes(label=Luvenga$code, x = long, y = lat, hjust=-0.5, vjust=0), size=3, position="jitter") + guides(size=FALSE)
dev.off()
embedFonts("map_White_sea.pdf")

pdf("map_White_sea_unlab.pdf", family="NimbusSan")
ggmap(WS_stamen) + geom_point(data=Luvenga, aes(x = long, y = lat, size=1), pch=21, col="black", fill="red") + guides(size=FALSE)
dev.off()
embedFonts("map_White_sea_unlab.pdf")


# ========= Лувеньга на гуглоспутнике ===================

macoma_google <- get_map(location = c(32.699546, 67.091331),
                         color = "bw",
                         source = "google",
                         maptype = "satellite",
                         zoom = 13)
macoma_ggmap<-ggmap(macoma_google,
                    extent = "device",
                    ylab = "Latitude",
                    xlab = "Longitude")

png("Luvenga.png")
macoma_ggmap + geom_point(data=Luvenga, aes(x = long, y = lat, size=1), size=5, pch=21, col="black", fill="white") + guides(size=FALSE)
dev.off()


