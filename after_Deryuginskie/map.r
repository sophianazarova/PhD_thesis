setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/after_Deryuginskie")


st_coord <- read.csv2("station_coordinates.csv", header=T)
## ggmap
#####
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ggmap")
require(ggmap)


##Дальние Зеленцы
DZ_google <- get_map(location = c(36.083723, 69.118148),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 14)

##Дальний пляж со станциями
DPL_google <- get_map(location = c(36.099988, 69.110957),                      
                     color = "color",
                     source = "google",
                     maptype = "satellite",
                     zoom = 16)
DPL_ggmap<-ggmap(DPL_google,
                extent = "device",
                ylab = "Latitude",
                xlab = "Longitude")

str(st_coord)

DPL_ggmap + geom_point(data=st_coord, aes(x = Long, y = Lat, size=5)) + geom_text(data=st_coord, aes(label=as.character(seq(1:9)), x = Long, y = Lat, hjust=-0.7, vjust=0)) + guides(size=FALSE)

ggsave(filename = "map_station.pdf")
