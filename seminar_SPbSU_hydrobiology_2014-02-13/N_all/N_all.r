#setwd("~/Dropbox/PhD_thesis/PhD_thesis/seminar_SPbSU_hydrobiology_2014-02-13/N_all/")
setwd("~/Dropbox/PhD_thesis/PhD_thesis/All_N/")
#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="N_all_samples.csv", sep=";", dec=",", head=T)
ishodnik$area<-ordered(ishodnik$area, levels=c("YuG", "ZRS", "lomnishniy", "goreliy", "estuary", "razrez2", 
                                               "Ura", "Pechenga",
                                               "Abram", "Nagornoe", "Retinskoe", "Pala", 
                                               "Gavrilovo", "Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Ivanovskaya"))
ishodnik$region<-ordered(ishodnik$region, levels=c("North_archipelago", "Luvenga", "West_Murman", "Kola_bay", "East_Murman"))
ishodnik$sea<-ordered(ishodnik$sea, levels=c("White", "Barents"))


str(ishodnik)
summary(ishodnik$area)

pdf(file="N2_sea.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter ~ ishodnik$sea)
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$sea))){
  text(x=ishodnik$sea[ishodnik$sea==levels(ishodnik$sea)[i]][1], y=15000,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter, INDEX=ishodnik$sea, FUN=median)))[i])
}
dev.off()
embedFonts("N2_sea.pdf") #встройка шрифтов в файл

pdf(file="N2_region.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter ~ ishodnik$region)
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$region))){
  text(x=ishodnik$region[ishodnik$region==levels(ishodnik$region)[i]][1], y=15000,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter, INDEX=ishodnik$region, FUN=median)))[i])
}
dev.off()
embedFonts("N2_region.pdf") #встройка шрифтов в файл


pdf(file="N2_area.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter ~ ishodnik$area,  names=abbreviate(levels(ishodnik$area)), ylim=c(0,10000))
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$area))){
text(x=ishodnik$area[ishodnik$area==levels(ishodnik$area)[i]][1], y=10000,
     labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter, INDEX=ishodnik$area, FUN=median)))[i])
}
dev.off()
embedFonts("N2_area.pdf") #встройка шрифтов в файл


pdf(file="N2_area_White.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter[ishodnik$sea=="White",drop=T] ~ ishodnik$area[ishodnik$sea=="White",drop=T],  
        names=(levels(ishodnik$area[ishodnik$sea=="White",drop=T])), ylim=c(0,10000))
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$area))){
  text(x=ishodnik$area[ishodnik$area==levels(ishodnik$area[ishodnik$sea=="White",drop=T])[i]][1], y=10000,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="White",drop=T], 
                              INDEX=ishodnik$area[ishodnik$sea=="White",drop=T], FUN=median)))[i])
}
dev.off()
embedFonts("N2_area_White.pdf") #встройка шрифтов в файл

pdf(file="N2_area_Barents.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T] ~ abbreviate(ishodnik$area[ishodnik$sea=="Barents",drop=T]),  
        names=(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T])), ylim=c(0,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T])))
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
  text(x=seq(1:12)[i], y=3900,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
                                     INDEX=ishodnik$area[ishodnik$sea=="Barents",drop=T], FUN=median)))[i])
}
dev.off()
embedFonts("N2_area_Barents.pdf") #встройка шрифтов в файл

#районы Баренцева моря

pdf(file="N2_region_Barents.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents"] ~ ishodnik$region[ishodnik$sea=="Barents", drop=T])
##подпишем на график средние
#for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
#  text(x=seq(1:12)[i], y=3900,
       #labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
#                                     INDEX=ishodnik$region[ishodnik$sea=="Barents",drop=T], FUN=mean)))[i])
#}
dev.off()
embedFonts("N2_region_Barents.pdf") #встройка шрифтов в файл

#tapply(ishodnik$N2.indd.sqmeter, INDEX=ishodnik$area, FUN=median)


hist(ishodnik$N2.indd.sqmeter[ishodnik$sea=="White"], main="White Sea", xlab="N,экз./кв.м",
     breaks=c(0,50,100,500,1000,2000,5000,10000,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="White"])))

hist(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents"], main="Barents Sea", xlab="N,экз./кв.м",
     breaks=c(0,50,100,500,1000,2000,5000,10000,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents"])))


# между горизонтами внутри участка Белое 
str(ishodnik)

sink(file="Kruskal_White_area_mareography.txt")
for (i in c(1,2,3,4,8,10,13,16,17,18,19,20)){
  
  kruskal.test(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$area==paste(ishodnik$area[i])] ~ 
                 ishodnik$mareography[ishodnik$sea=="White" & ishodnik$area==paste(ishodnik$area[i]), drop=T])
  
}
