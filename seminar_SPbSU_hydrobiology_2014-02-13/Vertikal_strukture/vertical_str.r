setwd("~/Dropbox/PhD_thesis/PhD_thesis/seminar_SPbSU_hydrobiology_2014-02-13/Vertikal_strukture/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="vertikal.csv", sep=";", dec=",", head=T)
ishodnik$tidal_gradation<-ordered(ishodnik$tidal_gradation, levels=c("hydrographic_datum", "low", "middle", "high"))
ishodnik$area<-ordered(ishodnik$area, levels=c("goreliy", "razrez2", "Abram", "Pala", "Gavrilovo", "Yarnyshnaya", "DZ", "Shelpino", "Porchnikha"))
attach(ishodnik)
str(ishodnik)

levels(area)



pdf(file="all1.pdf", family="NimbusSan")
par(mfrow=c(3,5))
for( i in 1:length(levels(area)))
  {
  boxplot(N2.indd.sqmeter[area==(levels(area)[i])] ~ tidal_gradation[area==(levels(area)[i])], horizontal=T, 
          main=paste(levels(area)[i]))
  }
par(mfrow=c(1,1))
dev.off()
embedFonts(paste(levels(area)[i],sep="_","vertical.pdf")) #встройка шрифтов в файл


for( i in 1:length(levels(area)))
{
  pdf(file=paste(levels(area)[i],sep="_","vertical.pdf"), family="NimbusSan")
  boxplot(N2.indd.sqmeter[area==(levels(area)[i])] ~ tidal_gradation[area==(levels(area)[i])], horizontal=T, 
          main=paste(levels(area)[i]))
  dev.off()
  embedFonts(paste(levels(area)[i],sep="_","vertical.pdf")) #встройка шрифтов в файл
}

str(ishodnik)
mean.tidal<-tapply(X=ishodnik$N2.indd.sqmeter, INDEX=list(area, year, tidal_gradation), FUN=mean)
str(mean.tidal)

#проверяем идею проградиент в каждый конкретный момент времени... что ниже маком больше
hist(c(as.vector(mean.tidal[,,1]-mean.tidal[,,2]), 
       as.vector(mean.tidal[,,1]-mean.tidal[,,3]),
       as.vector(mean.tidal[,,1]-mean.tidal[,,4]),
       as.vector(mean.tidal[,,2]-mean.tidal[,,3]),
       as.vector(mean.tidal[,,2]-mean.tidal[,,4]),
       as.vector(mean.tidal[,,3]-mean.tidal[,,4])))



hist()


sign(apply(mean.tidal[,,2]-mean.tidal[,,1], MARGIN=1, FUN=prod, na.rm=T))
sign(apply(mean.tidal[,,3]-mean.tidal[,,1], MARGIN=1, FUN=prod, na.rm=T))
sign(apply(mean.tidal[,,3]-mean.tidal[,,2], MARGIN=1, FUN=prod, na.rm=T))
sign(apply(mean.tidal[,,4]-mean.tidal[,,1], MARGIN=1, FUN=prod, na.rm=T))
sign(apply(mean.tidal[,,4]-mean.tidal[,,2], MARGIN=1, FUN=prod, na.rm=T))
sign(apply(mean.tidal[,,4]-mean.tidal[,,3], MARGIN=1, FUN=prod, na.rm=T))
