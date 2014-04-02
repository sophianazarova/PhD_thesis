setwd("~/Dropbox/PhD_thesis/PhD_thesis/All_N/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

ishodnik<-read.table(file="all_Nmean.csv", sep=";", dec=",", head=T)
ishodnik$area<-ordered(ishodnik$area, levels=c("Suhaya", "Klyushiha", "Podpahta","Lisya",
                                               "YuG", "ZRS", "Lomnishniy", 
                                               "Goreliy", "Estuary", "razrez2", 
                                               "Ura", "Pechenga",
                                               "Abram", "Nagornoe", "Retinskoe", "Pala", 
                                               "Gavrilovo", "Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Ivanovskaya"))
ishodnik$region<-ordered(ishodnik$region, levels=c("Chupa_bay", "North_archipelago", "Luvenga", 
                                                   "West_Murman", "Kola_bay", "East_Murman"))
ishodnik$sea<-ordered(ishodnik$sea, levels=c("White", "Barents"))
ishodnik$tidal_level<-ordered(ishodnik$tidal_level, levels=c("high", "high_beatch", "middle", "fucus_zone", "zostera_zone", 
                                                             "low", "low_beatch", "hydrographic_datum", "subtidal"))
ishodnik$mareography<-ordered(ishodnik$mareography, levels=c("high","middle", "low", "hydrographic_datum", "subtidal"))

str(ishodnik)

#минимумы-максимумы
tapply(X=ishodnik$N.indd[ishodnik$sea=="White",drop=T], INDEX=ishodnik$region[ishodnik$sea=="White",drop=T], summary)
hist(ishodnik$N.indd[ishodnik$region=="Chupa_bay"], breaks=seq(0,9000, 500))
hist(ishodnik$N.indd[ishodnik$region=="Luvenga"], breaks=seq(0,9000, 500))
hist(ishodnik$N.indd[ishodnik$region=="North_archipelago"], breaks=seq(0,9000, 500))

## рисуем частотное распределение средних численностей, чтобы понять, какие они.
hist(ishodnik$N.indd[ishodnik$sea=="White"], breaks=seq(0,9000, 500))
hist(ishodnik$N.indd[ishodnik$sea=="Barents"], breaks=seq(0,9000, 500))

hist(ishodnik$N.indd[ishodnik$sea=="Barents" & ishodnik$N.indd<2000], breaks=seq(0,2000, 100))

pdf(file="Nmean_hist_White.pdf", family="NimbusSan")
hist(ishodnik$N.indd[ishodnik$sea=="White"], breaks=seq(0,9000, 100), main="Белое море", xlab="",ylab="")
dev.off()
embedFonts("Nmean_hist_White.pdf") #встройка шрифтов в файл

pdf(file="Nmean_hist_Barents.pdf", family="NimbusSan")
hist(ishodnik$N.indd[ishodnik$sea=="Barents"], breaks=seq(0,9000, 100), main="Баренцево море", xlab="",ylab="")
dev.off()
embedFonts("Nmean_hist_Barents.pdf") #встройка шрифтов в файл

## сводка по численностям
#Белое
summary(ishodnik$N.indd[ishodnik$sea=="White"])

boxplot(ishodnik$N.indd[ishodnik$sea=="White"] ~ ishodnik$area[ishodnik$sea=="White",drop=T])


#мин и макс - не на одном ли участке? нет
subset(ishodnik, ishodnik$sea=="White" & ishodnik$N.indd==13)
subset(ishodnik, ishodnik$sea=="White" & ishodnik$N.indd==8530)

subset(ishodnik, ishodnik$sea=="White" & ishodnik$N.indd>2800)
length(ishodnik$N.indd[ishodnik$sea=="White"])

#по регионам
pdf(file="Nmean_region_White.pdf", family="NimbusSan")
boxplot(ishodnik$N.indd[ishodnik$sea=="White"] ~ ishodnik$region[ishodnik$sea=="White", drop=T])
#подпишем на график средние
#for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
#  text(x=seq(1:12)[i], y=3900,
#labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
#                                     INDEX=ishodnik$region[ishodnik$sea=="Barents",drop=T], FUN=mean)))[i])
#}
dev.off()
embedFonts("Nmean_region_White.pdf") #встройка шрифтов в файл

#по участкам
boxplot(ishodnik$N.indd[ishodnik$sea=="White"] ~ ishodnik$area[ishodnik$sea=="White", drop=T])
anova(lm((ishodnik$N.indd[ishodnik$sea=="White"] ~ ishodnik$area[ishodnik$sea=="White", drop=T])))

# по горизонтам у нас что?
summary(ishodnik$N.indd[ishodnik$sea=="White"])

str(ishodnik)
boxplot(ishodnik$N.indd[ishodnik$sea=="White"] ~ ishodnik$mareography[ishodnik$sea=="White"])

#смотрим не различаются ли средние/медианы между горизонтами, но они скорее не отличаются, если в среднем по больнице...
kruskal.test(ishodnik$N.indd[ishodnik$sea=="White"] ~ ishodnik$mareography[ishodnik$sea=="White"])
anova(lm(ishodnik$N.indd[ishodnik$sea=="White"] ~ ishodnik$mareography[ishodnik$sea=="White"]))
TukeyHSD(aov(lm(ishodnik$N.indd[ishodnik$sea=="White"] ~ ishodnik$mareography[ishodnik$sea=="White"])))

#по участкам отдельно горизонты
boxplot(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="middle"] ~ 
          ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="middle", drop=T])
anova(lm(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="middle"] ~ 
           ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="middle", drop=T]))
kruskal.test((ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="middle"] ~ 
                ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="middle", drop=T]))

boxplot(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="low"] ~ 
          ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="low", drop=T])
anova(lm(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="low"] ~ 
           ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="low", drop=T]))
kruskal.test(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="low"] ~ 
               ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="low", drop=T])

boxplot(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum"] ~ 
          ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum", drop=T])
anova(lm(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum"] ~ 
           ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum", drop=T]))
kruskal.test(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum"] ~ 
               ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum", drop=T])

boxplot(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="subtidal"] ~ 
          ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="subtidal", drop=T])
kruskal.test(ishodnik$N.indd[ishodnik$sea=="White" & ishodnik$mareography=="subtidal"] ~ 
               ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="subtidal", drop=T])


##Баренцево
summary(ishodnik$N.indd[ishodnik$sea=="Barents"])

boxplot(ishodnik$N.indd[ishodnik$sea=="Barents"] ~ ishodnik$area[ishodnik$sea=="Barents",drop=T])

pdf(file="Nmean_region_Barents.pdf", family="NimbusSan")
boxplot(ishodnik$N.indd[ishodnik$sea=="Barents"] ~ ishodnik$region[ishodnik$sea=="Barents", drop=T])
#подпишем на график средние
#for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
#  text(x=seq(1:12)[i], y=3900,
#labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
#                                     INDEX=ishodnik$region[ishodnik$sea=="Barents",drop=T], FUN=mean)))[i])
#}
dev.off()
embedFonts("Nmean_region_Barents.pdf") #встройка шрифтов в файл

kruskal.test(ishodnik$N.indd[ishodnik$sea=="Barents"] ~ ishodnik$region[ishodnik$sea=="Barents", drop=T])

# по горизонтам у нас что?
summary(ishodnik$N.indd[ishodnik$sea=="Barents"])

str(ishodnik)
boxplot(ishodnik$N.indd[ishodnik$sea=="Barents"] ~ ishodnik$mareography[ishodnik$sea=="Barents"])

#смотрим не различаются ли средние/медианы между горизонтами, но они скорее не отличаются, если в среднем по больнице...
kruskal.test(ishodnik$N.indd[ishodnik$sea=="Barents"] ~ ishodnik$mareography[ishodnik$sea=="Barents"])
anova(lm(ishodnik$N.indd[ishodnik$sea=="Barents"] ~ ishodnik$mareography[ishodnik$sea=="Barents"]))
TukeyHSD(aov(lm(ishodnik$N.indd[ishodnik$sea=="Barents"] ~ ishodnik$mareography[ishodnik$sea=="Barents"])))

#по участкам отдельно горизонты
boxplot(ishodnik$N.indd[ishodnik$sea=="Barents" & ishodnik$mareography=="middle"] ~ 
          ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="middle", drop=T])
anova(lm(ishodnik$N.indd[ishodnik$sea=="Barents" & ishodnik$mareography=="middle"] ~ 
           ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="middle", drop=T]))
kruskal.test((ishodnik$N.indd[ishodnik$sea=="Barents" & ishodnik$mareography=="middle"] ~ 
                ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="middle", drop=T]))

boxplot(ishodnik$N.indd[ishodnik$sea=="Barents" & ishodnik$mareography=="low"] ~ 
          ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="low", drop=T])
anova(lm(ishodnik$N.indd[ishodnik$sea=="Barents" & ishodnik$mareography=="low"] ~ 
           ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="low", drop=T]))
kruskal.test(ishodnik$N.indd[ishodnik$sea=="Barents" & ishodnik$mareography=="low"] ~ 
               ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="low", drop=T])