setwd("~/Dropbox/PhD_thesis/PhD_thesis/All_B/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

ishodnik<-read.table(file="All_B_mean_Macoma.csv", sep=";", dec=",", head=T)
str(ishodnik)


ishodnik$area<-ordered(ishodnik$area, levels=c("Suhaya", "Klushiha", "Podpahta","Lisya",
                                               "YuG", "ZRS", "Lomnishniy", 
                                               "Goreliy", "Estuary", "razrez2", 
                                               "Abram", "Pala", 
                                               "Gavrilovo", "Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Ivanovskaya"))
ishodnik$region<-ordered(ishodnik$region, levels=c("Chupa", "North_archipelago", "Luvenga", 
                                                   "West_Murman", "Kola_bay", "East_Murman"))
ishodnik$sea<-ordered(ishodnik$sea, levels=c("White", "Barents"))
ishodnik$tidal_level<-ordered(ishodnik$tidal_level, levels=c("high", "high_beatch", "middle", "fucus_zone", "zostera_zone", 
                                                             "low", "low_beatch", "hydrographical_datum", "subtidal"))
ishodnik$mareography<-ordered(ishodnik$mareography, levels=c("high","middle", "low", "hydrographical_datum", "subtidal"))

str(ishodnik)

#===== мин и макс для морей ================================================
# Белое

ishodnik$D_B<-ishodnik$SEM_B/ishodnik$B_mean.g*100

subset(ishodnik, ishodnik$B_mean.g == min(ishodnik$B_mean.g[ ishodnik$sea == "White"]))

subset(ishodnik, ishodnik$B_mean.g == max(ishodnik$B_mean.g[ ishodnik$sea == "White"]))

# Баренцево
subset(ishodnik, ishodnik$B_mean.g == min(ishodnik$B_mean.g[ ishodnik$sea == "Barents"]))

subset(ishodnik, ishodnik$B_mean.g == max(ishodnik$B_mean.g[ ishodnik$sea == "Barents"]))



# =========== summary по участкам для макрораспределения ========================

summ_area<-tapply(ishodnik$B_mean.g, ishodnik$area, summary, na.rm=T)
#соберем в табличку
area_summary_df<-data.frame(area=levels(ishodnik$area), Bmin=numeric(length=length(levels(ishodnik$area))), Bmax=numeric(length=length(levels(ishodnik$area))), Bmean=numeric(length=length(levels(ishodnik$area))))

(summ_area[[1]])

for (i in 1:length(summ_area)) {
  area_summary_df$Bmin[i]<-summ_area[[i]]["Min."]
  area_summary_df$Bmax[i]<-summ_area[[i]]["Max."]
  area_summary_df$Bmean[i]<-summ_area[[i]]["Mean"]
}
write.table(area_summary_df, "area_summary.csv", sep=";", dec=",")

#минимумы-максимумы
tapply(X=ishodnik$B_mean.g[ishodnik$sea=="White",drop=T], INDEX=ishodnik$region[ishodnik$sea=="White",drop=T], summary)
hist(ishodnik$B_mean.g[ishodnik$region=="Chupa_bay"], breaks=seq(0,9000, 500))
hist(ishodnik$B_mean.g[ishodnik$region=="Luvenga"], breaks=seq(0,9000, 500))
hist(ishodnik$B_mean.g[ishodnik$region=="North_archipelago"], breaks=seq(0,9000, 500))

# ==== рисуем частотное распределение средних численностей, чтобы понять, какие они. =====
hist(ishodnik$B_mean.g[ishodnik$sea=="White"], breaks=seq(0,9000, 500))
hist(ishodnik$B_mean.g[ishodnik$sea=="Barents"], breaks=seq(0,9000, 500))


pdf(file="Bmean_hist_White.pdf", family="NimbusSan")
hist(ishodnik$B_mean.g[ishodnik$sea=="White"], breaks=seq(0,220,5), main="Белое море", xlab="",ylab="")
dev.off()
embedFonts("Bmean_hist_White.pdf") #встройка шрифтов в файл

summary(ishodnik$B_mean.g[ishodnik$sea=="Barents"])

pdf(file="Bmean_hist_Barents.pdf", family="NimbusSan")
hist(ishodnik$B_mean.g[ishodnik$sea=="Barents"], breaks=seq(0,220,5), main="Баренцево море", xlab="",ylab="")
dev.off()
embedFonts("Bmean_hist_Barents.pdf") #встройка шрифтов в файл

## ===== сводка по численностям Белое ==========
summary(ishodnik$B_mean.g[ishodnik$sea=="White"])

pdf("B_White_uchastki.pdf")
boxplot(ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$area[ishodnik$sea=="White", drop=T])
dev.off()
embedFonts("B_White_uchastki.pdf")


Kanda <- subset(ishodnik, ishodnik$sea=="White" & ishodnik$area!="Suhaya" & ishodnik$area!="Klushiha" & ishodnik$area!="Podpahta" & ishodnik$area!="Lisya", drop=T)

pdf("B_Kanda.pdf")
boxplot(Kanda$B_mean.g ~ Kanda$area[Kanda$sea=="White",  drop=T])
dev.off()
embedFonts("B_Kanda.pdf")

#мин и макс - не на одном ли участке? нет
subset(ishodnik, ishodnik$sea=="White" & ishodnik$B_mean.g==13)
subset(ishodnik, ishodnik$sea=="White" & ishodnik$B_mean.g==8530)

subset(ishodnik, ishodnik$sea=="White" & ishodnik$B_mean.g>2800)
length(ishodnik$B_mean.g[ishodnik$sea=="White"])

#по регионам
pdf(file="Bmean_region_White.pdf", family="NimbusSan")
boxplot(ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$region[ishodnik$sea=="White", drop=T])
#подпишем на график средние
#for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
#  text(x=seq(1:12)[i], y=3900,
#labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
#                                     INDEX=ishodnik$region[ishodnik$sea=="Barents",drop=T], FUN=mean)))[i])
#}
dev.off()
embedFonts("Bmean_region_White.pdf") #встройка шрифтов в файл


kruskal.test(ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$region[ishodnik$sea=="White", drop=T])

#по участкам
boxplot(ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$area[ishodnik$sea=="White", drop=T])
anova(lm((ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$area[ishodnik$sea=="White", drop=T])))

# по горизонтам у нас что?
summary(ishodnik$B_mean.g[ishodnik$sea=="White"])

str(ishodnik)
boxplot(ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$mareography[ishodnik$sea=="White"])

#смотрим не различаются ли средние/медианы между горизонтами, но они скорее не отличаются, если в среднем по больнице...
kruskal.test(ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$mareography[ishodnik$sea=="White"])
anova(lm(ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$mareography[ishodnik$sea=="White"]))
TukeyHSD(aov(lm(ishodnik$B_mean.g[ishodnik$sea=="White"] ~ ishodnik$mareography[ishodnik$sea=="White"])))

#по участкам отдельно горизонты
boxplot(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="middle"] ~ 
          ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="middle", drop=T])
anova(lm(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="middle"] ~ 
           ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="middle", drop=T]))
kruskal.test((ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="middle"] ~ 
                ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="middle", drop=T]))

boxplot(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="low"] ~ 
          ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="low", drop=T])
anova(lm(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="low"] ~ 
           ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="low", drop=T]))
kruskal.test(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="low"] ~ 
               ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="low", drop=T])

boxplot(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum"] ~ 
          ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum", drop=T])
anova(lm(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum"] ~ 
           ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum", drop=T]))
kruskal.test(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum"] ~ 
               ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="hydrographic_datum", drop=T])

boxplot(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="subtidal"] ~ 
          ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="subtidal", drop=T])
kruskal.test(ishodnik$B_mean.g[ishodnik$sea=="White" & ishodnik$mareography=="subtidal"] ~ 
               ishodnik$area[ishodnik$sea=="White" & ishodnik$mareography=="subtidal", drop=T])


# ======== сводка по численностям Баренцево ================================
subset(ishodnik, ishodnik$sea == "Barents")

summary(ishodnik$B_mean.g[ishodnik$sea=="Barents"])

pdf("B_Barents_uchastki.pdf")
boxplot(ishodnik$B_mean.g[ishodnik$sea=="Barents"] ~ ishodnik$area[ishodnik$sea=="Barents",drop=T])
dev.off()
embedFonts("B_Barents_uchastki.pdf")

pdf(file="Bmean_region_Barents.pdf", family="NimbusSan")
boxplot(ishodnik$B_mean.g[ishodnik$sea=="Barents"] ~ ishodnik$region[ishodnik$sea=="Barents", drop=T])
#подпишем на график средние
#for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
#  text(x=seq(1:12)[i], y=3900,
#labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
#                                     INDEX=ishodnik$region[ishodnik$sea=="Barents",drop=T], FUN=mean)))[i])
#}
dev.off()
embedFonts("Bmean_region_Barents.pdf") #встройка шрифтов в файл

kruskal.test(ishodnik$B_mean.g[ishodnik$sea=="Barents"] ~ ishodnik$region[ishodnik$sea=="Barents", drop=T])

# по горизонтам у нас что?
summary(ishodnik$B_mean.g[ishodnik$sea=="Barents"])

str(ishodnik)
boxplot(ishodnik$B_mean.g[ishodnik$sea=="Barents"] ~ ishodnik$mareography[ishodnik$sea=="Barents"])

#смотрим не различаются ли средние/медианы между горизонтами, но они скорее не отличаются, если в среднем по больнице...
kruskal.test(ishodnik$B_mean.g[ishodnik$sea=="Barents"] ~ ishodnik$mareography[ishodnik$sea=="Barents"])
anova(lm(ishodnik$B_mean.g[ishodnik$sea=="Barents"] ~ ishodnik$mareography[ishodnik$sea=="Barents"]))
TukeyHSD(aov(lm(ishodnik$B_mean.g[ishodnik$sea=="Barents"] ~ ishodnik$mareography[ishodnik$sea=="Barents"])))

#по участкам отдельно горизонты
boxplot(ishodnik$B_mean.g[ishodnik$sea=="Barents" & ishodnik$mareography=="middle"] ~ 
          ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="middle", drop=T])
anova(lm(ishodnik$B_mean.g[ishodnik$sea=="Barents" & ishodnik$mareography=="middle"] ~ 
           ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="middle", drop=T]))
kruskal.test((ishodnik$B_mean.g[ishodnik$sea=="Barents" & ishodnik$mareography=="middle"] ~ 
                ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="middle", drop=T]))

boxplot(ishodnik$B_mean.g[ishodnik$sea=="Barents" & ishodnik$mareography=="low"] ~ 
          ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="low", drop=T])
anova(lm(ishodnik$B_mean.g[ishodnik$sea=="Barents" & ishodnik$mareography=="low"] ~ 
           ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="low", drop=T]))
kruskal.test(ishodnik$B_mean.g[ishodnik$sea=="Barents" & ishodnik$mareography=="low"] ~ 
               ishodnik$area[ishodnik$sea=="Barents" & ishodnik$mareography=="low", drop=T])