setwd("~/Dropbox/PhD_thesis/PhD_thesis/size_structure_types")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## читаем файл с исходником.
ishodnik<-read.table(file="size_str_percent_All_df.csv", sep=";", dec=",", head=T)

# добавляем индекс
for (i in 1:length(ishodnik$percentage)){
  ishodnik$index[i]<-paste(abbreviate(ishodnik$area[i], minlength = 2), abbreviate(ishodnik$year[i], ), abbreviate(ishodnik$mareographic[i], minlength = 1),sep="_")  
}
ishodnik$index<-as.factor(ishodnik$index)

str(ishodnik)

# ========== все моря вместе ===============================
sizestr_matrx<-tapply(ishodnik$percentage, list(ishodnik$size_class, ishodnik$index), min)

str(sizestr_matrx)

sizestr_matrx[is.na(sizestr_matrx)]<-0

rowMeans(sizestr_matrx)

#делаем для матрицы аннотацию для раскрашивания картинок
sizestr_matrx_annotation<-data.frame(index=colnames(sizestr_matrx),sea=rep(NA, ncol(sizestr_matrx)),area=rep(NA, ncol(sizestr_matrx)), year=rep(NA, ncol(sizestr_matrx)), mareography=rep(NA, ncol(sizestr_matrx)))
for (i in 1:length(sizestr_matrx_annotation$index)){
  sizestr_matrx_annotation$sea[i]<-levels(ishodnik$sea)[ishodnik$sea[ishodnik$index==sizestr_matrx_annotation$index[i]][1]]
  
  sizestr_matrx_annotation$area[i]<-levels(ishodnik$area)[ishodnik$area[ishodnik$index==sizestr_matrx_annotation$index[i]][1]]
  
  sizestr_matrx_annotation$mareography[i]<-levels(ishodnik$mareographic)[ ishodnik$mareographic[ishodnik$index==sizestr_matrx_annotation$index[i]][1]]
  
  sizestr_matrx_annotation$year[i]<-ishodnik$year[ishodnik$index==sizestr_matrx_annotation$index[i]][1]
}
#сделаем факторы
sizestr_matrx_annotation$sea<-as.factor(sizestr_matrx_annotation$sea)
sizestr_matrx_annotation$area<-as.factor(sizestr_matrx_annotation$area)
sizestr_matrx_annotation$mareography<-ordered(sizestr_matrx_annotation$mareography, levels=c("hydrographical_datum", "low", "middle", "high"))

str(sizestr_matrx_annotation)


# ============= Все моря вместе - считаем non-metric MDS ============
#считаем расстояние. 
library(cluster)
library(vegan)

#Попробуем Эвклида
sizestr.dist <- vegdist(t(sizestr_matrx), "euclidean")

#Попробуем Bray-Cutris
sizestr.dist <- vegdist(t(sizestr_matrx), "bray")


library(MASS)
sizestr_nMDS<-isoMDS(sizestr.dist)


x <- sizestr_nMDS$points[,1]
y <- sizestr_nMDS$points[,2]

#рисуем картинку раскрашенную по морям
#Эвклид
pdf("Sizestr_sea_euclid.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", col=as.numeric(sizestr_matrx_annotation$sea), pch=15)
legend(legend=levels(sizestr_matrx_annotation$sea), x="bottomright", pch=15, col=seq(1:length(levels(sizestr_matrx_annotation$sea))))
dev.off()
embedFonts("Sizestr_sea_euclid.pdf")

# пытаемся считать ANOSIM
library(vegan)
(anosim_sea<-anosim(sizestr.dist, grouping = sizestr_matrx_annotation$sea, permutations = 999))

pdf("Anosim_sea_euclid.pdf", family="NimbusSan")
plot(anosim_sea)
dev.off()
embedFonts("Anosim_sea_euclid.pdf")

#Брей-Кертис
pdf("Sizestr_sea_bray.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", col=as.numeric(sizestr_matrx_annotation$sea), pch=15)
legend(legend=levels(sizestr_matrx_annotation$sea), x="topleft", pch=15, col=seq(1:length(levels(sizestr_matrx_annotation$sea))))
dev.off()
embedFonts("Sizestr_sea_bray.pdf")

# пытаемся считать ANOSIM
library(vegan)
(anosim_area<-anosim(sizestr.dist, grouping = White_sizestr_matrx_annotation$area, permutations = 999))

pdf("Anosim_sea_brey.pdf", family="NimbusSan")
plot(anosim_area)
dev.off()
embedFonts("Anosim_sea_brey.pdf")


# ====== Кластерный анализ ===========================
library(cluster)

#считаем расстояние. 
library(vegan)

#Попробуем Эвклида
sizestr.dist <- vegdist(t(sizestr_matrx), "euclidean")

#Попробуем Bray-Cutris
sizestr.dist <- vegdist(t(sizestr_matrx), "bray")

#делаем кластерный анализ методом Варда
sizestr.h <- hclust(sizestr.dist, method="ward.D")
plot(sizestr.h, labels=abbreviate(sizestr_matrx_annotation$sea, minlength = 1, strict = T), main="")



# ======= Белое море - готовим матрицу и аннотацию к ней ==========================
#делаем из датафрейма матрицу
White_sizestr_matrx<-tapply(ishodnik$percentage[ ishodnik$sea == "White", drop=T], list(ishodnik$size_class[ ishodnik$sea == "White", drop=T], ishodnik$index[ ishodnik$sea == "White", drop=T]), min)

str(White_sizestr_matrx)

#уберем все крупнее 12, т.к. их мало
White_sizestr_matrx_less_12<-rbind(White_sizestr_matrx[1:3,], White_sizestr_matrx[13:19,])

#делаем для матрицы аннотацию для раскрашивания картинок
White_sizestr_matrx_annotation<-data.frame(index=colnames(White_sizestr_matrx),area=rep(NA, ncol(White_sizestr_matrx)), year=rep(NA, ncol(White_sizestr_matrx)), mareography=rep(NA, ncol(White_sizestr_matrx)))
for (i in 1:length(White_sizestr_matrx_annotation$index)){
  White_sizestr_matrx_annotation$area[i]<-levels(ishodnik$area)[ishodnik$area[ishodnik$index==White_sizestr_matrx_annotation$index[i]][1], drop=T]
  
  White_sizestr_matrx_annotation$mareography[i]<-levels(ishodnik$mareographic)[ ishodnik$mareographic[ishodnik$index==White_sizestr_matrx_annotation$index[i]][1], drop=T]
  
  White_sizestr_matrx_annotation$year[i]<-ishodnik$year[ishodnik$index==White_sizestr_matrx_annotation$index[i], drop=T][1]
}
#сделаем факторы
White_sizestr_matrx_annotation$area<-as.factor(White_sizestr_matrx_annotation$area)
White_sizestr_matrx_annotation$mareography<-ordered(White_sizestr_matrx_annotation$mareography, levels=c("hydrographical_datum", "low", "middle", "high"))

str(White_sizestr_matrx_annotation)

#считаем расстояние. 
library(cluster)

#Эвклид
sizestr.dist <- dist(t(White_sizestr_matrx), "euclidean")
sizestr.l12.dist <- dist(t(White_sizestr_matrx_less_12), "euclidean")

#Попробуем Канберру
sizestr.dist <- dist(t(White_sizestr_matrx),"canberra")

#Попробуем Канберру
sizestr.dist <- vegdist(t(White_sizestr_matrx), "bray")

# ============= Белое море - считаем non-metric MDS ============
library(MASS)
sizestr_nMDS<-isoMDS(sizestr.dist)
sizestr_nMDS<-isoMDS(sizestr.l12.dist)

x <- sizestr_nMDS$points[,1]
y <- sizestr_nMDS$points[,2]

#рисуем картинку раскрашенную по участкам
#Эвклид
pdf("Sizestr_area_euclid.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", col=as.numeric(White_sizestr_matrx_annotation$area), pch=15)
legend(legend=levels(White_sizestr_matrx_annotation$area), x="topleft", pch=15, col=seq(1:length(levels(White_sizestr_matrx_annotation$area))))
dev.off()
embedFonts("Sizestr_area_euclid.pdf")

#Канберра
pdf("Sizestr_area_canberra.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", col=as.numeric(White_sizestr_matrx_annotation$area), pch=15)
legend(legend=levels(White_sizestr_matrx_annotation$area), x="topleft", pch=15, col=seq(1:length(levels(White_sizestr_matrx_annotation$area))))
dev.off()
embedFonts("Sizestr_area_canberra.pdf")

#Брей-Кертис
pdf("Sizestr_area_bray.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", col=as.numeric(White_sizestr_matrx_annotation$area), pch=15)
legend(legend=levels(White_sizestr_matrx_annotation$area), x="topleft", pch=15, col=seq(1:length(levels(White_sizestr_matrx_annotation$area))))
dev.off()
embedFonts("Sizestr_area_bray.pdf")

# пытаемся считать ANOSIM
library(vegan)
(anosim_area<-anosim(sizestr.dist, grouping = White_sizestr_matrx_annotation$area, permutations = 999))

pdf("Anosim_area_canberra.pdf", family="NimbusSan")
plot(anosim_area)
dev.off()
embedFonts("Anosim_area_canberra.pdf")

#рисуем картинку раскрашенную по годам
pdf("Sizestr_year_canberra.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", col=as.numeric(White_sizestr_matrx_annotation$year), pch=15)
legend(legend=levels(as.factor(White_sizestr_matrx_annotation$year)), x="topleft", pch=15, col=seq(1:length(levels(as.factor(White_sizestr_matrx_annotation$year)))))
dev.off()
embedFonts("Sizestr_year_canberra.pdf")

# пытаемся считать ANOSIM
anosim_year<-anosim(sizestr.dist, grouping = White_sizestr_matrx_annotation$year, permutations = 999)

pdf("Anosim_year_canberra.pdf", family="NimbusSan")
plot(anosim_year)
dev.off()
embedFonts("Anosim_year_canberra.pdf")


#рисуем картинку раскрашенную по мареографии
pdf("Sizestr_tidal_canberra.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", col=as.numeric(White_sizestr_matrx_annotation$mareography), pch=15)
legend(legend=levels(White_sizestr_matrx_annotation$mareography), x="topleft", pch=15, col=seq(1:length(levels(White_sizestr_matrx_annotation$mareography))))
dev.off()
embedFonts("Sizestr_tidal_canberra.pdf")

# пытаемся считать ANOSIM
anosim_tidal<-anosim(sizestr.dist, grouping = White_sizestr_matrx_annotation$mareography, permutations = 999)

pdf("Anosim_tidal_canberra.pdf", family="NimbusSan")
plot(anosim_area)
dev.off()
embedFonts("Anosim_tidal_canberra.pdf")

# ========= попытка про подбор оптимальной конфигурации  =======================
library(vegan)
sizestr_metaMDS<-metaMDS(t(White_sizestr_matrx), distance = "euclidean", k = 2)
ordiplot(sizestr_metaMDS)

# ===== Белое море --  Principal component analisys =============

White_sizestr_prcomp<-prcomp(t(White_sizestr_matrx_less_12))
plot(White_sizestr_prcomp)
summary(White_sizestr_prcomp)

White_sizestr_p<-predict(sizestr_prcomp)
plot(White_sizestr_p[,1:2],col=as.numeric(White_sizestr_matrx_annotation$area)+1, pch=16)

pdf("White_sizestr_PCA_biplot.pdf", family="NimbusSan")
biplot(White_sizestr_prcomp, col = c("gray80", "red"), xlabs = White_sizestr_matrx_annotation$year)
dev.off()
embedFonts("White_sizestr_PCA_biplot.pdf")

## ade4
install.packages("ade4")
library(ade4)

sizestr_dudi<-dudi.pca(t(White_sizestr_matrx_less_12), scannf=FALSE)
mycolors<-rainbow(length(levels(as.factor(White_sizestr_matrx_annotation$year))))


for (i in 1:21){
  White_sizestr_matrx_annotation$year_color[White_sizestr_matrx_annotation$year==levels(as.factor((White_sizestr_matrx_annotation$year)))[i]]<-mycolors[i]  
}


s.class(sizestr_dudi$l1, as.factor(White_sizestr_matrx_annotation$year), clabel = 0.5, col = White_sizestr_matrx_annotation$year_color)


##по Маринкиному мануалу vegan
library(vegan)
sizestr_pca<-rda(White_sizestr_matrx_less_12)

summary(sizestr_pca)

#собственные числа
eigenvals(sizestr_pca) #значения
screeplot(sizestr_pca, type="line") # график
abline(h=1)

#рисуем нагрузки

##не получается
library(ggplot2)

# Факторные нагрузки
p_loadings <- autoplot(sizestr_pca, type="var")
# другие компоненты
 autoplot(prot_pca, type = "var", PC = c(2, 3))
p_loadings

scores(sizestr_pca, display = "species", choices = c(1, 2, 3), scaling = 0)


# ======== Баренцево море - готовим матрицу и аннотацию к ней ======================
#делаем из датафрейма матрицу
Barents_sizestr_matrx<-tapply(ishodnik$percentage[ ishodnik$sea == "Barents"], list(ishodnik$size_class[ ishodnik$sea == "Barents"], ishodnik$index[ ishodnik$sea == "Barents"]), min)


#делаем для матрицы аннотацию для раскрашивания картинок
Barents_sizestr_matrx_annotation<-data.frame(index=colnames(Barents_sizestr_matrx),area=rep(NA, ncol(Barents_sizestr_matrx)), year=rep(NA, ncol(Barents_sizestr_matrx)), mareography=rep(NA, ncol(Barents_sizestr_matrx)))
for (i in 1:length(Barents_sizestr_matrx_annotation$index)){
  Barents_sizestr_matrx_annotation$area[i]<-levels(ishodnik$area)[ishodnik$area[ishodnik$index==Barents_sizestr_matrx_annotation$index[i]][1]]
  
  Barents_sizestr_matrx_annotation$mareography[i]<-levels(ishodnik$mareographic)[ ishodnik$mareographic[ishodnik$index==Barents_sizestr_matrx_annotation$index[i]][1]]
  
  Barents_sizestr_matrx_annotation$year[i]<-ishodnik$year[ishodnik$index==Barents_sizestr_matrx_annotation$index[i]][1]
}
#сделаем факторы
Barents_sizestr_matrx_annotation$area<-as.factor(Barents_sizestr_matrx_annotation$area)
Barents_sizestr_matrx_annotation$mareography<-ordered(Barents_sizestr_matrx_annotation$mareography, levels=c("hydrographical_datum", "low", "middle", "high"))

str(Barents_sizestr_matrx_annotation)

Barents_sizestr_matrx<-tapply(ishodnik$percentage[ ishodnik$sea == "Barents", drop=T], list(ishodnik$size_class[ ishodnik$sea == "Barents", drop=T], ishodnik$index[ ishodnik$sea == "Barents", drop=T]), min)

Barents_sizestr_matrx[is.na(Barents_sizestr_matrx)]<-0


# ===== Баренцево море --  Principal component analisys =============

Barents_sizestr_prcomp<-prcomp((Barents_sizestr_matrx))
plot(Barents_sizestr_prcomp)
summary(Barents_sizestr_prcomp)

Barents_sizestr_p<-predict(Barents_sizestr_prcomp)
plot(Barents_sizestr_p[,1:2],col=as.numeric(Barents_sizestr_matrx_annotation$area)+1, pch=16)

pdf("Barents_sizestr_PCA_biplot.pdf", family="NimbusSan")
biplot(Barents_sizestr_prcomp, col = c("gray80", "red"), xlabs = Barents_sizestr_matrx_annotation$year)
dev.off()
embedFonts("Barents_sizestr_PCA_biplot.pdf")

#============== simprof Similiarity Profile Analysis ====================
#install.packages("clustsig")
library(clustsig)


#Баренцево море
Barents_sizestr_clust<-simprof(t(Barents_sizestr_matrx), num.expected = 999, num.simulated = 999, method.cluster = "average", method.distance = "braycurtis", alpha = 0.01)

pdf("Barents_sizestr_bray_average.pdf", family="NimbusSan")
simprof.plot(Barents_sizestr_clust)
dev.off()
embedFonts("Barents_sizestr_bray_average.pdf")

pdf("Barents_sizestr_bray_average_BW.pdf", family="NimbusSan")
simprof.plot(Barents_sizestr_clust, siglinetype = 5, leafcolors = rep("black", length(Barents_sizestr_clust$significantclusters)))
dev.off()
embedFonts("Barents_sizestr_bray_average_BW.pdf")

#Белое море
White_sizestr_clust<-simprof(t(White_sizestr_matrx), num.expected = 999, num.simulated = 999, method.cluster = "average", method.distance = "braycurtis")

pdf("White_sizestr_bray_average.pdf", family="NimbusSan")
simprof.plot(White_sizestr_clust)
dev.off()
embedFonts("White_sizestr_bray_average.pdf")

pdf("White_sizestr_bray_average_BW.pdf", family="NimbusSan")
simprof.plot(White_sizestr_clust, siglinetype = 5, leafcolors = rep("black", length(White_sizestr_clust$significantclusters)))
dev.off()
embedFonts("White_sizestr_bray_average_BW.pdf")



#все моря вместе
all_sizestr_clust<-simprof(t(sizestr_matrx), num.expected = 999, num.simulated = 999, method.cluster = "average", method.distance = "braycurtis")

pdf("allsea_sizestr_bray_average.pdf", family="NimbusSan")
simprof.plot(all_sizestr_clust)
dev.off()
embedFonts("allsea_sizestr_bray_average.pdf")

pdf("allsea_sizestr_bray_average_BW.pdf", family="NimbusSan")
simprof.plot(all_sizestr_clust, siglinetype = 5, leafcolors = rep("black", length(all_sizestr_clust$significantclusters)))
dev.off()
embedFonts("allsea_sizestr_bray_average_BW.pdf")