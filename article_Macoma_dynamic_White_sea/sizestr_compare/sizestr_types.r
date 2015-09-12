setwd("~/Dropbox/PhD_thesis/PhD_thesis/article_Macoma_dynamic_White_sea/sizestr_compare/")

ishodnik <- read.csv2("Luvenga_sizestr_percent.csv")
str(ishodnik)

# добавляем индекс
ishodnik$abbr[ ishodnik$site == "Goreliy"] <- "Г"
ishodnik$abbr[ ishodnik$site == "Estuary"] <- "Э"
ishodnik$abbr[ ishodnik$site == "razrez2"] <- "Л"

ishodnik$index <- paste(ishodnik$abbr, ishodnik$year ,sep="_")  
ishodnik$index<-as.factor(ishodnik$index)

ishodnik$date <- paste(ishodnik$year, "-01-01", sep="")
ishodnik$date <- as.POSIXlt(ishodnik$date, format = "%Y-%m-%d", tz = "") 
ishodnik$yr <- format(ishodnik$date,"%y")
  
# делаем матрицу для анализа
sizestr_matrx<-tapply(ishodnik$N_percent, list(ishodnik$Length_int, ishodnik$index), min)
str(sizestr_matrx)
sizestr_matrx[is.na(sizestr_matrx)]<-0


#делаем для матрицы аннотацию для раскрашивания картинок
sizestr_matrx_annotation<-data.frame(index=colnames(sizestr_matrx),site=rep(NA, ncol(sizestr_matrx)), year=rep(NA, ncol(sizestr_matrx)), code = rep(NA, ncol(sizestr_matrx)))
for (i in 1:length(sizestr_matrx_annotation$index)){
  sizestr_matrx_annotation$site[i]<-levels(ishodnik$site)[ishodnik$site[ishodnik$index==sizestr_matrx_annotation$index[i]][1]]
  
    sizestr_matrx_annotation$year[i]<-ishodnik$year[ishodnik$index==sizestr_matrx_annotation$index[i]][1]
  
  sizestr_matrx_annotation$code[i] <- ishodnik$yr[ishodnik$index==sizestr_matrx_annotation$index[i]][1]
}
#сделаем факторы
sizestr_matrx_annotation$site<-as.factor(sizestr_matrx_annotation$site)

str(sizestr_matrx_annotation)

#============== simprof Similiarity Profile Analysis ====================
#install.packages("clustsig")
library(clustsig)


sizestr_clust<-simprof(t(sizestr_matrx), num.expected = 999, num.simulated = 999, method.cluster = "average", method.distance = "braycurtis", alpha = 0.01)

pdf("Luvenga_sizestr_bray_average_BW.pdf", family="NimbusSan")
simprof.plot(sizestr_clust, siglinetype = 5, leafcolors = rep("black", length(sizestr_clust$significantclusters)))
dev.off()
embedFonts("Luvenga_sizestr_bray_average_BW.pdf")

pdf("Luvenga_sizestr_bray_average.pdf", family="NimbusSan")
simprof.plot(sizestr_clust, leafcolors = as.numeric(sizestr_matrx_annotation$site))
dev.off()
embedFonts("Luvenga_sizestr_bray_average.pdf")


# ============= non-metric MDS ============
#считаем расстояние. 
library(cluster)
library(vegan)

# Брей-Кертис
sizestr.dist <- vegdist(t(sizestr_matrx), "bray")

library(MASS)
sizestr_nMDS<-isoMDS(sizestr.dist)
sizestr_nMDS<-isoMDS(sizestr.l12.dist)

x <- sizestr_nMDS$points[,1]
y <- sizestr_nMDS$points[,2]

#картинка раскрашенная по участкам
pdf("Sizestr_area_bray.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", pch = as.numeric(sizestr_matrx_annotation$site))
legend(legend=levels(sizestr_matrx_annotation$site), x="topleft", pch = seq(1:3))
dev.off()
embedFonts("Sizestr_area_bray.pdf")
# ========= PCA ===============

sizestr_prcomp<-princomp(t(sizestr_matrx))
plot(sizestr_prcomp)
summary(sizestr_prcomp)
str(sizestr_prcomp)

#запишем нагрузки в файл
write.table(loadings(sizestr_prcomp), "loadings_PCA.csv", sep=";", dec=",")

sizestr_p<-predict(sizestr_prcomp)

pdf("sizestr_PCA12.pdf", family="NimbusSan")
plot(sizestr_p[,1:2],pch=as.numeric(sizestr_matrx_annotation$site))
legend(legend=levels(sizestr_matrx_annotation$site), x="topleft", pch = seq(1:3))
dev.off()
embedFonts("sizestr_PCA12.pdf")

pdf("sizestr_PCA12_year.pdf", family="NimbusSan")
plot(sizestr_p[,1:2],pch=as.numeric(sizestr_matrx_annotation$site))
legend(legend=levels(sizestr_matrx_annotation$site), x="topleft", pch = seq(1:3), bty = "n")
text(x=sizestr_p[,1]+1, y=sizestr_p[,2]+1, sizestr_matrx_annotation$code, col = "gray70")
dev.off()
embedFonts("sizestr_PCA12_year.pdf")


pdf("sizestr_PCA_biplot.pdf", family="NimbusSan")
biplot(sizestr_prcomp, col = c("gray80", "red"), xlabs = sizestr_matrx_annotation$index)
dev.off()
embedFonts("sizestr_PCA_biplot.pdf")

pdf("sizestr_PCA_arrows.pdf", family="NimbusSan")
plot(sizestr_p[,1:2],pch=as.numeric(sizestr_matrx_annotation$site))
arrows(x0=sizestr_p[,1][ sizestr_matrx_annotation$site == "razrez2"][1:length(sizestr_p[,1][ sizestr_matrx_annotation$site == "razrez2"])-1],
       y0=sizestr_p[,2][ sizestr_matrx_annotation$site == "razrez2"][1:length(sizestr_p[,2][ sizestr_matrx_annotation$site == "razrez2"])-1], 
       x1=sizestr_p[,1][ sizestr_matrx_annotation$site == "razrez2"][2:length(sizestr_p[,1][ sizestr_matrx_annotation$site == "razrez2"])],
       y1=sizestr_p[,2][ sizestr_matrx_annotation$site == "razrez2"][2:length(sizestr_p[,2][ sizestr_matrx_annotation$site == "razrez2"])],
       length = 0.2, col=2)
arrows(x0=sizestr_p[,1][ sizestr_matrx_annotation$site == "Goreliy"][1:length(sizestr_p[,1][ sizestr_matrx_annotation$site == "Goreliy"])-1],
       y0=sizestr_p[,2][ sizestr_matrx_annotation$site == "Goreliy"][1:length(sizestr_p[,2][ sizestr_matrx_annotation$site == "Goreliy"])-1], 
       x1=sizestr_p[,1][ sizestr_matrx_annotation$site == "Goreliy"][2:length(sizestr_p[,1][ sizestr_matrx_annotation$site == "Goreliy"])],
       y1=sizestr_p[,2][ sizestr_matrx_annotation$site == "Goreliy"][2:length(sizestr_p[,2][ sizestr_matrx_annotation$site == "Goreliy"])],
       length = 0.2, col=3)
arrows(x0=sizestr_p[,1][ sizestr_matrx_annotation$site == "Estuary"][1:length(sizestr_p[,1][ sizestr_matrx_annotation$site == "Estuary"])-1],
       y0=sizestr_p[,2][ sizestr_matrx_annotation$site == "Estuary"][1:length(sizestr_p[,2][ sizestr_matrx_annotation$site == "Estuary"])-1], 
       x1=sizestr_p[,1][ sizestr_matrx_annotation$site == "Estuary"][2:length(sizestr_p[,1][ sizestr_matrx_annotation$site == "Estuary"])],
       y1=sizestr_p[,2][ sizestr_matrx_annotation$site == "Estuary"][2:length(sizestr_p[,2][ sizestr_matrx_annotation$site == "Estuary"])],
       length = 0.2, col=4)
text(x=sizestr_p[,1]+1, y=sizestr_p[,2]+1, sizestr_matrx_annotation$year, col = "gray80")
legend(legend=levels(sizestr_matrx_annotation$site), x="topleft", pch = seq(1:3), col = c(4,3,2))
dev.off()
embedFonts("sizestr_PCA_arrows.pdf")

col_years_palette <-  rainbow(21)
sizestr_matrx_annotation$col_year <- paste(col_years_palette[ as.numeric(as.factor(sizestr_matrx_annotation$year))])

pdf("sizestr_PCA12_col.pdf", family="NimbusSan")
plot(sizestr_p[,1:2],pch=as.numeric(sizestr_matrx_annotation$site), col = sizestr_matrx_annotation$col_year)
legend(legend=levels(sizestr_matrx_annotation$site), x="bottomleft", pch = seq(1:3))
legend(legend=seq(1992,2012,1), x="topleft", pch=15, col = col_years_palette,ncol = 2)
dev.off()
embedFonts("sizestr_PCA12_col.pdf")