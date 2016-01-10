setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/uchastki_svodka")

biotop <- read.csv2("biotops.csv")
str(biotop)

#делаем матрицу
biotop_m <- as.matrix(biotop[,2:15])
rownames(biotop_m) <- biotop[,1]

#считаем расстояние. 
library(cluster)
library(vegan)

#Попробуем Брей-Кертиса
dist_bray <- vegdist(t(biotop_m), "bray")

#Попробуем Жаккард
dist_jac <- vegdist(t(biotop_m), "jaccard")

# ============= Белое море - считаем non-metric MDS ============
library(MASS)
biotop_nMDS<-isoMDS(dist_bray)
biotop_nMDS<-isoMDS(dist_jac)


x <- biotop_nMDS$points[,1]
y <- biotop_nMDS$points[,2]

plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="Nonmetric MDS")

legend(legend=levels(White_sizestr_matrx_annotation$area), x="topleft", pch=15, col=seq(1:length(levels(White_sizestr_matrx_annotation$area))))