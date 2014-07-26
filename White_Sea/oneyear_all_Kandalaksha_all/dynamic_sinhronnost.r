setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/oneyear_all_Kandalaksha_all/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

ishodnik<-read.table("N_all_Kandalaksha.csv", header=T, sep=";", dec=",")
str(ishodnik)

# соответствие динамик через Мантелевские корреляции
#####

library(vegan)
str(ishodnik)
attach(ishodnik)

N.1year.matrix<-tapply(X=N.1year, list(year,area), max)
(N.1year.df<-data.frame(years=as.numeric(dimnames(N.1year.matrix)[[1]]), as.data.frame(N.1year.matrix[,!is.na(colMeans(N.1year.matrix, na.rm=T))])))


str(N.1year.df)

##считаем частные корреляции мантеля по участкам, мытым на сите 0,5 мм
#делаем пустые матрицы для результатов
(N1.mantel.signif<-matrix(nrow=(ncol(N.1year.df)-1), ncol=ncol(N.1year.df)-1, dimnames=list(colnames(N.1year.df[2:ncol(N.1year.df)]),colnames(N.1year.df[2:ncol(N.1year.df)]))))
(N1.mantel.statistic<-matrix(nrow=(ncol(N.1year.df)-1), ncol=ncol(N.1year.df)-1, dimnames=list(colnames(N.1year.df[2:ncol(N.1year.df)]),colnames(N.1year.df[2:ncol(N.1year.df)]))))
#для каждой пары участков i и j
for (i in 2:ncol(N.1year.df)) {
  for (j in 2:ncol(N.1year.df)) {
    # делаем модельную матрицу по годам
    year.period<-N.1year.df$years[!is.na(N.1year.df[,i]) &  !is.na(N.1year.df[,j])]
    if (length(year.period)==0) j<-j+1
    year.period<-N.1year.df$years[!is.na(N.1year.df[,i]) &  !is.na(N.1year.df[,j])]
    if (length(year.period)==0) j<-j+1
    year.m<-vegdist(year.period, method="euclidean")
    # делаем матрицу по численностям
    area1.m<-vegdist(N.1year.df[,i][N.1year.df$years%in%N.1year.df$years[!is.na(N.1year.df[,i])] &  !is.na(N.1year.df[,j])], method="euclidean")
    area2.m<-vegdist(N.1year.df[,j][N.1year.df$years%in%N.1year.df$years[!is.na(N.1year.df[,i])] &  !is.na(N.1year.df[,j])], method="euclidean")
    #считаем частные корреляции мантеля с учетом детрендинга
    cormant<-mantel.partial(area1.m, area2.m, year.m, permutations=999)
    N1.mantel.signif[i-1, j-1]<-cormant$signif
    N1.mantel.statistic[i-1, j-1]<-round(cormant$statistic, digits=3)
  }
}
N1.mantel.signif
N1.mantel.statistic

write.table(N1.mantel.statistic, file="N1_mantel_statistic.csv", sep=";", dec=",")
write.table(N1.mantel.signif, file="N1_mantel_signif.csv", sep=";", dec=",")


#Считаем сходство с матрицей расстояний
#####

#читаем матрицу расстояний
distance_N_km<-read.table("coordinates_N.csv", sep=";", dec=",", header =T)
rownames(distance_N_km)<-distance_N_km[,1]
distance_N_km<-as.matrix(distance_N_km[,2:9])
colnames(distance_N_km)<-rownames(distance_N_km)

distance_N2_km<-read.table("coordinates_N2.csv", sep=";", dec=",", header =T)
rownames(distance_N2_km)<-distance_N2_km[,1]
distance_N2_km<-as.matrix(distance_N2_km[,2:9])
colnames(distance_N2_km)<-rownames(distance_N2_km)

# считаем мантеля между матрицей расстояний и корреляциями динамики
mantel(xdis=distance_N_km, N.mantel.statistic, na.rm=T)
mantel(xdis=distance_N2_km, N2.mantel.statistic, na.rm=T)

#попробую убрать 2 разрез и посчитать без na.rm
N2.mantel.razrez2.rm<-N2.mantel.statistic[c(1:4,6:8),c(1:4,6:8)]
distance_N2_km.razrez2.rm<-distance_N2_km[c(1:4,6:8),c(1:4,6:8)]

mantel(xdis=distance_N2_km.razrez2.rm, N2.mantel.razrez2.rm)

#####

#картинки расстояние vs корреляция мантеля
as.vector(distance_N_km)
as.vector(N.mantel.statistic)
plot(distance_N_km, N.mantel.statistic)
