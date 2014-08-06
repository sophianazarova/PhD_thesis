setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/dynamic_N_N1/")

ishodnik<-read.table("N2_mean_SEM.csv", header=T, sep=";", dec=",")

str(ishodnik)
names(ishodnik)

#делаем табличку без второго разреза
ishodnik2<-tapply(ishodnik$N2.mean.sqmeter,list(ishodnik$area,ishodnik$year), max)

#делаем табличку без второго разреза
dimnames(ishodnik2)
ishodnik3<-ishodnik2[c(1:5,7:10),]

region<-c(1,1,3,2,3,3,3,2,2)
region<-factor(region,levels = c(1,2,3), labels =  c("Luvenga", "North", "Chupa"))

library(MASS)

d <- dist(ishodnik3) # euclidean distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
pdf(file="mds_areas.pdf", family="NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Nonmetric MDS", type="n")
text(x, y, labels = abbreviate(row.names(ishodnik3)), cex=.7, col = 1+as.numeric(region)) 
#points(x, y, pch=15,col=1+as.numeric(region)) 
dev.off()
embedFonts("mds_areas.pdf")
  
  
library(vegan)
reg_anosim<-anosim(d, region)
summary(reg_anosim)
plot(reg_anosim)

#запишем результат анозима в файл....
sink("anosim_region.txt")
summary(reg_anosim)
sink()
