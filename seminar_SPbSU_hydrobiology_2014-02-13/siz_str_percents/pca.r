setwd("~/Dropbox/PhD_thesis/PhD_thesis/seminar_SPbSU_hydrobiology_2014-02-13/siz_str_percents/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

ishodnik<-read.table(file="size_str_percent_White.csv", sep=";", dec=",", head=T)

str(ishodnik)
(ishodnik.m<-as.matrix(ishodnik[,2:ncol(ishodnik)]))
rownames(ishodnik.m)<- ishodnik[,1]

##Principal component analisys
#sizestr.pca<-prcomp(t(sum.sizestr.sqmeter.percents))
sizestr.pca<-princomp(t(ishodnik.m)) #princomp хочет чтобы признаков было больше чем групп. Тут 21 и 20 - т.е. это возможно
sizestr.p<-predict(sizestr.pca)
loadings(sizestr.pca) #с prcomp похоже не работает :(
plot(sizestr.pca)

write.table(loadings(sizestr.pca), file="nagruzki.csv", sep=";", dec=",")

#1х2компоненты
plot(sizestr.p[,1:2], type="n", xlab="PC1", ylab="PC2")
text(sizestr.p[,1:2], cex=0.5,
     labels=abbreviate(colnames(ishodnik.m)))

biplot(sizestr.pca, cex=c(0.4,1), col=c("black", "blue"))

#1х3компоненты
plot(sizestr.p[,c(1,3)], type="n", xlab="PC1", ylab="PC3")
text(sizestr.p[,c(1,3)],
     labels=abbreviate(colnames(sum.sizestr.sqmeter.percents)))
#2х3компоненты
plot(sizestr.p[,2:3], type="n", xlab="PC2", ylab="PC3")
text(sizestr.p[,2:3],
     labels=abbreviate(colnames(sum.sizestr.sqmeter.percents)))