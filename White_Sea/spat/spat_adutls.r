setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/spat/")

##СПАТ РС
#####
spat<-read.table(file="spat_2006_Chupa.csv", sep=";", dec=",", head=T)
str(spat)

#смотрим только средний горизонт!
spat_middle<-subset(spat, spat$tidal_zone=="middle")
max(spat_middle$Length.mm)
spat.int<-cut(spat_middle$Length.mm, breaks=seq(0,1.5,0.1))

(spat.str.table<-table(spat.int, spat_middle$sample, spat_middle$area))
N.spat<-apply(spat.str.table, MARGIN = c(2,3), sum)

#все чего реально нет - заменяем на NA
for (i in 1:length(levels(spat$sample))){
  for (j in 1:length(levels(spat$area))){
    if (N.spat[i,j]==0) {spat.str.table[,i,j]<-NA}    
  }
}

spat.str.df<-as.data.frame(spat.str.table)
names(spat.str.df)<-c("spat.int", "sample", "area", "N.sample")

# считаем на квадратный метр
spat.str.df$N.sqmeter<-spat.str.df$N.sample*100

(spat.mean.sqmeter<-t(tapply(X = spat.str.df$N.sqmeter, list(spat.str.df$area, spat.str.df$spat.int), mean, na.rm=T)))
   
(spat.sd.sqmeter<-t(tapply(X = spat.str.df$N.sqmeter, list(spat.str.df$area, spat.str.df$spat.int),FUN=sd, na.rm=T)))

(spat.sem.sqmeter <-spat.sd.sqmeter/sqrt(5))

spat.class<-seq(0.1,1.5,0.1)

#from R-book 
error.bars<-function(yv,z,nn){
  xv<-
    barplot(yv,ylim=c(0,(max(yv)+max(z))),names=nn)#,ylab=deparse(substitute(yv)))
  g=(max(xv)-min(xv))/50
  for (i in 1:length(xv)) {
    lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i], yv[i]+z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i], yv[i]-z[i]))
  }}


for (j in 1:length(colnames(spat.mean.sqmeter)))
{
  pdf(file=paste("spat_str", colnames(spat.mean.sqmeter)[j], ".pdf",sep="_"), family="NimbusSan")
  error.bars(yv=spat.mean.sqmeter[,j], nn=spat.class, z=spat.sem.sqmeter[,j])
  #title(main=colnames(spat.mean.sqmeter)[j], xlab="L,мм", ylab="N, экз./кв.м.")
  dev.off()
  embedFonts(paste("spat_str", colnames(spat.mean.sqmeter)[j], ".pdf",sep="_"))
}
#####

## Взрослые макомы РС
#####

adult<-read.table(file="adults_Length_2006_Chupa.csv", sep=";", dec=",", head=T)
str(adult)

#смотрим только средний горизонт!
adult_middle<-subset(adult, adult$tidal_zone=="middle")
max(adult_middle$Length.mm)
adult.int<-cut(adult_middle$Length.mm, breaks=seq(0,16,1))

(adult.str.table<-table(adult.int, adult_middle$sample, adult_middle$area))
N.adult<-apply(adult.str.table, MARGIN = c(2,3), sum)

#все чего реально нет - заменяем на NA
for (i in 1:length(levels(adult$sample))){
  for (j in 1:length(levels(adult$area))){
    if (N.adult[i,j]==0) {adult.str.table[,i,j]<-NA}    
  }
}

adult.str.df<-as.data.frame(adult.str.table)
names(adult.str.df)<-c("adult.int", "sample", "area", "N.sample")

# считаем на квадратный метр
adult.str.df$N.sqmeter<-adult.str.df$N.sample
adult.str.df$N.sqmeter[ adult.str.df$area=="Lisya" | adult.str.df$area=="Klushiha"] <-adult.str.df$N.sample[ adult.str.df$area=="Lisya" | adult.str.df$area=="Klushiha"]*20
adult.str.df$N.sqmeter[ adult.str.df$area=="Suhaya" | adult.str.df$area=="Podpahta"] <-adult.str.df$N.sample[ adult.str.df$area=="Suhaya" | adult.str.df$area=="Podpahta"]*10


(adult.mean.sqmeter<-t(tapply(X = adult.str.df$N.sqmeter, list(adult.str.df$area, adult.str.df$adult.int), mean, na.rm=T)))

(adult.sd.sqmeter<-t(tapply(X = adult.str.df$N.sqmeter, list(adult.str.df$area, adult.str.df$adult.int),FUN=sd, na.rm=T)))

(adult.sem.sqmeter <-adult.sd.sqmeter/sqrt(5))

adult.class<-seq(1,16,1)


for (j in 1:length(colnames(adult.mean.sqmeter)))
{
  pdf(file=paste("adult_str", colnames(adult.mean.sqmeter)[j], ".pdf",sep="_"), family="NimbusSan")
  error.bars(yv=adult.mean.sqmeter[,j], nn=adult.class, z=adult.sem.sqmeter[,j])
#  title(main=colnames(adult.mean.sqmeter)[j], xlab="L,мм", ylab="N, экз./кв.м.")
 dev.off()
 embedFonts(paste("adult_str", colnames(adult.mean.sqmeter)[j], ".pdf",sep="_"))
}

#library(lattice)
#histogram(~ spat$Length.mm[ spat$tidal_zone=="middle"] | spat$area[ spat$tidal_zone=="middle"], breaks = seq(0.1,max(spat$Length.mm)+1, 0.1))

#####

## Корреляция обилия молоди с обилием крупных
#####
N.adult.sqmeter<-t(N.adult)*c(20,20,10,10)
N.adult.sqmeter[N.adult.sqmeter==0]<-NA
Nmean.adult.sqmeter<-apply(N.adult.sqmeter, 1, mean, na.rm=T)

N.spat.sqmeter<-t(N.spat)*100
N.spat.sqmeter[N.spat.sqmeter==0]<-NA
Nmean.spat.sqmeter<-apply(N.spat.sqmeter, 1, mean, na.rm=T)

#биомассу рассчетную я набила из рассчетов в ВКР, чтобы не считать сейчас...
B.mean.sqmeter<-c(Klushiha=1122.4, Lisya=1903.3, Podpahta=1885.3, Suhaya=6223.5)

cor.test(as.vector(Nmean.spat.sqmeter), as.vector(Nmean.adult.sqmeter), method = "spearman")

cor.test(as.vector(Nmean.spat.sqmeter), as.vector(B.mean.sqmeter), method = "spearman")
#что-то тут совсем другие цифры чем в ВКР из статистики :(
#####

## корреляция численности спата с особями разного размера - размерные классы делаем по данным про рост из Максимовича и др., 1991
Sunaya_middle_age<-c(1.4, 3.1, 6.2, 9.5, 11.6, 13.1, 14.2)
Klushiha_middle_age<-c(1.2, 3.2, 5.2, 7.8, 9.6, 11.2, 12.8)
mean_age_Chupa<-(Sunaya_middle_age+Klushiha_middle_age)/2

adult.age.int<-cut(adult_middle$Length.mm, breaks=c(0, mean_age_Chupa, 16))

(adult.age.table<-table(adult.age.int, adult_middle$sample, adult_middle$area))

#все чего реально нет - заменяем на NA
for (i in 1:length(levels(adult$sample))){
  for (j in 1:length(levels(adult$area))){
    if (N.adult[i,j]==0) {adult.age.table[,i,j]<-NA}    
  }
}

adult.age.df<-as.data.frame(adult.age.table)
names(adult.age.df)<-c("adult.age", "sample", "area", "N.sample")

# считаем на квадратный метр
adult.age.df$N.sqmeter<-adult.age.df$N.sample
adult.age.df$N.sqmeter[ adult.age.df$area=="Lisya" | adult.age.df$area=="Klushiha"] <-adult.age.df$N.sample[ adult.age.df$area=="Lisya" | adult.age.df$area=="Klushiha"]*20
adult.age.df$N.sqmeter[ adult.age.df$area=="Suhaya" | adult.age.df$area=="Podpahta"] <-adult.age.df$N.sample[ adult.age.df$area=="Suhaya" | adult.age.df$area=="Podpahta"]*10


(adult.age.mean.sqmeter<-t(tapply(X = adult.age.df$N.sqmeter, list(adult.age.df$area, adult.age.df$adult.age), mean, na.rm=T)))

library(psych)
spearman_spat_age<-corr.test(cbind(t(adult.age.mean.sqmeter), spat=Nmean.spat.sqmeter), method = "spearman")
plot(spearman_spat_age$r[9,1:8], col=c(spearman_spat_age$p[9,1:8]<=0.05+1))

pearson_spat_age<-corr.test(cbind(t(adult.age.mean.sqmeter), spat=Nmean.spat.sqmeter))
plot(pearson_spat_age$r[9,1:8], col=c(pearson_spat_age$p[9,1:8]<=0.05+1))
# и ничерта ни с чем не коррелирует...
#####

##Нарисую я новые картинки по данным из ВКР, рассчитанным в статистике
#####
# шаг 2 мм
spearman_spat_2mm<-read.table(file="Spearman_2mm_from_VKR.csv", sep=";", dec=",", head=T)

pdf("spearman_spat_2mm.pdf", family="NimbusSan")
plot(spearman_spat_2mm$Spearman, pch=15, col=c((spearman_spat_2mm$p.level<=0.05)+2))
dev.off()
embedFonts("spearman_spat_2mm.pdf")

# шаг 3 мм
spearman_spat_3mm<-read.table(file="Spearman_3mm_from_VKR.csv", sep=";", dec=",", head=T)

pdf("spearman_spat_3mm.pdf", family="NimbusSan")
plot(spearman_spat_3mm$Spearman, pch=15, col=c((spearman_spat_3mm$p.level<=0.05)+2))
dev.off()
embedFonts("spearman_spat_3mm.pdf")
#####
