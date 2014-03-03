setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Klyushiha_Chupa/")


#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.N<-read.table(file="samples_N.csv", sep=";", dec=",", head=T)
attach(ishodnik)
#year<-factor(year)

str(ishodnik)
str(samples.N)

levels(samples.N$tidal)
samples.N$tidal<-ordered(x=samples.N$tidal, levels=c("middle", "low", "subtidal"))

## считаем средние значения по горизонтам + ошибки
(N.mean.sqmeter<-tapply(samples.N$N.indd*samples.N$square, samples.N$tidal, FUN=mean))
(N.sd.sqmeter<-tapply(samples.N$N.indd*samples.N$square, samples.N$tidal, FUN=sd))
(N.sem.sqmeter<-N.sd.sqmeter/sqrt(tapply(samples.N$N.indd, samples.N$tidal, FUN=length)))

