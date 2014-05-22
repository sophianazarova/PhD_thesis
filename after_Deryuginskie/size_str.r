setwd("~/Dropbox/PhD_thesis/PhD_thesis/after_Deryuginskie")

#загружаем исходники
ishodnik<-read.table("Macoma_Cerastoderma_Mya_size.csv", header=T, sep=";", dec=",")
str(ishodnik)

samples.names<-read.table(file="samples.csv", sep=";", dec=",", head=T)

## ДЕЛАЕМ ОЦЕНКУ ОБИЛИЯ ДЛЯ СРАВНЕНИЯ С 1973 ГОДОМ, ОТРЕЗАЯ ВСЕХ КТО МЕНЬШЕ 5ММ, т.к.ТАМ МЫЛИ НА 5 мм сите
#####

#данные за 1973 год (по: Агарова и др., 1976)
Macoma_1973_sqmeter<-c(16,42, 12)
cockle_1973_sqmeter<-c(10,3,5,10)
Mya_1973_sqmeter<-c(10)

Macoma_1973_mean<-mean(Macoma_1973_sqmeter)
Macoma_1973_SEM<-sd(Macoma_1973_sqmeter)/sqrt(3)

cockle_1973_mean<-mean(cockle_1973_sqmeter)
cockle_1973_SEM<-sd(cockle_1973_sqmeter)/sqrt(4)



#фильтруем все данные крупнее 5 мм для сравнения с 70ми годами
ish_more_5mm<-subset(ishodnik, ishodnik$Length.mm>=5)

# делаем отдельные датасеты для макомы и сердцевидки и мии
ish_Macoma_5mm<-subset(ish_more_5mm, ish_more_5mm$species=="Macoma balthica")

ish_cockle_5mm<-subset(ish_more_5mm, ish_more_5mm$species=="Cerastoderma edule")

ish_Mya_5mm<-subset(ish_more_5mm, ish_more_5mm$species=="Mya arenaria")

#считаем число особей в пробах, убирая качественные выборки
(Macoma_N_samples_5mm<-table(ish_Macoma_5mm$year[ish_Macoma_5mm$samples!="kach", drop=T], ish_Macoma_5mm$samples[ish_Macoma_5mm$samples!="kach", drop=T]))
(Macoma_N_samples_5mm.df<-as.data.frame(Macoma_N_samples_5mm))
names(Macoma_N_samples_5mm.df)<-c("year", "sample", "N_sample")

cockle_N_samples_5mm<-table(ish_cockle_5mm$year[ish_cockle_5mm$samples!="kach",drop=T], ish_cockle_5mm$samples[ish_cockle_5mm$samples!="kach", drop=T])
cockle_N_samples_5mm.df<-as.data.frame(cockle_N_samples_5mm)
names(cockle_N_samples_5mm.df)<-c("year", "sample", "N_sample")

(Mya_N_samples_5mm<-table(ish_Mya_5mm$year[ish_Mya_5mm$samples!="kach", drop=T], ish_Mya_5mm$samples[ish_Mya_5mm$samples!="kach", drop=T]))
(Mya_N_samples_5mm.df<-as.data.frame(Mya_N_samples_5mm))
names(Mya_N_samples_5mm.df)<-c("year", "sample", "N_sample")

# надо вычеркнуть те пробы которых нету
for (i in 1:length(levels(Macoma_N_samples_5mm.df$year)))
{ (xxx<-Macoma_N_samples_5mm.df$sample[Macoma_N_samples_5mm.df$year==levels(Macoma_N_samples_5mm.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(Macoma_N_samples_5mm.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  Macoma_N_samples_5mm.df$N_sample[Macoma_N_samples_5mm.df$year==levels(Macoma_N_samples_5mm.df$year)[i]][antixxx]<-NA
}

for (i in 1:length(levels(cockle_N_samples_5mm.df$year)))
{ (xxx<-cockle_N_samples_5mm.df$sample[cockle_N_samples_5mm.df$year==levels(cockle_N_samples_5mm.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(cockle_N_samples_5mm.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  cockle_N_samples_5mm.df$N_sample[cockle_N_samples_5mm.df$year==levels(cockle_N_samples_5mm.df$year)[i]][antixxx]<-NA
}

for (i in 1:length(levels(Mya_N_samples_5mm.df$year)))
{ (xxx<-Mya_N_samples_5mm.df$sample[Mya_N_samples_5mm.df$year==levels(Mya_N_samples_5mm.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(Mya_N_samples_5mm.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  Mya_N_samples_5mm.df$N_sample[Mya_N_samples_5mm.df$year==levels(Mya_N_samples_5mm.df$year)[i]][antixxx]<-NA
}

#объединяем две таблички в одну - чтобы пририсовать, какая площадь и считаем на квадратный метр
# аналог функции merge из Шипунова и Ко (стр.237)
recode <- function(var, from, to)
{
  x <- as.vector(var)
  x.tmp <- x
  for (i in 1:length(from)) {x <- replace(x, x.tmp == from[i],
                                          to[i])}
  if(is.factor(var)) factor(x) else x
}

samp<-recode(Macoma_N_samples_5mm.df$sample, samples.names$sample, as.numeric(samples.names$square))
Macoma_N_samples_5mm.df<-(cbind(square=as.numeric(as.character(samp)), Macoma_N_samples_5mm.df))
Macoma_N_samples_5mm.df$N_sqmeter<-Macoma_N_samples_5mm.df$N_sample * Macoma_N_samples_5mm.df$square


samp<-recode(cockle_N_samples_5mm.df$sample, samples.names$sample, as.numeric(samples.names$square))
(cockle_N_samples_5mm.df<-(cbind(square=as.numeric(as.character(samp)), cockle_N_samples_5mm.df)))
cockle_N_samples_5mm.df$N_sqmeter<-cockle_N_samples_5mm.df$N_sample * cockle_N_samples_5mm.df$square

samp<-recode(Mya_N_samples_5mm.df$sample, samples.names$sample, as.numeric(samples.names$square))
Mya_N_samples_5mm.df<-(cbind(square=as.numeric(as.character(samp)), Mya_N_samples_5mm.df))
Mya_N_samples_5mm.df$N_sqmeter<-Mya_N_samples_5mm.df$N_sample * Mya_N_samples_5mm.df$square

#теперь считаем средние численности и ошибки в каждом году
(n.samples<-table(samples.names$year[samples.names$square==10 | samples.names$square==30]))

Macoma_5mm_mean<-tapply(Macoma_N_samples_5mm.df$N_sqmeter, INDEX=Macoma_N_samples_5mm.df$year, mean, na.rm=T)
Macoma_5mm_sd<-tapply(Macoma_N_samples_5mm.df$N_sqmeter, INDEX=Macoma_N_samples_5mm.df$year, sd, na.rm=T)
Macoma_5mm_SEM<-Macoma_5mm_sd/sqrt(n.samples)

cockle_5mm_mean<-tapply(cockle_N_samples_5mm.df$N_sqmeter, INDEX=cockle_N_samples_5mm.df$year, mean, na.rm=T)
cockle_5mm_sd<-tapply(cockle_N_samples_5mm.df$N_sqmeter, INDEX=cockle_N_samples_5mm.df$year, sd, na.rm=T)
cockle_5mm_SEM<-cockle_5mm_sd/sqrt(n.samples)

Mya_5mm_mean<-tapply(Mya_N_samples_5mm.df$N_sqmeter, INDEX=Mya_N_samples_5mm.df$year, mean, na.rm=T)
Mya_5mm_sd<-tapply(Mya_N_samples_5mm.df$N_sqmeter, INDEX=Mya_N_samples_5mm.df$year, sd, na.rm=T)
Mya_5mm_SEM<-Mya_5mm_sd/sqrt(n.samples)

#####

## Считаем динамику обилия с учетом всех особей.
#####

# делаем отдельные датасеты для макомы и сердцевидки
ish_Macoma<-subset(ishodnik, ishodnik$species=="Macoma balthica")

ish_cockle<-subset(ishodnik, ishodnik$species=="Cerastoderma edule")

ish_Mya<-subset(ishodnik,ishodnik$species=="Mya arenaria")

#считаем число особей в пробах, убирая качественные выборки
(Macoma_N_samples<-table(ish_Macoma$year[ish_Macoma$samples!="kach", drop=T], ish_Macoma$samples[ish_Macoma$samples!="kach", drop=T]))
(Macoma_N_samples.df<-as.data.frame(Macoma_N_samples))
names(Macoma_N_samples.df)<-c("year", "sample", "N_sample")

cockle_N_samples<-table(ish_cockle$year[ish_cockle$samples!="kach",drop=T], ish_cockle$samples[ish_cockle$samples!="kach", drop=T])
cockle_N_samples.df<-as.data.frame(cockle_N_samples)
names(cockle_N_samples.df)<-c("year", "sample", "N_sample")

(Mya_N_samples<-table(ish_Mya$year[ish_Mya$samples!="kach", drop=T], ish_Mya$samples[ish_Mya$samples!="kach", drop=T]))
(Mya_N_samples.df<-as.data.frame(Mya_N_samples))
names(Mya_N_samples.df)<-c("year", "sample", "N_sample")

# надо вычеркнуть те пробы которых нету
for (i in 1:length(levels(Macoma_N_samples.df$year)))
{ (xxx<-Macoma_N_samples.df$sample[Macoma_N_samples.df$year==levels(Macoma_N_samples.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(Macoma_N_samples.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  Macoma_N_samples.df$N_sample[Macoma_N_samples.df$year==levels(Macoma_N_samples.df$year)[i]][antixxx]<-NA
}

for (i in 1:length(levels(cockle_N_samples.df$year)))
{ (xxx<-cockle_N_samples.df$sample[cockle_N_samples.df$year==levels(cockle_N_samples.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(cockle_N_samples.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  cockle_N_samples.df$N_sample[cockle_N_samples.df$year==levels(cockle_N_samples.df$year)[i]][antixxx]<-NA
}

for (i in 1:length(levels(Mya_N_samples.df$year)))
{ (xxx<-Mya_N_samples.df$sample[Mya_N_samples.df$year==levels(Mya_N_samples.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(Mya_N_samples.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  Mya_N_samples.df$N_sample[Mya_N_samples.df$year==levels(Mya_N_samples.df$year)[i]][antixxx]<-NA
}


#объединяем две таблички в одну - чтобы пририсовать, какая площадь и считаем на квадратный метр

samp<-recode(Macoma_N_samples.df$sample, samples.names$sample, as.numeric(samples.names$square))
Macoma_N_samples.df<-(cbind(square=as.numeric(as.character(samp)), Macoma_N_samples.df))
Macoma_N_samples.df$N_sqmeter<-Macoma_N_samples.df$N_sample * Macoma_N_samples.df$square


samp<-recode(cockle_N_samples.df$sample, samples.names$sample, as.numeric(samples.names$square))
(cockle_N_samples.df<-(cbind(square=as.numeric(as.character(samp)), cockle_N_samples.df)))
cockle_N_samples.df$N_sqmeter<-cockle_N_samples.df$N_sample * cockle_N_samples.df$square

samp<-recode(Mya_N_samples.df$sample, samples.names$sample, as.numeric(samples.names$square))
Mya_N_samples.df<-(cbind(square=as.numeric(as.character(samp)), Mya_N_samples.df))
Mya_N_samples.df$N_sqmeter<-Mya_N_samples.df$N_sample * Mya_N_samples.df$square

#теперь считаем средние численности и ошибки в каждом году

Macoma_mean<-tapply(Macoma_N_samples.df$N_sqmeter, INDEX=Macoma_N_samples.df$year, mean, na.rm=T)
Macoma_sd<-tapply(Macoma_N_samples.df$N_sqmeter, INDEX=Macoma_N_samples.df$year, sd, na.rm=T)
Macoma_SEM<-Macoma_sd/sqrt(n.samples)

cockle_mean<-tapply(cockle_N_samples.df$N_sqmeter, INDEX=cockle_N_samples.df$year, mean, na.rm=T)
cockle_sd<-tapply(cockle_N_samples.df$N_sqmeter, INDEX=cockle_N_samples.df$year, sd, na.rm=T)
cockle_SEM<-cockle_sd/sqrt(n.samples)

Mya_mean<-tapply(Mya_N_samples.df$N_sqmeter, INDEX=Mya_N_samples.df$year, mean, na.rm=T)
Mya_sd<-tapply(Mya_N_samples.df$N_sqmeter, INDEX=Mya_N_samples.df$year, sd, na.rm=T)
Mya_SEM<-Mya_sd/sqrt(n.samples)

#####


## рисуем графики
#на одном графике общее обилие и обилие тех кто больше 5 мм
Macoma_means<-matrix(c(Macoma_1973_mean, Macoma_5mm_mean, NA, Macoma_mean), nrow=8,ncol=2, byrow=F)
dimnames(Macoma_means)<-(list(c(1973, names(Macoma_mean)), c(">5mm", "all")))

pdf(file="Macoma_N_dynamic_all.pdf", family="NimbusSan")
barplot(t(Macoma_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="L, мм", ylim=c(0, max(Macoma_mean)+max(Macoma_SEM)))
arrows(x0=seq(1.5,24.5,3),
       y0=(c(Macoma_1973_mean, Macoma_5mm_mean) + c(Macoma_1973_SEM, Macoma_5mm_SEM)),
       x1=seq(1.5,24.5,3),
       y1=c(Macoma_1973_mean, Macoma_5mm_mean) - c(Macoma_1973_SEM, Macoma_5mm_SEM),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,25.5,3),
       y0=(c(NA, Macoma_mean) + c(NA, Macoma_SEM)),
       x1=seq(2.5,25.5,3),
       y1=(c(NA, Macoma_mean) - c(NA, Macoma_SEM)),
       angle=90, code=3, length=.06)
dev.off()
embedFonts("Macoma_N_dynamic_all.pdf") #встройка шрифтов в файл


cockle_means<-matrix(c(cockle_1973_mean, cockle_5mm_mean, NA, cockle_mean), nrow=8,ncol=2, byrow=F)
dimnames(cockle_means)<-(list(c(1973, names(cockle_mean)), c(">5mm", "all")))

pdf(file="cockle_N_dynamic_all.pdf", family="NimbusSan")
barplot(t(cockle_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="L, мм", ylim=c(0, max(cockle_mean)+max(cockle_SEM)+1))
arrows(x0=seq(1.5,24.5,3),
       y0=(c(cockle_1973_mean, cockle_5mm_mean) + c(cockle_1973_SEM, cockle_5mm_SEM)),
       x1=seq(1.5,24.5,3),
       y1=c(cockle_1973_mean, cockle_5mm_mean) - c(cockle_1973_SEM, cockle_5mm_SEM),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,25.5,3),
       y0=(c(NA, cockle_mean) + c(NA, cockle_SEM)),
       x1=seq(2.5,25.5,3),
       y1=(c(NA, cockle_mean) - c(NA, cockle_SEM)),
       angle=90, code=3, length=.06)
dev.off()
embedFonts("cockle_N_dynamic_all.pdf") #встройка шрифтов в файл

Mya_means<-matrix(c(Mya_1973_sqmeter, Mya_5mm_mean, NA, Mya_mean), nrow=8,ncol=2, byrow=F)
dimnames(Mya_means)<-(list(c(1973, names(Mya_mean)), c(">5mm", "all")))

pdf(file="Mya_N_dynamic_all.pdf", family="NimbusSan")
barplot(t(Mya_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="L, мм", ylim=c(0, max(Mya_mean)+max(Mya_SEM)))
arrows(x0=seq(1.5,24.5,3),
       y0=(c(Mya_1973_mean, Mya_5mm_mean) + c(Mya_1973_SEM, Mya_5mm_SEM)),
       x1=seq(1.5,24.5,3),
       y1=c(Mya_1973_mean, Mya_5mm_mean) - c(Mya_1973_SEM, Mya_5mm_SEM),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,25.5,3),
       y0=(c(NA, Mya_mean) + c(NA, Mya_SEM)),
       x1=seq(2.5,25.5,3),
       y1=(c(NA, Mya_mean) - c(NA, Mya_SEM)),
       angle=90, code=3, length=.06)
dev.off()
embedFonts("Mya_N_dynamic_all.pdf") #встройка шрифтов в файл



#гистограммы - размерная структура
library(lattice)

pdf(file="size_structure_Macoma_Cerastoderma_Mya.pdf", family="NimbusSan", width=190, height=280, paper="a4")
histogram(~ishodnik$Length.mm | ishodnik$species + ordered(ishodnik$year, levels<-c(2008, 2007, 2006, 2005, 2004, 2003, 2002)))
dev.off()
embedFonts("size_structure_Macoma_Cerastoderma_Mya.pdf")

