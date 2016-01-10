setwd("~/Dropbox/PhD_thesis/PhD_thesis/after_Deryuginskie")

detach(ishodnik)
#загружаем исходники
ishodnik<-read.table("Macoma_Cerastoderma_Mya_Mytilus_size.csv", header=T, sep=";", dec=",")
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

# ===== сравниваем точечную оценку по 1973 году с нашими ======================
wilcox.test(x = Macoma_5mm_mean[1:6], mu = Macoma_1973_mean)
wilcox.test(x = cockle_5mm_mean, mu = cockle_1973_mean)



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


## ======= рисуем графики =================
#на одном графике общее обилие и обилие тех кто больше 5 мм
Macoma_means<-matrix(c(Macoma_1973_mean, Macoma_5mm_mean, NA, Macoma_mean), nrow=8,ncol=2, byrow=F)
dimnames(Macoma_means)<-(list(c(1973, names(Macoma_mean)), c(">5mm", "all")))

pdf(file="Macoma_N_dynamic_all.pdf", family="NimbusSan")
barplot(t(Macoma_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, max(Macoma_mean)+max(Macoma_SEM)), col = c("grey70", "white"))
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
barplot(t(cockle_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, max(cockle_mean)+max(cockle_SEM)+1), col = c("grey70", "white"))
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
barplot(t(Mya_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, max(Mya_5mm_mean)+max(Mya_5mm_SEM)), col = c("grey70", "white"))
arrows(x0=seq(1.5,24.5,3),
       y0=(c(Mya_1973_sqmeter, Mya_5mm_mean) + c(0, Mya_5mm_SEM)),
       x1=seq(1.5,24.5,3),
       y1=c(Mya_1973_sqmeter, Mya_5mm_mean) - c(0, Mya_5mm_SEM),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,25.5,3),
       y0=(c(NA, Mya_mean) + c(NA, Mya_SEM)),
       x1=seq(2.5,25.5,3),
       y1=(c(NA, Mya_mean) - c(NA, Mya_SEM)),
       angle=90, code=3, length=.06)
dev.off()
embedFonts("Mya_N_dynamic_all.pdf") #встройка шрифтов в файл

#тренды
kruskal.test(Macoma_N_samples.df$N_sqmeter ~ as.factor(Macoma_N_samples.df$year))

kruskal.test(cockle_N_samples.df$N_sqmeter ~ as.factor(cockle_N_samples.df$year))

kruskal.test(Mya_N_samples.df$N_sqmeter ~ as.factor(Mya_N_samples.df$year))

plot(TukeyHSD(aov(lm(Macoma_N_samples.df$N_sqmeter ~ as.factor(Macoma_N_samples.df$year)))))

#гистограммы 
library(lattice)
 #размерные класс = 1 мм
pdf(file="size_structure_Macoma_Cerastoderma_Mya_1mmclass.pdf", family="NimbusSan",
    width=190, height=280, paper="a4")
histogram(~ishodnik$Length.mm[ ishodnik$species!="Mytilus edulis"] | ishodnik$species[ ishodnik$species!="Mytilus edulis"] + ordered(ishodnik$year[ ishodnik$species!="Mytilus edulis"], levels<-c(2008, 2007, 2006, 2005, 2004, 2003, 2002)),
          type="percent",
          breaks=seq(0, max(ishodnik$Length.mm+1, na.rm=T),1), 
          xlab="L, мм", ylab="%")
dev.off()
embedFonts("size_structure_Macoma_Cerastoderma_Mya_1mmclass.pdf")

#размерные класс = 2 мм
pdf(file="size_structure_Macoma_Cerastoderma_Mya_2mmclass.pdf", family="NimbusSan", width=190, height=280, paper="a4")
histogram(~ishodnik$Length.mm[ ishodnik$species!="Mytilus edulis"] | ishodnik$species[ ishodnik$species!="Mytilus edulis"] + ordered(ishodnik$year[ ishodnik$species!="Mytilus edulis"], levels<-c(2008, 2007, 2006, 2005, 2004, 2003, 2002)),
          type="percent",
          breaks=seq(0, max(ishodnik$Length.mm+2, na.rm=T),2), 
          xlab="L, мм", ylab="%")
dev.off()
embedFonts("size_structure_Macoma_Cerastoderma_Mya_2mmclass.pdf")


pdf(file="size_structure_Cerastoderma_Mya_2mmclass.pdf", family="NimbusSan", width=190, height=280, paper="a4")
histogram(~ishodnik$Length.mm[ ishodnik$species!="Mytilus edulis" & ishodnik$species!="Macoma balthica"] | ishodnik$species[ ishodnik$species!="Mytilus edulis" & ishodnik$species!="Macoma balthica"] + ordered(ishodnik$year[ ishodnik$species!="Mytilus edulis" & ishodnik$species!="Macoma balthica"], levels<-c(2008, 2007, 2006, 2005, 2004, 2003, 2002)),
          type="percent",
          breaks=seq(0, max(ishodnik$Length.mm+2, na.rm=T),2), 
          xlab="L, мм", ylab="%")
dev.off()
embedFonts("size_structure_Cerastoderma_Mya_2mmclass.pdf")

#####

## Mytilus edulis - считаем обилие в нижней литорали. Пробы M6-M11. Площадь 1/10 кв.м. 2002 год отрезаем
#####
ish_Mytilus<-subset(ishodnik, ishodnik$samples== "M-6" | ishodnik$samples=="M-7" | ishodnik$samples=="M-8" | ishodnik$samples=="M-9" | ishodnik$samples=="M-10"| ishodnik$samples=="M-11" & ishodnik$species=="Mytilus edulis")

str(ish_Mytilus)

(Mytilus_N_samples<-table(ish_Mytilus$year, ish_Mytilus$samples))
(Mytilus_N_samples.df<-as.data.frame(Mytilus_N_samples))
names(Mytilus_N_samples.df)<-c("year", "sample", "N_sample")

Mytilus_N_samples.df$N_sample[Mytilus_N_samples.df$N_sample==0 & Mytilus_N_samples.df$sample!="M-6" & Mytilus_N_samples.df$sample!="M-7" & Mytilus_N_samples.df$sample!="M-8" & Mytilus_N_samples.df$sample!="M-9" & Mytilus_N_samples.df$sample!="M-10" & Mytilus_N_samples.df$sample!="M-11"]<-NA

Mytilus_N_samples.df$N_sqmeter<-Mytilus_N_samples.df$N_sample*10

#теперь считаем средние численности и ошибки в каждом году

Mytilus_mean<-tapply(Mytilus_N_samples.df$N_sqmeter, INDEX=Mytilus_N_samples.df$year, mean, na.rm=T)
Mytilus_sd<-tapply(Mytilus_N_samples.df$N_sqmeter, INDEX=Mytilus_N_samples.df$year, sd, na.rm=T)
Mytilus_SEM<-Mytilus_sd/sqrt(6)


#график обилия мидий

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

pdf(file="Mytilus_N_dynamic_low.pdf", family="NimbusSan")
error.bars(Mytilus_mean[2:5], Mytilus_SEM[2:5], names(Mytilus_mean)[2:5])
dev.off()
embedFonts("Mytilus_N_dynamic_low.pdf") #встройка шрифтов в файл

#гистограммы - размерная структура мидий
library(lattice)
#размерные класс = 5 мм
pdf(file="size_structure_Mytilus_5mmklass.pdf", family="NimbusSan", width=190, height=280, paper="a4")
histogram(~ish_Mytilus$Length.mm[ ish_Mytilus$year!=2002] | ordered(ish_Mytilus$year[ ish_Mytilus$year!=2002], levels<-c(2003, 2004, 2005, 2006)),
          type="percent",
          breaks=seq(0, max(ish_Mytilus$Length.mm[ ish_Mytilus$year!=2002], na.rm=T)+4.5,5), 
          xlab="L, мм", ylab="%")
dev.off()
embedFonts("size_structure_Mytilus_5mmklass.pdf")
