setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Luvenga_II_razrez/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Luvenga_II_razrez/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)

ishodnik$tidal_level<-ordered(x=ishodnik$tidal_level, levels=c("high_beatch", "fucus_zone", "zostera_zone", "low_beatch"))

(oneyear.int<-cut(Length.mm, breaks=c(0.1,1.2,1.8,max(Length.mm, na.rm=T))))

(oneyear.table<-table(oneyear.int, year, tidal_level, sample))

oneyear.df<-as.data.frame(oneyear.table) # как таблица данных

#убираем те пробы которых на самом деле нету

for (i in 1:length(levels(oneyear.df$year))){
  for(j in 1:length(levels(oneyear.df$tidal_level))){
    (xxx<-oneyear.df$sample[oneyear.df$year==levels(oneyear.df$year)[i] & 
                               oneyear.df$tidal_level==levels(oneyear.df$tidal_level)[j]]%in%
       samples.names$sample[samples.names$year==levels(oneyear.df$year)[i] & 
                              samples.names$tidal.level==levels(oneyear.df$tidal_level)[j]])
    antixxx<-as.logical(1-xxx)
    oneyear.df$Freq[oneyear.df$year==levels(oneyear.df$year)[i] & 
                       oneyear.df$tidal_level==levels(oneyear.df$tidal_level)[j]][antixxx]<-NA
  }}

summary(oneyear.df)

#теперь на квадратный метр
oneyear.sqmeter<-oneyear.df
for (i in 1:length(levels(oneyear.sqmeter$year)))
{
  oneyear.sqmeter$Freq[oneyear.sqmeter$year==levels(oneyear.sqmeter$year)[i]]<-
    oneyear.sqmeter$Freq[oneyear.sqmeter$year==levels(oneyear.sqmeter$year)[i]] * 
    samples.squares$square[samples.squares$year==levels(oneyear.sqmeter$year)[i]]
}


for (i in 1: length(levels(oneyear.sqmeter$tidal_level)))
{
  assign(paste(levels(oneyear.sqmeter$tidal_level)[i]), 
         subset(oneyear.sqmeter, oneyear.sqmeter$tidal_level==levels(oneyear.sqmeter$tidal_level)[i]))
}



(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal.level), length ))
(n.samples.df<-as.data.frame(n.samples))

#Верхний пляж
(mean.sqmeter.high_beatch<-t(tapply(high_beatch$Freq,INDEX=list(high_beatch$year,  high_beatch$oneyear.int),FUN=sd, na.rm=T)))
mean.sqmeter.high_beatch.df<-as.data.frame(mean.sqmeter.high_beatch)

(sd.sqmeter.high_beatch<-tapply(high_beatch$Freq,INDEX=list(high_beatch$year,  high_beatch$oneyear.int),FUN=sd, na.rm=T))

(sem.sqmeter.high_beatch <-t(sd.sqmeter.high_beatch/sqrt(n.samples.df$high_beatch)))
sem.sqmeter.high_beatch.df<-as.data.frame(sem.sqmeter.high_beatch)

#пояс фукоидов
mean.sqmeter.fucus_zone<-t(tapply(fucus_zone$Freq,INDEX=list(fucus_zone$year,  fucus_zone$oneyear.int),FUN=sd, na.rm=T))
mean.sqmeter.fucus_zone.df<-as.data.frame(mean.sqmeter.fucus_zone)

sd.sqmeter.fucus_zone<-tapply(fucus_zone$Freq,INDEX=list(fucus_zone$year,  fucus_zone$oneyear.int),FUN=sd, na.rm=T)

sem.sqmeter.fucus_zone <-t(sd.sqmeter.fucus_zone/sqrt(n.samples.df$fucus_zone))
sem.sqmeter.fucus_zone.df<-as.data.frame(sem.sqmeter.fucus_zone)

#пояс зостеры
mean.sqmeter.zostera_zone<-t(tapply(zostera_zone$Freq,INDEX=list(zostera_zone$year,  zostera_zone$oneyear.int),FUN=sd, na.rm=T))
mean.sqmeter.zostera_zone.df<-as.data.frame(mean.sqmeter.zostera_zone)

sd.sqmeter.zostera_zone<-tapply(zostera_zone$Freq,INDEX=list(zostera_zone$year,  zostera_zone$oneyear.int),FUN=sd, na.rm=T)

sem.sqmeter.zostera_zone <-t(sd.sqmeter.zostera_zone/sqrt(n.samples.df$zostera_zone))
sem.sqmeter.zostera_zone.df<-as.data.frame(sem.sqmeter.zostera_zone)

#нижний пляж
(mean.sqmeter.low_beatch<-t(tapply(low_beatch$Freq,INDEX=list(low_beatch$year,  low_beatch$oneyear.int),FUN=sd, na.rm=T)))
mean.sqmeter.low_beatch.df<-as.data.frame(mean.sqmeter.low_beatch)

sd.sqmeter.low_beatch<-tapply(low_beatch$Freq,INDEX=list(low_beatch$year,  low_beatch$oneyear.int),FUN=sd, na.rm=T)

sem.sqmeter.low_beatch <-t(sd.sqmeter.low_beatch/sqrt(n.samples.df$low_beatch))
sem.sqmeter.low_beatch.df<-as.data.frame(sem.sqmeter.low_beatch)


#рисуем график

pdf(file="2razrez_N_oneyear.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.sqmeter.high_beatch[2,], x=as.numeric(colnames(mean.sqmeter.high_beatch)),pch=15, type="n", 
     main="Материковая литораль в районе пос. Лувеньга high_beatch",
     ylim=c(0, 6000),
          #  max(mean.sqmeter.high_beatch[2,]+sem.sqmeter.high_beatch[2,], 
          #      mean.sqmeter.fucus_zone[2,]+sem.sqmeter.fucus_zone[2,],
          #      mean.sqmeter.zostera_zone[2,]+sem.sqmeter.zostera_zone[2,], 
          #      mean.sqmeter.low_beatch[2,]+sem.sqmeter.low_beatch[2,], na.rm=T)),
     xlab="год", ylab="N1+, экз./кв.м")
#high_beatch
lines(as.numeric(colnames(mean.sqmeter.high_beatch)), 
      mean.sqmeter.high_beatch[2,], pch=15, type="b", col=2)
arrows(x0=as.numeric(colnames(mean.sqmeter.high_beatch)), 
       x1=as.numeric(colnames(mean.sqmeter.high_beatch)),
       y0=mean.sqmeter.high_beatch[2,]-sem.sqmeter.high_beatch[2,], 
       y1=mean.sqmeter.high_beatch[2,]+sem.sqmeter.high_beatch[2,], angle=90, code=3, length=0.1, col=2)
#fucus_zone
lines(as.numeric(colnames(mean.sqmeter.fucus_zone)), 
      mean.sqmeter.fucus_zone[2,], pch=16, type="b", col=3)
arrows(x0=as.numeric(colnames(mean.sqmeter.fucus_zone)), 
       x1=as.numeric(colnames(mean.sqmeter.fucus_zone)),
       y0=mean.sqmeter.fucus_zone[2,]-sem.sqmeter.fucus_zone[2,], 
       y1=mean.sqmeter.fucus_zone[2,]+sem.sqmeter.fucus_zone[2,], angle=90, code=3, length=0.1, col=3)
#zostera_zone
lines(as.numeric(colnames(mean.sqmeter.zostera_zone)), 
      mean.sqmeter.zostera_zone[2,], pch=17, type="b", col=4)
arrows(x0=as.numeric(colnames(mean.sqmeter.zostera_zone)), 
       x1=as.numeric(colnames(mean.sqmeter.zostera_zone)),
       y0=mean.sqmeter.zostera_zone[2,]-sem.sqmeter.zostera_zone[2,], 
       y1=mean.sqmeter.zostera_zone[2,]+sem.sqmeter.zostera_zone[2,], angle=90, code=3, length=0.1, col=4)
#low_beatch
lines(as.numeric(colnames(mean.sqmeter.low_beatch)), 
      mean.sqmeter.low_beatch[2,], pch=18, type="b", col=5)
arrows(x0=as.numeric(colnames(mean.sqmeter.low_beatch)), 
       x1=as.numeric(colnames(mean.sqmeter.low_beatch)),
       y0=mean.sqmeter.low_beatch[2,]-sem.sqmeter.low_beatch[2,], 
       y1=mean.sqmeter.low_beatch[2,]+sem.sqmeter.low_beatch[2,], angle=90, code=3, length=0.1, col=5)
legend(legend=levels(ishodnik$tidal_level),x=1999, y=6040, pch=seq(15,18,1), col=seq(2,5,1))
dev.off()
embedFonts("2razrez_N_oneyear.pdf") #встройка шрифтов в файл

#locator()

## корреляция молоди с обилием взрослых на год раньше 
#high_beatch
(N_bez_spata.high_beatch<-mean.sqmeter.high_beatch[2,]+mean.sqmeter.high_beatch[3,])
(N1age.high_beatch<-mean.sqmeter.high_beatch[2,])

plot(N_bez_spata.high_beatch[1:length(N_bez_spata.high_beatch)-1], N1age.high_beatch[2:length(N1age.high_beatch)])
cor.test(N_bez_spata.high_beatch[1:length(N_bez_spata.high_beatch)-1], N1age.high_beatch[2:length(N1age.high_beatch)], method="spearman")
cor.test(N_bez_spata.high_beatch[1:length(N_bez_spata.high_beatch)-1], N1age.high_beatch[2:length(N1age.high_beatch)])

write.table(data.frame(N1age.high_beatch[2:length(N1age.high_beatch)], N_bez_spata.high_beatch[1:length(N_bez_spata.high_beatch)-1]),
            file="2razrez_high_beatch_corr_1_all.csv", sep=";", dec=",")

#fucus_zone
(N_bez_spata.fucus_zone<-mean.sqmeter.fucus_zone[2,]+mean.sqmeter.fucus_zone[3,])
(N1age.fucus_zone<-mean.sqmeter.fucus_zone[2,])

plot(N_bez_spata.fucus_zone[1:length(N_bez_spata.fucus_zone)-1], N1age.fucus_zone[2:length(N1age.fucus_zone)])
cor.test(N_bez_spata.fucus_zone[1:length(N_bez_spata.fucus_zone)-1], N1age.fucus_zone[2:length(N1age.fucus_zone)], method="spearman")
cor.test(N_bez_spata.fucus_zone[1:length(N_bez_spata.fucus_zone)-1], N1age.fucus_zone[2:length(N1age.fucus_zone)])

write.table(data.frame(N1age.fucus_zone[2:length(N1age.fucus_zone)], N_bez_spata.fucus_zone[1:length(N_bez_spata.fucus_zone)-1]),
            file="2razrez_fucus_zone_corr_1_all.csv", sep=";", dec=",")

#zostera_zone
(N_bez_spata.zostera_zone<-mean.sqmeter.zostera_zone[2,]+mean.sqmeter.zostera_zone[3,])
(N1age.zostera_zone<-mean.sqmeter.zostera_zone[2,])

plot(N_bez_spata.zostera_zone[1:length(N_bez_spata.zostera_zone)-1], N1age.zostera_zone[2:length(N1age.zostera_zone)])
cor.test(N_bez_spata.zostera_zone[1:length(N_bez_spata.zostera_zone)-1], N1age.zostera_zone[2:length(N1age.zostera_zone)], method="spearman")
cor.test(N_bez_spata.zostera_zone[1:length(N_bez_spata.zostera_zone)-1], N1age.zostera_zone[2:length(N1age.zostera_zone)])

write.table(data.frame(N1age.zostera_zone[2:length(N1age.zostera_zone)], N_bez_spata.zostera_zone[1:length(N_bez_spata.zostera_zone)-1]),
            file="2razrez_zostera_zone_corr_1_all.csv", sep=";", dec=",")

#low_beatch
(N_bez_spata.low_beatch<-mean.sqmeter.low_beatch[2,]+mean.sqmeter.low_beatch[3,])
(N1age.low_beatch<-mean.sqmeter.low_beatch[2,])

plot(N_bez_spata.low_beatch[1:length(N_bez_spata.low_beatch)-1], N1age.low_beatch[2:length(N1age.low_beatch)])
cor.test(N_bez_spata.low_beatch[1:length(N_bez_spata.low_beatch)-1], N1age.low_beatch[2:length(N1age.low_beatch)], method="spearman")
cor.test(N_bez_spata.low_beatch[1:length(N_bez_spata.low_beatch)-1], N1age.low_beatch[2:length(N1age.low_beatch)])

write.table(data.frame(N1age.low_beatch[2:length(N1age.low_beatch)], N_bez_spata.low_beatch[1:length(N_bez_spata.low_beatch)-1]),
            file="2razrez_low_beatch_corr_1_all.csv", sep=";", dec=",")