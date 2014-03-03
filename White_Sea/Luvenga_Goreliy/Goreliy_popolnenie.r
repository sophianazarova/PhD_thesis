setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Luvenga_Goreliy/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Luvenga_Goreliy//")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)

(oneyear.int<-cut(Length.mm, breaks=c(0.1,1.2,1.8,max(Length.mm, na.rm=T))))

(oneyear.table<-table(oneyear.int,year, tidal_level, sample))

oneyear.df<-as.data.frame(oneyear.table) # как таблица данных

#убираем те пробы которых на самом деле нету

for (i in 1:length(levels(oneyear.df$year))){
  for(j in 1:length(levels(oneyear.df$tidal_level))){
    (xxx<-oneyear.df$sample[oneyear.df$year==levels(oneyear.df$year)[i] & 
                               oneyear.df$tidal_level==levels(oneyear.df$tidal_level)[j]]%in%
       samples.names$sample[samples.names$year==levels(oneyear.df$year)[i] & 
                              samples.names$tidal_level==levels(oneyear.df$tidal_level)[j]])
    antixxx<-as.logical(1-xxx)
    oneyear.df$Freq[oneyear.df$year==levels(oneyear.df$year)[i] & 
                       oneyear.df$tidal_level==levels(oneyear.df$tidal_level)[j]][antixxx]<-NA
  }}



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
  write.table(assign(paste(levels(oneyear.sqmeter$tidal_level)[i]), 
                     subset(oneyear.sqmeter, oneyear.sqmeter$tidal_level==levels(oneyear.sqmeter$tidal_level)[i])), file=paste(levels(oneyear.sqmeter$tidal_level)[i]), sep=",")
}



(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal_level), length ))
(n.samples.df<-as.data.frame(n.samples))

#верхний горизонт
(mean.sqmeter.high<-t(tapply(high$Freq,INDEX=list(high$year,  high$oneyear.int),FUN=mean, na.rm=T)))
mean.sqmeter.high.df<-as.data.frame(mean.sqmeter.high)

(sd.sqmeter.high<-tapply(high$Freq,INDEX=list(high$year,  high$oneyear.int),FUN=sd, na.rm=T))

(sem.sqmeter.high <-t(sd.sqmeter.high/sqrt(n.samples.df$high)))
#TODO надо те строки где по одной пробе сделать вместо ошибки NA!!
sem.sqmeter.high.df<-as.data.frame(sem.sqmeter.high)
sem.sqmeter.high[sem.sqmeter.high==t(sd.sqmeter.high)]<-0


#средний горизонт
mean.sqmeter.middle<-t(tapply(middle$Freq,INDEX=list(middle$year,  middle$oneyear.int),FUN=mean, na.rm=T))
mean.sqmeter.middle.df<-as.data.frame(mean.sqmeter.middle)

sd.sqmeter.middle<-tapply(middle$Freq,INDEX=list(middle$year,  middle$oneyear.int),FUN=sd, na.rm=T)

sem.sqmeter.middle <-t(sd.sqmeter.middle/sqrt(n.samples.df$middle))
sem.sqmeter.middle.df<-as.data.frame(sem.sqmeter.middle)
sem.sqmeter.middle[sem.sqmeter.middle==t(sd.sqmeter.middle)]<-0

#граница среднего и нижнего горизонта, в фукусах
mean.sqmeter.midlow<-t(tapply(midlow$Freq,INDEX=list(midlow$year,  midlow$oneyear.int),FUN=mean, na.rm=T))
mean.sqmeter.midlow.df<-as.data.frame(mean.sqmeter.midlow)

sd.sqmeter.midlow<-tapply(midlow$Freq,INDEX=list(midlow$year,  midlow$oneyear.int),FUN=sd, na.rm=T)

sem.sqmeter.midlow <-t(sd.sqmeter.midlow/sqrt(n.samples.df$midlow))
sem.sqmeter.midlow.df<-as.data.frame(sem.sqmeter.midlow)
sem.sqmeter.midlow[sem.sqmeter.midlow==t(sd.sqmeter.midlow)]<-0

#нижний горизонт, у нуля глубин
(mean.sqmeter.low<-t(tapply(low$Freq,INDEX=list(low$year,  low$oneyear.int),FUN=mean, na.rm=T)))
mean.sqmeter.low.df<-as.data.frame(mean.sqmeter.low)

sd.sqmeter.low<-tapply(low$Freq,INDEX=list(low$year,  low$oneyear.int),FUN=sd, na.rm=T)

sem.sqmeter.low <-t(sd.sqmeter.low/sqrt(n.samples.df$low))
sem.sqmeter.low.df<-as.data.frame(sem.sqmeter.low)
sem.sqmeter.low[sem.sqmeter.low==t(sd.sqmeter.low)]<-0

#рисуем график

pdf(file="Goreliy_N_oneyear.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.sqmeter.high[2,], x=as.numeric(colnames(mean.sqmeter.high)),pch=15, type="n", 
     main="о. Горелый",
     ylim=c(0, 5000),
           # max(mean.sqmeter.high[2,], 
           # mean.sqmeter.middle[2,],
           # mean.sqmeter.midlow[2,], 
           # mean.sqmeter.low[2,], na.rm=T)),
     xlab="год", ylab="N1+, экз./кв.м")
#high
lines(as.numeric(colnames(mean.sqmeter.high)), 
      mean.sqmeter.high[2,], pch=15, type="b", col=2)
arrows(x0=as.numeric(colnames(mean.sqmeter.high)), 
       x1=as.numeric(colnames(mean.sqmeter.high)),
       y0=mean.sqmeter.high[2,]-sem.sqmeter.high[2,], 
       y1=mean.sqmeter.high[2,]+sem.sqmeter.high[2,], angle=90, code=3, length=0.1, col=2)
#middle
lines(as.numeric(colnames(mean.sqmeter.middle)), 
      mean.sqmeter.middle[2,], pch=16, type="b", col=3)
arrows(x0=as.numeric(colnames(mean.sqmeter.middle)), 
       x1=as.numeric(colnames(mean.sqmeter.middle)),
       y0=mean.sqmeter.middle[2,]-sem.sqmeter.middle[2,], 
       y1=mean.sqmeter.middle[2,]+sem.sqmeter.middle[2,], angle=90, code=3, length=0.1, col=3)
#midlow
lines(as.numeric(colnames(mean.sqmeter.midlow)), 
      mean.sqmeter.midlow[2,], pch=17, type="b", col=4)
arrows(x0=as.numeric(colnames(mean.sqmeter.midlow)), 
       x1=as.numeric(colnames(mean.sqmeter.midlow)),
       y0=mean.sqmeter.midlow[2,]-sem.sqmeter.midlow[2,], 
       y1=mean.sqmeter.midlow[2,]+sem.sqmeter.midlow[2,], angle=90, code=3, length=0.1, col=4)
#low
lines(as.numeric(colnames(mean.sqmeter.low)), 
      mean.sqmeter.low[2,], pch=18, type="b", col=5)
arrows(x0=as.numeric(colnames(mean.sqmeter.low)), 
       x1=as.numeric(colnames(mean.sqmeter.low)),
       y0=mean.sqmeter.low[2,]-sem.sqmeter.low[2,], 
       y1=mean.sqmeter.low[2,]+sem.sqmeter.low[2,], angle=90, code=3, length=0.1, col=5)
legend(legend=levels(ishodnik$tidal_level),x=2001, y=4780, pch=seq(15,18,1), col=seq(2,5,1))
dev.off()
embedFonts("Goreliy_N_oneyear.pdf") #встройка шрифтов в файл

#locator()

## корреляция молоди с обилием взрослых на год раньше 
#high
(N_bez_spata.high<-mean.sqmeter.high[2,]+mean.sqmeter.high[3,])
(N1age.high<-mean.sqmeter.high[2,])

plot(N_bez_spata.high[1:length(N_bez_spata.high)-1], N1age.high[2:length(N1age.high)])
cor.test(N_bez_spata.high[1:length(N_bez_spata.high)-1], N1age.high[2:length(N1age.high)], method="spearman")
cor.test(N_bez_spata.high[1:length(N_bez_spata.high)-1], N1age.high[2:length(N1age.high)])

write.table(data.frame(N1age.high[2:length(N1age.high)], N_bez_spata.high[1:length(N_bez_spata.high)-1]),
            file="goreliy_high_corr_1_all.csv", sep=";", dec=",")

#middle
(N_bez_spata.middle<-mean.sqmeter.middle[2,]+mean.sqmeter.middle[3,])
(N1age.middle<-mean.sqmeter.middle[2,])

plot(N_bez_spata.middle[1:length(N_bez_spata.middle)-1], N1age.middle[2:length(N1age.middle)])
cor.test(N_bez_spata.middle[1:length(N_bez_spata.middle)-1], N1age.middle[2:length(N1age.middle)], method="spearman")
cor.test(N_bez_spata.middle[1:length(N_bez_spata.middle)-1], N1age.middle[2:length(N1age.middle)])

write.table(data.frame(N1age.middle[2:length(N1age.middle)], N_bez_spata.middle[1:length(N_bez_spata.middle)-1]),
            file="goreliy_middle_corr_1_all.csv", sep=";", dec=",")

#midlow
(N_bez_spata.midlow<-mean.sqmeter.midlow[2,]+mean.sqmeter.midlow[3,])
(N1age.midlow<-mean.sqmeter.midlow[2,])

plot(N_bez_spata.midlow[1:length(N_bez_spata.midlow)-1], N1age.midlow[2:length(N1age.midlow)])
cor.test(N_bez_spata.midlow[1:length(N_bez_spata.midlow)-1], N1age.midlow[2:length(N1age.midlow)], method="spearman")
cor.test(N_bez_spata.midlow[1:length(N_bez_spata.midlow)-1], N1age.midlow[2:length(N1age.midlow)])

write.table(data.frame(N1age.midlow[2:length(N1age.midlow)], N_bez_spata.midlow[1:length(N_bez_spata.midlow)-1]),
            file="goreliy_midlow_corr_1_all.csv", sep=";", dec=",")

#low
(N_bez_spata.low<-mean.sqmeter.low[2,]+mean.sqmeter.low[3,])
(N1age.low<-mean.sqmeter.low[2,])

plot(N_bez_spata.low[1:length(N_bez_spata.low)-1], N1age.low[2:length(N1age.low)])
cor.test(N_bez_spata.low[1:length(N_bez_spata.low)-1], N1age.low[2:length(N1age.low)], method="spearman")
cor.test(N_bez_spata.low[1:length(N_bez_spata.low)-1], N1age.low[2:length(N1age.low)])

write.table(data.frame(N1age.low[2:length(N1age.low)], N_bez_spata.low[1:length(N_bez_spata.low)-1]),
            file="goreliy_low_corr_1_all.csv", sep=";", dec=",")