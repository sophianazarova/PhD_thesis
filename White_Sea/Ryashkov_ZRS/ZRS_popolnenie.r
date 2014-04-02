setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Ryashkov_ZRS/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Ryashkov_ZRS/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)

# посчитали по данным 2012-2013 годов для ЗРС что размер 1+ от 1,3 до 1,9 мм. 
(oneyear.int<-cut(Length.mm, breaks=c(0.1,1.3,1.9,8.0,max(Length.mm, na.rm=T))))

(oneyear.table<-table(oneyear.int,year,sample))

oneyear.df<-as.data.frame(oneyear.table) # как таблица данных

#убираем те пробы которых на самом деле нету
for (i in 1:length(levels(oneyear.df$year)))
{ (xxx<-oneyear.df$sample[oneyear.df$year==levels(oneyear.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(oneyear.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  oneyear.df$Freq[oneyear.df$year==levels(oneyear.df$year)[i]][antixxx]<-NA
}

#теперь на квадратный метр
oneyear.sqmeter<-oneyear.df
for (i in 1:length(levels(oneyear.sqmeter$year)))
{
  oneyear.sqmeter$Freq[oneyear.sqmeter$year==levels(oneyear.sqmeter$year)[i]]<-
    oneyear.sqmeter$Freq[oneyear.sqmeter$year==levels(oneyear.sqmeter$year)[i]] * 
    samples.squares$square[samples.squares$year==levels(oneyear.sqmeter$year)[i]]
}
str(oneyear.sqmeter)


(mean.oneyear.sqmeter<-t(tapply(oneyear.sqmeter$Freq,INDEX=list(oneyear.sqmeter$year, oneyear.sqmeter$oneyear.int),FUN=mean, na.rm=T)))

(sd.oneyear.sqmeter<-tapply(oneyear.sqmeter$Freq,INDEX=list(oneyear.sqmeter$year, oneyear.sqmeter$oneyear.int),FUN=sd, na.rm=T))

n.samples<-tapply(samples.names$sample,samples.names$year, length )

(sem.oneyear.sqmeter <-t(sd.oneyear.sqmeter/sqrt(as.vector(n.samples))))

#пишем численность молоди в файл
write.table(mean.oneyear.sqmeter[2,], file="Nmean_1year.csv",sep=";", dec=",")
#пишем численности в файл
write.table(mean.oneyear.sqmeter, file="Nmean_all.csv",sep=";", dec=",")

#рисуем график 
pdf(file="ZRS_N_oneyear.pdf", family="NimbusSan") # указываем шрифт подпией
plot(seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
     mean.oneyear.sqmeter[2,], pch=15, type="b", xlab="год", ylab="N1+, экз./кв.м", main="Западная Ряшкова салма",
     ylim=c(0, max(mean.oneyear.sqmeter[2,], mean.oneyear.sqmeter[2,])+max(sem.oneyear.sqmeter[2,], sem.oneyear.sqmeter[2,])))
arrows(x0=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1),
       y0=mean.oneyear.sqmeter[2,]-sem.oneyear.sqmeter[2,], 
       y1=mean.oneyear.sqmeter[2,]+sem.oneyear.sqmeter[2,], angle=90, code=3, length=0.1)
dev.off()
embedFonts("ZRS_N_oneyear.pdf") #встройка шрифтов в файл


## корреляция молоди с обилием взрослых на год раньше 
(N_bez_spata<-mean.oneyear.sqmeter[2,]+mean.oneyear.sqmeter[3,])
(N1age<-mean.oneyear.sqmeter[2,])

plot(N_bez_spata[1:length(N_bez_spata)-1], N1age[2:length(N1age)])
cor.test(N_bez_spata[1:length(N_bez_spata)-1], N1age[2:length(N1age)], method="spearman")
cor.test(N_bez_spata[1:length(N_bez_spata)-1], N1age[2:length(N1age)])

#пишем в файл
write.table(data.frame(N1age[2:length(N1age)], N_bez_spata[1:length(N_bez_spata)-1]),file="zrs_corr_1_all.csv", sep=";", dec=",")

#рисуем график N1+ & N>8мм(=половозрелые)
pdf(file="ZRS_N1y_N8mm.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.oneyear.sqmeter[2,], x=colnames(mean.oneyear.sqmeter),pch=15, type="n", main="Западная Ряшкова салма", 
     ylim=c(0, 
            max(mean.oneyear.sqmeter[2,], mean.oneyear.sqmeter[4,])+max(sem.oneyear.sqmeter[2,], sem.oneyear.sqmeter[4,])),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
      mean.oneyear.sqmeter[2,], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1),
       y0=mean.oneyear.sqmeter[2,]-sem.oneyear.sqmeter[2,], 
       y1=mean.oneyear.sqmeter[2,]+sem.oneyear.sqmeter[2,], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
      mean.oneyear.sqmeter[4,], pch=15, type="b", col=4)
arrows(x0=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1),
       y0=mean.oneyear.sqmeter[4,]-sem.oneyear.sqmeter[4,], 
       y1=mean.oneyear.sqmeter[4,]+sem.oneyear.sqmeter[4,], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("ZRS_N1y_N8mm.pdf") #встройка шрифтов в файл

#считаем среднюю численность и ошибки особей старше 1+
str(oneyear.sqmeter)
levels(oneyear.sqmeter$oneyear.int)
(old.sqmeter<-subset(oneyear.sqmeter, subset=oneyear.sqmeter$oneyear.int==c("(1.9,8]","(8,19.6]")))
(N.old.sqmeter<-t(tapply(old.sqmeter$Freq,INDEX=list(old.sqmeter$year, old.sqmeter$sample),FUN=sum, na.rm=T)))
N.old.sqmeter[N.old.sqmeter==0]<-NA

(mean.old.sqmeter<-apply(N.old.sqmeter, 2, FUN=mean, na.rm=T))

(sd.old.sqmeter<-apply(N.old.sqmeter, 2, FUN=sd, na.rm=T))

n.samples<-tapply(samples.names$sample,samples.names$year, length )

(sem.old.sqmeter <-t(sd.old.sqmeter/sqrt(as.vector(n.samples))))

#пишем в файл численность старых
write.table(mean.old.sqmeter, file="Nold_mean.csv", dec=",", sep=";")

#рисуем график N1+ & N>1+
pdf(file="ZRS_N1_Nold.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.oneyear.sqmeter[2,], x=colnames(mean.oneyear.sqmeter),pch=15, type="n", main="Западная Ряшкова салма", 
     ylim=c(0, 
            max(mean.oneyear.sqmeter[2,], mean.old.sqmeter, na.rm=T)+max(sem.oneyear.sqmeter[2,], sem.old.sqmeter, na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
      mean.oneyear.sqmeter[2,], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1),
       y0=mean.oneyear.sqmeter[2,]-sem.oneyear.sqmeter[2,], 
       y1=mean.oneyear.sqmeter[2,]+sem.oneyear.sqmeter[2,], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
      mean.old.sqmeter, pch=15, type="b", col=4)
arrows(x0=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.oneyear.sqmeter))),as.numeric(max(colnames(mean.oneyear.sqmeter))),1),
       y0=mean.old.sqmeter-sem.old.sqmeter, 
       y1=mean.old.sqmeter+sem.old.sqmeter, angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("ZRS_N1_Nold.pdf") #встройка шрифтов в файл
