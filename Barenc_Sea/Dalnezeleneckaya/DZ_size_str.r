setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/Dalnezeleneckaya/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Estuatiy_Luvenga")


ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
attach(ishodnik)

## размерная структура суммарно по годам по горизонтам
Length.int<-cut(Length.mm, breaks=seq(0,20,1))

(size.str.table<-table(Length.int,year,sample))

size.str.df<-as.data.frame(size.str.table) # как таблица данных

#убираем те пробы которых на самом деле нету
for (i in 1:length(levels(size.str.df$year)))
{ (xxx<-size.str.df$sample[size.str.df$year==levels(size.str.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(size.str.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  size.str.df$Freq[size.str.df$year==levels(size.str.df$year)[i]][antixxx]<-NA
}


# SUBSET - для фильтрации таблицы данных
# APPLY - кто-то из них для средней и СД по фрейму

#теперь на квадратный метр
size.str.sqmeter<-size.str.df
for (i in 1:length(levels(size.str.sqmeter$year)))
{
  size.str.sqmeter$Freq[size.str.sqmeter$year==levels(size.str.sqmeter$year)[i]]<-size.str.sqmeter$Freq[size.str.sqmeter$year==levels(size.str.sqmeter$year)[i]] * samples.squares$square[samples.squares$year==levels(size.str.sqmeter$year)[i]]
}

n.samples<-tapply(samples.names$sample,samples.names$year, length )

#subset(size.str.sqmeter,subset=size.str.sqmeter$year=="1992")

#и среднее??
# tapply выдает как резудьтат матрицу
(mean.sizestr.sqmeter<-t(tapply(size.str.sqmeter$Freq,INDEX=list(size.str.sqmeter$year, size.str.sqmeter$Length.int),FUN=mean, na.rm=T)))
mean.sqmeter.df<-as.data.frame(mean.sizestr.sqmeter)
(sd.sizestr.sqmeter<-tapply(size.str.sqmeter$Freq,INDEX=list(size.str.sqmeter$year, size.str.sqmeter$Length.int),FUN=sd, na.rm=T))

n.samples<-tapply(samples.names$sample,samples.names$year, length )

(sem.sizestr.sqmeter <-t(sd.sizestr.sqmeter/sqrt(as.vector(n.samples))))
sem.sqmeter.df<-as.data.frame(sem.sizestr.sqmeter)

##>2mm mean size structure
(mean.sizestr.sqmeter2<-mean.sizestr.sqmeter[2:20,])
mean.sqmeter.df2<-as.data.frame(mean.sizestr.sqmeter2)
(sd.sizestr.sqmeter2<-sd.sizestr.sqmeter[,2:20])

n.samples<-tapply(samples.names$sample,samples.names$year, length )

(sem.sizestr.sqmeter2<-t(sd.sizestr.sqmeter2/sqrt(as.vector(n.samples))))
sem.sqmeter.df2<-as.data.frame(sem.sizestr.sqmeter2)


length.class<-seq(1,20,1)
length.class2<-seq(2,20,1)


## size structure plot
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


for (j in 1:length(colnames(mean.sizestr.sqmeter)))
{
  pdf(file=paste("DZ", colnames(mean.sizestr.sqmeter)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sizestr.sqmeter[,j], nn=length.class, z=sem.sizestr.sqmeter[,j])
  title(main=colnames(mean.sizestr.sqmeter)[j], xlab="", ylab="")
  dev.off()
}

#>2mm
for (j in 1:length(colnames(mean.sizestr.sqmeter2)))
{
  pdf(file=paste("DZ2", colnames(mean.sizestr.sqmeter2)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sizestr.sqmeter2[,j], nn=length.class2, z=sem.sizestr.sqmeter2[,j])
  title(main=colnames(mean.sizestr.sqmeter2)[j], xlab="", ylab="")
  dev.off()
}

##динамика обилия
(N.sqmeter<-(t(tapply(size.str.sqmeter$Freq, list(size.str.sqmeter$year, size.str.sqmeter$sample), sum))))
(N.mean.sqmeter<-colMeans(N.sqmeter, na.rm=T))
(N.sd.sqmeter<-apply(N.sqmeter, 2, sd, na.rm=T))
(N.sem.sqmeter<-N.sd.sqmeter/sqrt(n.samples))

#точность учета
(D.n<-N.sem.sqmeter/N.mean.sqmeter*100)

pdf(file="N_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N.mean.sqmeter, x=names(N.mean.sqmeter),pch=15, main="Эстуарий р. Лувеньги", 
     ylim=c(min(N.mean.sqmeter)-max(N.sem.sqmeter), max(N.mean.sqmeter)+max(N.sem.sqmeter)),
     xlab="год", ylab="N, экз./кв.м")
lines(seq(as.numeric(min(names(N.mean.sqmeter))),as.numeric(max(names(N.mean.sqmeter))),1), N.mean.sqmeter, pch=1, type="b")
arrows(x0=seq(as.numeric(min(names(N.mean.sqmeter))),as.numeric(max(names(N.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(N.mean.sqmeter))),as.numeric(max(names(N.mean.sqmeter))),1),
       y0=N.mean.sqmeter-N.sem.sqmeter, y1=N.mean.sqmeter+N.sem.sqmeter, angle=90, code=3, length=0.1)
dev.off()
embedFonts("N_dynamic.pdf") #встройка шрифтов в файл

##динамика без молод ( больше 2+)
(N2.sqmeter<-(t(tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"], 
                       list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"],
                            size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]"]), sum))))
(N2.mean.sqmeter<-colMeans(N2.sqmeter, na.rm=T))
N2.sd.sqmeter<-apply(N2.sqmeter, 2, sd, na.rm=T)
N2.sem.sqmeter<-N2.sd.sqmeter/sqrt(n.samples)
#точность учета
(D.n2<-N2.sem.sqmeter/N2.mean.sqmeter*100)

write.table(N2.mean.sqmeter, file="DZ_N2.csv", sep=";", dec=",")

pdf(file="N2_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter, x=names(N2.mean.sqmeter),pch=15, main="Эстуарий р. Лувеньги", 
     ylim=c(min(N2.mean.sqmeter)-max(N2.sem.sqmeter), max(N2.mean.sqmeter)+max(N2.sem.sqmeter)),
     xlab="год", ylab="N, экз./кв.м")
lines(seq(as.numeric(min(names(N2.mean.sqmeter))),as.numeric(max(names(N2.mean.sqmeter))),1), N2.mean.sqmeter, pch=1, type="b")
arrows(x0=seq(as.numeric(min(names(N2.mean.sqmeter))),as.numeric(max(names(N2.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(N2.mean.sqmeter))),as.numeric(max(names(N2.mean.sqmeter))),1),
       y0=N2.mean.sqmeter-N2.sem.sqmeter, y1=N2.mean.sqmeter+N2.sem.sqmeter, angle=90, code=3, length=0.1)
dev.off()
embedFonts("N2_dynamic.pdf") #встройка шрифтов в файл

##рассчетная биомасса по Максимовичу и др., 1993
biomass.count<-0.00016*(Length.mm^2.96)
(biomass.samples<-tapply(biomass.count, list(year, sample), sum, na.rm=T))

(biomass.sqmeter<-biomass.samples*samples.squares$square)

(B.mean.sqmeter<-rowMeans(biomass.sqmeter, na.rm=T))
(B.sd.sqmeter<-apply(biomass.sqmeter, 1, sd, na.rm=T))
n.samples<-tapply(samples.names$sample,samples.names$year, length )
(B.sem.sqmeter<-B.sd.sqmeter/sqrt(n.samples))
(D.b<-B.sem.sqmeter/B.mean.sqmeter*100)

pdf(file="B_count_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter, x=names(B.mean.sqmeter),pch=15, main="Эстуарий р. Лувеньги", 
     ylim=c(min(B.mean.sqmeter)-max(B.sem.sqmeter), max(B.mean.sqmeter)+max(B.sem.sqmeter)),
     xlab="год", ylab="B, г/кв.м")
lines(seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1), B.mean.sqmeter, pch=1, type="b")
arrows(x0=seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1),
       y0=B.mean.sqmeter-B.sem.sqmeter, y1=B.mean.sqmeter+B.sem.sqmeter, angle=90, code=3, length=0.1)
dev.off()
embedFonts("B_count_dynamic.pdf") #встройка шрифтов в файл