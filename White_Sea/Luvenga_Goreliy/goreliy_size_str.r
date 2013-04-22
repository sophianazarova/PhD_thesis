
setwd("~/Dropbox/PhD_thesis/White_Sea/Luvenga_Goreliy//")
setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Luvenga_Goreliy//")

# размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)
#year<-factor(year)

Length.int<-cut(Length.mm, breaks=seq(0,20,1))

(size.str.table<-table(Length.int,year, tidal_level, sample))

size.str.df<-as.data.frame(size.str.table) # как таблица данных

#убираем те пробы которых на самом деле нету
for (i in 1:length(levels(size.str.df$year)))
{ (xxx<-size.str.df$sample[size.str.df$year==levels(size.str.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(size.str.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  size.str.df$Freq[size.str.df$year==levels(size.str.df$year)[i]][antixxx]<-NA
}

subset(size.str.df, size.str.df$year=="1995" & size.str.df$sample=="mg4")
# SUBSET - для фильтрации таблицы данных
# APPLY - кто-то из них для средней и СД по фрейму

#теперь на квадратный метр
size.str.sqmeter<-size.str.df
for (i in 1:length(levels(size.str.sqmeter$year)))
{
  size.str.sqmeter$Freq[size.str.sqmeter$year==levels(size.str.sqmeter$year)[i]]<-
    size.str.sqmeter$Freq[size.str.sqmeter$year==levels(size.str.sqmeter$year)[i]] * 
    samples.squares$square[samples.squares$year==levels(size.str.sqmeter$year)[i]]
}

for (i in 1: length(levels(size.str.sqmeter$tidal_level)))
{
  write.table(assign(paste(levels(size.str.sqmeter$tidal_level)[i]), 
                     subset(size.str.sqmeter, size.str.sqmeter$tidal_level==levels(size.str.sqmeter$tidal_level)[i])), file=paste(levels(size.str.sqmeter$tidal_level)[i]), sep=",")
}


#subset(size.str.sqmeter,subset=size.str.sqmeter$year=="1992")

#и среднее??
# tapply выдает как резудьтат матрицу

# надо как-то дружить array's в момент рассчета ошибок... и как потом рисовать картики? или проще разделить на отдельные горизонтв и поработать с ними?? 

#attach(paste(levels(size.str.sqmeter$tidal_level)[1]))

#
#assign(paste("mean.sqmeter", levels(size.str.sqmeter$tidal_level)[1], sep="."), 
#       tapply(paste(levels(size.str.sqmeter$tidal_level)[1])$Freq, 
#              INDEX=list(levels(size.str.sqmeter$tidal_level)[1]$year, levels(size.str.sqmeter$tidal_level)[1]$Length.int)), 
#              FUN=mean, na.rm=T))
#КАК АВТОМАТИЗИРОВАТЬ ТЕМУ ПРО НЕСКОЛЬКО УЧАСТКОВ Я НЕ ПРИДУМАЛА :( ПИШУ РУЧКАМИ 4 РАЗА...

(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal_level), length ))
(n.samples.df<-as.data.frame(n.samples))

#верхний горизонт
(mean.sqmeter.high<-t(tapply(high$Freq,INDEX=list(high$year,  high$Length.int),FUN=sd, na.rm=T)))
mean.sqmeter.high.df<-as.data.frame(mean.sqmeter.high)

(sd.sqmeter.high<-tapply(high$Freq,INDEX=list(high$year,  high$Length.int),FUN=sd, na.rm=T))

(sem.sqmeter.high <-t(sd.sqmeter.high/sqrt(n.samples.df$high)))
#TODO надо те строки где по одной пробе сделать вместо ошибки NA!!
sem.sqmeter.high.df<-as.data.frame(sem.sqmeter.high)
sem.sqmeter.high[sem.sqmeter.high==mean.sqmeter.high]<-0


#средний горизонт
mean.sqmeter.middle<-t(tapply(middle$Freq,INDEX=list(middle$year,  middle$Length.int),FUN=sd, na.rm=T))
mean.sqmeter.middle.df<-as.data.frame(mean.sqmeter.middle)

sd.sqmeter.middle<-tapply(middle$Freq,INDEX=list(middle$year,  middle$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.middle <-t(sd.sqmeter.middle/sqrt(n.samples.df$middle))
sem.sqmeter.middle.df<-as.data.frame(sem.sqmeter.middle)
sem.sqmeter.middle[sem.sqmeter.middle==mean.sqmeter.middle]<-0

#граница среднего и нижнего горизонта, в фукусах
mean.sqmeter.midlow<-t(tapply(midlow$Freq,INDEX=list(midlow$year,  midlow$Length.int),FUN=sd, na.rm=T))
mean.sqmeter.midlow.df<-as.data.frame(mean.sqmeter.midlow)

sd.sqmeter.midlow<-tapply(midlow$Freq,INDEX=list(midlow$year,  midlow$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.midlow <-t(sd.sqmeter.midlow/sqrt(n.samples.df$midlow))
sem.sqmeter.midlow.df<-as.data.frame(sem.sqmeter.midlow)
sem.sqmeter.midlow[sem.sqmeter.midlow==mean.sqmeter.midlow]<-0

#нижний горизонт, у нуля глубин
(mean.sqmeter.low<-t(tapply(low$Freq,INDEX=list(low$year,  low$Length.int),FUN=sd, na.rm=T)))
mean.sqmeter.low.df<-as.data.frame(mean.sqmeter.low)

sd.sqmeter.low<-tapply(low$Freq,INDEX=list(low$year,  low$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.low <-t(sd.sqmeter.low/sqrt(n.samples.df$low))
sem.sqmeter.low.df<-as.data.frame(sem.sqmeter.low)
sem.sqmeter.low[sem.sqmeter.low==mean.sqmeter.low]<-0

length.class<-seq(1,20,1)

#size structure >1mm

#верхний горизонт
(mean.sqmeter2.high<-mean.sqmeter.middle[2:20,])
mean.sqmeter2.high.df<-as.data.frame(mean.sqmeter2.high)

(sd.sqmeter2.high<-sd.sqmeter.middle[,2:20])

(sem.sqmeter2.high <-t(sd.sqmeter2.high/sqrt(n.samples.df$high)))
#TODO надо те строки где по одной пробе сделать вместо ошибки NA!!
sem.sqmeter2.high.df<-as.data.frame(sem.sqmeter2.high)
sem.sqmeter2.high[sem.sqmeter2.high==mean.sqmeter2.high]<-0


#средний горизонт
mean.sqmeter2.middle<-mean.sqmeter.middle[2:20,]
mean.sqmeter2.middle.df<-as.data.frame(mean.sqmeter2.middle)

sd.sqmeter2.middle<-sd.sqmeter.middle[,2:20]

sem.sqmeter2.middle <-t(sd.sqmeter2.middle/sqrt(n.samples.df$middle))
sem.sqmeter2.middle.df<-as.data.frame(sem.sqmeter2.middle)
sem.sqmeter2.middle[sem.sqmeter2.middle==mean.sqmeter2.middle]<-0

#граница среднего и нижнего горизонта, в фукусах
mean.sqmeter2.midlow<-mean.sqmeter.middle[2:20,]
mean.sqmeter2.midlow.df<-as.data.frame(mean.sqmeter2.midlow)

sd.sqmeter2.midlow<-sd.sqmeter.middle[,2:20]

sem.sqmeter2.midlow <-t(sd.sqmeter2.midlow/sqrt(n.samples.df$midlow))
sem.sqmeter2.midlow.df<-as.data.frame(sem.sqmeter2.midlow)
sem.sqmeter2.midlow[sem.sqmeter2.midlow==mean.sqmeter2.midlow]<-0

#нижний горизонт, у нуля глубин
mean.sqmeter2.low<-mean.sqmeter.middle[2:20,]
mean.sqmeter2.low.df<-as.data.frame(mean.sqmeter2.low)

sd.sqmeter2.low<-sd.sqmeter.middle[,2:20]

sem.sqmeter2.low <-t(sd.sqmeter2.low/sqrt(n.samples.df$low))
sem.sqmeter2.low.df<-as.data.frame(sem.sqmeter2.low)
sem.sqmeter2.low[sem.sqmeter2.low==mean.sqmeter2.low]<-0

length.class2<-seq(2,20,1)


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


#верхний горизонт
for (j in 1:length(colnames(mean.sqmeter.high)))
{
  pdf(file=paste("high", colnames(mean.sqmeter.high)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.high[,j], nn=length.class,  z=sem.sqmeter.high[,j])
  title(main=colnames(mean.sqmeter.high)[j], xlab="", ylab="")
  dev.off()
}

#средний горизонт
for (j in 1:length(colnames(mean.sqmeter.middle)))
{
  pdf(file=paste("middle", colnames(mean.sqmeter.middle)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.middle[,j], nn=length.class,  z=sem.sqmeter.middle[,j])
  title(main=colnames(mean.sqmeter.middle)[j], xlab="", ylab="")
  dev.off()
}

#граница среднего и нижнего горизонтов, в фукоидах
for (j in 1:length(colnames(mean.sqmeter.midlow)))
{
  pdf(file=paste("midlow", colnames(mean.sqmeter.midlow)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.midlow[,j], nn=length.class,  z=sem.sqmeter.midlow[,j])
  title(main=colnames(mean.sqmeter.midlow)[j], xlab="", ylab="")
  dev.off()
}

#нижний горизонт, у нуля глубин
for (j in 1:length(colnames(mean.sqmeter.low)))
{
  pdf(file=paste("low", colnames(mean.sqmeter.low)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.low[,j], nn=length.class,  z=sem.sqmeter.low[,j])
  title(main=colnames(mean.sqmeter.low)[j], xlab="", ylab="")
  dev.off()
}



# все 4 на одном графике
for (j in 1:length(colnames(mean.sqmeter.low_beatch)))
{
  pdf(file=paste("all_tidal", colnames(mean.sqmeter.low_beatch)[j], ".pdf",sep="_"))
  error.bars(yv=matrix(mean.sqmeter.high_beatch[,j], mean.sqmeter.fucus_zone[,j], 
                       mean.sqmeter.zostera_zone[,j],mean.sqmeter.low_beatch[,j]), 
             nn=length.class,  
             z=sem.sqmeter.low_beatch[,j])
  title(main=colnames(mean.sqmeter.low_beatch)[j], xlab="", ylab="")
  dev.off()
}


barplot(matrix(mean.sqmeter.high_beatch[,1], mean.sqmeter.fucus_zone[,1], 
               mean.sqmeter.zostera_zone[,1],mean.sqmeter.low_beatch[,1]))


#>1mm
#верхний горизонт
for (j in 1:length(colnames(mean.sqmeter2.high)))
{
  pdf(file=paste("high2", colnames(mean.sqmeter2.high)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter2.high[,j], nn=length.class2,  z=sem.sqmeter2.high[,j])
  title(main=colnames(mean.sqmeter2.high)[j], xlab="", ylab="")
  dev.off()
}

#средний горизонт
for (j in 1:length(colnames(mean.sqmeter2.middle)))
{
  pdf(file=paste("middle2", colnames(mean.sqmeter2.middle)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter2.middle[,j], nn=length.class2,  z=sem.sqmeter2.middle[,j])
  title(main=colnames(mean.sqmeter2.middle)[j], xlab="", ylab="")
  dev.off()
}

#граница среднего и нижнего горизонтов, в фукоидах
for (j in 1:length(colnames(mean.sqmeter2.midlow)))
{
  pdf(file=paste("midlow2", colnames(mean.sqmeter2.midlow)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter2.midlow[,j], nn=length.class2,  z=sem.sqmeter2.midlow[,j])
  title(main=colnames(mean.sqmeter2.midlow)[j], xlab="", ylab="")
  dev.off()
}

#нижний горизонт, у нуля глубин
for (j in 1:length(colnames(mean.sqmeter2.low)))
{
  pdf(file=paste("low2", colnames(mean.sqmeter2.low)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter2.low[,j], nn=length.class2,  z=sem.sqmeter2.low[,j])
  title(main=colnames(mean.sqmeter2.low)[j], xlab="", ylab="")
  dev.off()
}



#динамика обилия
(N.sqmeter<-tapply(size.str.sqmeter$Freq, list(size.str.sqmeter$year, size.str.sqmeter$sample, size.str.df$tidal_level), sum))
(N.mean.sqmeter<-apply(N.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=mean))
N.mean.sqmeter[11,3]<-NA
(N.sd.sqmeter<-apply(N.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=sd))
(N.sem.sqmeter<-N.sd.sqmeter/sqrt(n.samples))

pdf(file="N_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N.mean.sqmeter[,1], x=as.numeric(rownames(N.mean.sqmeter)), type="n", main="о. Горелый (Лувеньгские шхеры)",
     #     ylim=c(min(N.mean.sqmeter, na.rm=T)-max(N.sem.sqmeter, na.rm=T), max(N.mean.sqmeter, na.rm=T)+max(N.sem.sqmeter, na.rm=T)),
     ylim=c(0, max(N.mean.sqmeter, na.rm=T)+max(N.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="N, экз./кв.м")
for (i in 1:ncol(N.mean.sqmeter))
{lines(as.numeric(rownames(N.mean.sqmeter)), N.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(N.mean.sqmeter)), x1=as.numeric(rownames(N.mean.sqmeter)),
        y0=N.mean.sqmeter[,i]-N.sem.sqmeter[,i], y1=N.mean.sqmeter[,i]+N.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}
legend(legend=colnames(N.mean.sqmeter),x=2005, y=31900, pch=seq(15,15+ncol(N.mean.sqmeter),1), col=seq(1,1+ncol(N.mean.sqmeter),1))
dev.off()
embedFonts("N_dynamic.pdf") #встройка шрифтов в файл


#динамика без молод ( больше 2+)
(N2.sqmeter<-tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"], 
                    list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"],
                         size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]"],
                         size.str.df$tidal_level[size.str.sqmeter$Length.int!="(0,1]"]), sum))
(N2.mean.sqmeter<-apply(N2.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=mean))
2N2.mean.sqmeter[11,3]<-NA
(N2.sd.sqmeter<-apply(N2.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=sd))
(N2.sem.sqmeter<-N2.sd.sqmeter/sqrt(n.samples))

pdf(file="N2_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter[,1], x=as.numeric(rownames(N2.mean.sqmeter)), type="n", main="о. Горелый (Лувеньгские шхеры)",
     #     ylim=c(min(N2.mean.sqmeter, na.rm=T)-max(N2.sem.sqmeter, na.rm=T), max(N2.mean.sqmeter, na.rm=T)+max(N2.sem.sqmeter, na.rm=T)),
     ylim=c(0, max(N2.mean.sqmeter, na.rm=T)+max(N2.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="N, экз./кв.м")
for (i in 1:ncol(N2.mean.sqmeter))
{lines(as.numeric(rownames(N2.mean.sqmeter)), N2.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(N2.mean.sqmeter)), x1=as.numeric(rownames(N2.mean.sqmeter)),
        y0=N2.mean.sqmeter[,i]-N2.sem.sqmeter[,i], y1=N2.mean.sqmeter[,i]+N2.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}
legend(legend=colnames(N2.mean.sqmeter),x=2005, y=31900, pch=seq(15,15+ncol(N2.mean.sqmeter),1), col=seq(1,1+ncol(N2.mean.sqmeter),1))
dev.off()
embedFonts("N2_dynamic.pdf") #встройка шрифтов в файл


#динамика максимального размера
str(ishodnik)
(Length.max<-tapply(Length.mm, list(year, tidal_level), max, na.rm=T))
#plot(x=names(Length.max), y=Length.max, type=none)
max(Length.mm[year=="1995"] & tidal_level=="low"])
subset(ishodnik, year=="1995")

pdf(file="L_max.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=rownames(Length.max), y=Length.max[,1], type="n", main="о. Горелый", xlab="год", ylab="L max, мм", 
     ylim=c(min(Length.max), max(Length.max)))
for (i in 1:ncol(Length.max))
{lines(as.numeric(rownames(Length.max)), Length.max[,i], pch=14+i, col=0+i, type="b")
}
legend(legend=colnames(Length.max),x=1993.6, y=11.6, pch=seq(15,15+ncol(Length.max),1), col=seq(1,1+ncol(Length.max),1))

dev.off()
embedFonts("L_max.pdf") #встройка шрифтов в файл

#динамика молоди <2mm

#динамика половозрелых >8mm
