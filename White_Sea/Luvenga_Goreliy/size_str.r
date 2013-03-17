# размерная структура суммарно по годам по горизонтам
#ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
#attach(ishodnik)
#year<-factor(year)
#Length.int<-cut(Length.mm, breaks=seq(0,20,1))
#(size.str.table<-table(Length.int,year, tidal_zone))
#(size.str.df<-as.data.frame(size.str.table))
#length.class<-seq(1,20,1)
#for (i in 1:length(levels(size.str.df$tidal_zone)))
#{
#  for (j in 1:length(levels(size.str.df$year)))
#  {
#    pdf(file=paste("sizestr", size.str.df$tidal_zone[i], size.str.df$year[j], ".pdf",sep="_"),width=1000, height=790)
#    barplot(size.str.df$Freq[size.str.df$tidal_zone==paste(size.str.df$tidal_zone[i]) & size.str.df$year==paste(size.str.df$year[j])], names.arg=length.class)
#    dev.off()
#}}


setwd("~/Dropbox/PhD_thesis/White_Sea/Luvenga_Goreliy//")

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



#средний горизонт
mean.sqmeter.middle<-t(tapply(middle$Freq,INDEX=list(middle$year,  middle$Length.int),FUN=sd, na.rm=T))
mean.sqmeter.middle.df<-as.data.frame(mean.sqmeter.middle)

sd.sqmeter.middle<-tapply(middle$Freq,INDEX=list(middle$year,  middle$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.middle <-t(sd.sqmeter.middle/sqrt(n.samples.df$middle))
sem.sqmeter.middle.df<-as.data.frame(sem.sqmeter.middle)

#граница среднего и нижнего горизонта, в фукусах
mean.sqmeter.midlow<-t(tapply(midlow$Freq,INDEX=list(midlow$year,  midlow$Length.int),FUN=sd, na.rm=T))
mean.sqmeter.midlow.df<-as.data.frame(mean.sqmeter.midlow)

sd.sqmeter.midlow<-tapply(midlow$Freq,INDEX=list(midlow$year,  midlow$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.midlow <-t(sd.sqmeter.midlow/sqrt(n.samples.df$midlow))
sem.sqmeter.midlow.df<-as.data.frame(sem.sqmeter.midlow)

#нижний горизонт, у нуля глубин
(mean.sqmeter.low<-t(tapply(low$Freq,INDEX=list(low$year,  low$Length.int),FUN=sd, na.rm=T)))
mean.sqmeter.low.df<-as.data.frame(mean.sqmeter.low)

sd.sqmeter.low<-tapply(low$Freq,INDEX=list(low$year,  low$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.low <-t(sd.sqmeter.low/sqrt(n.samples.df$low))
sem.sqmeter.low.df<-as.data.frame(sem.sqmeter.low)


length.class<-seq(1,20,1)

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
  pdf(file=paste("high", colnames(mean.sqmeter.high)[j], ".pdf",sep="_"), paper="a4")
  error.bars(yv=mean.sqmeter.high[,j], nn=length.class,  z=sem.sqmeter.high[,j])
  title(main=colnames(mean.sqmeter.high)[j], xlab="", ylab="")
  dev.off()
}

#средний горизонт
for (j in 1:length(colnames(mean.sqmeter.middle)))
{
  pdf(file=paste("middle", colnames(mean.sqmeter.middle)[j], ".pdf",sep="_"), paper="a4")
  error.bars(yv=mean.sqmeter.middle[,j], nn=length.class,  z=sem.sqmeter.middle[,j])
  title(main=colnames(mean.sqmeter.middle)[j], xlab="", ylab="")
  dev.off()
}

#граница среднего и нижнего горизонтов, в фукоидах
for (j in 1:length(colnames(mean.sqmeter.midlow)))
{
  pdf(file=paste("midlow", colnames(mean.sqmeter.midlow)[j], ".pdf",sep="_"), paper="a4")
  error.bars(yv=mean.sqmeter.midlow[,j], nn=length.class,  z=sem.sqmeter.midlow[,j])
  title(main=colnames(mean.sqmeter.midlow)[j], xlab="", ylab="")
  dev.off()
}

#нижний горизонт, у нуля глубин
for (j in 1:length(colnames(mean.sqmeter.low)))
{
  pdf(file=paste("low", colnames(mean.sqmeter.low)[j], ".pdf",sep="_"), paper="a4")
  error.bars(yv=mean.sqmeter.low[,j], nn=length.class,  z=sem.sqmeter.low[,j])
  title(main=colnames(mean.sqmeter.low)[j], xlab="", ylab="")
  dev.off()
}


# все 4 на одном графике
for (j in 1:length(colnames(mean.sqmeter.low_beatch)))
{
  pdf(file=paste("all_tidal", colnames(mean.sqmeter.low_beatch)[j], ".pdf",sep="_"), paper="a4")
  error.bars(yv=matrix(mean.sqmeter.high_beatch[,j], mean.sqmeter.fucus_zone[,j], 
                       mean.sqmeter.zostera_zone[,j],mean.sqmeter.low_beatch[,j]), 
             nn=length.class,  
             z=sem.sqmeter.low_beatch[,j])
  title(main=colnames(mean.sqmeter.low_beatch)[j], xlab="", ylab="")
  dev.off()
}


barplot(matrix(mean.sqmeter.high_beatch[,1], mean.sqmeter.fucus_zone[,1], 
               mean.sqmeter.zostera_zone[,1],mean.sqmeter.low_beatch[,1]))