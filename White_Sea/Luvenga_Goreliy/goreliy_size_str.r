setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Luvenga_Goreliy/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Luvenga_Goreliy//")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)
#year<-factor(year)

Length.int<-cut(Length.mm, breaks=seq(0,20,1))

(size.str.table<-table(Length.int,year, tidal_level, sample))

size.str.df<-as.data.frame(size.str.table) # как таблица данных

#убираем те пробы которых на самом деле нету

#for (i in 1:length(levels(size.str.df$year))){
#    (xxx<-size.str.df$sample[size.str.df$year==levels(size.str.df$year)[i]]%in%
#       samples.names$sample[samples.names$year==levels(size.str.df$year)[i]])
#    antixxx<-as.logical(1-xxx)
#    size.str.df$Freq[size.str.df$year==levels(size.str.df$year)[i] ][antixxx]<-NA
#  }


for (i in 1:length(levels(size.str.df$year))){
    for(j in 1:length(levels(size.str.df$tidal_level))){
    (xxx<-size.str.df$sample[size.str.df$year==levels(size.str.df$year)[i] & 
                               size.str.df$tidal_level==levels(size.str.df$tidal_level)[j]]%in%
     samples.names$sample[samples.names$year==levels(size.str.df$year)[i] & 
                            samples.names$tidal_level==levels(size.str.df$tidal_level)[j]])
  antixxx<-as.logical(1-xxx)
  size.str.df$Freq[size.str.df$year==levels(size.str.df$year)[i] & 
                     size.str.df$tidal_level==levels(size.str.df$tidal_level)[j]][antixxx]<-NA
}}



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
(mean.sqmeter.high<-t(tapply(high$Freq,INDEX=list(high$year,  high$Length.int),FUN=mean, na.rm=T)))
mean.sqmeter.high.df<-as.data.frame(mean.sqmeter.high)

(sd.sqmeter.high<-tapply(high$Freq,INDEX=list(high$year,  high$Length.int),FUN=sd, na.rm=T))

(sem.sqmeter.high <-t(sd.sqmeter.high/sqrt(n.samples.df$high)))
#TODO надо те строки где по одной пробе сделать вместо ошибки NA!!
sem.sqmeter.high.df<-as.data.frame(sem.sqmeter.high)
sem.sqmeter.high[sem.sqmeter.high==t(sd.sqmeter.high)]<-0


#средний горизонт
mean.sqmeter.middle<-t(tapply(middle$Freq,INDEX=list(middle$year,  middle$Length.int),FUN=mean, na.rm=T))
mean.sqmeter.middle.df<-as.data.frame(mean.sqmeter.middle)

sd.sqmeter.middle<-tapply(middle$Freq,INDEX=list(middle$year,  middle$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.middle <-t(sd.sqmeter.middle/sqrt(n.samples.df$middle))
sem.sqmeter.middle.df<-as.data.frame(sem.sqmeter.middle)
sem.sqmeter.middle[sem.sqmeter.middle==t(sd.sqmeter.middle)]<-0

#граница среднего и нижнего горизонта, в фукусах
mean.sqmeter.midlow<-t(tapply(midlow$Freq,INDEX=list(midlow$year,  midlow$Length.int),FUN=mean, na.rm=T))
mean.sqmeter.midlow.df<-as.data.frame(mean.sqmeter.midlow)

sd.sqmeter.midlow<-tapply(midlow$Freq,INDEX=list(midlow$year,  midlow$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.midlow <-t(sd.sqmeter.midlow/sqrt(n.samples.df$midlow))
sem.sqmeter.midlow.df<-as.data.frame(sem.sqmeter.midlow)
sem.sqmeter.midlow[sem.sqmeter.midlow==t(sd.sqmeter.midlow)]<-0

#нижний горизонт, у нуля глубин
(mean.sqmeter.low<-t(tapply(low$Freq,INDEX=list(low$year,  low$Length.int),FUN=mean, na.rm=T)))
mean.sqmeter.low.df<-as.data.frame(mean.sqmeter.low)

sd.sqmeter.low<-tapply(low$Freq,INDEX=list(low$year,  low$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.low <-t(sd.sqmeter.low/sqrt(n.samples.df$low))
sem.sqmeter.low.df<-as.data.frame(sem.sqmeter.low)
sem.sqmeter.low[sem.sqmeter.low==t(sd.sqmeter.low)]<-0

length.class<-seq(1,20,1)

##size structure >1mm

#верхний горизонт
(mean.sqmeter2.high<-mean.sqmeter.middle[2:20,])
mean.sqmeter2.high.df<-as.data.frame(mean.sqmeter2.high)

(sd.sqmeter2.high<-sd.sqmeter.middle[,2:20])

(sem.sqmeter2.high <-t(sd.sqmeter2.high/sqrt(n.samples.df$high)))
#TODO надо те строки где по одной пробе сделать вместо ошибки NA!!
sem.sqmeter2.high.df<-as.data.frame(sem.sqmeter2.high)
sem.sqmeter2.high[sem.sqmeter2.high==t(sd.sqmeter2.high)]<-0


#средний горизонт
mean.sqmeter2.middle<-mean.sqmeter.middle[2:20,]
mean.sqmeter2.middle.df<-as.data.frame(mean.sqmeter2.middle)

sd.sqmeter2.middle<-sd.sqmeter.middle[,2:20]

sem.sqmeter2.middle <-t(sd.sqmeter2.middle/sqrt(n.samples.df$middle))
sem.sqmeter2.middle.df<-as.data.frame(sem.sqmeter2.middle)
sem.sqmeter2.middle[sem.sqmeter2.middle==t(sd.sqmeter2.middle)]<-0

#граница среднего и нижнего горизонта, в фукусах
mean.sqmeter2.midlow<-mean.sqmeter.middle[2:20,]
mean.sqmeter2.midlow.df<-as.data.frame(mean.sqmeter2.midlow)

sd.sqmeter2.midlow<-sd.sqmeter.middle[,2:20]

sem.sqmeter2.midlow <-t(sd.sqmeter2.midlow/sqrt(n.samples.df$midlow))
sem.sqmeter2.midlow.df<-as.data.frame(sem.sqmeter2.midlow)
sem.sqmeter2.midlow[sem.sqmeter2.midlow==t(sd.sqmeter2.midlow)]<-0

#нижний горизонт, у нуля глубин
mean.sqmeter2.low<-mean.sqmeter.middle[2:20,]
mean.sqmeter2.low.df<-as.data.frame(mean.sqmeter2.low)

sd.sqmeter2.low<-sd.sqmeter.middle[,2:20]

sem.sqmeter2.low <-t(sd.sqmeter2.low/sqrt(n.samples.df$low))
sem.sqmeter2.low.df<-as.data.frame(sem.sqmeter2.low)
sem.sqmeter2.low[sem.sqmeter2.low==t(sd.sqmeter2.low)]<-0

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


##>1mm
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



## динамика обилия
(N.sqmeter<-tapply(size.str.sqmeter$Freq, list(size.str.sqmeter$year, size.str.sqmeter$sample, size.str.df$tidal_level), sum))
(N.mean.sqmeter<-apply(N.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=mean))
N.mean.sqmeter[11,3]<-NA
(N.sd.sqmeter<-apply(N.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=sd))
(N.sem.sqmeter<-N.sd.sqmeter/sqrt(n.samples))
N.sem.sqmeter[N.sem.sqmeter==N.sd.sqmeter]<-NA

##COUNT
(D.n<-N.sem.sqmeter/N.mean.sqmeter*100)

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
legend(legend=colnames(N.mean.sqmeter),x=2004, y=42965, pch=seq(15,15+ncol(N.mean.sqmeter),1), col=seq(1,1+ncol(N.mean.sqmeter),1))
dev.off()
embedFonts("N_dynamic.pdf") #встройка шрифтов в файл



## динамика без молод ( больше 2+)
(N2.sqmeter<-tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"], 
                    list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"],
                         size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]"],
                         size.str.df$tidal_level[size.str.sqmeter$Length.int!="(0,1]"]), sum))
(N2.mean.sqmeter<-apply(N2.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=mean))
N2.mean.sqmeter[11,3]<-NA
(N2.sd.sqmeter<-apply(N2.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=sd))
(N2.sem.sqmeter<-N2.sd.sqmeter/sqrt(n.samples))
N2.sem.sqmeter[N2.sem.sqmeter==N2.sd.sqmeter]<-NA

##COUNT!
(D.n2<-N.sem.sqmeter/N.mean.sqmeter*100)

# запишем численность всех крупнее 1 мм в файл
write.table(data.frame(N2.mean.sqmeter, N2.sem.sqmeter), file="goreliy_N2.csv", sep=";", dec=",")

# запишем пересчет обилия >1мм в пробах на квадратный метр в файл
write.table(as.data.frame(as.table(N2.sqmeter)), file="goreliy_N2_in samples_sqmeter.csv", sep=";", dec=",")



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
legend(legend=colnames(N2.mean.sqmeter),x=2005, y=8733, pch=seq(15,15+ncol(N2.mean.sqmeter),1), col=seq(1,1+ncol(N2.mean.sqmeter),1))
dev.off()
embedFonts("N2_dynamic.pdf") #встройка шрифтов в файл

#locator()

##про численность 2+
(N2.sqmeter.high<-(N2.sqmeter)[,,1])
(N2.92.12.df.high<-data.frame(subset(samples.names, samples.names$tidal_level=="high"),
                             as.vector(t(N2.sqmeter.high))[!is.na(as.vector(t(N2.sqmeter.high)))]))
(N2.sqmeter.high[!is.na(N2.sqmeter.high)])

kruskal.test(N2.92.98.df$as.vector.N2.92.98...is.na.as.vector.N2.92.98... ~ N2.92.98.df$year)
boxplot(N2.92.98.df$as.vector.N2.92.98...is.na.as.vector.N2.92.98... ~ N2.92.98.df$year)

str(as.data.frame(N2.sqmeter))
as.data.frame(t(N2.sqmeter[15:17,,"low"]))
 

#install.packages("reshape2") - пеерстройка объектов построчно
#install.packages("doBy") - считать по датафрейму
#library(doBy)
#example(summaryBy) 

#делаем таблицу данных с годами, где станции взяты по пробам, а не интегрально. после чего - краскел-уоллис
#high
#as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"high"])[!is.na(as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"high"]))]
(N2.high<-subset(samples.names, subset=samples.names$tidal_level=="high"))
(N2.multisample.high<-data.frame(subset(N2.high, subset= N2.high$year==2004 | 
                                         N2.high$year==2006 | N2.high$year==2007 | N2.high$year==2008 | 
                                         N2.high$year==2011), 
                                as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"high"])
                                [!is.na(as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"high"]))]))
kruskal.test(N2.multisample.high$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... ~ N2.multisample.high$year)

#mean, d
mean(N2.multisample.high$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011....)

(sd(N2.multisample.high$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... )/
   sqrt(length(N2.multisample.high$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... )))/
  mean(N2.multisample.high$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011....)*100

#middle
#as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"middle"])[!is.na(as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"middle"]))]
(N2.middle<-subset(samples.names, subset=samples.names$tidal_level=="middle"))
(N2.multisample.middle<-data.frame(subset(N2.middle, subset= N2.middle$year==2004 | 
                                         N2.middle$year==2006 | N2.middle$year==2007 | N2.middle$year==2008 | 
                                         N2.middle$year==2011), 
                                as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"middle"])
                                [!is.na(as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"middle"]))]))
kruskal.test(N2.multisample.middle$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... ~ N2.multisample.middle$year)

#mean, d
mean(N2.multisample.middle$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011....)

(sd(N2.multisample.middle$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... )/
   sqrt(length(N2.multisample.middle$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... )))/
  mean(N2.multisample.middle$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011....)*100

#midlow
#as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"midlow"])[!is.na(as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"midlow"]))]
(N2.midlow<-subset(samples.names, subset=samples.names$tidal_level=="midlow"))
(N2.multisample.midlow<-data.frame(subset(N2.midlow, subset= N2.midlow$year==2004 | 
                                         N2.midlow$year==2006 | N2.midlow$year==2007 | N2.midlow$year==2008 | 
                                         N2.midlow$year==2011), 
                                as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"midlow"])
                                [!is.na(as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"midlow"]))]))
kruskal.test(N2.multisample.midlow$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... ~ N2.multisample.midlow$year)

#mean,d
mean(N2.multisample.midlow$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011....)

(sd(N2.multisample.midlow$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... )/
   sqrt(length(N2.multisample.midlow$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... )))/
  mean(N2.multisample.midlow$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011....)*100

#low 
#as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"low"])[!is.na(as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"low"]))]
(N2.low<-subset(samples.names, subset=samples.names$tidal_level=="low"))
 (N2.multisample.low<-data.frame(subset(N2.low, subset= N2.low$year==2004 | 
                             N2.low$year==2006 | N2.low$year==2007 | N2.low$year==2008 | 
                             N2.low$year==2011), 
                                 as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"low"])
                                 [!is.na(as.vector(N2.sqmeter[c("2004","2006","2007","2008","2011"),,"low"]))]))
kruskal.test(N2.multisample.low$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... ~ N2.multisample.low$year)

#mean, d
mean(N2.multisample.low$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011....)

(sd(N2.multisample.low$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... )/
   sqrt(length(N2.multisample.low$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011.... )))/
  mean(N2.multisample.low$as.vector.N2.sqmeter.c..2004....2006....2007....2008....2011....)*100


##размерная структура в %
str(size.str.sqmeter)

#high
(sum.sizestr.sqmeter.high<-t(tapply(high$Freq,INDEX=list(high$year, high$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.high<-t(t(sum.sizestr.sqmeter.high)/colSums(sum.sizestr.sqmeter.high)*100))
#>1mm
(sum.sizestr2.sqmeter.percents.high<-t(t(sum.sizestr.sqmeter.high[2:nrow(sum.sizestr.sqmeter.high),])/
   colSums(sum.sizestr.sqmeter.high[2:nrow(sum.sizestr.sqmeter.high),])*100))

# запишем в файл размерную структуру в процентах
write.table(x=sum.sizestr2.sqmeter.percents.high, file="goreliy_high_sizestr2_percent.csv", sep=";", dec=",")

#middle
(sum.sizestr.sqmeter.middle<-t(tapply(middle$Freq,INDEX=list(middle$year, middle$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.middle<-t(t(sum.sizestr.sqmeter.middle)/colSums(sum.sizestr.sqmeter.middle)*100))
#>1mm
(sum.sizestr2.sqmeter.percents.middle<-t(t(sum.sizestr.sqmeter.middle[2:nrow(sum.sizestr.sqmeter.middle),])/
   colSums(sum.sizestr.sqmeter.middle[2:nrow(sum.sizestr.sqmeter.middle),])*100))

# запишем в файл размерную структуру в процентах
write.table(x=sum.sizestr2.sqmeter.percents.middle, file="goreliy_middle_sizestr2_percent.csv", sep=";", dec=",")
 
#midlow
(sum.sizestr.sqmeter.midlow<-t(tapply(midlow$Freq,INDEX=list(midlow$year, midlow$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.midlow<-t(t(sum.sizestr.sqmeter.midlow)/colSums(sum.sizestr.sqmeter.midlow)*100))
#>1mm
(sum.sizestr2.sqmeter.percents.midlow<-t(t(sum.sizestr.sqmeter.midlow[2:nrow(sum.sizestr.sqmeter.midlow),])/
   colSums(sum.sizestr.sqmeter.midlow[2:nrow(sum.sizestr.sqmeter.midlow),])*100))

# запишем в файл размерную структуру в процентах
write.table(x=sum.sizestr2.sqmeter.percents.midlow, file="goreliy_midlow_sizestr2_percent.csv", sep=";", dec=",")

#low
(sum.sizestr.sqmeter.low<-t(tapply(low$Freq,INDEX=list(low$year, low$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.low<-t(t(sum.sizestr.sqmeter.low)/colSums(sum.sizestr.sqmeter.low)*100))
#>1mm
(sum.sizestr2.sqmeter.percents.low<-t(t(sum.sizestr.sqmeter.low[2:nrow(sum.sizestr.sqmeter.low),])/
   colSums(sum.sizestr.sqmeter.low[2:nrow(sum.sizestr.sqmeter.low),])*100))

# запишем в файл размерную структуру в процентах
write.table(x=sum.sizestr2.sqmeter.percents.low, file="goreliy_low_sizestr2_percent.csv", sep=";", dec=",")

 ##динамика максимального размера
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


## рассчетная биомасса по Максимовичу и др., 1993
biomass.count<-0.00016*(Length.mm^2.96)
(biomass.samples<-tapply(biomass.count, list(year, sample, tidal_level), sum, na.rm=T))

(biomass.sqmeter<-biomass.samples*samples.squares$square)

(B.mean.sqmeter<-apply(biomass.sqmeter, c(1,3), mean, na.rm=T))
(B.sd.sqmeter<-apply(biomass.sqmeter, c(1,3), sd, na.rm=T))
(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal_level), length ))
(B.sem.sqmeter<-B.sd.sqmeter/sqrt(n.samples))
(D.b<-B.sem.sqmeter/B.mean.sqmeter*100)

pdf(file="B_count_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter[,1], x=as.numeric(rownames(B.mean.sqmeter)), type="n", main="о. Горелый (Лувеньгские шхеры)",
         ylim=c(min(B.mean.sqmeter, na.rm=T)-max(B.sem.sqmeter, na.rm=T), max(B.mean.sqmeter, na.rm=T)+max(B.sem.sqmeter, na.rm=T)),
     # ylim=c(0, max(B.mean.sqmeter, na.rm=T)+max(B.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="B, г/кв.м")
for (i in 1:ncol(B.mean.sqmeter))
{lines(as.numeric(rownames(B.mean.sqmeter)), B.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(B.mean.sqmeter)), x1=as.numeric(rownames(B.mean.sqmeter)),
        y0=B.mean.sqmeter[,i]-B.sem.sqmeter[,i], y1=B.mean.sqmeter[,i]+B.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}
legend(legend=colnames(B.mean.sqmeter),x=2002.6, y=38.3, pch=seq(15,15+ncol(B.mean.sqmeter),1), col=seq(1,1+ncol(B.mean.sqmeter),1))
dev.off()
embedFonts("B_count_dynamic.pdf") #встройка шрифтов в файл

## рассчетная биомасса только с учетом >1mm особей
biomass2.count<-0.00016*(Length.mm[Length.mm>1.0]^2.96)
(biomass2.samples<-tapply(biomass2.count, list(year[Length.mm>1.0], sample[Length.mm>1.0], tidal_level[Length.mm>1.0]), sum, na.rm=T))

(biomass2.sqmeter<-biomass2.samples*samples.squares$square)

(B2.mean.sqmeter<-apply(biomass2.sqmeter, c(1,3), mean, na.rm=T))
(B2.sd.sqmeter<-apply(biomass2.sqmeter, c(1,3), sd, na.rm=T))
(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal_level), length ))
(B2.sem.sqmeter<-B2.sd.sqmeter/sqrt(n.samples))
(D.b2<-B2.sem.sqmeter/B2.mean.sqmeter*100)

#запишем в файл рассчетную биомассу
write.table(data.frame(B2.mean.sqmeter, B2.sem.sqmeter), file="Goreliy_B2_mean.csv",sep=";", dec=",")

pdf(file="B2_count_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B2.mean.sqmeter[,1], x=as.numeric(rownames(B2.mean.sqmeter)), type="n", main="о. Горелый (Лувеньгские шхеры)",
     ylim=c(min(B2.mean.sqmeter, na.rm=T)-max(B2.sem.sqmeter, na.rm=T), max(B2.mean.sqmeter, na.rm=T)+max(B2.sem.sqmeter, na.rm=T)),
     # ylim=c(0, max(B.mean.sqmeter, na.rm=T)+max(B.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="B, г/кв.м")
for (i in 1:ncol(B2.mean.sqmeter))
{lines(as.numeric(rownames(B2.mean.sqmeter)), B2.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(B2.mean.sqmeter)), x1=as.numeric(rownames(B2.mean.sqmeter)),
        y0=B2.mean.sqmeter[,i]-B2.sem.sqmeter[,i], y1=B2.mean.sqmeter[,i]+B2.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}
legend(legend=colnames(B2.mean.sqmeter),x=2003.7, y=18.2, pch=seq(15,15+ncol(B2.mean.sqmeter),1), col=seq(1,1+ncol(B2.mean.sqmeter),1))
dev.off()
embedFonts("B2_count_dynamic.pdf") #встройка шрифтов в файл


## измеренная биомасса (реальная)
str(biomass.measure)
(biomass.real.m<-tapply(biomass.measure$biomass.g, list(biomass.measure$year, biomass.measure$sample), function(x){x*1}))
(biomass.real.sqmeter<-biomass.real.m*samples.squares$square)

(Br.mean.sqmeter<-rowMeans(biomass.real.sqmeter, na.rm=T))
(Br.sd.sqmeter<-apply(biomass.real.sqmeter, 1, sd, na.rm=T))
(Br.sem.sqmeter<-Br.sd.sqmeter/sqrt(n.samples))
(D.br<-Br.sem.sqmeter/Br.mean.sqmeter*100)

## сравнение рассчетной и реальной биомассы
pdf(file="Bcount_Bmeasure_compare.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter, x=names(B.mean.sqmeter),pch=15, main="Эстуарий р. Лувеньги", type="n",
     ylim=c(min(min(B.mean.sqmeter)-max(B.sem.sqmeter), min(Br.mean.sqmeter, na.rm=T)-max(Br.sem.sqmeter, na.rm=T)), 
            max(max(B.mean.sqmeter)+max(B.sem.sqmeter), max(Br.mean.sqmeter, na.rm=T)+max(Br.sem.sqmeter, na.rm=T))),
     xlab="год", ylab="B, г/кв.м")
#B
lines(seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1), B.mean.sqmeter, pch=15, col=2, type="b")
arrows(x0=seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1),
       y0=B.mean.sqmeter-B.sem.sqmeter, y1=B.mean.sqmeter+B.sem.sqmeter, angle=90, code=3, length=0.1, col=2)
#Breal
lines(seq(as.numeric(min(names(Br.mean.sqmeter))),as.numeric(max(names(Br.mean.sqmeter))),1), Br.mean.sqmeter, pch=16, col=4, type="b")
arrows(x0=seq(as.numeric(min(names(Br.mean.sqmeter))),as.numeric(max(names(Br.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(Br.mean.sqmeter))),as.numeric(max(names(Br.mean.sqmeter))),1),
       y0=Br.mean.sqmeter-Br.sem.sqmeter, y1=Br.mean.sqmeter+Br.sem.sqmeter, angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("Bcount_Bmeasure.pdf") #встройка шрифтов в файл


## динамика молоди <2mm и половозрелых  >8mm

young.old.int<-cut(Length.mm, breaks=c(1,2.5,7.9,max(Length.mm, na.rm=T)))

(young.old.table<-table(young.old.int, year, tidal_level, sample))

young.old.df<-as.data.frame(young.old.table) # как таблица данных

#убираем те пробы которых на самом деле нету
 for (i in 1:length(levels(young.old.df$year)))
 { (xxx<-young.old.df$sample[young.old.df$year==levels(young.old.df$year)[i] ]%in%
      samples.names$sample[samples.names$year==levels(young.old.df$year)[i]])
   antixxx<-as.logical(1-xxx)
   young.old.df$Freq[young.old.df$year==levels(young.old.df$year)[i]][antixxx]<-NA
 }
 
 #теперь на квадратный метр
 young.old.sqmeter<-young.old.df
 for (i in 1:length(levels(young.old.sqmeter$year)))
 {
   young.old.sqmeter$Freq[young.old.sqmeter$year==levels(young.old.sqmeter$year)[i]]<-
     young.old.sqmeter$Freq[young.old.sqmeter$year==levels(young.old.sqmeter$year)[i]] * 
     samples.squares$square[samples.squares$year==levels(young.old.sqmeter$year)[i]]
 }
str(young.old.sqmeter)


(mean.young.old.sqmeter<-tapply(young.old.sqmeter$Freq,
                                INDEX=list(young.old.sqmeter$year,young.old.sqmeter$young.old.int, young.old.sqmeter$tidal_level),
                                FUN=mean, na.rm=T))
for (i in 1:4)
{assign(paste("mean",dimnames(mean.young.old.sqmeter)[[3]][i],sep="."),mean.young.old.sqmeter[,,i])
}

(sd.young.old.sqmeter<-tapply(young.old.sqmeter$Freq,
                              INDEX=list(young.old.sqmeter$year, young.old.sqmeter$tidal_level, young.old.sqmeter$young.old.int),
                              FUN=sd, na.rm=T))

(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal_level), length ))
(n.samples.df<-as.data.frame(n.samples))


(sem.young.old.sqmeter <-sd.young.old.sqmeter/sqrt(as.vector(n.samples)))
sem.young.old.sqmeter[sem.young.old.sqmeter==sd.young.old.sqmeter]<-NA
for (i in 1:4)
{assign(paste("sem",dimnames(sem.young.old.sqmeter)[[2]][i],sep="."),sem.young.old.sqmeter[,i,])
}

str(mean.young.old.sqmeter)

write.table(mean.young.old.sqmeter, file="goreliy_young_old_mean.csv", sep=";", dec=",")

# и все же не умею я делать стопку графиков одним движением руки :(
#for (j in 1:4){
#pdf(file=paste("young_old", dimnames(mean.young.old.sqmeter)[[3]][j], ".pdf", sep="_"), family="NimbusSan") # указываем шрифт подпией
#for (j in 1:4){
#plot(y=paste("mean",dimnames(mean.young.old.sqmeter)[[3]][j],sep=".")[,1], x=as.numeric(rownames(paste("mean",dimnames(mean.young.old.sqmeter)[[3]][i],sep="."))), 
#             type="n", main="о. Горелый (Лувеньгские шхеры)",
##     ylim=c(min(mean.young.old.sqmeter, na.rm=T)-max(sd.young.old.sqmeter, na.rm=T), max(mean.young.old.sqmeter, na.rm=T)+max(sd.young.old.sqmeter, na.rm=T)),
#ylim=c(0, max(paste("mean",dimnames(mean.young.old.sqmeter)[[3]][j],sep="."), na.rm=T)+max(sd.young.old.sqmeter, na.rm=T)), 
#xlab="год", ylab="N, экз./кв.м")
#for (i in 1:ncol(paste("mean",dimnames(mean.young.old.sqmeter)[[3]][j],sep=".")))
#{lines(as.numeric(rownames(mean.young.old.sqmeter)), paste("mean",dimnames(mean.young.old.sqmeter)[[3]][i],sep=".")[,i], 
#       pch=14+i, col=0+i, type="b")
#arrows(x0=as.numeric(rownames(mean.young.old.sqmeter)), x1=as.numeric(rownames(mean.young.old.sqmeter)),
#y0=mean.young.old.sqmeter[,i]-paste("sem",dimnames(sem.young.old.sqmeter)[[3]][i],sep=".")[,i], 
#       y1=mean.young.old.sqmeter[,i]+paste("sem",dimnames(sem.young.old.sqmeter)[[3]][i],sep=".")[,i], 
#       angle=90, code=3, length=.1, col=0+i)
# }
#legend(legend=colnames(mean.young.old.sqmeter),x=2005, y=31900, pch=seq(15,15+ncol(mean.young.old.sqmeter),1), col=seq(1,1+ncol(mean.young.old.sqmeter),1))
#dev.off()
#embedFonts("N_dynamic.pdf") #встройка шрифтов в файл
#}

# молодь и половозрелые - график high
pdf(file="young_old_high.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.high[,1], x=as.numeric(rownames(mean.high)),pch=15, type="n", main="о. Горелый (Лувеньгские шхеры) high", 
     #     ylim=c(min(mean.high[,1], mean.high[,3])-max(sem.high[,1], sem.high[,3]), 
     #            max(mean.high[,1], mean.high[,3])+max(sem.high[,1], sem.high[,3])),
     ylim=c(0, 
            max(mean.high[,1], mean.high[,3], na.rm=T)+max(sem.high[,1], sem.high[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1), 
      mean.high[,1], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1), 
       x1=seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1),
       y0=mean.high[,1]-sem.high[,1], 
       y1=mean.high[,1]+sem.high[,1], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1), 
      mean.high[,3], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1), 
       x1=seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1),
       y0=mean.high[,3]-sem.high[,3], 
       y1=mean.high[,3]+sem.high[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old_high.pdf") #встройка шрифтов в файл

# молодь и половозрелые - график middle
pdf(file="young_old_middle.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.middle[,1], x=as.numeric(rownames(mean.middle)),pch=15, type="n", main="о. Горелый (Лувеньгские шхеры) middle", 
     #     ylim=c(min(mean.middle[,1], mean.middle[,3])-max(sem.middle[,1], sem.middle[,3]), 
     #            max(mean.middle[,1], mean.middle[,3])+max(sem.middle[,1], sem.middle[,3])),
     ylim=c(0, 
            max(mean.middle[,1], mean.middle[,3], na.rm=T)+max(sem.middle[,1], sem.middle[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1), 
      mean.middle[,1], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1), 
       x1=seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1),
       y0=mean.middle[,1]-sem.middle[,1], 
       y1=mean.middle[,1]+sem.middle[,1], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1), 
      mean.middle[,3], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1), 
       x1=seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1),
       y0=mean.middle[,3]-sem.middle[,3], 
       y1=mean.middle[,3]+sem.middle[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old_middle.pdf") #встройка шрифтов в файл

# молодь и половозрелые - график midlow
pdf(file="young_old_midlow.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.midlow[,1], x=as.numeric(rownames(mean.midlow)),pch=15, type="n", main="о. Горелый (Лувеньгские шхеры) midlow", 
     #     ylim=c(min(mean.midlow[,1], mean.midlow[,3])-max(sem.midlow[,1], sem.midlow[,3]), 
     #            max(mean.midlow[,1], mean.midlow[,3])+max(sem.midlow[,1], sem.midlow[,3])),
     ylim=c(0, 
            max(mean.midlow[,1], mean.midlow[,3], na.rm=T)+max(sem.midlow[,1], sem.midlow[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1), 
      mean.midlow[,1], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1), 
       x1=seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1),
       y0=mean.midlow[,1]-sem.midlow[,1], 
       y1=mean.midlow[,1]+sem.midlow[,1], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1), 
      mean.midlow[,3], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1), 
       x1=seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1),
       y0=mean.midlow[,3]-sem.midlow[,3], 
       y1=mean.midlow[,3]+sem.midlow[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old_midlow.pdf") #встройка шрифтов в файл

# молодь и половозрелые - график low
pdf(file="young_old_low.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.low[,1], x=as.numeric(rownames(mean.low)),pch=15, type="n", main="о. Горелый (Лувеньгские шхеры) low", 
     #     ylim=c(min(mean.low[,1], mean.low[,3])-max(sem.low[,1], sem.low[,3]), 
     #            max(mean.low[,1], mean.low[,3])+max(sem.low[,1], sem.low[,3])),
     ylim=c(0, 
            max(mean.low[,1], mean.low[,3], na.rm=T)+max(sem.low[,1], sem.low[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1), 
      mean.low[,1], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1), 
       x1=seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1),
       y0=mean.low[,1]-sem.low[,1], 
       y1=mean.low[,1]+sem.low[,1], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1), 
      mean.low[,3], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1), 
       x1=seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1),
       y0=mean.low[,3]-sem.low[,3], 
       y1=mean.low[,3]+sem.low[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old_low.pdf") #встройка шрифтов в файл

#динамика молоди и половозрелых в %
(sum.young.old<-(tapply(young.old.sqmeter$Freq[young.old.sqmeter$young.old.int!="(0,1]"],
                         INDEX=list(young.old.sqmeter$year[young.old.sqmeter$young.old.int!="(0,1]"], 
                                    young.old.sqmeter$tidal_level[young.old.sqmeter$young.old.int!="(0,1]"],
                                    young.old.sqmeter$young.old.int[young.old.sqmeter$young.old.int!="(0,1]"]),
                         FUN=sum, na.rm=T)))

#это извращение - повторить три раза сумму, чтобы поделить на это трехмерный массив. 
#потому что как делить трехмерный на двумерный я не догоняю
summ.all.3times<-array(dim=dim(sum.young.old), dimnames=dimnames(sum.young.old), 
                       data=rep(x=apply(sum.young.old, MARGIN=c(1,2), FUN=sum, na.rm=T), 3)) 
(young.old.percents<-(sum.young.old)/summ.all.3times*100)

write.table((young.old.percents), file="goreliy_young_old_percent.csv", sep=";", dec=",")

#корреляция - молодь и половозрелые в %
str(young.old.percents)
(spearman.young.old.sum.percent<-cor.test(young.old.percents[2:21,,1], young.old.percents[1:20,,3], method="spearman"))
plot(y=young.old.percents[2:21,,1], x=young.old.percents[1:20,,3])


## молодь и общая численность - графики
# молодь и все - график high
pdf(file="young_all_high.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter[,"high"], x=as.numeric(rownames(mean.high)),pch=15, type="n", main="о. Горелый (Лувеньгские шхеры) high", 
     #     ylim=c(min(mean.high[,1], N2.mean.sqmeter[,"high"])-max(sem.high[,1], N2.sem.sqmeter[,"high"]), 
     #            max(mean.high[,1], N2.mean.sqmeter[,"high"])+max(sem.high[,1], N2.sem.sqmeter[,"high"])),
     ylim=c(0, 
            max(mean.high[,1], N2.mean.sqmeter[,"high"], na.rm=T)+max(sem.high[,1], N2.sem.sqmeter[,"high"], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1), 
      mean.high[,1], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1), 
       x1=seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1),
       y0=mean.high[,1]-sem.high[,1], 
       y1=mean.high[,1]+sem.high[,1], angle=90, code=3, length=0.1, col=2)
#все
lines(seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1), 
      N2.mean.sqmeter[,"high"], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1), 
       x1=seq(as.numeric(min(rownames(mean.high))),as.numeric(max(rownames(mean.high))),1),
       y0=N2.mean.sqmeter[,"high"]-N2.sem.sqmeter[,"high"], 
       y1=N2.mean.sqmeter[,"high"]+N2.sem.sqmeter[,"high"], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_all_high.pdf") #встройка шрифтов в файл

# молодь и все - график middle
pdf(file="young_all_middle.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter[,"middle"], x=as.numeric(rownames(mean.middle)),pch=15, type="n", main="о. Горелый (Лувеньгские шхеры) middle", 
     #     ylim=c(min(mean.middle[,1], N2.mean.sqmeter[,"middle"])-max(sem.middle[,1], N2.sem.sqmeter[,"middle"]), 
     #            max(mean.middle[,1], N2.mean.sqmeter[,"middle"])+max(sem.middle[,1], N2.sem.sqmeter[,"middle"])),
     ylim=c(0, 
            max(mean.middle[,1], N2.mean.sqmeter[,"middle"], na.rm=T)+max(sem.middle[,1], N2.sem.sqmeter[,"middle"], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1), 
      mean.middle[,1], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1), 
       x1=seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1),
       y0=mean.middle[,1]-sem.middle[,1], 
       y1=mean.middle[,1]+sem.middle[,1], angle=90, code=3, length=0.1, col=2)
#все
lines(seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1), 
      N2.mean.sqmeter[,"middle"], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1), 
       x1=seq(as.numeric(min(rownames(mean.middle))),as.numeric(max(rownames(mean.middle))),1),
       y0=N2.mean.sqmeter[,"middle"]-N2.sem.sqmeter[,"middle"], 
       y1=N2.mean.sqmeter[,"middle"]+N2.sem.sqmeter[,"middle"], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_all_middle.pdf") #встройка шрифтов в файл

# молодь и все - график midlow
pdf(file="young_all_midlow.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter[,"midlow"], x=as.numeric(rownames(mean.midlow)),pch=15, type="n", main="о. Горелый (Лувеньгские шхеры) midlow", 
     #     ylim=c(min(mean.midlow[,1], N2.mean.sqmeter[,"midlow"])-max(sem.midlow[,1], N2.sem.sqmeter[,"midlow"]), 
     #            max(mean.midlow[,1], N2.mean.sqmeter[,"midlow"])+max(sem.midlow[,1], N2.sem.sqmeter[,"midlow"])),
     ylim=c(0, 
            max(mean.midlow[,1], N2.mean.sqmeter[,"midlow"], na.rm=T)+max(sem.midlow[,1], N2.sem.sqmeter[,"midlow"], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1), 
      mean.midlow[,1], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1), 
       x1=seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1),
       y0=mean.midlow[,1]-sem.midlow[,1], 
       y1=mean.midlow[,1]+sem.midlow[,1], angle=90, code=3, length=0.1, col=2)
#все
lines(seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1), 
      N2.mean.sqmeter[,"midlow"], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1), 
       x1=seq(as.numeric(min(rownames(mean.midlow))),as.numeric(max(rownames(mean.midlow))),1),
       y0=N2.mean.sqmeter[,"midlow"]-N2.sem.sqmeter[,"midlow"], 
       y1=N2.mean.sqmeter[,"midlow"]+N2.sem.sqmeter[,"midlow"], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_all_midlow.pdf") #встройка шрифтов в файл

# молодь и все - график low
pdf(file="young_all_low.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter[,"low"], x=as.numeric(rownames(mean.low)),pch=15, type="n", main="о. Горелый (Лувеньгские шхеры) low", 
     #     ylim=c(min(mean.low[,1], N2.mean.sqmeter[,"low"])-max(sem.low[,1], N2.sem.sqmeter[,"low"]), 
     #            max(mean.low[,1], N2.mean.sqmeter[,"low"])+max(sem.low[,1], N2.sem.sqmeter[,"low"])),
     ylim=c(0, 
            max(mean.low[,1], N2.mean.sqmeter[,"low"], na.rm=T)+max(sem.low[,1], N2.sem.sqmeter[,"low"], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1), 
      mean.low[,1], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1), 
       x1=seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1),
       y0=mean.low[,1]-sem.low[,1], 
       y1=mean.low[,1]+sem.low[,1], angle=90, code=3, length=0.1, col=2)
#все
lines(seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1), 
      N2.mean.sqmeter[,"low"], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1), 
       x1=seq(as.numeric(min(rownames(mean.low))),as.numeric(max(rownames(mean.low))),1),
       y0=N2.mean.sqmeter[,"low"]-N2.sem.sqmeter[,"low"], 
       y1=N2.mean.sqmeter[,"low"]+N2.sem.sqmeter[,"low"], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_all_low.pdf") #встройка шрифтов в файл

##  численность молоди и биомасса половозрелых
## рассчетная биомасса только с учетом >8mm особей
biomass8.count<-0.00016*(Length.mm[Length.mm>8.0]^2.96)
(biomass8.samples<-tapply(biomass8.count, list(year[Length.mm>8.0], sample[Length.mm>8.0], tidal_level[Length.mm>8.0]), sum, na.rm=T))

(biomass8.sqmeter<-biomass8.samples*samples.squares$square)

(B2.mean.sqmeter<-apply(biomass8.sqmeter, c(1,3), mean, na.rm=T))
(B2.sd.sqmeter<-apply(biomass8.sqmeter, c(1,3), sd, na.rm=T))
(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal_level), length ))
(B2.sem.sqmeter<-B2.sd.sqmeter/sqrt(n.samples))


write.table(B8.mean.sqmeter, file="goreliy_biomass_old.csv", sep=";", dec=",")

##размеры маком на разных горизонтах
str(ishodnik)
boxplot(Length.mm ~ tidal_level, horizontal=T)

