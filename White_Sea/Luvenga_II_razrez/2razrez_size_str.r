setwd("~/Dropbox/PhD_thesis/White_Sea/Luvenga_II_razrez/")

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

#for (i in 1: length(levels(size.str.sqmeter$tidal_level)))
#{
#  write.table(assign(paste(levels(size.str.sqmeter$tidal_level)[i]), 
#       subset(size.str.sqmeter, size.str.sqmeter$tidal_level==levels(size.str.sqmeter$tidal_level)[i])), file=paste(levels(size.str.sqmeter$tidal_level)[i]), sep=",")
#}


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

(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal.level), length ))
(n.samples.df<-as.data.frame(n.samples))

#Верхний пляж
(mean.sqmeter.high_beatch<-t(tapply(high_beatch$Freq,INDEX=list(high_beatch$year,  high_beatch$Length.int),FUN=sd, na.rm=T)))
mean.sqmeter.high_beatch.df<-as.data.frame(mean.sqmeter.high_beatch)

(sd.sqmeter.high_beatch<-tapply(high_beatch$Freq,INDEX=list(high_beatch$year,  high_beatch$Length.int),FUN=sd, na.rm=T))

(sem.sqmeter.high_beatch <-t(sd.sqmeter.high_beatch/sqrt(n.samples.df$high_beatch)))
sem.sqmeter.high_beatch.df<-as.data.frame(sem.sqmeter.high_beatch)

#пояс фукоидов
mean.sqmeter.fucus_zone<-t(tapply(fucus_zone$Freq,INDEX=list(fucus_zone$year,  fucus_zone$Length.int),FUN=sd, na.rm=T))
mean.sqmeter.fucus_zone.df<-as.data.frame(mean.sqmeter.fucus_zone)

sd.sqmeter.fucus_zone<-tapply(fucus_zone$Freq,INDEX=list(fucus_zone$year,  fucus_zone$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.fucus_zone <-t(sd.sqmeter.fucus_zone/sqrt(n.samples.df$fucus_zone))
sem.sqmeter.fucus_zone.df<-as.data.frame(sem.sqmeter.fucus_zone)

#пояс зостеры
mean.sqmeter.zostera_zone<-t(tapply(zostera_zone$Freq,INDEX=list(zostera_zone$year,  zostera_zone$Length.int),FUN=sd, na.rm=T))
mean.sqmeter.zostera_zone.df<-as.data.frame(mean.sqmeter.zostera_zone)

sd.sqmeter.zostera_zone<-tapply(zostera_zone$Freq,INDEX=list(zostera_zone$year,  zostera_zone$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.zostera_zone <-t(sd.sqmeter.zostera_zone/sqrt(n.samples.df$zostera_zone))
sem.sqmeter.zostera_zone.df<-as.data.frame(sem.sqmeter.zostera_zone)

#нижний пляж
(mean.sqmeter.low_beatch<-t(tapply(low_beatch$Freq,INDEX=list(low_beatch$year,  low_beatch$Length.int),FUN=sd, na.rm=T)))
mean.sqmeter.low_beatch.df<-as.data.frame(mean.sqmeter.low_beatch)

sd.sqmeter.low_beatch<-tapply(low_beatch$Freq,INDEX=list(low_beatch$year,  low_beatch$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.low_beatch <-t(sd.sqmeter.low_beatch/sqrt(n.samples.df$low_beatch))
sem.sqmeter.low_beatch.df<-as.data.frame(sem.sqmeter.low_beatch)


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


#верхний пляж
for (j in 1:length(colnames(mean.sqmeter.high_beatch)))
  {
    pdf(file=paste("high_beatch", colnames(mean.sqmeter.high_beatch)[j], ".pdf",sep="_"))
    error.bars(yv=mean.sqmeter.high_beatch[,j], nn=length.class,  z=sem.sqmeter.high_beatch[,j])
    title(main=colnames(mean.sqmeter.high_beatch)[j], xlab="", ylab="")
    dev.off()
  }

#пояс фукоидов
for (j in 1:length(colnames(mean.sqmeter.fucus_zone)))
{
  pdf(file=paste("fucus_zone", colnames(mean.sqmeter.fucus_zone)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.fucus_zone[,j], nn=length.class,  z=sem.sqmeter.fucus_zone[,j])
  title(main=colnames(mean.sqmeter.fucus_zone)[j], xlab="", ylab="")
  dev.off()
}

#пояс зостеры
for (j in 1:length(colnames(mean.sqmeter.zostera_zone)))
{
  pdf(file=paste("zostera_zone", colnames(mean.sqmeter.zostera_zone)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.zostera_zone[,j], nn=length.class,  z=sem.sqmeter.zostera_zone[,j])
  title(main=colnames(mean.sqmeter.zostera_zone)[j], xlab="", ylab="")
  dev.off()
}

#нижний пляж
for (j in 1:length(colnames(mean.sqmeter.low_beatch)))
{
  pdf(file=paste("low_beatch", colnames(mean.sqmeter.low_beatch)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.low_beatch[,j], nn=length.class,  z=sem.sqmeter.low_beatch[,j])
  title(main=colnames(mean.sqmeter.low_beatch)[j], xlab="", ylab="")
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


#динамика обилия
(N.sqmeter<-tapply(size.str.sqmeter$Freq, list(size.str.sqmeter$year, size.str.sqmeter$sample, size.str.df$tidal_level), sum))
(N.mean.sqmeter<-apply(N.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=mean))
  N.mean.sqmeter[11,3]<-NA
(N.sd.sqmeter<-apply(N.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=sd))
(N.sem.sqmeter<-N.sd.sqmeter/sqrt(n.samples))

pdf(file="N_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N.mean.sqmeter[,1], x=as.numeric(rownames(N.mean.sqmeter)), type="n", main="Материковая литораль в районе пос. Лувеньга",
#     ylim=c(min(N.mean.sqmeter, na.rm=T)-max(N.sem.sqmeter, na.rm=T), max(N.mean.sqmeter, na.rm=T)+max(N.sem.sqmeter, na.rm=T)),
     ylim=c(0, max(N.mean.sqmeter, na.rm=T)+max(N.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="N, экз./кв.м")
for (i in 1:ncol(N.mean.sqmeter))
{lines(as.numeric(rownames(N.mean.sqmeter)), N.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
arrows(x0=as.numeric(rownames(N.mean.sqmeter)), x1=as.numeric(rownames(N.mean.sqmeter)),
       y0=N.mean.sqmeter[,i]-N.sem.sqmeter[,i], y1=N.mean.sqmeter[,i]+N.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
 }
legend(legend=colnames(N.mean.sqmeter),x=2000, y=7996, pch=seq(15,15+ncol(N.mean.sqmeter),1), col=seq(1,1+ncol(N.mean.sqmeter),1))
dev.off()
embedFonts("N_dynamic.pdf") #встройка шрифтов в файл

#динамика без молод ( больше 2+)
(N2.sqmeter<-tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"],
                   list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"],
                        size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]"],
                        size.str.df$tidal_level[size.str.sqmeter$Length.int!="(0,1]"]), sum))
(N2.mean.sqmeter<-apply(N2.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=mean))
N2.mean.sqmeter[11,3]<-NA
(N2.sd.sqmeter<-apply(N2.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=sd))
(N2.sem.sqmeter<-N2.sd.sqmeter/sqrt(n.samples))

pdf(file="N2_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter[,1], x=as.numeric(rownames(N2.mean.sqmeter)), type="n", main="Материковая литораль в районе пос. Лувеньга",
     #     ylim=c(min(N2.mean.sqmeter, na.rm=T)-max(N2.sem.sqmeter, na.rm=T), max(N2.mean.sqmeter, na.rm=T)+max(N2.sem.sqmeter, na.rm=T)),
     ylim=c(0, max(N2.mean.sqmeter, na.rm=T)+max(N2.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="N, экз./кв.м")
for (i in 1:ncol(N2.mean.sqmeter))
{lines(as.numeric(rownames(N2.mean.sqmeter)), N2.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(N2.mean.sqmeter)), x1=as.numeric(rownames(N2.mean.sqmeter)),
        y0=N2.mean.sqmeter[,i]-N2.sem.sqmeter[,i], y1=N2.mean.sqmeter[,i]+N2.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}
legend(legend=colnames(N2.mean.sqmeter),x=2000, y=7996, pch=seq(15,15+ncol(N2.mean.sqmeter),1), col=seq(1,1+ncol(N2.mean.sqmeter),1))
dev.off()
embedFonts("N2_dynamic.pdf") #встройка шрифтов в файл