setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/Dalnezeleneckaya/")


#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)


ishodnik<-read.table(file="length_age0708.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample0708.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares0708.csv", sep=";", dec=",", head=T)
attach(ishodnik)

## возрастная структура
(age.int<-as.factor(age))

(age.str.table<-table(age.int, year, sample))

age.str.df<-as.data.frame(age.str.table) # как таблица данных


#убираем те пробы которых на самом деле нету
for (i in 1:length(levels(age.str.df$year)))
{ (xxx<-age.str.df$sample[age.str.df$year==levels(age.str.df$year)[i] ]%in%
     samples.names$sample[samples.names$year==levels(age.str.df$year)[i]])
  antixxx<-as.logical(1-xxx)
  age.str.df$Freq[age.str.df$year==levels(age.str.df$year)[i]][antixxx]<-NA
}


str(age.str.df)
summary(age.str.df$sample)
str(samples.names)
summary(samples.names$sample)
str(samples.squares)
summary(samples.squares$sample)

#теперь на квадратный метр
(age.str.sqmeter<-age.str.df)
age.str.sqmeter$Freq<-age.str.sqmeter$Freq*30

(n.samples<-tapply(samples.names$sample,samples.names$year, length ))

#средние
(age.mean.sqmeter<-t(tapply(age.str.sqmeter$Freq,INDEX=list(age.str.sqmeter$year, age.str.sqmeter$age.int),FUN=mean, na.rm=T)))
age.mean.sqmeter.df<-as.data.frame(age.mean.sqmeter)

(age.sd.sqmeter<-tapply(age.str.sqmeter$Freq,INDEX=list(age.str.sqmeter$year,  age.str.sqmeter$age.int),FUN=sd, na.rm=T))

(age.sem.sqmeter <-t(age.sd.sqmeter/sqrt(as.vector(n.samples))))
age.sem.sqmeter.df<-as.data.frame(age.sem.sqmeter)

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

#рисуем возрастную структуру
for (j in 1:length(colnames(age.mean.sqmeter)))
{
  pdf(file=paste("age", colnames(age.mean.sqmeter)[j], ".pdf",sep="_"))
  error.bars(yv=age.mean.sqmeter[,j], nn=as.numeric(levels(age.int)),  z=age.sem.sqmeter[,j])
  title(main=colnames(age.mean.sqmeter)[j], xlab="", ylab="")
  dev.off()
  embedFonts(paste("age", colnames(age.mean.sqmeter)[j], ".pdf",sep="_"))
}
