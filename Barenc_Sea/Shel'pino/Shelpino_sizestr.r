setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/Shel'pino/")


#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

ishodnik<-read.table(file="length_age.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)

str(ishodnik)


# ==========размерная структура ==============================================
max(Length.mm, na.rm=T)
Length.int<-cut(Length.mm, breaks=seq(0,20,1))

(size.str.table<-table(Length.int, year, tidal_level, sample))

size.str.df<-as.data.frame(size.str.table) # как таблица данных

#убираем те пробы которых на самом деле нету


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



(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal_level), length ))
(n.samples.df<-as.data.frame(n.samples))


#средний горизонт
(mean.sqmeter.middle<-t(tapply(middle$Freq,INDEX=list(middle$year,  middle$Length.int),FUN=mean, na.rm=T)))
mean.sqmeter.middle.df<-as.data.frame(mean.sqmeter.middle)

sd.sqmeter.middle<-tapply(middle$Freq,INDEX=list(middle$year,  middle$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.middle <-t(sd.sqmeter.middle/sqrt(n.samples.df$middle))
sem.sqmeter.middle.df<-as.data.frame(sem.sqmeter.middle)


#верхний горизонт
(mean.sqmeter.high<-t(tapply(high$Freq,INDEX=list(high$year,  high$Length.int),FUN=mean, na.rm=T)))
mean.sqmeter.high.df<-as.data.frame(mean.sqmeter.high)

sd.sqmeter.high<-tapply(high$Freq,INDEX=list(high$year,  high$Length.int),FUN=sd, na.rm=T)

sem.sqmeter.high <-t(sd.sqmeter.high/sqrt(n.samples.df$high))
sem.sqmeter.high.df<-as.data.frame(sem.sqmeter.high)

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


#средний горизонт
for (j in 1:length(colnames(mean.sqmeter.middle)))
{
  pdf(file=paste("middle", colnames(mean.sqmeter.middle)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.middle[,j], nn=length.class,  z=sem.sqmeter.middle[,j])
  title(main=colnames(mean.sqmeter.middle)[j], xlab="", ylab="")
  dev.off()
  embedFonts(paste("middle", colnames(mean.sqmeter.middle)[j], ".pdf",sep="_"))
}

#верхний горизонт
for (j in 1:length(colnames(mean.sqmeter.high)))
{
  pdf(file=paste("high", colnames(mean.sqmeter.high)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter.high[,j], nn=length.class,  z=sem.sqmeter.high[,j])
  title(main=colnames(mean.sqmeter.high)[j], xlab="", ylab="")
  dev.off()
  embedFonts(paste("high", colnames(mean.sqmeter.high)[j], ".pdf",sep="_"))
}

#================= возрастная структура ====================================
(age.int<-as.factor(age))

(age.str.table<-table(age.int, year, tidal_level, sample))

age.str.df<-as.data.frame(age.str.table) # как таблица данных

#убираем те пробы которых на самом деле нету


for (i in 1:length(levels(age.str.df$year))){
  for(j in 1:length(levels(age.str.df$tidal_level))){
    (xxx<-age.str.df$sample[age.str.df$year==levels(age.str.df$year)[i] & 
                              age.str.df$tidal_level==levels(age.str.df$tidal_level)[j]]%in%
       samples.names$sample[samples.names$year==levels(age.str.df$year)[i] & 
                              samples.names$tidal_level==levels(age.str.df$tidal_level)[j]])
    antixxx<-as.logical(1-xxx)
    age.str.df$Freq[age.str.df$year==levels(age.str.df$year)[i] & 
                      age.str.df$tidal_level==levels(age.str.df$tidal_level)[j]][antixxx]<-NA
  }}




#теперь на квадратный метр
age.str.sqmeter<-age.str.df
for (i in 1:length(levels(age.str.sqmeter$year)))
{
  age.str.sqmeter$Freq[age.str.sqmeter$year==levels(age.str.sqmeter$year)[i]]<-
    age.str.sqmeter$Freq[age.str.sqmeter$year==levels(age.str.sqmeter$year)[i]] * 
    samples.squares$square[samples.squares$year==levels(age.str.sqmeter$year)[i]]
}

for (i in 1: length(levels(age.str.sqmeter$tidal_level)))
{
  write.table(assign(paste("age",levels(age.str.sqmeter$tidal_level)[i], sep="."), 
                     subset(age.str.sqmeter, age.str.sqmeter$tidal_level==levels(age.str.sqmeter$tidal_level)[i])), file=paste(levels(age.str.sqmeter$tidal_level)[i]), sep=",")
}





#средний горизонт
(age.mean.sqmeter.middle<-t(tapply(age.middle$Freq,INDEX=list(age.middle$year,  age.middle$age.int),FUN=mean, na.rm=T)))
age.mean.sqmeter.middle.df<-as.data.frame(age.mean.sqmeter.middle)

age.sd.sqmeter.middle<-tapply(age.middle$Freq,INDEX=list(age.middle$year,  age.middle$age.int),FUN=sd, na.rm=T)

age.sem.sqmeter.middle <-t(age.sd.sqmeter.middle/sqrt(n.samples.df$middle))
age.sem.sqmeter.middle.df<-as.data.frame(age.sem.sqmeter.middle)


#верхний горизонт
(age.mean.sqmeter.high<-t(tapply(age.high$Freq,INDEX=list(age.high$year,  age.high$age.int),FUN=mean, na.rm=T)))
age.mean.sqmeter.high.df<-as.data.frame(age.mean.sqmeter.high)

age.sd.sqmeter.high<-tapply(age.high$Freq,INDEX=list(age.high$year,  age.high$age.int),FUN=sd, na.rm=T)

age.sem.sqmeter.high <-t(age.sd.sqmeter.high/sqrt(n.samples.df$high))
age.sem.sqmeter.high.df<-as.data.frame(age.sem.sqmeter.high)



#средний горизонт
for (j in 1:length(colnames(mean.sqmeter.middle)))
{
  pdf(file=paste("age_middle", colnames(age.mean.sqmeter.middle)[j], ".pdf",sep="_"))
  error.bars(yv=age.mean.sqmeter.middle[,j], nn=as.numeric(levels(age.int)),  z=age.sem.sqmeter.middle[,j])
  title(main=colnames(age.mean.sqmeter.middle)[j], xlab="", ylab="")
  dev.off()
  embedFonts(paste("age_middle", colnames(age.mean.sqmeter.middle)[j], ".pdf",sep="_"))
}

#верхниц горизонт
for (j in 1:length(colnames(mean.sqmeter.high)))
{
  pdf(file=paste("age_high", colnames(age.mean.sqmeter.high)[j], ".pdf",sep="_"))
  error.bars(yv=age.mean.sqmeter.high[,j], nn=as.numeric(levels(age.int)),  z=age.sem.sqmeter.high[,j])
  title(main=colnames(age.mean.sqmeter.high)[j], xlab="", ylab="")
  dev.off()
  embedFonts(paste("high", colnames(mean.sqmeter.high)[j], ".pdf",sep="_"))
}

# ============ размерная структура в %=========================================
str(size.str.sqmeter)

#high
(sum.sizestr.sqmeter.high<-t(tapply(high$Freq,INDEX=list(high$year, high$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.high<-t(t(sum.sizestr.sqmeter.high)/colSums(sum.sizestr.sqmeter.high)*100))
#>1mm
(sum.sizestr2.sqmeter.percents.high<-t(t(sum.sizestr.sqmeter.high[2:nrow(sum.sizestr.sqmeter.high),])/
                                         colSums(sum.sizestr.sqmeter.high)*100))

# запишем в файл размерную структуру в процентах
write.table(x=as.data.frame(as.table(sum.sizestr2.sqmeter.percents.high)), file="shelpino_high_sizestr2_percent.csv", sep=";", dec=",")

#middle
(sum.sizestr.sqmeter.middle<-t(tapply(middle$Freq,INDEX=list(middle$year, middle$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.middle<-t(t(sum.sizestr.sqmeter.middle)/colSums(sum.sizestr.sqmeter.middle)*100))
#>1mm
(sum.sizestr2.sqmeter.percents.middle<-t(t(sum.sizestr.sqmeter.middle[2:nrow(sum.sizestr.sqmeter.middle),])/
                                           colSums(sum.sizestr.sqmeter.middle)*100))

# запишем в файл размерную структуру в процентах
write.table(x=as.data.frame(as.table(sum.sizestr2.sqmeter.percents.middle)), file="shelpino_middle_sizestr2_percent.csv", sep=";", dec=",")
