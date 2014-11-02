setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/Porchnikha/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)


ishodnik<-read.table(file="length_age.csv", sep=";", dec=",", head=T)
attach(ishodnik)

str(ishodnik)

# ======= размерная структура суммарно ======================================
max(Length.mm)
Length.int<-cut(Length.mm, breaks=seq(0,21,1))

(size.str.table<-table(Length.int,sample))

size.str.df<-as.data.frame(size.str.table)

#теперь на квадратный метр
size.str.sqmeter<-size.str.df
size.str.sqmeter$Freq<-size.str.sqmeter$Freq*30


#среднее
# tapply выдает как резудьтат матрицу
(mean.sizestr.sqmeter<-t(tapply(size.str.sqmeter$Freq, size.str.sqmeter$Length.int,FUN=mean, na.rm=T)))
(mean.sqmeter.df<-as.data.frame(mean.sizestr.sqmeter))
(sd.sizestr.sqmeter<-tapply(size.str.sqmeter$Freq, size.str.sqmeter$Length.int, FUN=sd, na.rm=T))

(sem.sizestr.sqmeter <-t(sd.sizestr.sqmeter/sqrt(as.vector(length(levels(sample))))))
sem.sqmeter.df<-as.data.frame(sem.sizestr.sqmeter)

length.class<-seq(1,21,1)

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

pdf(file="sizestr2007.pdf")
error.bars(yv=mean.sizestr.sqmeter, nn=length.class, z=sem.sizestr.sqmeter)
title(main="2008", xlab="", ylab="")
dev.off()
embedFonts("sizestr2007.pdf") #встройка шрифтов в файл


# ====== возрастная структура =================================================
(age.int<-as.factor(age))

(age.str.table<-table(age.int,sample))

age.str.df<-as.data.frame(age.str.table)

#теперь на квадратный метр
age.str.sqmeter<-age.str.df
age.str.sqmeter$Freq<-age.str.sqmeter$Freq*30


#среднее
# tapply выдает как резудьтат матрицу
(mean.agestr.sqmeter<-t(tapply(age.str.sqmeter$Freq, age.str.sqmeter$age.int,FUN=mean, na.rm=T)))
(mean.sqmeter.df<-as.data.frame(mean.agestr.sqmeter))
(sd.agestr.sqmeter<-tapply(age.str.sqmeter$Freq, age.str.sqmeter$age.int, FUN=sd, na.rm=T))

(sem.agestr.sqmeter <-t(sd.agestr.sqmeter/sqrt(as.vector(length(levels(sample))))))
sem.sqmeter.df<-as.data.frame(sem.agestr.sqmeter)


pdf(file="agestr2007.pdf")
error.bars(yv=mean.agestr.sqmeter, nn=as.numeric(levels(age.int)), z=sem.agestr.sqmeter)
title(main="2008", xlab="", ylab="")
dev.off()
embedFonts("agestr2007.pdf") #встройка шрифтов в файл

# ========= размерная структура в % ============================================
str(size.str.sqmeter)
(sum.sizestr.sqmeter<-t(tapply(size.str.sqmeter$Freq,INDEX=size.str.sqmeter$Length.int,FUN=sum, na.rm=T)))

(sum.sizestr.sqmeter.percents<-t(t(sum.sizestr.sqmeter)/rowSums(sum.sizestr.sqmeter)*100))

#>1mm
(sum.sizestr2.sqmeter.percents<-t(t(sum.sizestr.sqmeter[,2:ncol(sum.sizestr.sqmeter)])/
                                    rowSums(sum.sizestr.sqmeter)*100))

# запишем в файл размерную структуру в процентах
write.table(x=as.data.frame(as.table(sum.sizestr2.sqmeter.percents)), file="porchnikha_sizestr2_percent.csv", sep=";", dec=",")
