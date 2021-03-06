setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Lomnishniy/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Lomnishniy/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура суммарно по годам по горизонтам
ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)
#year<-factor(year)

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

length.class<-seq(1,20,1)

##>2mm mean size structure
(mean.sizestr.sqmeter2<-mean.sizestr.sqmeter[2:20,])
mean.sqmeter.df2<-as.data.frame(mean.sizestr.sqmeter2)
(sd.sizestr.sqmeter2<-sd.sizestr.sqmeter[,2:20])

n.samples<-tapply(samples.names$sample,samples.names$year, length )

(sem.sizestr.sqmeter2<-t(sd.sizestr.sqmeter2/sqrt(as.vector(n.samples))))
sem.sqmeter.df2<-as.data.frame(sem.sizestr.sqmeter2)

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




  for (j in 1:length(colnames(mean.sizestr.sqmeter)))
  {
    pdf(file=paste("Lomnishniy", colnames(mean.sizestr.sqmeter)[j], ".pdf",sep="_"))
    error.bars(yv=mean.sizestr.sqmeter[,j], nn=length.class, z=sem.sizestr.sqmeter[,j])
    title(main=colnames(mean.sizestr.sqmeter)[j], xlab="", ylab="")
    dev.off()
  }



## to .eps
#for (j in 1:length(colnames(mean.sizestr.sqmeter)))
#{
#  postscript(file=paste("YuG1", colnames(mean.sizestr.sqmeter)[j], ".eps",sep="_"))
#  error.bars(yv=mean.sizestr.sqmeter[,j], nn=length.class, z=sem.sizestr.sqmeter[,j])
#  title(main=colnames(mean.sizestr.sqmeter)[j], xlab="", ylab="")
#  dev.off()
#}

#>1mm
for (j in 1:length(colnames(mean.sizestr.sqmeter2)))
{
  pdf(file=paste("Lomnishniy2", colnames(mean.sizestr.sqmeter2)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sizestr.sqmeter2[,j], nn=length.class2, z=sem.sizestr.sqmeter2[,j])
  title(main=colnames(mean.sizestr.sqmeter2)[j], xlab="", ylab="")
  dev.off()
}


## динамика обилия
(N.sqmeter<-(t(tapply(size.str.sqmeter$Freq, list(size.str.sqmeter$year, size.str.sqmeter$sample), sum))))
(N.mean.sqmeter<-colMeans(N.sqmeter, na.rm=T))
N.sd.sqmeter<-apply(N.sqmeter, 2, sd, na.rm=T)
N.sem.sqmeter<-N.sd.sqmeter/sqrt(n.samples)
(D.n<-N.sem.sqmeter/N.mean.sqmeter*100)

# запишем численность всех в файл
write.table(data.frame(N.mean.sqmeter, N.sem.sqmeter), file="lomnishniy_N.csv", sep=";", dec=",")


pdf(file="N_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N.mean.sqmeter, x=names(N.mean.sqmeter),pch=15, main="о. Ломнишный",
     ylim=c(min(N.mean.sqmeter)-max(N.sem.sqmeter), max(N.mean.sqmeter)+max(N.sem.sqmeter)),
     xlab="год", ylab="N, экз./кв.м")
lines(seq(as.numeric(min(names(N.mean.sqmeter))),as.numeric(max(names(N.mean.sqmeter))),1), N.mean.sqmeter, pch=1, type="b")
arrows(x0=seq(as.numeric(min(names(N.mean.sqmeter))),as.numeric(max(names(N.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(N.mean.sqmeter))),as.numeric(max(names(N.mean.sqmeter))),1),
       y0=N.mean.sqmeter-N.sem.sqmeter, y1=N.mean.sqmeter+N.sem.sqmeter, angle=90, code=3, length=0.1)
dev.off()
embedFonts("N_dynamic.pdf") #встройка шрифтов в файл

## динамика без молод ( больше 2+)


(N2.sqmeter<-(t(tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"], 
                       list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"], 
                            size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]"]), sum))))
(N2.mean.sqmeter<-colMeans(N2.sqmeter, na.rm=T))
(N2.sd.sqmeter<-apply(N2.sqmeter, 2, sd, na.rm=T))
N2.sem.sqmeter<-N2.sd.sqmeter/sqrt(n.samples)
(D.n2<-N2.sem.sqmeter/N2.mean.sqmeter*100)

# запишем численность всех крупнее 1 мм в файл
write.table(data.frame(N2.mean.sqmeter, N2.sem.sqmeter), file="Lomnishniy_N2.csv", sep=";", dec=",")

# запишем пересчет обилия >1мм в пробах на квадратный метр в файл
write.table(as.data.frame(as.table(N2.sqmeter)), file="Lomnishniy_N2_in samples_sqmeter.csv", sep=";", dec=",")


pdf(file="N2_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter, x=names(N2.mean.sqmeter),pch=15, main="о. Ломнишный",
     ylim=c(min(N2.mean.sqmeter)-max(N2.sem.sqmeter), max(N2.mean.sqmeter)+max(N2.sem.sqmeter)),
     xlab="год", ylab="N, экз./кв.м")
lines(seq(as.numeric(min(names(N2.mean.sqmeter))),as.numeric(max(names(N2.mean.sqmeter))),1), N2.mean.sqmeter, pch=1, type="b")
arrows(x0=seq(as.numeric(min(names(N2.mean.sqmeter))),as.numeric(max(names(N2.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(N2.mean.sqmeter))),as.numeric(max(names(N2.mean.sqmeter))),1),
       y0=N2.mean.sqmeter-N2.sem.sqmeter, y1=N2.mean.sqmeter+N2.sem.sqmeter, angle=90, code=3, length=0.1)
dev.off()
embedFonts("N2_dynamic.pdf") #встройка шрифтов в файл

# динамика >2mm с осью 1992-2012
(y92.12<-seq(1992,2012,1))
N2.92.12.mean<-c(rep(NA,(as.numeric(min(names(N2.mean.sqmeter)))-1992)), N2.mean.sqmeter)
N2.92.12.sem<-c(rep(NA,(as.numeric(min(names(N2.sem.sqmeter)))-1992)), N2.sem.sqmeter)

pdf(file="N2_dynamic_92_12.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.92.12.mean, x=y92.12,pch=15, main="о. Ломнишный",
     ylim=c(min(N2.92.12.mean, na.rm=T)-max(N2.92.12.sem, na.rm=T), max(N2.92.12.mean, na.rm=T)+max(N2.92.12.sem, na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
lines(y92.12, N2.92.12.mean, pch=1, type="b")
arrows(x0=y92.12, 
       x1=y92.12,
       y0=N2.92.12.mean-N2.92.12.sem, y1=N2.92.12.mean+N2.92.12.sem, angle=90, code=3, length=0.1)
dev.off()
embedFonts("N2_dynamic_92_12.pdf") #встройка шрифтов в файл


#plot(y=N.mean.sqmeter, x=names(N.mean.sqmeter),pch=1)
#segments(x0=seq(as.numeric(min(names(N.mean.sqmeter))),as.numeric(max(names(N.mean.sqmeter))),1), 
#         x1=seq(as.numeric(min(names(N.mean.sqmeter))),as.numeric(max(names(N.mean.sqmeter))),1),
#         y0=N.mean.sqmeter-N.sem.sqmeter, y1=N.mean.sqmeter+N.sem.sqmeter)

##про численность 2+
#Kruskal-Wallis
(N2.df<-data.frame(samples.names, as.vector(N2.sqmeter)[!is.na(as.vector(N2.sqmeter))]))
kruskal.test(N2.df$as.vector.N2.sqmeter...is.na.as.vector.N2.sqmeter... ~ N2.df$year)
boxplot(N2.df$as.vector.N2.sqmeter...is.na.as.vector.N2.sqmeter... ~ N2.df$year)
(tukey.01.12<-TukeyHSD(aov(lm(N2.df$as.vector.N2.sqmeter...is.na.as.vector.N2.sqmeter... ~ as.factor(N2.df$year)))))

(N2.mean.07.12<-mean(as.vector(N2.sqmeter), na.rm=T))
(N2.sd.07.12<-sd(as.vector(N2.sqmeter), na.rm=T))
N2.sem..07.12<-N2.sd.07.12/sqrt(sum(n.samples[1:(2012-2007+1)]))
(D.n2..07.12<-N2.sem.07.12/N2.mean.01.10*100)


##размерная структура в %
(sum.sizestr.sqmeter<-t(tapply(size.str.sqmeter$Freq,INDEX=list(size.str.sqmeter$year, size.str.sqmeter$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents<-t(t(sum.sizestr.sqmeter)/colSums(sum.sizestr.sqmeter)*100))

#>1mm
(sum.sizestr2.sqmeter.percents<-t(t(sum.sizestr.sqmeter[2:nrow(sum.sizestr.sqmeter),])/
                                    colSums(sum.sizestr.sqmeter[2:nrow(sum.sizestr.sqmeter),])*100))
# запишем в файл размерную структуру в процентах
write.table(x=as.data.frame(as.table(sum.sizestr2.sqmeter.percents)), file="lomnishniy_sizestr2_percent.csv", sep=";", dec=",")

## динамика максимального размера
str(ishodnik)
(Length.max<-tapply(Length.mm, year, max, na.rm=T))
plot(x=names(Length.max), y=Length.max, type=none)


pdf(file="L_max.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=names(Length.max), y=Length.max, type="none", main="о. Ломнишный", xlab="год", ylab="L max, мм")
lines(x=names(Length.max), y=Length.max, pch=1, type="b")
dev.off()
embedFonts("L_max.pdf") #встройка шрифтов в файл

## рассчетная биомасса по Максимовичу и др., 1993
biomass.count<-0.00016*(Length.mm^2.96)
(biomass.samples<-tapply(biomass.count, list(year, sample), sum, na.rm=T))

(biomass.sqmeter<-biomass.samples*samples.squares$square)

(B.mean.sqmeter<-rowMeans(biomass.sqmeter, na.rm=T))
(B.sd.sqmeter<-apply(biomass.sqmeter, 1, sd, na.rm=T))
n.samples<-tapply(samples.names$sample,samples.names$year, length )
(B.sem.sqmeter<-B.sd.sqmeter/sqrt(n.samples))
(D.b<-B.sem.sqmeter/B.mean.sqmeter*100)

pdf(file="B_count_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter, x=names(B.mean.sqmeter),pch=15, main="о. Ломнишный", 
     ylim=c(min(B.mean.sqmeter)-max(B.sem.sqmeter), max(B.mean.sqmeter)+max(B.sem.sqmeter)),
     xlab="год", ylab="B, г/кв.м")
lines(seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1), B.mean.sqmeter, pch=1, type="b")
arrows(x0=seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1),
       y0=B.mean.sqmeter-B.sem.sqmeter, y1=B.mean.sqmeter+B.sem.sqmeter, angle=90, code=3, length=0.1)
dev.off()
embedFonts("B_count_dynamic.pdf") #встройка шрифтов в файл

## рассчетная биомасса только с учетом >1mm особей
biomass2.count<-0.00016*(Length.mm[Length.mm>1.0]^2.96)
(biomass2.samples<-tapply(biomass2.count, list(year[Length.mm>1.0], sample[Length.mm>1.0]), sum, na.rm=T))

(biomass2.sqmeter<-biomass2.samples*samples.squares$square)

(B2.mean.sqmeter<-rowMeans(biomass2.sqmeter, na.rm=T))
(B2.sd.sqmeter<-apply(biomass2.sqmeter, 1, sd, na.rm=T))
n.samples<-tapply(samples.names$sample,samples.names$year, length )
(B2.sem.sqmeter<-B2.sd.sqmeter/sqrt(n.samples))
(D.b2<-B2.sem.sqmeter/B2.mean.sqmeter*100)

#запишем в файл рассчетную биомассу
write.table(data.frame(B2.mean.sqmeter, B2.sem.sqmeter), file="lomnishniy_B2_mean.csv",sep=";", dec=",")

pdf(file="B2_count_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B2.mean.sqmeter, x=names(B2.mean.sqmeter),pch=15, main="о. Ломнишный", 
     ylim=c(min(B2.mean.sqmeter)-max(B2.sem.sqmeter), max(B2.mean.sqmeter)+max(B2.sem.sqmeter)),
     xlab="год", ylab="B, г/кв.м")
lines(seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1), B2.mean.sqmeter, pch=1, type="b")
arrows(x0=seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1),
       y0=B2.mean.sqmeter-B2.sem.sqmeter, y1=B2.mean.sqmeter+B2.sem.sqmeter, angle=90, code=3, length=0.1)
dev.off()
embedFonts("B2_count_dynamic.pdf") #встройка шрифтов в файл

#влияние особей <=1mm на рассчетную биомассу
pdf(file="B_B2_compare.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter, x=names(B.mean.sqmeter),pch=15, main="о. Ломнишный", type="n",
     ylim=c(min(min(B.mean.sqmeter)-max(B.sem.sqmeter), min(B2.mean.sqmeter)-max(B2.sem.sqmeter)), 
            max(max(B.mean.sqmeter)+max(B.sem.sqmeter), max(B2.mean.sqmeter)+max(B2.sem.sqmeter))),
     xlab="год", ylab="B, г/кв.м")
#B
lines(seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1), B.mean.sqmeter, pch=15, col=2, type="b")
arrows(x0=seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(B.mean.sqmeter))),as.numeric(max(names(B.mean.sqmeter))),1),
       y0=B.mean.sqmeter-B.sem.sqmeter, y1=B.mean.sqmeter+B.sem.sqmeter, angle=90, code=3, length=0.1, col=2)
#B2
lines(seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1), B2.mean.sqmeter, pch=2, col=4, type="b")
arrows(x0=seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1),
       y0=B2.mean.sqmeter-B2.sem.sqmeter, y1=B2.mean.sqmeter+B2.sem.sqmeter, angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("B_B2_compare.pdf") #встройка шрифтов в файл


## динамика молоди <2mm и половозрелых  >8mm


(young.old.int<-cut(Length.mm, breaks=c(1,2.5,7.9,max(Length.mm, na.rm=T))))

(young.old.table<-table(young.old.int,year,sample))

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



(mean.young.old.sqmeter<-t(tapply(young.old.sqmeter$Freq,INDEX=list(young.old.sqmeter$year, young.old.sqmeter$young.old.int),FUN=mean, na.rm=T)))

(sd.young.old.sqmeter<-tapply(young.old.sqmeter$Freq,INDEX=list(young.old.sqmeter$year, young.old.sqmeter$young.old.int),FUN=sd, na.rm=T))

n.samples<-tapply(samples.names$sample,samples.names$year, length )

(sem.young.old.sqmeter <-t(sd.young.old.sqmeter/sqrt(as.vector(n.samples))))

write.table(t(mean.young.old.sqmeter), file="lomnishniy_young_old_mean.csv", sep=";", dec=",")

#корреляция - молодь и половозрелые
(spearman.young.old.mean<-cor.test(mean.young.old.sqmeter[1,], mean.young.old.sqmeter[3,], method="spearman"))


# молодь и половозрелые - график
pdf(file="young_old.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.young.old.sqmeter[1,], x=colnames(mean.young.old.sqmeter),pch=15, type="n", main="о. Ломнишный", 
     #     ylim=c(min(mean.young.old.sqmeter[1,], mean.young.old.sqmeter[3,])-max(sem.young.old.sqmeter[1,], sem.young.old.sqmeter[3,]), 
     #            max(mean.young.old.sqmeter[1,], mean.young.old.sqmeter[3,])+max(sem.young.old.sqmeter[1,], sem.young.old.sqmeter[3,])),
     ylim=c(0, 
            max(mean.young.old.sqmeter[1,], mean.young.old.sqmeter[3,])+max(sem.young.old.sqmeter[1,], sem.young.old.sqmeter[3,])),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1), 
      mean.young.old.sqmeter[1,], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1),
       y0=mean.young.old.sqmeter[1,]-sem.young.old.sqmeter[1,], 
       y1=mean.young.old.sqmeter[1,]+sem.young.old.sqmeter[1,], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1), 
      mean.young.old.sqmeter[3,], pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1),
       y0=mean.young.old.sqmeter[3,]-sem.young.old.sqmeter[3,], 
       y1=mean.young.old.sqmeter[3,]+sem.young.old.sqmeter[3,], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old.pdf") #встройка шрифтов в файл

#динамика молоди и половозрелых в %
(sum.young.old<-t(tapply(young.old.sqmeter$Freq[young.old.sqmeter$young.old.int!="(0,1]"],
                         INDEX=list(young.old.sqmeter$year[young.old.sqmeter$young.old.int!="(0,1]"], 
                                    young.old.sqmeter$young.old.int[young.old.sqmeter$young.old.int!="(0,1]"]),
                         FUN=sum, na.rm=T)))
(young.old.percents<-t(t(sum.young.old)/colSums(sum.young.old, na.rm=T))*100)

write.table(t(young.old.percents), file="lomnishniy_young_old_percent.csv", sep=";", dec=",")

#корреляция - молодь и половозрелые в %
str(young.old.percents)
(spearman.young.old.sum.percent<-cor.test(young.old.percents[1,2:6], young.old.percents[3,1:5], method="spearman"))
plot(x=young.old.percents[3,1:5], y=young.old.percents[1,2:6])

#и график в %
pdf(file="young_old_percents.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=young.old.percents[1,], x=colnames(young.old.percents),pch=15, type="n", main="о. Ломнишный", 
     #     ylim=c(min(young.old.percents[1,], young.old.percents[3,])-max(sem.young.old.sqmeter[1,], sem.young.old.sqmeter[3,]), 
     #            max(young.old.percents[1,], young.old.percents[3,])+max(sem.young.old.sqmeter[1,], sem.young.old.sqmeter[3,])),
     ylim=c(0, 
            max(young.old.percents[1,], young.old.percents[3,])),
     xlab="год", ylab="доля от общей численности, %")
#молодь
lines(seq(as.numeric(min(colnames(young.old.percents))),as.numeric(max(colnames(young.old.percents))),1), 
      young.old.percents[1,], pch=15, type="b", col=2)
#половозрелые
lines(seq(as.numeric(min(colnames(young.old.percents))),as.numeric(max(colnames(young.old.percents))),1), 
      young.old.percents[3,], pch=16, type="b", col=4)
dev.off()
embedFonts("young_old_percents.pdf") #встройка шрифтов в файл

# численность общая и численность молоди - график
pdf(file="young_all.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter, x=colnames(mean.young.old.sqmeter),pch=15, type="n", main="о. Ломнишный", 
     #     ylim=c(min(mean.young.old.sqmeter[1,], mean.young.old.sqmeter[3,])-max(sem.young.old.sqmeter[1,], sem.young.old.sqmeter[3,]), 
     #            max(mean.young.old.sqmeter[1,], mean.young.old.sqmeter[3,])+max(sem.young.old.sqmeter[1,], sem.young.old.sqmeter[3,])),
     ylim=c(0, 
            max(mean.young.old.sqmeter[1,], N2.mean.sqmeter)+max(sem.young.old.sqmeter[1,], N2.sem.sqmeter)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1), 
      mean.young.old.sqmeter[1,], pch=15, type="b", col=2)
arrows(x0=seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1),
       y0=mean.young.old.sqmeter[1,]-sem.young.old.sqmeter[1,], 
       y1=mean.young.old.sqmeter[1,]+sem.young.old.sqmeter[1,], angle=90, code=3, length=0.1, col=2)
#все
lines(seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1), 
      N2.mean.sqmeter, pch=16, type="b", col=4)
arrows(x0=seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1), 
       x1=seq(as.numeric(min(colnames(mean.young.old.sqmeter))),as.numeric(max(colnames(mean.young.old.sqmeter))),1),
       y0=N2.mean.sqmeter-N2.sem.sqmeter, 
       y1=N2.mean.sqmeter+N2.sem.sqmeter, angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old.pdf") #встройка шрифтов в файл

##  численность молоди и биомасса половозрелых
##рассчетная биомасса только с учетом >8mm особей
biomass8.count<-0.00016*(Length.mm[Length.mm>8.0]^2.96)
(biomass8.samples<-tapply(biomass8.count, list(year[Length.mm>8.0], sample[Length.mm>8.0]), sum, na.rm=T))

(biomass8.sqmeter<-biomass8.samples*samples.squares$square)

(B8.mean.sqmeter<-rowMeans(biomass8.sqmeter, na.rm=T))
(B8.sd.sqmeter<-apply(biomass8.sqmeter, 1, sd, na.rm=T))
n.samples<-tapply(samples.names$sample,samples.names$year, length )
(B8.sem.sqmeter<-B8.sd.sqmeter/sqrt(n.samples))

write.table(B8.mean.sqmeter, file="lomnishniy_biomass_old.csv", sep=";", dec=",")
