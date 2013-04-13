setwd("~/Dropbox/PhD_thesis/White_Sea/Estuatiy_Luvenga")

# размерная структура суммарно по годам по горизонтам
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

#>2mm mean size structure
(mean.sizestr.sqmeter2<-mean.sizestr.sqmeter[2:20,])
mean.sqmeter.df2<-as.data.frame(mean.sizestr.sqmeter2)
(sd.sizestr.sqmeter2<-sd.sizestr.sqmeter[,2:20])

n.samples<-tapply(samples.names$sample,samples.names$year, length )

(sem.sizestr.sqmeter2<-t(sd.sizestr.sqmeter2/sqrt(as.vector(n.samples))))
sem.sqmeter.df2<-as.data.frame(sem.sizestr.sqmeter2)


length.class<-seq(1,20,1)
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
    pdf(file=paste("sizestr", colnames(mean.sizestr.sqmeter)[j], ".pdf",sep="_"))
    error.bars(yv=mean.sizestr.sqmeter[,j], nn=length.class, z=sem.sizestr.sqmeter[,j])
    title(main=colnames(mean.sizestr.sqmeter)[j], xlab="", ylab="")
    dev.off()
  }

#>2mm
for (j in 1:length(colnames(mean.sizestr.sqmeter2)))
{
  pdf(file=paste("sizestr2", colnames(mean.sizestr.sqmeter2)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sizestr.sqmeter2[,j], nn=length.class2, z=sem.sizestr.sqmeter2[,j])
  title(main=colnames(mean.sizestr.sqmeter2)[j], xlab="", ylab="")
  dev.off()
}

#динамика обилия
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

#динамика без молод ( больше 2+)
(N2.sqmeter<-(t(tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"], 
                       list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"],
                            size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]"]), sum))))
(N2.mean.sqmeter<-colMeans(N2.sqmeter, na.rm=T))
N2.sd.sqmeter<-apply(N2.sqmeter, 2, sd, na.rm=T)
N2.sem.sqmeter<-N2.sd.sqmeter/sqrt(n.samples)
#точность учета
(D.n2<-N2.sem.sqmeter/N2.mean.sqmeter*100)

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

#про численность 2+
#период 1992-1998

(N2.92.98<-(t(tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]" & as.vector(size.str.sqmeter$year)<=1998], 
                       list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]" & as.vector(size.str.sqmeter$year)<=1998],
                            size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]" & as.vector(size.str.sqmeter$year)<=1998]), sum))))
(N2.mean.92.98<-mean(as.vector(N2.92.98), na.rm=T))
(N2.sd.92.98<-sd(as.vector(N2.92.98), na.rm=T))
N2.sem.92.98<-N2.sd.92.98/sqrt(sum(n.samples[1:(1998-1992+1)]))
(D.n2.92.98<-N2.sem.92.98/N2.mean.92.98*100)

#период 1999-2012

(N2.99.12<-(t(tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]" & as.vector(size.str.sqmeter$year)>=1999], 
                     list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]" & as.vector(size.str.sqmeter$year)>=1999],
                          size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]" & as.vector(size.str.sqmeter$year)>=1999]), sum))))
(N2.mean.99.12<-mean(as.vector(N2.99.12), na.rm=T))
(N2.sd.99.12<-sd(as.vector(N2.99.12), na.rm=T))
N2.sem.99.12<-N2.sd.99.12/sqrt(sum(n.samples[1:(2012-1999+1)]))
(D.n2.99.12<-N2.sem.99.12/N2.mean.99.12*100)

#период 1999-2006
(N2.99.06<-(t(tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]" 
                                           & as.vector(size.str.sqmeter$year)>=1999 & as.vector(size.str.sqmeter$year)<=2006], 
                     list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]" 
                                                & as.vector(size.str.sqmeter$year)>=1999 & as.vector(size.str.sqmeter$year)<=2006],
                          size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]" 
                                                  & as.vector(size.str.sqmeter$year)>=1999 & as.vector(size.str.sqmeter$year)<=2006]),
                     sum))))
(N2.mean.99.06<-mean(as.vector(N2.99.06), na.rm=T))
(N2.sd.99.06<-sd(as.vector(N2.99.06), na.rm=T))
N2.sem.99.06<-N2.sd.99.06/sqrt(sum(n.samples[1:(2006-1999+1)]))
(D.n2.99.06<-N2.sem.99.06/N2.mean.99.06*100)

#период 2007-2012
(N2.07.12<-(t(tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]" 
                                           & as.vector(size.str.sqmeter$year)>=2007], 
                     list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]" 
                                                & as.vector(size.str.sqmeter$year)>=2007],
                          size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]" 
                                                  & as.vector(size.str.sqmeter$year)>=2007]),
                     sum))))
(N2.mean.07.12<-mean(as.vector(N2.07.12), na.rm=T))
(N2.sd.07.12<-sd(as.vector(N2.07.12), na.rm=T))
N2.sem.07.12<-N2.sd.07.12/sqrt(sum(n.samples[1:(2012-2007+1)]))
(D.n2.07.12<-N2.sem.07.12/N2.mean.07.12*100)

#wilcox-test
#до 1999 года и после
wilcox.test(as.vector(N2.92.98), as.vector(N2.99.12),na.rm=T)

#1999-2006 и 2007-2012
wilcox.test(as.vector(N2.99.06), as.vector(N2.07.12),na.rm=T)

#размерная структура в %
str(size.str.sqmeter)
(sum.sizestr.sqmeter<-t(tapply(size.str.sqmeter$Freq,INDEX=list(size.str.sqmeter$year, size.str.sqmeter$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents<-sum.sizestr.sqmeter/colSums(sum.sizestr.sqmeter)*100)

for (j in 1:length(colnames(sum.sizestr.sqmeter.percents)))
{
  pdf(file=paste("sizestr_percents", colnames(sum.sizestr.sqmeter.percents)[j], ".pdf",sep="_"))
  barplot(sum.sizestr.sqmeter.percents[,j], length.class)
  title(main=colnames(sum.sizestr.sqmeter.percents)[j], xlab="", ylab="")
  dev.off()
}

#Principal component analisys
#sizestr.pca<-prcomp(t(sum.sizestr.sqmeter.percents))
sizestr.pca<-princomp(t(sum.sizestr.sqmeter.percents)) #princomp хочет чтобы признаков было больше чем групп. Тут 21 и 20 - т.е. это возможно
sizestr.p<-predict(sizestr.pca)
loadings(sizestr.pca) #с prcomp похоже не работает :(
plot(sizestr.pca)


#1х2компоненты
plot(sizestr.p[,1:2], type="n", xlab="PC1", ylab="PC2")
text(sizestr.p[,1:2],
     labels=abbreviate(colnames(sum.sizestr.sqmeter.percents)))

biplot(sizestr.pca)

#1х3компоненты
plot(sizestr.p[,c(1,3)], type="n", xlab="PC1", ylab="PC3")
text(sizestr.p[,c(1,3)],
     labels=abbreviate(colnames(sum.sizestr.sqmeter.percents)))
#2х3компоненты
plot(sizestr.p[,2:3], type="n", xlab="PC2", ylab="PC3")
text(sizestr.p[,2:3],
     labels=abbreviate(colnames(sum.sizestr.sqmeter.percents)))


#все крупнее 1+
str(size.str.sqmeter)
(sum.sizestr.sqmeter2<-t(tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"],
                                INDEX=list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"], 
                                           size.str.sqmeter$Length.int[size.str.sqmeter$Length.int!="(0,1]"]),
                                FUN=sum, na.rm=T)))
sum.sizestr.sqmeter2<-sum.sizestr.sqmeter2[2:19,]
(sum.sizestr.sqmeter2.percents<-sum.sizestr.sqmeter2/colSums(sum.sizestr.sqmeter2, na.rm=T)*100)

#Principal component analisys
sizestr2.pca<-prcomp(t(sum.sizestr.sqmeter2.percents))
plot(sizestr2.pca)
sizestr2.p<-predict(sizestr2.pca)

plot(sizestr2.p[,1:2], type="n", xlab="PC1", ylab="PC2")
text(sizestr2.p[,1:2],
     labels=abbreviate(colnames(sum.sizestr.sqmeter2.percents)))
lines(sizestr2.p[,1:2], pch=3)
biplot(sizestr2.pca)

#cluster analisys
#install.packages("cluster")
library("cluster")

(sizestr.dist<-daisy(t(sum.sizestr.sqmeter)))
sizestr.h <- hclust(sizestr.dist, method="ward")
plot(sizestr.h, labels=colnames(sum.sizestr.sqmeter), main="")

#>2mm
(sizestr2.dist<-daisy(t(sum.sizestr.sqmeter2)))
sizestr2.h <- hclust(sizestr2.dist, method="ward")
plot(sizestr2.h, labels=colnames(sum.sizestr.sqmeter2), main="")

#cluster with bootstrap
install.packages