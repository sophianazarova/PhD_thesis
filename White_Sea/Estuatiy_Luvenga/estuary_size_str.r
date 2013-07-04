setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Estuatiy_Luvenga")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Estuatiy_Luvenga")


ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
biomass.measure<-read.table(file="biomass.csv", sep=";", dec=",", head=T)
attach(ishodnik)

#year<-factor(year)

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

## попытка построить трехмерный график
#install.packages("lattice", "ggplot2")
#library("lattice", "ggplot2")

# пример трехмерного графика
#state.info <- data.frame(name = state.name, long = state.center$x,
#                         lat = state.center$y, area = state.x77[, "Area"],
#                         population = 1000 * state.x77[, "Population"])
#state.info$density <- with(state.info, population/area)
#
#pl <- cloud(density ~ long + lat, state.info, subset = !(name %in% c("Alaska", "Hawaii")), 
#            type = "h", lwd = 2, zlim = c(0, max(state.info$density)), scales = list(arrows = FALSE))
#print(pl)

#mean.sqmeter.df2.2<-as.data.frame(as.table(mean.sizestr.sqmeter2))
#pl <- cloud(Freq ~ Var1 + Var2, mean.sqmeter.df2.2, 
            type = "h", lwd = 2, zlim = c(0, max(mean.sqmeter.df2.2$Freq)), scales = list(arrows = FALSE))
#print(pl)


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

write.table(N2.mean.sqmeter, file="estuary_N2.csv", sep=";", dec=",")

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

##про численность 2+
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

##Principal component analisys
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


##все крупнее 1+
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

##cluster analisys
#install.packages("cluster")
library("cluster")

(sizestr.dist<-daisy(t(sum.sizestr.sqmeter)))
sizestr.h <- hclust(sizestr.dist, method="ward")
plot(sizestr.h, labels=colnames(sum.sizestr.sqmeter), main="")

#>2mm
(sizestr2.dist<-daisy(t(sum.sizestr.sqmeter2)))
sizestr2.h <- hclust(sizestr2.dist, method="ward")
plot(sizestr2.h, labels=colnames(sum.sizestr.sqmeter2), main="")


##динамика максимального размера
str(ishodnik)
(Length.max<-tapply(Length.mm, year, max, na.rm=T))
plot(x=names(Length.max), y=Length.max, type=none)


pdf(file="L_max.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=names(Length.max), y=Length.max, type="none", main="Эстуарий р. Лувеньги", xlab="год", ylab="L max, мм")
lines(x=names(Length.max), y=Length.max, pch=1, type="b")
dev.off()
embedFonts("L_max.pdf") #встройка шрифтов в файл


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

##рассчетная биомасса только с учетом >1mm особей
biomass2.count<-0.00016*(Length.mm[Length.mm>1.0]^2.96)
(biomass2.samples<-tapply(biomass2.count, list(year[Length.mm>1.0], sample[Length.mm>1.0]), sum, na.rm=T))

(biomass2.sqmeter<-biomass2.samples*samples.squares$square)

(B2.mean.sqmeter<-rowMeans(biomass2.sqmeter, na.rm=T))
(B2.sd.sqmeter<-apply(biomass.sqmeter, 1, sd, na.rm=T))
n.samples<-tapply(samples.names$sample,samples.names$year, length )
(B2.sem.sqmeter<-B2.sd.sqmeter/sqrt(n.samples))
(D.b2<-B2.sem.sqmeter/B2.mean.sqmeter*100)

pdf(file="B2_count_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B2.mean.sqmeter, x=names(B2.mean.sqmeter),pch=15, main="Эстуарий р. Лувеньги", 
     ylim=c(min(B2.mean.sqmeter)-max(B2.sem.sqmeter), max(B2.mean.sqmeter)+max(B2.sem.sqmeter)),
     xlab="год", ylab="B, г/кв.м")
lines(seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1), B2.mean.sqmeter, pch=1, type="b")
arrows(x0=seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1), 
       x1=seq(as.numeric(min(names(B2.mean.sqmeter))),as.numeric(max(names(B2.mean.sqmeter))),1),
       y0=B2.mean.sqmeter-B2.sem.sqmeter, y1=B2.mean.sqmeter+B2.sem.sqmeter, angle=90, code=3, length=0.1)
dev.off()
embedFonts("B2_count_dynamic.pdf") #встройка шрифтов в файл

##влияние особей <=1mm на рассчетную биомассу
pdf(file="B_B2_compare.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter, x=names(B.mean.sqmeter),pch=15, main="Эстуарий р. Лувеньги", type="n",
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

##измеренная биомасса (реальная)
str(biomass.measure)
(biomass.real.m<-tapply(biomass.measure$biomass.g, list(biomass.measure$year, biomass.measure$sample), function(x){x*1}))
(biomass.real.sqmeter<-biomass.real.m*samples.squares$square)

(Br.mean.sqmeter<-rowMeans(biomass.real.sqmeter, na.rm=T))
(Br.sd.sqmeter<-apply(biomass.real.sqmeter, 1, sd, na.rm=T))
(Br.sem.sqmeter<-Br.sd.sqmeter/sqrt(n.samples))
(D.br<-Br.sem.sqmeter/Br.mean.sqmeter*100)

##сравнение рассчетной и реальной биомассы
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


##динамика молоди <2mm и половозрелых  >8mm


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
str(young.old.sqmeter)


(mean.young.old.sqmeter<-t(tapply(young.old.sqmeter$Freq,INDEX=list(young.old.sqmeter$year, young.old.sqmeter$young.old.int),FUN=mean, na.rm=T)))

(sd.young.old.sqmeter<-tapply(young.old.sqmeter$Freq,INDEX=list(young.old.sqmeter$year, young.old.sqmeter$young.old.int),FUN=sd, na.rm=T))

n.samples<-tapply(samples.names$sample,samples.names$year, length )

(sem.young.old.sqmeter <-t(sd.young.old.sqmeter/sqrt(as.vector(n.samples))))

write.table(t(mean.young.old.sqmeter), file="estuary_young_old_mean.csv", sep=";", dec=",")


# молодь и половозрелые - график
pdf(file="young_old.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.young.old.sqmeter[1,], x=colnames(mean.young.old.sqmeter),pch=15, type="n", main="Эстуарий р. Лувеньги", 
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

#корреляция - молодь и половозрелые
(spearman.young.old.mean<-cor.test(mean.young.old.sqmeter[1,2:21], mean.young.old.sqmeter[3,1:20], method="spearman"))
plot(x=mean.young.old.sqmeter[3,1:20], y=mean.young.old.sqmeter[1,2:21])

#динамика молоди и половозрелых в %
(sum.young.old<-t(tapply(young.old.sqmeter$Freq[young.old.sqmeter$young.old.int!="(0,1]"],
                                INDEX=list(young.old.sqmeter$year[young.old.sqmeter$young.old.int!="(0,1]"], 
                                           young.old.sqmeter$young.old.int[young.old.sqmeter$young.old.int!="(0,1]"]),
                                FUN=sum, na.rm=T)))
(young.old.percents<-t(t(sum.young.old)/colSums(sum.young.old, na.rm=T))*100)

write.table(t(young.old.percents), file="estuary_young_old_percent.csv", sep=";", dec=",")

#корреляция - молодь и половозрелые в %
(spearman.young.old.sum.percent<-cor.test(young.old.percents[1,2:21], young.old.percents[3,1:20], method="spearman"))
plot(x=young.old.percents[3,1:20], y=young.old.percents[1,2:21])

#и график в %
pdf(file="young_old_percents.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=young.old.percents[1,], x=colnames(young.old.percents),pch=15, type="n", main="Эстуарий р. Лувеньги", 
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
plot(y=N2.mean.sqmeter, x=colnames(mean.young.old.sqmeter),pch=15, type="n", main="Эстуарий р. Лувеньги", 
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

write.table(B8.mean.sqmeter, file="estuary_biomass_old.csv", sep=";", dec=",")

plot(x=B8.mean.sqmeter[1:length(B8.mean.sqmeter)-1], y=mean.young.old.sqmeter[1,][2:length(B8.mean.sqmeter)])
