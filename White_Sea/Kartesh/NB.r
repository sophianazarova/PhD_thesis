setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Kartesh/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="NB_samples.csv", sep=";", dec=",", head=T)
samples<-read.table(file="samples.csv", sep=";", dec=",", head=T)

#пересчитываем на квадратный метр
ishodnik$N.sqmeter<-ishodnik$N.sample*20
ishodnik$B.sqmeter<-ishodnik$B.sample*20

attach(ishodnik)

str(ishodnik)
str(samples)

(n.samples<-tapply(samples$samples, list(samples$year, samples$area), FUN=sum))

#считаем численность
(N.mean.sqmeter<-tapply(N.sqmeter, list(year, area), FUN=mean, na.rm=T))
(N.sd.sqmeter<-tapply(N.sqmeter, list(year, area), FUN=sd, na.rm=T))
(N.sem.sqmeter<-N.sd.sqmeter/sqrt(n.samples))

(D.n<-N.sem.sqmeter/N.mean.sqmeter*100)


#Сельдяная
pdf(file="N_dynamic_Seldyanaya.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N.mean.sqmeter[,1], x=rownames(N.mean.sqmeter),pch=15, main="г. Сельдяная", 
     ylim=c(min(N.mean.sqmeter[,1])-max(N.sem.sqmeter[,1]), max(N.mean.sqmeter[,1])+max(N.sem.sqmeter[,1])),
     xlab="год", ylab="N, экз./кв.м")
lines(as.numeric(rownames(N.mean.sqmeter)), N.mean.sqmeter[,1], pch=1, type="b")
arrows(x0=as.numeric(rownames(N.mean.sqmeter)), 
       x1=as.numeric(rownames(N.mean.sqmeter)),
       y0=N.mean.sqmeter[,1]-N.sem.sqmeter[,1], y1=N.mean.sqmeter[,1]+N.sem.sqmeter[,1], angle=90, code=3, length=0.1)
dev.off()
embedFonts("N_dynamic_Seldyanaya.pdf") #встройка шрифтов в файл

#Медвежья
pdf(file="N_dynamic_Medvezhya.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N.mean.sqmeter[,2], x=rownames(N.mean.sqmeter),pch=15, main="г. Сельдяная", 
     ylim=c(min(N.mean.sqmeter[,2])-max(N.sem.sqmeter[,2]), max(N.mean.sqmeter[,2])+max(N.sem.sqmeter[,2])),
     xlab="год", ylab="N, экз./кв.м")
lines(as.numeric(rownames(N.mean.sqmeter)), N.mean.sqmeter[,2], pch=1, type="b")
arrows(x0=as.numeric(rownames(N.mean.sqmeter)), 
       x1=as.numeric(rownames(N.mean.sqmeter)),
       y0=N.mean.sqmeter[,2]-N.sem.sqmeter[,2], y1=N.mean.sqmeter[,2]+N.sem.sqmeter[,2], angle=90, code=3, length=0.1)
dev.off()
embedFonts("N_dynamic_Medvezhya.pdf") #встройка шрифтов в файл

# c 1992 года!
#Сельдяная
pdf(file="N_dynamic_Seldyanaya_1992_2012.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N.mean.sqmeter[6:26,1], x=rownames(N.mean.sqmeter)[6:26],pch=15, main="г. Сельдяная", 
     ylim=c(min(N.mean.sqmeter[6:26,1])-max(N.sem.sqmeter[6:26,1]), max(N.mean.sqmeter[6:26,1])+max(N.sem.sqmeter[6:26,1])),
     xlab="год", ylab="N, экз./кв.м")
lines(as.numeric(rownames(N.mean.sqmeter)[6:26]), N.mean.sqmeter[6:26,1], pch=1, type="b")
arrows(x0=as.numeric(rownames(N.mean.sqmeter)[6:26]), 
       x1=as.numeric(rownames(N.mean.sqmeter)[6:26]),
       y0=N.mean.sqmeter[6:26,1]-N.sem.sqmeter[6:26,1], y1=N.mean.sqmeter[6:26,1]+N.sem.sqmeter[6:26,1], angle=90, code=3, length=0.1)
dev.off()
embedFonts("N_dynamic_Seldyanaya_1992_2012.pdf") #встройка шрифтов в файл

#Медвежья
pdf(file="N_dynamic_Medvezhya_1992_2012.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N.mean.sqmeter[6:26,2], x=rownames(N.mean.sqmeter)[6:26],pch=15, main="г. Сельдяная", 
     ylim=c(min(N.mean.sqmeter[6:26,2])-max(N.sem.sqmeter[6:26,2]), max(N.mean.sqmeter[6:26,2])+max(N.sem.sqmeter[6:26,2])),
     xlab="год", ylab="N, экз./кв.м")
lines(as.numeric(rownames(N.mean.sqmeter)[6:26]), N.mean.sqmeter[6:26,2], pch=1, type="b")
arrows(x0=as.numeric(rownames(N.mean.sqmeter)[6:26]), 
       x1=as.numeric(rownames(N.mean.sqmeter)[6:26]),
       y0=N.mean.sqmeter[6:26,2]-N.sem.sqmeter[6:26,2], y1=N.mean.sqmeter[6:26,2]+N.sem.sqmeter[6:26,2], angle=90, code=3, length=0.1)
dev.off()
embedFonts("N_dynamic_Medvezhya_1992_2012.pdf") #встройка шрифтов в файл

#запишем численности в файл
write.table(x=data.frame(N.mean.sqmeter, N.sem.sqmeter), file="Nmean.csv", sep=";", dec=",")

#считаем биомассу

(B.mean.sqmeter<-tapply(B.sqmeter, list(year, area), FUN=mean, na.rm=T))
(B.sd.sqmeter<-tapply(B.sqmeter, list(year, area), FUN=sd, na.rm=T))
(B.sem.sqmeter<-B.sd.sqmeter/sqrt(n.samples))

(D.n<-B.sem.sqmeter/B.mean.sqmeter*100)

#Сельдяная
pdf(file="B_dynamic_Seldyanaya.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter[,1], x=rownames(B.mean.sqmeter),pch=15, main="г. Сельдяная", 
     ylim=c(min(B.mean.sqmeter[,1])-max(B.sem.sqmeter[,1]), max(B.mean.sqmeter[,1])+max(B.sem.sqmeter[,1])),
     xlab="год", ylab="N, экз./кв.м")
lines(as.numeric(rownames(B.mean.sqmeter)), B.mean.sqmeter[,1], pch=1, type="b")
arrows(x0=as.numeric(rownames(B.mean.sqmeter)), 
       x1=as.numeric(rownames(B.mean.sqmeter)),
       y0=B.mean.sqmeter[,1]-B.sem.sqmeter[,1], y1=B.mean.sqmeter[,1]+B.sem.sqmeter[,1], angle=90, code=3, length=0.1)
dev.off()
embedFonts("B_dynamic_Seldyanaya.pdf") #встройка шрифтов в файл

#Медвежья
pdf(file="B_dynamic_Medvezhya.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter[,2], x=rownames(B.mean.sqmeter),pch=15, main="г. Сельдяная", 
     ylim=c(min(B.mean.sqmeter[,2])-max(B.sem.sqmeter[,2]), max(B.mean.sqmeter[,2])+max(B.sem.sqmeter[,2])),
     xlab="год", ylab="N, экз./кв.м")
lines(as.numeric(rownames(B.mean.sqmeter)), B.mean.sqmeter[,2], pch=1, type="b")
arrows(x0=as.numeric(rownames(B.mean.sqmeter)), 
       x1=as.numeric(rownames(B.mean.sqmeter)),
       y0=B.mean.sqmeter[,2]-B.sem.sqmeter[,2], y1=B.mean.sqmeter[,2]+B.sem.sqmeter[,2], angle=90, code=3, length=0.1)
dev.off()
embedFonts("B_dynamic_Medvezhya.pdf") #встройка шрифтов в файл

# c 1992 года!
#Сельдяная
pdf(file="B_dynamic_Seldyanaya_1992_2012.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter[6:26,1], x=rownames(B.mean.sqmeter)[6:26],pch=15, main="г. Сельдяная", 
     ylim=c(min(B.mean.sqmeter[6:26,1])-max(B.sem.sqmeter[6:26,1]), max(B.mean.sqmeter[6:26,1])+max(B.sem.sqmeter[6:26,1])),
     xlab="год", ylab="N, экз./кв.м")
lines(as.numeric(rownames(B.mean.sqmeter)[6:26]), B.mean.sqmeter[6:26,1], pch=1, type="b")
arrows(x0=as.numeric(rownames(B.mean.sqmeter)[6:26]), 
       x1=as.numeric(rownames(B.mean.sqmeter)[6:26]),
       y0=B.mean.sqmeter[6:26,1]-B.sem.sqmeter[6:26,1], y1=B.mean.sqmeter[6:26,1]+B.sem.sqmeter[6:26,1], angle=90, code=3, length=0.1)
dev.off()
embedFonts("B_dynamic_Seldyanaya_1992_2012.pdf") #встройка шрифтов в файл

#Медвежья
pdf(file="B_dynamic_Medvezhya_1992_2012.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter[6:26,2], x=rownames(B.mean.sqmeter)[6:26],pch=15, main="г. Сельдяная", 
     ylim=c(min(B.mean.sqmeter[6:26,2])-max(B.sem.sqmeter[6:26,2]), max(B.mean.sqmeter[6:26,2])+max(B.sem.sqmeter[6:26,2])),
     xlab="год", ylab="N, экз./кв.м")
lines(as.numeric(rownames(B.mean.sqmeter)[6:26]), B.mean.sqmeter[6:26,2], pch=1, type="b")
arrows(x0=as.numeric(rownames(B.mean.sqmeter)[6:26]), 
       x1=as.numeric(rownames(B.mean.sqmeter)[6:26]),
       y0=B.mean.sqmeter[6:26,2]-B.sem.sqmeter[6:26,2], y1=B.mean.sqmeter[6:26,2]+B.sem.sqmeter[6:26,2], angle=90, code=3, length=0.1)
dev.off()
embedFonts("B_dynamic_Medvezhya_1992_2012.pdf") #встройка шрифтов в файл

#запишем биомассу в файл
write.table(x=data.frame(B.mean.sqmeter, B.sem.sqmeter), file="Bmean.csv", sep=";", dec=",")
