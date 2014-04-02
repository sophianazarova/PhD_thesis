setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Kartesh/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="Nsqmeter.csv", sep=";", dec=",", head=T)
samples<-read.table(file="samples.csv", sep=";", dec=",", head=T)
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
