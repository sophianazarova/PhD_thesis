setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/Luvenga_II_razrez/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Luvenga_II_razrez/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)


ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)
#year<-factor(year)
str(ishodnik)
ishodnik$tidal_level<-ordered(x=ishodnik$tidal_level, levels=c("high_beatch", "fucus_zone", "zostera_zone", "low_beatch"))

#========= размерная структура средние по годам по горизонтам ==================

Length.int<-cut(Length.mm, breaks=seq(0,20,1))

(size.str.table<-table(Length.int,year, tidal_level, sample))

size.str.df<-as.data.frame(size.str.table) # как таблица данных

#убираем те пробы которых на самом деле нету

#for (i in 1:length(levels(size.str.df$year)))
#{ (xxx<-size.str.df$sample[size.str.df$year==levels(size.str.df$year)[i] ]%in%
#     samples.names$sample[samples.names$year==levels(size.str.df$year)[i]])
#  antixxx<-as.logical(1-xxx)
#  size.str.df$Freq[size.str.df$year==levels(size.str.df$year)[i]][antixxx]<-NA
#}

for (i in 1:length(levels(size.str.df$year))){
  for(j in 1:length(levels(size.str.df$tidal_level))){
    (xxx<-size.str.df$sample[size.str.df$year==levels(size.str.df$year)[i] & 
                               size.str.df$tidal_level==levels(size.str.df$tidal_level)[j]]%in%
       samples.names$sample[samples.names$year==levels(size.str.df$year)[i] & 
                              samples.names$tidal.level==levels(size.str.df$tidal_level)[j]])
    antixxx<-as.logical(1-xxx)
    size.str.df$Freq[size.str.df$year==levels(size.str.df$year)[i] & 
                       size.str.df$tidal_level==levels(size.str.df$tidal_level)[j]][antixxx]<-NA
  }}


summary(size.str.df, na.rm=T)
# SUBSET - для фильтрации таблицы данных
# APPLY - кто-то из них для средней и СД по фрейму

#теперь РС на квадратный метр
size.str.sqmeter<-size.str.df
for (i in 1:length(levels(size.str.sqmeter$year)))
{
  size.str.sqmeter$Freq[size.str.sqmeter$year==levels(size.str.sqmeter$year)[i]]<-
    size.str.sqmeter$Freq[size.str.sqmeter$year==levels(size.str.sqmeter$year)[i]] * 
    samples.squares$square[samples.squares$year==levels(size.str.sqmeter$year)[i]]
}


for (i in 1: length(levels(size.str.sqmeter$tidal_level)))
{
  assign(paste(levels(size.str.sqmeter$tidal_level)[i]), 
       subset(size.str.sqmeter, size.str.sqmeter$tidal_level==levels(size.str.sqmeter$tidal_level)[i]))
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

# ======== для летописи вывод в таблицу ===============
letopis.table<-table( sample, Length.int, year)

letopis.table.2dim <- cbind(year = as.numeric(rep(dimnames(letopis.table)$year[1], dim(letopis.table)[1])), letopis.table[,,1])
for (i in 2:dim(letopis.table)[3]) {
  letopis.table.2dim <- rbind(letopis.table.2dim, cbind(year = as.numeric(rep(dimnames(letopis.table)$year[i], dim(letopis.table)[1])), letopis.table[,,i]))}

letopis.table.2dim <- cbind(letopis.table.2dim, rowSums(letopis.table.2dim[,2:21]))



#убираем те пробы которых на самом деле нету
for (i in 1:dim(letopis.table)[3]) 
{ (xxx <- rownames(letopis.table.2dim)[letopis.table.2dim[,1] == as.numeric(dimnames(letopis.table)$year)[i] ]%in%
     samples.names$sample[samples.names$year == as.numeric(dimnames(letopis.table)$year)[i] ])
  antixxx<-as.logical(1-xxx)
  letopis.table.2dim[,22][letopis.table.2dim[,1] == as.numeric(dimnames(letopis.table)$year)[i]][antixxx]<-NA
}
#letopis.table.2dim[,22][letopis.table.2dim[,22] == 0] <- NA
letopis.table.2dim <- na.omit(letopis.table.2dim)

sampnames <- rownames(letopis.table.2dim)
letopis.table.2dim.df <- letopis.table.2dim
rownames(letopis.table.2dim.df) <- seq(1:nrow(letopis.table.2dim.df))
letopis.table.2dim.df <- as.data.frame(letopis.table.2dim.df)
letopis.table.2dim.df$sample <- sampnames
  
letopis.table.2dim.df <- merge(x = letopis.table.2dim.df, y = samples.names, by = c("sample", "year"))


write.csv2(letopis.table.2dim.df, "razrez2_size_str_all.csv")

##=============== РС >1mm ===============================================
#Верхний пляж
(mean.sqmeter2.high_beatch<-mean.sqmeter.high_beatch[2:20,])
mean.sqmeter2.high_beatch.df<-as.data.frame(mean.sqmeter2.high_beatch)

(sd.sqmeter2.high_beatch<-sd.sqmeter.high_beatch[,2:20])

(sem.sqmeter2.high_beatch <-t(sd.sqmeter2.high_beatch/sqrt(n.samples.df$high_beatch)))
sem.sqmeter2.high_beatch.df<-as.data.frame(sem.sqmeter2.high_beatch)

#пояс фукоидов
mean.sqmeter2.fucus_zone<-mean.sqmeter.fucus_zone[2:20,]
mean.sqmeter2.fucus_zone.df<-as.data.frame(mean.sqmeter2.fucus_zone)

sd.sqmeter2.fucus_zone<-sd.sqmeter.fucus_zone[,2:20]

sem.sqmeter2.fucus_zone <-t(sd.sqmeter2.fucus_zone/sqrt(n.samples.df$fucus_zone))
sem.sqmeter2.fucus_zone.df<-as.data.frame(sem.sqmeter2.fucus_zone)

#пояс зостеры
mean.sqmeter2.zostera_zone<-mean.sqmeter.zostera_zone[2:20,]
mean.sqmeter2.zostera_zone.df<-as.data.frame(mean.sqmeter2.zostera_zone)

sd.sqmeter2.zostera_zone<-sd.sqmeter.zostera_zone[,2:20]

sem.sqmeter2.zostera_zone <-t(sd.sqmeter2.zostera_zone/sqrt(n.samples.df$zostera_zone))
sem.sqmeter2.zostera_zone.df<-as.data.frame(sem.sqmeter2.zostera_zone)

#нижний пляж
mean.sqmeter2.low_beatch<-mean.sqmeter.low_beatch[2:20,]
mean.sqmeter2.low_beatch.df<-as.data.frame(mean.sqmeter2.low_beatch)

sd.sqmeter2.low_beatch<-sd.sqmeter.low_beatch[,2:20]

sem.sqmeter2.low_beatch <-t(sd.sqmeter2.low_beatch/sqrt(n.samples.df$low_beatch))
sem.sqmeter2.low_beatch.df<-as.data.frame(sem.sqmeter2.low_beatch)


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


#>1mm
#верхний пляж
for (j in 1:length(colnames(mean.sqmeter2.high_beatch)))
{
  pdf(file=paste("high_beatch2", colnames(mean.sqmeter2.high_beatch)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter2.high_beatch[,j], nn=length.class2,  z=sem.sqmeter2.high_beatch[,j])
  title(main=colnames(mean.sqmeter2.high_beatch)[j], xlab="", ylab="")
  dev.off()
}

#пояс фукоидов
for (j in 1:length(colnames(mean.sqmeter2.fucus_zone)))
{
  pdf(file=paste("fucus_zone2", colnames(mean.sqmeter2.fucus_zone)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter2.fucus_zone[,j], nn=length.class2,  z=sem.sqmeter2.fucus_zone[,j])
  title(main=colnames(mean.sqmeter2.fucus_zone)[j], xlab="", ylab="")
  dev.off()
}

#пояс зостеры
for (j in 1:length(colnames(mean.sqmeter2.zostera_zone)))
{
  pdf(file=paste("zostera_zone2", colnames(mean.sqmeter2.zostera_zone)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter2.zostera_zone[,j], nn=length.class2,  z=sem.sqmeter2.zostera_zone[,j])
  title(main=colnames(mean.sqmeter2.zostera_zone)[j], xlab="", ylab="")
  dev.off()
}

#нижний пляж
for (j in 1:length(colnames(mean.sqmeter2.low_beatch)))
{
  pdf(file=paste("low_beatch2", colnames(mean.sqmeter.low_beatch)[j], ".pdf",sep="_"))
  error.bars(yv=mean.sqmeter2.low_beatch[,j], nn=length.class2,  z=sem.sqmeter2.low_beatch[,j])
  title(main=colnames(mean.sqmeter2.low_beatch)[j], xlab="", ylab="")
  dev.off()
}


# ================= динамика обилия =======================================
(N.sqmeter<-tapply(size.str.sqmeter$Freq, list(size.str.sqmeter$year, size.str.sqmeter$sample, size.str.df$tidal_level), sum))
(N.mean.sqmeter<-apply(N.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=mean))
  N.mean.sqmeter[11,3]<-NA
(N.sd.sqmeter<-apply(N.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=sd))
(N.sem.sqmeter<-N.sd.sqmeter/sqrt(n.samples))
(D.n<-N.sem.sqmeter/N.mean.sqmeter*100)

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
legend(legend=colnames(N.mean.sqmeter),x=2000, y=24777, pch=seq(15,15+ncol(N.mean.sqmeter),1), col=seq(1,1+ncol(N.mean.sqmeter),1))
dev.off()
embedFonts("N_dynamic.pdf") #встройка шрифтов в файл


#техническая картинка чтобы понять как устроена нижняя часть графика, без выпадающего 1998 года
plot(y=N.mean.sqmeter[,1], x=as.numeric(rownames(N.mean.sqmeter)), type="n", main="Материковая литораль в районе пос. Лувеньга",
     #     ylim=c(min(N.mean.sqmeter, na.rm=T)-max(N.sem.sqmeter, na.rm=T), max(N.mean.sqmeter, na.rm=T)+max(N.sem.sqmeter, na.rm=T)),
     ylim=c(0, 5000), 
     xlab="год", ylab="N, экз./кв.м")
for (i in 1:ncol(N.mean.sqmeter))
{lines(as.numeric(rownames(N.mean.sqmeter)), N.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(N.mean.sqmeter)), x1=as.numeric(rownames(N.mean.sqmeter)),
        y0=N.mean.sqmeter[,i]-N.sem.sqmeter[,i], y1=N.mean.sqmeter[,i]+N.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}

# ============ динамика без разделения на горизонты ============================
str(size.str.sqmeter)
(N.all.sqmeter<-tapply(size.str.sqmeter$Freq, list(size.str.sqmeter$year, size.str.sqmeter$sample), sum,na.rm=T))
N.all.sqmeter[N.all.sqmeter==0]<-NA
(N.all.mean.sqmeter<-apply(N.all.sqmeter, na.rm=T, MARGIN=1, FUN=mean ))
(N.all.sd.sqmeter<-apply(N.all.sqmeter, na.rm=T, MARGIN=1, FUN=sd))
(N.all.sem.sqmeter<-N.all.sd.sqmeter/sqrt(rowSums(as.matrix(n.samples),na.rm=T)))
(D.n.all<-N.all.sem.sqmeter/N.all.mean.sqmeter*100)

write.table(data.frame(N.all.mean.sqmeter, N.all.sem.sqmeter), file="Nall_mean.csv", dec=",", sep=";")

# ===========  динамика без спата ( больше 1 мм) ==========================
(N2.sqmeter<-tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"],
                   list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"],
                        size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]"],
                        size.str.df$tidal_level[size.str.sqmeter$Length.int!="(0,1]"]), sum))
(N2.mean.sqmeter<-apply(N2.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=mean))
N2.mean.sqmeter[11,3]<-NA
(N2.sd.sqmeter<-apply(N2.sqmeter, na.rm=T, MARGIN=c(1,3), FUN=sd))
(N2.sem.sqmeter<-N2.sd.sqmeter/sqrt(n.samples))
(D.n2<-N.sem.sqmeter/N.mean.sqmeter*100)

# запишем численность всех крупнее 1 мм в файл
write.table(data.frame(N2.mean.sqmeter, N2.sem.sqmeter), file="2razrez_N2.csv", sep=";", dec=",")

# запишем пересчет обилия >1мм в пробах на квадратный метр в файл
write.table(as.data.frame(as.table(N2.sqmeter)), file="2razrez_N2_in samples_sqmeter.csv", sep=";", dec=",")


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
legend(legend=colnames(N2.mean.sqmeter),x=1992, y=12749, pch=seq(15,15+ncol(N2.mean.sqmeter),1), col=seq(1,1+ncol(N2.mean.sqmeter),1))
dev.off()
embedFonts("N2_dynamic.pdf") #встройка шрифтов в файл

#locator()

#техническая картинка чтобы понять как устроена нижняя часть графика, без выпадающего 1998 года
plot(y=N2.mean.sqmeter[,1], x=as.numeric(rownames(N.mean.sqmeter)), type="n", main="Материковая литораль в районе пос. Лувеньга",
     #     ylim=c(min(N.mean.sqmeter, na.rm=T)-max(N.sem.sqmeter, na.rm=T), max(N.mean.sqmeter, na.rm=T)+max(N.sem.sqmeter, na.rm=T)),
     ylim=c(0, 5000), 
     xlab="год", ylab="N, экз./кв.м")
for (i in 1:ncol(N2.mean.sqmeter))
{lines(as.numeric(rownames(N2.mean.sqmeter)), N2.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(N2.mean.sqmeter)), x1=as.numeric(rownames(N2.mean.sqmeter)),
        y0=N2.mean.sqmeter[,i]-N2.sem.sqmeter[,i], y1=N2.mean.sqmeter[,i]+N2.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}

# ======== динамика без спата без разделения на горизонты ========================
str(size.str.sqmeter)
(N2.all.sqmeter<-tapply(size.str.sqmeter$Freq[size.str.sqmeter$Length.int!="(0,1]"],
                    list(size.str.sqmeter$year[size.str.sqmeter$Length.int!="(0,1]"],
                         size.str.sqmeter$sample[size.str.sqmeter$Length.int!="(0,1]"]), sum, na.rm=T))
N2.all.sqmeter[N2.all.sqmeter==0]<-NA
(N2.all.mean.sqmeter<-apply(N2.all.sqmeter, na.rm=T, MARGIN=1, FUN=mean ))
(N2.all.sd.sqmeter<-apply(N2.all.sqmeter, na.rm=T, MARGIN=1, FUN=sd))
(N2.all.sem.sqmeter<-N2.all.sd.sqmeter/sqrt(rowSums(as.matrix(n.samples),na.rm=T)))
(D.n2.all<-N2.all.sem.sqmeter/N2.all.mean.sqmeter*100)

write.table(data.frame(N2.all.mean.sqmeter, N2.all.sem.sqmeter), file="N2all_mean.csv", dec=",", sep=";")

#график без разделения на горизрнты без молоди
pdf(file="N2_dynamic_all.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.all.mean.sqmeter, x=as.numeric(names(N2.all.mean.sqmeter)), type="b", main="Лувеньга, материковая литораль",
     ylim=c(0, max(N2.all.mean.sqmeter, na.rm=T)+max(N2.all.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="N, экз./кв.м", pch=15)
arrows(x0 = as.numeric(names(N2.all.mean.sqmeter)), x1 = as.numeric(names(N2.all.mean.sqmeter)),
       y0=N2.all.mean.sqmeter-N2.all.sem.sqmeter, y1=N2.all.mean.sqmeter+N2.all.sem.sqmeter, angle=90, code=3, length=.1)
dev.off()
embedFonts("N2_dynamic_all.pdf") #встройка шрифтов в файл

## =========== анализ динамики без молоди суммарной ===========
N2.all.sqmeter.df <- as.data.frame(as.table(N2.all.sqmeter))
str(N2.all.sqmeter.df)
colnames(N2.all.sqmeter.df) <- c("year", "sample", "N2.indd.sqm")
N2.all.sqmeter.df$year <- as.numeric(as.vector(N2.all.sqmeter.df$year))

# Kruskal test по периодам
kruskal.test(N2.all.sqmeter.df$N2.indd.sqm ~ N2.all.sqmeter.df$year)

kruskal.test(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year < 1994] ~ N2.all.sqmeter.df$year[ N2.all.sqmeter.df$year < 1994])
kruskal.test(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year < 1998 & N2.all.sqmeter.df$year > 1993 ] ~ N2.all.sqmeter.df$year[ N2.all.sqmeter.df$year < 1998  & N2.all.sqmeter.df$year > 1993])
kruskal.test(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year > 1997] ~ N2.all.sqmeter.df$year[ N2.all.sqmeter.df$year > 1997])

# средние численности в стабильных периодах
mean(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year > 1999], na.rm=T)
sd(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year > 1999], na.rm=T)/sqrt(length(na.omit(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year > 1999])))
sd(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year > 1999], na.rm=T)/sqrt(length(na.omit(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year > 1999])))/mean(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year > 1999], na.rm=T)*100

mean(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year < 1999 & N2.all.sqmeter.df$year > 1993 ], na.rm=T)
sd(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year < 1999 & N2.all.sqmeter.df$year > 1993 ], na.rm=T)/length(na.omit((N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year < 1999 & N2.all.sqmeter.df$year > 1993 ])))
sd(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year < 1999 & N2.all.sqmeter.df$year > 1993 ], na.rm=T)/length(na.omit((N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year < 1999 & N2.all.sqmeter.df$year > 1993 ])))/mean(N2.all.sqmeter.df$N2.indd.sqm[ N2.all.sqmeter.df$year < 1999 & N2.all.sqmeter.df$year > 1993 ], na.rm=T)*100



# ======================про N2===========================
#доля молоди
(N.mean.sqmeter-N2.mean.sqmeter)/N.mean.sqmeter*100

# сравниваем обилие до 1998 года краскел уоллисом. для этого делаем выборку для каждой зоны нужные года по пробам
#high_beatch
(N2.high_beatch<-subset(samples.names, subset=samples.names$tidal.level=="high_beatch"))
(N2.92.98.high_beatch<-data.frame(subset(N2.high_beatch, subset= N2.high_beatch$year<=1997), 
                                 as.vector(N2.sqmeter[c(as.character(seq(1992,1997,1))),,"high_beatch"])
                                 [!is.na(as.vector(N2.sqmeter[c(as.character(seq(1992,1997,1))),,"high_beatch"]))]))
kruskal.test(N2.92.98.high_beatch$as.vector.N2.sqmeter.c.as.character.seq.1992..1997..1........high_beatch.....is.na.as.vector.N2.sqmeter.c.as.character.seq.1992..
             ~ N2.92.98.high_beatch$year)
#mean, d
mean(N2.92.98.high_beatch$as.vector.N2.sqmeter.c.as.character.seq.1992..1997..1........high_beatch.....is.na.as.vector.N2.sqmeter.c.as.character.seq.1992..)

sd(N2.92.98.high_beatch$as.vector.N2.sqmeter.c.as.character.seq.1992..1997..1........high_beatch.....is.na.as.vector.N2.sqmeter.c.as.character.seq.1992..)/
   sqrt(length((N2.92.98.high_beatch$as.vector.N2.sqmeter.c.as.character.seq.1992..1997..1........high_beatch.....is.na.as.vector.N2.sqmeter.c.as.character.seq.1992..)))/
  mean(N2.92.98.high_beatch$as.vector.N2.sqmeter.c.as.character.seq.1992..1997..1........high_beatch.....is.na.as.vector.N2.sqmeter.c.as.character.seq.1992..)*100

#fucus_zone
(N2.fucus_zone<-subset(samples.names, subset=samples.names$tidal.level=="fucus_zone"))
(N2.92.98.fucus_zone<-data.frame(subset(N2.fucus_zone, subset= N2.fucus_zone$year<=1997), 
                                  as.vector(N2.sqmeter[c(as.character(seq(1992,1997,1))),,"fucus_zone"])
                                  [!is.na(as.vector(N2.sqmeter[c(as.character(seq(1992,1997,1))),,"fucus_zone"]))]))
kruskal.test(N2.92.98.fucus_zone$as.vector.N2.sqmeter.c.as.character.seq.1992..1997..1........fucus_zone.....is.na.as.vector.N2.sqmeter.c.as.character.seq.1992..
             ~ N2.92.98.fucus_zone$year)
#отличаются!

#fucus_zone
(N2.fucus_zone<-subset(samples.names, subset=samples.names$tidal.level=="fucus_zone"))
(N2.92.98.fucus_zone<-data.frame(subset(N2.fucus_zone, subset= N2.fucus_zone$year<=1997), 
                                 as.vector(N2.sqmeter[c(as.character(seq(1992,1997,1))),,"fucus_zone"])
                                 [!is.na(as.vector(N2.sqmeter[c(as.character(seq(1992,1997,1))),,"fucus_zone"]))]))
kruskal.test(N2.92.98.fucus_zone$as.vector.N2.sqmeter.c.as.character.seq.1992..1997..1........fucus_zone.....is.na.as.vector.N2.sqmeter.c.as.character.seq.1992..
             ~ N2.92.98.fucus_zone$year)
#отличаются!

#============= размерная структура в %=====================================
str(size.str.sqmeter)

#high_beatch
(sum.sizestr.sqmeter.high_beatch<-t(tapply(high_beatch$Freq,INDEX=list(high_beatch$year, high_beatch$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.high_beatch<-t(t(sum.sizestr.sqmeter.high_beatch)/colSums(sum.sizestr.sqmeter.high_beatch))*100)



#>1mm
(sum.sizestr2.sqmeter.percents.high_beatch<-t(t(sum.sizestr.sqmeter.high_beatch[2:nrow(sum.sizestr.sqmeter.high_beatch),])/
   colSums(sum.sizestr.sqmeter.high_beatch[2:nrow(sum.sizestr.sqmeter.high_beatch),])*100))


# запишем в файл размерную структуру в процентах
write.table(x=as.data.frame(as.table(sum.sizestr2.sqmeter.percents.high_beatch)), file="2razrez_high_beatch_sizestr2_percent.csv", sep=";", dec=",")

#fucus_zone
(sum.sizestr.sqmeter.fucus_zone<-t(tapply(fucus_zone$Freq,INDEX=list(fucus_zone$year, fucus_zone$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.fucus_zone<-t(t(sum.sizestr.sqmeter.fucus_zone)/colSums(sum.sizestr.sqmeter.fucus_zone)*100))
#>1mm
(sum.sizestr2.sqmeter.percents.fucus_zone<-t(t(sum.sizestr.sqmeter.fucus_zone[2:nrow(sum.sizestr.sqmeter.fucus_zone),])/
   colSums(sum.sizestr.sqmeter.fucus_zone[2:nrow(sum.sizestr.sqmeter.fucus_zone),])*100))

#  запишем в файл размерную структуру в процентах
write.table(x=as.data.frame(as.table(sum.sizestr2.sqmeter.percents.fucus_zone)), file="2razrez_fucus_zone_sizestr2_percent.csv", sep=";", dec=",")

#zostera_zone
(sum.sizestr.sqmeter.zostera_zone<-t(tapply(zostera_zone$Freq,INDEX=list(zostera_zone$year, zostera_zone$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.zostera_zone<-t(t(sum.sizestr.sqmeter.zostera_zone)/colSums(sum.sizestr.sqmeter.zostera_zone)*100))
#>1mm
(sum.sizestr2.sqmeter.percents.zostera_zone<-t(t(sum.sizestr.sqmeter.zostera_zone[2:nrow(sum.sizestr.sqmeter.zostera_zone),])/
   colSums(sum.sizestr.sqmeter.zostera_zone[2:nrow(sum.sizestr.sqmeter.zostera_zone),])*100))

# запишем в файл размерную структуру в процентах
write.table(x=as.data.frame(as.table(sum.sizestr2.sqmeter.percents.zostera_zone)), file="2razrez_zostera_zone_sizestr2_percent.csv", sep=";", dec=",")

#low_beatch
(sum.sizestr.sqmeter.low_beatch<-t(tapply(low_beatch$Freq,INDEX=list(low_beatch$year, low_beatch$Length.int),FUN=sum, na.rm=T)))
(sum.sizestr.sqmeter.percents.low_beatch<-t(t(sum.sizestr.sqmeter.low_beatch/colSums(sum.sizestr.sqmeter.low_beatch)*100)))
#>1mm
(sum.sizestr2.sqmeter.percents.low_beatch<-t(t(sum.sizestr.sqmeter.low_beatch[2:nrow(sum.sizestr.sqmeter.low_beatch),])/
   colSums(sum.sizestr.sqmeter.low_beatch[2:nrow(sum.sizestr.sqmeter.low_beatch),])*100))

# запишем в файл размерную структуру в процентах
write.table(x=as.data.frame(as.table(sum.sizestr2.sqmeter.percents.low_beatch)), file="2razrez_low_beatch_sizestr2_percent.csv", sep=";", dec=",")


# ===================== динамика максимального размера ======================== 
str(ishodnik)
(Length.max<-tapply(Length.mm, list(year, tidal_level), max, na.rm=T))
#plot(x=names(Length.max), y=Length.max, type=none)
max(Length.mm, na.rm=T)

pdf(file="L_max.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=rownames(Length.max), y=Length.max[,1], type="n", main="Материковая литораль в районе пос. Лувеньга", xlab="год", ylab="L max, мм", 
     ylim=c(min(Length.max,na.rm=T), max(Length.max,na.rm=T)))
for (i in 1:ncol(Length.max))
{lines(as.numeric(rownames(Length.max)), Length.max[,i], pch=14+i, col=0+i, type="b")
}
legend(legend=colnames(Length.max),x=1993, y=12.2, pch=seq(15,15+ncol(Length.max),1), col=seq(1,1+ncol(Length.max),1))
dev.off()
embedFonts("L_max.pdf") #встройка шрифтов в файл

## рассчетная биомасса по Максимовичу и др., 1993
biomass.count<-0.00016*(Length.mm^2.96)
    (biomass.samples<-tapply(biomass.count, list(year, sample, tidal_level), sum, na.rm=T))

(biomass.sqmeter<-biomass.samples*samples.squares$square)

(B.mean.sqmeter<-apply(biomass.sqmeter, c(1,3), mean, na.rm=T))
(B.sd.sqmeter<-apply(biomass.sqmeter, c(1,3), sd, na.rm=T))
(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal.level), length))
(B.sem.sqmeter<-B.sd.sqmeter/sqrt(n.samples))
(D.b<-B.sem.sqmeter/B.mean.sqmeter*100)

pdf(file="B_count_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B.mean.sqmeter[,1], x=as.numeric(rownames(B.mean.sqmeter)), type="n", main="Материковая литораль в районе пос. Лувеньга",
     ylim=c(min(B.mean.sqmeter, na.rm=T)-max(B.sem.sqmeter, na.rm=T), max(B.mean.sqmeter, na.rm=T)+max(B.sem.sqmeter, na.rm=T)),
     # ylim=c(0, max(B.mean.sqmeter, na.rm=T)+max(B.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="B, г/кв.м")
for (i in 1:ncol(B.mean.sqmeter))
{lines(as.numeric(rownames(B.mean.sqmeter)), B.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(B.mean.sqmeter)), x1=as.numeric(rownames(B.mean.sqmeter)),
        y0=B.mean.sqmeter[,i]-B.sem.sqmeter[,i], y1=B.mean.sqmeter[,i]+B.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}
legend(legend=colnames(B.mean.sqmeter),x=2000, y=27.5, pch=seq(15,15+ncol(B.mean.sqmeter),1), col=seq(1,1+ncol(B.mean.sqmeter),1))
dev.off()
embedFonts("B_count_dynamic.pdf") #встройка шрифтов в файл

## рассчетная биомасса только с учетом >1mm особей
biomass2.count<-0.00016*(Length.mm[Length.mm>1.0]^2.96)
(biomass2.samples<-tapply(biomass2.count, list(year[Length.mm>1.0], sample[Length.mm>1.0], tidal_level[Length.mm>1.0]), sum, na.rm=T))

(biomass2.sqmeter<-biomass2.samples*samples.squares$square)

(B2.mean.sqmeter<-apply(biomass2.sqmeter, c(1,3), mean, na.rm=T))
(B2.sd.sqmeter<-apply(biomass2.sqmeter, c(1,3), sd, na.rm=T))
(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal.level), length ))
(B2.sem.sqmeter<-B2.sd.sqmeter/sqrt(n.samples))
(D.b2<-B2.sem.sqmeter/B2.mean.sqmeter*100)

pdf(file="B2_count_dynamic.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=B2.mean.sqmeter[,1], x=as.numeric(rownames(B2.mean.sqmeter)), type="n", main="Материковая литораль в районе пос. Лувеньга",
     ylim=c(min(B2.mean.sqmeter, na.rm=T)-max(B2.sem.sqmeter, na.rm=T), max(B2.mean.sqmeter, na.rm=T)+max(B2.sem.sqmeter, na.rm=T)),
     # ylim=c(0, max(B.mean.sqmeter, na.rm=T)+max(B.sem.sqmeter, na.rm=T)), 
     xlab="год", ylab="B, г/кв.м")
for (i in 1:ncol(B2.mean.sqmeter))
{lines(as.numeric(rownames(B2.mean.sqmeter)), B2.mean.sqmeter[,i], pch=14+i, col=0+i, type="b")
 arrows(x0=as.numeric(rownames(B2.mean.sqmeter)), x1=as.numeric(rownames(B2.mean.sqmeter)),
        y0=B2.mean.sqmeter[,i]-B2.sem.sqmeter[,i], y1=B2.mean.sqmeter[,i]+B2.sem.sqmeter[,i], angle=90, code=3, length=.1, col=0+i)
}
legend(legend=colnames(B2.mean.sqmeter),x=2000, y=30.6, pch=seq(15,15+ncol(B2.mean.sqmeter),1), col=seq(1,1+ncol(B2.mean.sqmeter),1))
dev.off()
embedFonts("B2_count_dynamic.pdf") #встройка шрифтов в файл

#запишем в файл рассчетную биомассу
write.table(data.frame(B2.mean.sqmeter, B2.sem.sqmeter), file="2 razrez_B2_mean.csv",sep=";", dec=",")


## измеренная биомасса (реальная)
str(biomass.measure)
(biomass.real.m<-tapply(biomass.measure$biomass.g, list(biomass.measure$year, biomass.measure$sample), function(x){x*1}))
(biomass.real.sqmeter<-biomass.real.m*samples.squares$square)

(Br.mean.sqmeter<-rowMeans(biomass.real.sqmeter, na.rm=T))
(Br.sd.sqmeter<-apply(biomass.real.sqmeter, 1, sd, na.rm=T))
(Br.sem.sqmeter<-Br.sd.sqmeter/sqrt(n.samples))
(D.br<-Br.sem.sqmeter/Br.mean.sqmeter*100)

#сравнение рассчетной и реальной биомассы
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

(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal.level), length ))
(n.samples.df<-as.data.frame(n.samples))


(sem.young.old.sqmeter <-sd.young.old.sqmeter/sqrt(as.vector(n.samples)))
sem.young.old.sqmeter[sem.young.old.sqmeter==sd.young.old.sqmeter]<-NA
for (i in 1:4)
{assign(paste("sem",dimnames(sem.young.old.sqmeter)[[2]][i],sep="."),sem.young.old.sqmeter[,i,])
}

write.table(mean.young.old.sqmeter, file="2razrez_young_old_mean.csv", sep=";", dec=",")

str(mean.young.old.sqmeter)



# молодь и половозрелые - график high_beatch
pdf(file="young_old_high_beatch.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.high_beatch[,1], x=as.numeric(rownames(mean.high_beatch)),pch=15, type="n", main="Материковая литораль в районе пос. Лувеньга high_beatch", 
     #     ylim=c(min(mean.high_beatch[,1], mean.high_beatch[,3])-max(sem.high_beatch[,1], sem.high_beatch[,3]), 
     #            max(mean.high_beatch[,1], mean.high_beatch[,3])+max(sem.high_beatch[,1], sem.high_beatch[,3])),
     ylim=c(0, 
            max(mean.high_beatch[,1], mean.high_beatch[,3], na.rm=T)+max(sem.high_beatch[,1], sem.high_beatch[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(as.numeric(rownames(mean.high_beatch)), 
      mean.high_beatch[,1], pch=15, type="b", col=2)
arrows(x0=as.numeric(rownames(mean.high_beatch)), 
       x1=as.numeric(rownames(mean.high_beatch)),
       y0=mean.high_beatch[,1]-sem.high_beatch[,1], 
       y1=mean.high_beatch[,1]+sem.high_beatch[,1], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(as.numeric(rownames(mean.high_beatch)), 
      mean.high_beatch[,3], pch=16, type="b", col=4)
arrows(x0=as.numeric(rownames(mean.high_beatch)), 
       x1=as.numeric(rownames(mean.high_beatch)),
       y0=mean.high_beatch[,3]-sem.high_beatch[,3], 
       y1=mean.high_beatch[,3]+sem.high_beatch[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old_high_beatch.pdf") #встройка шрифтов в файл

# молодь и половозрелые - график fucus_zone
pdf(file="young_old_fucus_zone.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.fucus_zone[,1], x=as.numeric(rownames(mean.fucus_zone)),pch=15, type="n", main="Материковая литораль в районе пос. Лувеньга fucus_zone", 
     #     ylim=c(min(mean.fucus_zone[,1], mean.fucus_zone[,3])-max(sem.fucus_zone[,1], sem.fucus_zone[,3]), 
     #            max(mean.fucus_zone[,1], mean.fucus_zone[,3])+max(sem.fucus_zone[,1], sem.fucus_zone[,3])),
     ylim=c(0, 
            max(mean.fucus_zone[,1], mean.fucus_zone[,3], na.rm=T)+max(sem.fucus_zone[,1], sem.fucus_zone[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(as.numeric(rownames(mean.fucus_zone)), 
      mean.fucus_zone[,1], pch=15, type="b", col=2)
arrows(x0=as.numeric(rownames(mean.fucus_zone)), 
       x1=as.numeric(rownames(mean.fucus_zone)),
       y0=mean.fucus_zone[,1]-sem.fucus_zone[,1], 
       y1=mean.fucus_zone[,1]+sem.fucus_zone[,1], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(as.numeric(rownames(mean.fucus_zone)), 
      mean.fucus_zone[,3], pch=16, type="b", col=4)
arrows(x0=as.numeric(rownames(mean.fucus_zone)), 
       x1=as.numeric(rownames(mean.fucus_zone)),
       y0=mean.fucus_zone[,3]-sem.fucus_zone[,3], 
       y1=mean.fucus_zone[,3]+sem.fucus_zone[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old_fucus_zone.pdf") #встройка шрифтов в файл

# молодь и половозрелые - график zostera_zone
pdf(file="young_old_zostera_zone.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.zostera_zone[,1], x=as.numeric(rownames(mean.zostera_zone)),pch=15, type="n", main="Материковая литораль в районе пос. Лувеньга zostera_zone", 
     #     ylim=c(min(mean.zostera_zone[,1], mean.zostera_zone[,3])-max(sem.zostera_zone[,1], sem.zostera_zone[,3]), 
     #            max(mean.zostera_zone[,1], mean.zostera_zone[,3])+max(sem.zostera_zone[,1], sem.zostera_zone[,3])),
     ylim=c(0, 
            max(mean.zostera_zone[,1], mean.zostera_zone[,3], na.rm=T)+max(sem.zostera_zone[,1], sem.zostera_zone[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(as.numeric(rownames(mean.zostera_zone)), 
      mean.zostera_zone[,1], pch=15, type="b", col=2)
arrows(x0=as.numeric(rownames(mean.zostera_zone)), 
       x1=as.numeric(rownames(mean.zostera_zone)),
       y0=mean.zostera_zone[,1]-sem.zostera_zone[,1], 
       y1=mean.zostera_zone[,1]+sem.zostera_zone[,1], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(as.numeric(rownames(mean.zostera_zone)), 
      mean.zostera_zone[,3], pch=16, type="b", col=4)
arrows(x0=as.numeric(rownames(mean.zostera_zone)), 
       x1=as.numeric(rownames(mean.zostera_zone)),
       y0=mean.zostera_zone[,3]-sem.zostera_zone[,3], 
       y1=mean.zostera_zone[,3]+sem.zostera_zone[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old_zostera_zone.pdf") #встройка шрифтов в файл

# молодь и половозрелые - график low_beatch
pdf(file="young_old_low_beatch.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.low_beatch[,1], x=as.numeric(rownames(mean.low_beatch)),pch=15, type="n", main="Материковая литораль в районе пос. Лувеньга low_beatch", 
     #     ylim=c(min(mean.low_beatch[,1], mean.low_beatch[,3])-max(sem.low_beatch[,1], sem.low_beatch[,3]), 
     #            max(mean.low_beatch[,1], mean.low_beatch[,3])+max(sem.low_beatch[,1], sem.low_beatch[,3])),
     ylim=c(0, 
            max(mean.low_beatch[,1], mean.low_beatch[,3], na.rm=T)+max(sem.low_beatch[,1], sem.low_beatch[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(as.numeric(rownames(mean.low_beatch)), 
      mean.low_beatch[,1], pch=15, type="b", col=2)
arrows(x0=as.numeric(rownames(mean.low_beatch)), 
       x1=as.numeric(rownames(mean.low_beatch)),
       y0=mean.low_beatch[,1]-sem.low_beatch[,1], 
       y1=mean.low_beatch[,1]+sem.low_beatch[,1], angle=90, code=3, length=0.1, col=2)
#половозрелые
lines(as.numeric(rownames(mean.low_beatch)), 
      mean.low_beatch[,3], pch=16, type="b", col=4)
arrows(x0=as.numeric(rownames(mean.low_beatch)), 
       x1=as.numeric(rownames(mean.low_beatch)),
       y0=mean.low_beatch[,3]-sem.low_beatch[,3], 
       y1=mean.low_beatch[,3]+sem.low_beatch[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_old_low_beatch.pdf") #встройка шрифтов в файл

## динамика молоди и половозрелых в %
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

write.table((young.old.percents), file="2razrez_young_old_percent.csv", sep=";", dec=",")

#корреляция - молодь и половозрелые в %
str(young.old.percents)
(spearman.young.old.sum.percent<-cor.test(young.old.percents[2:9,,1], young.old.percents[1:8,,3], method="spearman"))
plot(y=young.old.percents[2:9,,1], x=young.old.percents[1:8,,3])

## молодь и общая численность
# молодь и общая численность - график high_beatch
pdf(file="young_all_high_beatch.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=N2.mean.sqmeter[,"high_beatch"], x=as.numeric(rownames(mean.high_beatch)),pch=15, type="n", 
     main="Материковая литораль в районе пос. Лувеньга high_beatch", 
     #     ylim=c(min(mean.high_beatch[,1], N2.mean.sqmeter[,"high_beatch"])-max(sem.high_beatch[,1], N2.sem.sqmeter[,"high_beatch"]), 
     #            max(mean.high_beatch[,1], N2.mean.sqmeter[,"high_beatch"])+max(sem.high_beatch[,1], N2.sem.sqmeter[,"high_beatch"])),
     ylim=c(0, 
            max(mean.high_beatch[,1], N2.mean.sqmeter[,"high_beatch"], na.rm=T)+max(sem.high_beatch[,1], N2.sem.sqmeter[,"high_beatch"], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(as.numeric(rownames(mean.high_beatch)), 
      mean.high_beatch[,1], pch=15, type="b", col=2)
arrows(x0=as.numeric(rownames(mean.high_beatch)), 
       x1=as.numeric(rownames(mean.high_beatch)),
       y0=mean.high_beatch[,1]-sem.high_beatch[,1], 
       y1=mean.high_beatch[,1]+sem.high_beatch[,1], angle=90, code=3, length=0.1, col=2)
#общая численность
lines(as.numeric(rownames(mean.high_beatch)), 
      N2.mean.sqmeter[,"high_beatch"], pch=16, type="b", col=4)
arrows(x0=as.numeric(rownames(mean.high_beatch)), 
       x1=as.numeric(rownames(mean.high_beatch)),
       y0=N2.mean.sqmeter[,"high_beatch"]-N2.sem.sqmeter[,"high_beatch"], 
       y1=N2.mean.sqmeter[,"high_beatch"]+N2.sem.sqmeter[,"high_beatch"], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_all_high_beatch.pdf") #встройка шрифтов в файл

# молодь и общая численность - график fucus_zone
pdf(file="young_all_fucus_zone.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.fucus_zone[,1], x=as.numeric(rownames(mean.fucus_zone)),pch=15, type="n", main="Материковая литораль в районе пос. Лувеньга fucus_zone", 
     #     ylim=c(min(mean.fucus_zone[,1], mean.fucus_zone[,3])-max(sem.fucus_zone[,1], sem.fucus_zone[,3]), 
     #            max(mean.fucus_zone[,1], mean.fucus_zone[,3])+max(sem.fucus_zone[,1], sem.fucus_zone[,3])),
     ylim=c(0, 
            max(mean.fucus_zone[,1], mean.fucus_zone[,3], na.rm=T)+max(sem.fucus_zone[,1], sem.fucus_zone[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(as.numeric(rownames(mean.fucus_zone)), 
      mean.fucus_zone[,1], pch=15, type="b", col=2)
arrows(x0=as.numeric(rownames(mean.fucus_zone)), 
       x1=as.numeric(rownames(mean.fucus_zone)),
       y0=mean.fucus_zone[,1]-sem.fucus_zone[,1], 
       y1=mean.fucus_zone[,1]+sem.fucus_zone[,1], angle=90, code=3, length=0.1, col=2)
#общая численность
lines(as.numeric(rownames(mean.fucus_zone)), 
      mean.fucus_zone[,3], pch=16, type="b", col=4)
arrows(x0=as.numeric(rownames(mean.fucus_zone)), 
       x1=as.numeric(rownames(mean.fucus_zone)),
       y0=mean.fucus_zone[,3]-sem.fucus_zone[,3], 
       y1=mean.fucus_zone[,3]+sem.fucus_zone[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_all_fucus_zone.pdf") #встройка шрифтов в файл

# молодь и общая численность - график zostera_zone
pdf(file="young_all_zostera_zone.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.zostera_zone[,1], x=as.numeric(rownames(mean.zostera_zone)),pch=15, type="n", main="Материковая литораль в районе пос. Лувеньга zostera_zone", 
     #     ylim=c(min(mean.zostera_zone[,1], mean.zostera_zone[,3])-max(sem.zostera_zone[,1], sem.zostera_zone[,3]), 
     #            max(mean.zostera_zone[,1], mean.zostera_zone[,3])+max(sem.zostera_zone[,1], sem.zostera_zone[,3])),
     ylim=c(0, 
            max(mean.zostera_zone[,1], mean.zostera_zone[,3], na.rm=T)+max(sem.zostera_zone[,1], sem.zostera_zone[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(as.numeric(rownames(mean.zostera_zone)), 
      mean.zostera_zone[,1], pch=15, type="b", col=2)
arrows(x0=as.numeric(rownames(mean.zostera_zone)), 
       x1=as.numeric(rownames(mean.zostera_zone)),
       y0=mean.zostera_zone[,1]-sem.zostera_zone[,1], 
       y1=mean.zostera_zone[,1]+sem.zostera_zone[,1], angle=90, code=3, length=0.1, col=2)
#общая численность
lines(as.numeric(rownames(mean.zostera_zone)), 
      mean.zostera_zone[,3], pch=16, type="b", col=4)
arrows(x0=as.numeric(rownames(mean.zostera_zone)), 
       x1=as.numeric(rownames(mean.zostera_zone)),
       y0=mean.zostera_zone[,3]-sem.zostera_zone[,3], 
       y1=mean.zostera_zone[,3]+sem.zostera_zone[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_all_zostera_zone.pdf") #встройка шрифтов в файл

# молодь и общая численность - график low_beatch
pdf(file="young_all_low_beatch.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=mean.low_beatch[,1], x=as.numeric(rownames(mean.low_beatch)),pch=15, type="n", main="Материковая литораль в районе пос. Лувеньга low_beatch", 
     #     ylim=c(min(mean.low_beatch[,1], mean.low_beatch[,3])-max(sem.low_beatch[,1], sem.low_beatch[,3]), 
     #            max(mean.low_beatch[,1], mean.low_beatch[,3])+max(sem.low_beatch[,1], sem.low_beatch[,3])),
     ylim=c(0, 
            max(mean.low_beatch[,1], mean.low_beatch[,3], na.rm=T)+max(sem.low_beatch[,1], sem.low_beatch[,3], na.rm=T)),
     xlab="год", ylab="N, экз./кв.м")
#молодь
lines(as.numeric(rownames(mean.low_beatch)), 
      mean.low_beatch[,1], pch=15, type="b", col=2)
arrows(x0=as.numeric(rownames(mean.low_beatch)), 
       x1=as.numeric(rownames(mean.low_beatch)),
       y0=mean.low_beatch[,1]-sem.low_beatch[,1], 
       y1=mean.low_beatch[,1]+sem.low_beatch[,1], angle=90, code=3, length=0.1, col=2)
#общая численность
lines(as.numeric(rownames(mean.low_beatch)), 
      mean.low_beatch[,3], pch=16, type="b", col=4)
arrows(x0=as.numeric(rownames(mean.low_beatch)), 
       x1=as.numeric(rownames(mean.low_beatch)),
       y0=mean.low_beatch[,3]-sem.low_beatch[,3], 
       y1=mean.low_beatch[,3]+sem.low_beatch[,3], angle=90, code=3, length=0.1, col=4)
dev.off()
embedFonts("young_all_low_beatch.pdf") #встройка шрифтов в файл

##  численность молоди и биомасса половозрелых
## рассчетная биомасса только с учетом >1mm особей
biomass8.count<-0.00016*(Length.mm[Length.mm>8.0]^2.96)
(biomass8.samples<-tapply(biomass8.count, list(year[Length.mm>8.0], sample[Length.mm>8.0], tidal_level[Length.mm>8.0]), sum, na.rm=T))

(biomass8.sqmeter<-biomass8.samples*samples.squares$square)

(B2.mean.sqmeter<-apply(biomass8.sqmeter, c(1,3), mean, na.rm=T))
(B2.sd.sqmeter<-apply(biomass8.sqmeter, c(1,3), sd, na.rm=T))
(n.samples<-tapply(samples.names$sample,list(samples.names$year,samples.names$tidal.level), length ))
(B2.sem.sqmeter<-B2.sd.sqmeter/sqrt(n.samples))
(D.b2<-B2.sem.sqmeter/B2.mean.sqmeter*100)

write.table(B8.mean.sqmeter, file="2razrez_biomass_old.csv", sep=";", dec=",")

#========== РС маком без разделения на горизнты суммарная ============
library(lattice)

pdf("razrez2_total_size_str.pdf", family = "NimbusSan")
histogram(~ ishodnik$Length.mm[ishodnik$Length.mm >=1] |as.factor(ishodnik$year[ishodnik$Length.mm >=1 ]), xlab = "L, mm", breaks = seq(1, max(ishodnik$Length.mm, na.rm=T)+1, 1))
dev.off()
embedFonts("razrez2_total_size_str.pdf")

# считаем сводку по РС
sizestr_all <- table(Length.int, as.factor(ishodnik$year))

#РС в %
sizestr_all_percent <- t(t(sizestr_all[2:20,])/colSums(sizestr_all[2:20,])*100)

#пишем в файл
write.table(as.data.frame(as.table(sizestr_all_percent)), "razrez2_sizestr_all_percents.csv", sep=";", dec=",")
