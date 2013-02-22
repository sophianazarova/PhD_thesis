# размерная структура суммарно по годам по горизонтам
ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
samples.squares<-read.table(file="squares.csv", sep=";", dec=",", head=T)
samples.names<-read.table(file="sample.csv", sep=";", dec=",", head=T)
attach(ishodnik)
year<-factor(year)

Length.int<-cut(Length.mm, breaks=seq(0,20,1))

(size.str.table<-table(Length.int,year,sample))

size.str.df<-as.data.frame(size.str.table) # как таблица данных

# SUBSET - для фильтрации таблицы данных
# APPLY - кто-то из них для средней и СД по фрейму
# все что не сушествует надо сделать NA
# ЭТО НЕ РАБОТАЕТ??
#for (i in 1:length(levels(size.str.df$year)))
#{
#  size.str.df$Freq[size.str.df$year==levels(size.str.df$year)[i] & size.str.df$sample!=samples.names$sample[samples.names$year==levels(size.str.df$year)[i]]]<-NA
#}

size.str.df<-subset(size.str.df, size.str.df$Freq!="NA")

#теперь на квадратный метр
size.str.sqmeter<-size.str.df
for (i in 1:length(levels(size.str.sqmeter$year)))
{
  size.str.sqmeter$Freq[size.str.sqmeter$year==levels(size.str.sqmeter$year)[i]]<-size.str.sqmeter$Freq[size.str.sqmeter$year==levels(size.str.sqmeter$year)[i]] * samples.squares$square[samples.squares$year==levels(size.str.sqmeter$year)[i]]
}

subset(size.str.sqmeter,subset=size.str.sqmeter$year=="1992")
#и среднее??
# tapply выдает как резудьтат матрицу
(mean.sizestr.sqmeter<-tapply(size.str.sqmeter$Freq,INDEX=list(size.str.sqmeter$year, size.str.sqmeter$Length.int),FUN=mean))

  (sd.sizestr.sqmeter<-tapply(size.str.sqmeter$Freq,INDEX=list(size.str.sqmeter$year, size.str.sqmeter$Length.int),FUN=))

sem.sizestr.sqmeter <-sd.sizestr.sqmeter


(mean.size.str.df<-as.data.frame(size.str.table))
length.class<-seq(1,20,1)

for (i in 1:length(levels(size.str.df$tidal_zone)))
{
  for (j in 1:length(levels(size.str.df$year)))
  {
    pdf(file=paste("sizestr", size.str.df$tidal_zone[i], size.str.df$year[j], ".pdf",sep="_"),width=1000, height=790)
    barplot(size.str.df$Freq[size.str.df$tidal_zone==paste(size.str.df$tidal_zone[i]) & size.str.df$year==paste(size.str.df$year[j])], names.arg=length.class)
    dev.off()
}}