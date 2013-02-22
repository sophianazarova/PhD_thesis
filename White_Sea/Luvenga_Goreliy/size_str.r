# размерная структура суммарно по годам по горизонтам
ishodnik<-read.table(file="length.csv", sep=";", dec=",", head=T)
attach(ishodnik)
year<-factor(year)
Length.int<-cut(Length.mm, breaks=seq(0,20,1))
(size.str.table<-table(Length.int,year, tidal_zone))
(size.str.df<-as.data.frame(size.str.table))
length.class<-seq(1,20,1)
for (i in 1:length(levels(size.str.df$tidal_zone)))
{
  for (j in 1:length(levels(size.str.df$year)))
  {
    pdf(file=paste("sizestr", size.str.df$tidal_zone[i], size.str.df$year[j], ".pdf",sep="_"),width=1000, height=790)
    barplot(size.str.df$Freq[size.str.df$tidal_zone==paste(size.str.df$tidal_zone[i]) & size.str.df$year==paste(size.str.df$year[j])], names.arg=length.class)
    dev.off()
}}