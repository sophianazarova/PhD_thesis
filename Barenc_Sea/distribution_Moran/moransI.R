#install.packages("pgirmess")
#install.packages("ncf")
setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/distribution_Moran")
#library("pgirmess")
library("ncf")

#Пала-губа 2008
Pala.08<-read.table("Pala_autumn_2008.csv", header=T, sep=";", dec=",")
str(Pala.08)

for (i in 4:ncol(Pala.08)) {
  assign(paste("Pala_moran",colnames(Pala.08)[i],sep="_"),correlog(x=Pala.08$X, y=Pala.08$Y, z=Pala.08[,i], increment=100, resamp=1000))

pdf(file=paste("Pala_moran",colnames(Pala.08)[i], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
plot(correlog(x=Pala.08$X, y=Pala.08$Y, z=Pala.08[,i], increment=100, resamp=1000))
dev.off()
embedFonts(paste("Pala_moran",colnames(Pala.08)[i], ".pdf",sep="_")) #встройка шрифтов в файл
}
           
#кросскорреляции

for (i in 4:(ncol(Pala.08)-1)) {
  for(j in (i+1):ncol(Pala.08)){
assign(paste("Pala_cross",colnames(Pala.08)[i],colnames(Pala.08)[j],sep="_"),correlog(x=Pala.08$X, y=Pala.08$Y, z=Pala.08[,i], w=Pala.08[,j], increment=100, resamp=1000))
    
pdf(file=paste("Pala_cross",colnames(Pala.08)[i], colnames(Pala.08)[j], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
plot(correlog(x=Pala.08$X, y=Pala.08$Y, z=Pala.08[,i], w=Pala.08[,j], increment=100, resamp=1000))
dev.off()
embedFonts(paste("Pala_cross",colnames(Pala.08)[i], colnames(Pala.08)[j], ".pdf",sep="_")) #встройка шрифтов в файл
}}



#Дальний Пляж 2007
Plyazh.07<-read.table(file="Dalnezeleneckaya_2007.csv", header=T, sep=";", dec=",")
str(Plyazh.07)
summary(Plyazh.07)


for (i in 4:ncol(Plyazh.07)) {
  assign(paste("Plyazh07_moran",colnames(Plyazh.07)[i],sep="_"),
         correlog(x=Plyazh.07$X, y=Plyazh.07$Y, z=Plyazh.07[,i], increment=100, resamp=1000, na.rm=T))

  pdf(file=paste("Plyazh07_moran",colnames(Plyazh.07)[i], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
  plot(correlog(x=Plyazh.07$X, y=Plyazh.07$Y, z=Plyazh.07[,i], increment=100, resamp=1000, na.rm=T))
  dev.off()
  embedFonts(paste("Plyazh07_moran",colnames(Plyazh.07)[i], ".pdf",sep="_")) #встройка шрифтов в файл
}

#кросскорреляции

for (i in 4:(ncol(Plyazh.07)-1)) {
  for(j in (i+1):ncol(Plyazh.07)){
    assign(paste("Plyazh_07_cross",colnames(Plyazh.07)[i],colnames(Plyazh.07)[j],sep="_"),correlog(x=Plyazh.07$X, y=Plyazh.07$Y, z=Plyazh.07[,i], w=Plyazh.07[,j], na.rm=T, increment=100, resamp=1000))
    
    pdf(file=paste("Plyazh_07_cross",colnames(Plyazh.07)[i], colnames(Plyazh.07)[j], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
    plot(correlog(x=Plyazh.07$X, y=Plyazh.07$Y, z=Plyazh.07[,i], w=Plyazh.07[,j], increment=100, resamp=1000, na.rm=T))
    dev.off()
    embedFonts(paste("Plyazh_07_cross",colnames(Plyazh.07)[i], colnames(Plyazh.07)[j], ".pdf",sep="_")) #встройка шрифтов в файл
  }}


#Дальний Пляж 2008 квадрат1
Plyazh.081<-read.table(file="Dalnezeleneckaya_kv1_2008.csv", header=T, sep=";", dec=",")
str(Plyazh.081)
summary(Plyazh.081)


for (i in 4:ncol(Plyazh.081)) {
  assign(paste("Plyazh081_moran",colnames(Plyazh.081)[i],sep="_"),
         correlog(x=Plyazh.081$X, y=Plyazh.081$Y, z=Plyazh.081[,i], increment=100, resamp=1000))
  
  pdf(file=paste("Plyazh081_moran",colnames(Plyazh.081)[i], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
  plot(correlog(x=Plyazh.081$X, y=Plyazh.081$Y, z=Plyazh.081[,i], increment=100, resamp=1000))
  dev.off()
  embedFonts(paste("Plyazh081_moran",colnames(Plyazh.081)[i], ".pdf",sep="_")) #встройка шрифтов в файл
}

#Дальний Пляж 2008 квадрат2
Plyazh.082<-read.table(file="Dalnezeleneckaya_kv2_2008.csv", header=T, sep=";", dec=",")
str(Plyazh.082)
summary(Plyazh.082)


for (i in 4:ncol(Plyazh.082)) {
  assign(paste("Plyazh082_moran",colnames(Plyazh.082)[i],sep="_"),
         correlog(x=Plyazh.082$X, y=Plyazh.082$Y, z=Plyazh.082[,i], increment=100, resamp=1000))
  
  pdf(file=paste("Plyazh082_moran",colnames(Plyazh.082)[i], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
  plot(correlog(x=Plyazh.082$X, y=Plyazh.082$Y, z=Plyazh.082[,i], increment=100, resamp=1000))
  dev.off()
  embedFonts(paste("Plyazh082_moran",colnames(Plyazh.082)[i], ".pdf",sep="_")) #встройка шрифтов в файл
}

#Дальний Пляж 2008 квадраты 1+2
Plyazh.0812<-read.table(file="Dalnezeleneckaya_kv12_2008.csv", header=T, sep=";", dec=",")
str(Plyazh.0812)
summary(Plyazh.0812)


for (i in 4:ncol(Plyazh.0812)) {
  assign(paste("Plyazh0812_moran",colnames(Plyazh.0812)[i],sep="_"),
         correlog(x=Plyazh.0812$X, y=Plyazh.0812$Y, z=Plyazh.0812[,i], increment=100, resamp=1000))
  
  pdf(file=paste("Plyazh0812_moran",colnames(Plyazh.0812)[i], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
  plot(correlog(x=Plyazh.0812$X, y=Plyazh.0812$Y, z=Plyazh.0812[,i], increment=100, resamp=1000))
  dev.off()
  embedFonts(paste("Plyazh0812_moran",colnames(Plyazh.0812)[i], ".pdf",sep="_")) #встройка шрифтов в файл
}

# Ярнышная 2007 
Yarn.07<-read.table(file="Yarnyshnaya_2007.csv", header=T, sep=";", dec=",")
str(Yarn.07)
summary(Yarn.07)


for (i in 4:ncol(Yarn.07)) {
  assign(paste("Yarn07_moran",colnames(Yarn.07)[i],sep="_"),
         correlog(x=Yarn.07$X, y=Yarn.07$Y, z=Yarn.07[,i], increment=100, resamp=1000, na.rm=T))
  
  pdf(file=paste("Yarnyshnaya07_moran",colnames(Yarn.07)[i], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
  plot(correlog(x=Yarn.07$X, y=Yarn.07$Y, z=Yarn.07[,i], increment=100, resamp=1000, na.rm=T))
  dev.off()
  embedFonts(paste("Yarnyshnaya07_moran",colnames(Yarn.07)[i], ".pdf",sep="_")) #встройка шрифтов в файл
}
