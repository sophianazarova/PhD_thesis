#install.packages("lattice")
#install.packages("latticeExtra")
setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/distribution_Moran")
#library("lattice")
#library("latticeExtra")

##Пала-губа 2008
# 3d with lattice
library("lattice")
library("latticeExtra")

Pala.08<-read.table("Pala_autumn_2008.csv", header=T, sep=";", dec=",")
str(Pala.08)

# расположение проб на полигоне
#bubbles
pdf(file="Pala_samples.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Pala.08$X, Pala.08$Y, rep(1,length(Pala.08$X)), inches=0.07, fg="white", bg="black", xlim=c(0,1200), ylim=c(0,750),
        xlab="см", ylab="см")
abline(h=c(250,500), v=c(300,600,900))
dev.off()
embedFonts(paste("Pala_samples.pdf")) #встройка шрифтов в файл

Pala_N_Macoma_hist<-cloud(Pala.08$N_Macoma_balthica ~ Pala.08$X + Pala.08$Y, type="h", lwd = 5, scales = list(arrows = FALSE))
print(Pala_N_Macoma_hist)

pdf(file="Pala_N_Macoma_3d_1.pdf", family="NimbusSan") # указываем шрифт подпией
print(Pala_N_Macoma_hist)
dev.off()
embedFonts(paste("Pala_N_Macoma_3d_1.pdf")) #встройка шрифтов в файл

Pala_N_Macoma_hist<-cloud(Pala.08$N_Macoma_balthica ~ Pala.08$X * Pala.08$Y, 
                          screen = list(z = 20, x = -45), zoom = 1.1, col.facet = "grey",
                          xbase = 10, ybase = 10, par.settings = list(box.3d = list(col = "transparent")),
                          aspect = c(1, 1), panel.aspect = 0.75, panel.3d.cloud = panel.3dbars)
print(Pala_N_Macoma_hist)

pdf(file="Pala_N_Macoma_3d_2.pdf", family="NimbusSan") # указываем шрифт подпией
print(Pala_N_Macoma_hist)
dev.off()
embedFonts(paste("Pala_N_Macoma_3d_2.pdf")) #встройка шрифтов в файл

#3d with RGL
library(rgl)

Pala.08<-read.table("Pala_autumn_2008.csv", header=T, sep=";", dec=",")
str(Pala.08)

plot3d(x=Pala.08$X, y=Pala.08$y, z=Pala.08$N_Macoma_balthica, type="s", size=0.75, lit=FALSE)
#что-то все равно не работает

x<-rnorm(100)
y<-rnorm(100)
z<-rnorm(100)
plot3d(x,y,z)

#bubbles
pdf(file="Pala_N_Macoma_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Pala.08$X, Pala.08$Y, sqrt(Pala.08$N_Macoma_balthica)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Pala_N_Macoma_bubbles.pdf")) #встройка шрифтов в файл

pdf(file="Pala_B_Macoma_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Pala.08$X, Pala.08$Y, sqrt(Pala.08$B_Macoma_balthica)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Pala_B_Macoma_bubbles.pdf")) #встройка шрифтов в файл

pdf(file="Pala_N_cockle_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Pala.08$X, Pala.08$Y, sqrt(Pala.08$N_Cerastoderma_edule)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Pala_N_cockle_bubbles.pdf")) #встройка шрифтов в файл


## Пала осень 2008 корреляции маком разного возраста
Pala.macoma.ages<-read.table("Pala_Macoma_ages.csv", header=T, sep=";", dec=",")
str(Pala.macoma.ages)

for (i in 4:ncol(Pala.macoma.ages)) {
   pdf(file=paste("Pala_macoma_age_bubb",colnames(Pala.macoma.ages)[i], ".pdf",sep="_"), family="NimbusSan") # указываем шрифт подпией
   symbols(Pala.macoma.ages$X, Pala.macoma.ages$Y, sqrt(Pala.macoma.ages[,i])/ pi, inches=0.2, fg="white", bg="blue", 
           main=paste(Pala.macoma.ages[i]))
  dev.off()
  embedFonts(paste("Pala_macoma_age_bubb",colnames(Pala.macoma.ages)[i], ".pdf",sep="_")) #встройка шрифтов в файл
}

## Ярнышная 2007 
Yarn.07<-read.table(file="Yarnyshnaya_2007.csv", header=T, sep=";", dec=",")
str(Yarn.07)
summary(Yarn.07)

pdf(file="Yarnyshnaya_N_Macoma_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Yarn.07$X, Yarn.07$Y, sqrt(Yarn.07$N_Macoma_balthica)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Yarnyshnaya_N_Macoma_bubbles.pdf")) #встройка шрифтов в файл

pdf(file="Yarnyshnaya_B_Macoma_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Yarn.07$X, Yarn.07$Y, sqrt(Yarn.07$B_Macoma_balthica)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Yarnyshnaya_B_Macoma_bubbles.pdf")) #встройка шрифтов в файл

pdf(file="Yarnyshnaya_N_cockle_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Yarn.07$X, Yarn.07$Y, sqrt(Yarn.07$N_Cerastoderma_edule)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Yarnyshnaya_N_cockle_bubbles.pdf")) #встройка шрифтов в файл

# расположение проб на полигоне
#bubbles
pdf(file="Yarnyshnaya_samples.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Yarn.07$X, Yarn.07$Y, rep(1,length(Yarn.07$X)), inches=0.07, fg="white", bg="black", xlim=c(0,1200), ylim=c(0,750),
        xlab="см", ylab="см")
abline(h=c(250,500), v=c(300,600,900))
dev.off()
embedFonts(paste("Yarnyshnaya_samples.pdf")) #встройка шрифтов в файл


##Дальний Пляж 2007
Plyazh.07<-read.table(file="Dalnezeleneckaya_2007.csv", header=T, sep=";", dec=",")

# расположение проб на полигоне
#bubbles
pdf(file="Plyazh07_samples.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Plyazh.07$X, Plyazh.07$Y, rep(1,length(Plyazh.07$X)), inches=0.07, fg="white", bg="black", xlim=c(0,1200), ylim=c(0,750),
        xlab="см", ylab="см")
abline(h=c(250,500), v=c(300,600,900))
dev.off()
embedFonts(paste("Plyazh07_samples.pdf")) #встройка шрифтов в файл

pdf(file="Plyzh07_N_cockle_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Plyazh.07$X, Plyazh.07$Y, sqrt(Plyazh.07$N_Cerastoderma_edule)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Plyzh07_N_cockle_bubbles.pdf")) #встройка шрифтов в файл

##Дальний Пляж 2008 квадраты 1+2
Plyazh.0812<-read.table(file="Dalnezeleneckaya_kv12_2008.csv", header=T, sep=";", dec=",")

# расположение проб на полигоне
#bubbles
pdf(file="Plyazh08_samples.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Plyazh.0812$X, Plyazh.0812$Y, rep(1,length(Plyazh.0812$X)), inches=0.07, fg="white", bg="black", xlim=c(0,1200), ylim=c(0,1500),
        xlab="см", ylab="см")
abline(h=c(250,500,750,1000,1250), v=c(300,600,900))
dev.off()
embedFonts(paste("Plyazh08_samples.pdf")) #встройка шрифтов в файл

#bubbles
pdf(file="Plyazh0812_N_Macoma_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Plyazh.0812$X, Plyazh.0812$Y, sqrt(Plyazh.0812$N_Macoma_balthica)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Plyazh0812_N_Macoma_bubbles.pdf")) #встройка шрифтов в файл

pdf(file="Plyazh0812_B_Macoma_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Plyazh.0812$X, Plyazh.0812$Y, sqrt(Plyazh.0812$B_Macoma_balthica)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Plyazh0812_B_Macoma_bubbles.pdf")) #встройка шрифтов в файл

##Дальний Пляж 2007
Plyazh.07<-read.table(file="Dalnezeleneckaya_2007.csv", header=T, sep=";", dec=",")
#bubbles

pdf(file="Plyazh07_N_Macoma_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Plyazh.07$X, Plyazh.07$Y, sqrt(Plyazh.07$N_Macoma_balthica)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Plyazh07_N_Macoma_bubbles.pdf")) #встройка шрифтов в файл

pdf(file="Plyazh07_B_Macoma_bubbles.pdf", family="NimbusSan") # указываем шрифт подпией
symbols(Plyazh.07$X, Plyazh.07$Y, sqrt(Plyazh.07$B_Macoma_balthica)/ pi, inches=0.2, fg="white", bg="blue")
dev.off()
embedFonts(paste("Plyazh07_B_Macoma_bubbles.pdf")) #встройка шрифтов в файл