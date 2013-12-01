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