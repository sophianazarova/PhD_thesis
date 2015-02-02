setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/article_Barents_microdistribution/")


ishodnik <- read.csv2("N_Macoma_cockle.csv", header=T)
str(ishodnik)
summary(ishodnik)

#рисуем картинку обилия по широте Macoma

pdf("Macoma_lat_vs_N.pdf", family="NimbusSan")
plot(x=ishodnik$Lattitude, y=ishodnik$Macoma, ylab="N, ind. m-2", xlab="N", main="Macoma balthica",)
dev.off()
embedFonts("Macoma_lat_vs_logN.pdf")

#logN
pdf("Macoma_lat_vs_logN.pdf", family="NimbusSan")
plot(x=ishodnik$Lattitude, y=log(ishodnik$Macoma, base = 10), ylab="logN, ind. m-2", xlab="lat", main="Macoma balthica",)
dev.off()
embedFonts("Macoma_lat_vs_logN.pdf")

#рисуем картинку обилия по широте Cerastoderma
pdf("cockle_lat_vs_N.pdf", family="NimbusSan")
plot(x=ishodnik$Lattitude, y=ishodnik$cockle, ylab="N, ind. m-2", xlab="lat", main="Cerastoderma edule")
dev.off()
embedFonts("cockle_lat_vs_N.pdf")

## усредняем N для каждых 5 градусов
ishodnik$lat_5grad <- cut(ishodnik$Lattitude, breaks = seq(30,70,5))

pdf("Macoma_lat_vs_N_boxplot.pdf", family="NimbusSan")
boxplot(ishodnik$Macoma ~ ishodnik$lat_5grad, main = "Macoma balthica")
dev.off()
embedFonts("Macoma_lat_vs_N_boxplot.pdf")


pdf("boxplot_lat_vs_N.pdf", family="NimbusSan")
par(mfrow = c(2,1))
par(mai = c(0.2,0.5,0.5,0.5))
boxplot(ishodnik$Macoma ~ ishodnik$lat_5grad, main = "Macoma balthica", names = F)
par(mai = c(0.8,0.5,0.5,0.5))
boxplot(ishodnik$cockle ~ ishodnik$lat_5grad, main = "Cerastoderma edule", ylim = c(0,750), xlab = "lat")
dev.off()
embedFonts("boxplot_lat_vs_N.pdf")


