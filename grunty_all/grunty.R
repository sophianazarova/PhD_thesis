setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/grunty_all/")

grunt <- read.csv2("grunty_all.csv")

str(grunt)

grunt$site<-ordered(grunt$site, levels=c("Suhaya", "Klushiha", 
                                                "ZRS",
                                               "Goreliy", "Estuary", 
                                               "Abram", "Pala", 
                                               "Gavrilovo", "Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Ivanovskaya"))

White <- subset(grunt, grunt$sea == "White")
pdf("grunty_white.pdf", family="NimbusSan")
barplot(t(as.matrix(White[,4:6])), names.arg = abbreviate(White$site_name), col = c("gray", "yellow", "brown") , cex.axis = 1.5, cex.names = 1.3)
dev.off()
embedFonts("grunty_white.pdf")

Barents <- subset(grunt, grunt$sea == "Barents")
pdf("grunty_barents.pdf", family="NimbusSan")
barplot(t(as.matrix(Barents[,4:6])), names.arg = abbreviate(Barents$site_name), col = c("gray", "yellow", "brown"), cex.axis = 1.5, cex.names = 1.3)
dev.off()
embedFonts("grunty_barents.pdf")

#==== PCA ==========
library(openxlsx)

all_data <- read.xlsx("taxa meta.xlsx", colNames = T, rowNames = T, sheet = "list2")
str(all_data)
all_grunts <- all_data[,9:16]

grunts_pca <- princomp(all_grunts)
biplot(grunts_pca)
