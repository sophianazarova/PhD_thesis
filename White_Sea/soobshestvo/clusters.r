setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/soobshestvo/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

#install.packages("clustsig")
#install.packages("prabclus")
library(clustsig)
library(vegan)
library(prabclus)

# ====== делаем функцию, которая считает коэффициент Жаккара ===========
Jaccard <- function(X) vegan::vegdist(X, method = "jaccard")

# ============ SIMPROF считаем по горизонтам ================================
## читаем файл с исходником.
ishodnik<-read.table(file="fauna_svodka_tidal.csv", sep=";", dec=",", head=T)

str(ishodnik)

# готовим матрицу
ish_tidal<-as.matrix(ishodnik[,3:ncol(ishodnik)])
rownames(ish_tidal)<-paste(abbreviate(ishodnik$area), abbreviate(ishodnik$tidal_level), sep = "_")
is.matrix(ish_tidal)

#считаем simprof
fauna_tidal_clust<-simprof(data = (ish_tidal), num.expected = 999, num.simulated = 999, method.cluster = "single", method.distance = Jaccard, alpha = 0.05)

fauna_tidal_clust_average<-simprof(data = (ish_tidal), num.expected = 999, num.simulated = 999, method.cluster = "average", method.distance = Jaccard, alpha = 0.05)
simprof.plot(fauna_tidal_clust_average)

#рисуем картинку
pdf("White_fauna_tidal_jaccard_single.pdf", family="NimbusSan")
simprof.plot(fauna_tidal_clust)
dev.off()
embedFonts("White_fauna_tidal_jaccard_single.pdf")

pdf("White_fauna_tidal_jaccard_single_BW.pdf", family="NimbusSan")
simprof.plot(fauna_tidal_clust, siglinetype = 5, leafcolors = rep("black", length(fauna_tidal_clust$significantclusters)))
dev.off()
embedFonts("White_fauna_tidal_jaccard_single_BW.pdf")


# ============ SIMPROF считаем по участкам ================
ish_sites<-read.table(file = "fauna_svodka_sites.csv", header=T, sep=";", dec=",")

ish_sites_m<-as.matrix(ish_sites[,2:ncol(ish_sites)])
rownames(ish_sites_m)<-abbreviate(ish_sites$area)

#считаем simprof
fauna_sites_clust<-simprof(data = ish_sites_m, num.expected = 999, num.simulated = 999, method.cluster = "single", method.distance = Jaccard, alpha = 0.05)

#рисуем картинку
pdf("White_fauna_sites_jaccard_single.pdf", family="NimbusSan")
simprof.plot(fauna_sites_clust)
dev.off()
embedFonts("White_fauna_sites_jaccard_single.pdf")

pdf("White_fauna_sites_jaccard_single_BW.pdf", family="NimbusSan")
simprof.plot(fauna_sites_clust, siglinetype = 5, leafcolors = rep("black", length(fauna_sites_clust$significantclusters)))
dev.off()
embedFonts("White_fauna_sites_jaccard_single_BW.pdf")

# ======= рисуем таксономический состав ==========================
taxa<-read.csv2("taxons.csv")

taxa_svodka<-table(taxa$taxon)

sort(taxa_svodka)

pdf("White_taxons.pdf", family="NimbusSan")
dotchart(x = sort(as.vector(taxa_svodka)), labels = names(taxa_svodka)[order(as.vector(taxa_svodka))])
dev.off()
embedFonts("White_taxons.pdf")

pdf("White_taxons_pie.pdf", family="NimbusSan")
pie(x = sort(as.vector(taxa_svodka)), labels = names(taxa_svodka)[order(as.vector(taxa_svodka))])
dev.off()
embedFonts("White_taxons_pie.pdf")


pdf("White_taxons_pie_big.pdf", family="NimbusSan")
pie(x = sort(as.vector(taxa_svodka)), labels = names(taxa_svodka)[order(as.vector(taxa_svodka))], cex=2)
dev.off()
embedFonts("White_taxons_pie_big.pdf")

pdf("White_taxons_pie_big.pdf", family="NimbusSan")
pie(x = as.vector(taxa_svodka), labels = names(taxa_svodka), cex=2)
dev.off()
embedFonts("White_taxons_pie_big.pdf")