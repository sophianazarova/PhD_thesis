setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/soobshestvo/")

#####
## суммарно по участкам
species_sites<-read.table("soobshestva_sites.csv", header=T, sep=";", dec=",")
rownames(species_sites)<-species_sites[,1]
species_sites<-species_sites[,2:53]
str(species_sites)

library(vegan)
#mds
# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d <- vegdist(x = species_sites, method = "jaccard") 
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
pdf(file="soobshestvo_summ_Jakkard_MDS.pdf", family="NimbusSan")
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(x, y, labels = row.names(species_sites), cex=.7)
dev.off()
embedFonts("soobshestvo_summ_Jakkard_MDS.pdf")

#cluster


#####
## участки + горизонты
species_tidal_sites<-read.table("soobshestva_tidla_sites.csv", header=T, sep=";", dec=",")
rownames(species_tidal_sites)<-species_tidal_sites[,1]
species_tidal_sites<-species_tidal_sites[,2:53]
str(species_tidal_sites)

library(vegan)
#mds
# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d <- vegdist(x = species_tidal_sites, method = "jaccard") 
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
pdf(file="soobshestvo_po_gorizontam_Jakkard_MDS.pdf", family="NimbusSan")
x <- fit$points[,1]
y <- fit$points[,2]
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", type="n")
text(x, y, labels = row.names(species_tidal_sites), cex=.7)
dev.off()
embedFonts("soobshestvo_po_gorizontam_Jakkard_MDS.pdf")