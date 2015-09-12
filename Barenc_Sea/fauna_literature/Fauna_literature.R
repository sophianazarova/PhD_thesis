setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/fauna_literature/")


fauna <- read.table("Fauna_obedinenie_guby_taxa_E.dat", sep = "\t", header = T)
region <- fauna$region

str(fauna)
rownames(fauna) <- fauna$site

fauna <- fauna[3:35]


# ======= Nonmetric MDS =========
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

library(MASS)
#install.packages("vegan")
library(vegan)
d <- vegdist(fauna,method = "jaccard" ) # Jaccard distances between the rows
fit <- isoMDS(d, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]

pdf("nMDS_fauna_littoral_only.pdf", family = "NimbusSan")
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", main="", pch=region-1, col = region+1, cex=2)
text(x, y, labels = seq(1: nrow(fauna)), cex=.7) 
#text(x, y, labels = rownames(fauna), cex=.7) 
#legend(x="bottomleft", legend = paste(seq(1: nrow(fauna)), rownames(fauna)), ncol = 3) 
dev.off()
embedFonts("nMDS_fauna_littoral_only.pdf")

