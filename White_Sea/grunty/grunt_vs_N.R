setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/White_Sea/grunty/")

ishodnik <- read.csv2("grunt_vs_N.csv")

ish_m <- as.matrix(ishodnik[,2:9])
row.names(ish_m) <- ishodnik[,1]

sp <- cor.test(ishodnik$more5.1mm,ishodnik$Nmax, method="spearman")  
str(sp)
sp$

spearman_grunt <- data.frame(sp_Nmean = rep(NA,6), p_mean = rep(NA,6), sp_Nmax = rep(NA,6), p_max = rep(NA,6), row.names = colnames(ishodnik)[2:7])
for (i in 2:7){
  for (j in c(8,9)){
    sp <- cor.test(ishodnik[,i], ishodnik[,j], method="spearman")     
    if (j==8) {
      spearman_grunt[i-1,1] <- sp$estimate
      spearman_grunt[i-1,2] <- sp$p.value
    }
    if (j==9) { spearman_grunt[i-1,3] <- sp$estimate
                spearman_grunt[i-1,4] <- sp$p.value
    }
  }
}

write.table(spearman_grunt, "grunt_N_spearman.csv", sep = ";", dec = ",")
