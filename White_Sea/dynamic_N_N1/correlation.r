setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/dynamic_N_N1")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

# ====== размерная структура средние по годам по горизонтам ====================
ishodnik<-read.table(file="All_Kandalaksha_N2mean.csv", sep=";", dec=",", head=T)
str(ishodnik)
cor(ishodnik, method="spearman", use="pairwise.complete.obs")

library(psych)
sink("spearman_N2_all.txt")
corr.test(ishodnik, method="spearman", use="pairwise")
sink()

ish_cor_spearman<-corr.test(ishodnik, method="spearman", use="pairwise")
str(ish_cor_spearman)

write.table(ish_cor_spearman$r, file="spearman_N2.csv", sep=";", dec=",")
write.table(ish_cor_spearman$p, file="pvalue_spearman_N2.csv", sep=";", dec=",")

plot(ishodnik$Estuary, ishodnik$ZRS)
plot(ishodnik$year, ishodnik$ZRS, type="b", pch=15, col=4)
lines(ishodnik$year, ishodnik$Estuary, type="b", pch=15, col=5)
