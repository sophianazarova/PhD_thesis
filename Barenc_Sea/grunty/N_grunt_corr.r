setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/grunty/")


#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)


ishodnik<-read.table(file="grunty_svodka.csv", sep=";", dec=",", head=T)
attach(ishodnik)

str(ishodnik)
summary(ishodnik)


sink("spearman.txt")
for (i in 4:12)
  {
  print(names(ishodnik)[i])
  print(cor.test(ishodnik[,2], ishodnik[,i],method="spearman"))
}
sink()

## пакет psych - корреляционная матрица, функция corr.test
#install.packages("psych")
library(psych)
sink("spearman_corrtest.txt")
corr.test(ishodnik[,c(2,4:12)],method="spearman", use="complete")
sink()