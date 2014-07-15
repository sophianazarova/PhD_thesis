setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/growth_from_MSc/")

# только СГЛ
middle_all<-read.table("prirost_middle.csv", header=T, sep=";", dec=",")

levels(middle_all$area)
middle_all$area<-ordered(middle_all$area, c("Abram-mys", "Pala", "Gavrilovo", "Yarnyshnaya", "DP", "Shelpino", "Porchnikha"))

levels(middle_all$start_size)
middle_all$start_size<-ordered(middle_all$start_size, c("<3,0", "3,1-6,0", "6,1-9,0", "9,1-12,0", "12,1-15,0", ">15,1"))

str(middle_all)

mid_anova<-anova(lm(growth.mm ~ area * start_size, data = middle_all))
write.table(mid_anova, "middle_anova.csv", dec=",", sep=";")

#поверхность отклика
library(lattice)
wireframe(growth.mm ~ as.numeric(start_size)*as.numeric(area), data = middle_all,
          xlab = "X Coordinate (feet)", ylab = "Y Coordinate (feet)",
          main = "Surface elevation data",
          drape = TRUE,
          colorkey = TRUE,
          screen = list(z = -60, x = -60)
)



# 2 горизонта литорали
low_middle<-read.table("prirost_low_middle.csv", header=T, sep=";", dec=",")

str(low_middle)

low_mid_anova<-anova(lm(growth.mm ~ area * tidal_level * start_size, data = low_middle))
write.table(low_mid_anova, "low_middle_anova.csv", dec=",", sep=";")
