setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/article_Barents_microdistribution/")

ishodnik <- read.csv2("All_dynamic_Murman.csv")
ishodnik$code <- ordered(ishodnik$code, levels=c("PG", "UR", "PL", "RT", "AB", "NG", "GV", "YA", "DZ", "SH", "PR", "TR", "IV"))

str(ishodnik)

N_mean <- tapply(log(ishodnik$N.sqmeter+1, base = 10), list(ishodnik$year, ishodnik$code, ishodnik$species), mean)

N_mean <- tapply(ishodnik$N.sqmeter, list(ishodnik$year, ishodnik$code, ishodnik$species), mean)

N_mean <- log(tapply(ishodnik$N.sqmeter, list(ishodnik$year, ishodnik$code, ishodnik$species), mean), base = 10)

#Macoma
pdf("N_dynamic_Macoma.pdf", family = "NimbusSan")
plot(x = as.numeric(rownames(N_mean)), N_mean[,"DZ",2], type = "n", ylim = c(0,max(N_mean[,,2], na.rm=T)), main = "Macoma balthica", xlab = "year", ylab = "lgN")
  points(as.numeric(rownames(N_mean)), N_mean[,1,2], pch=21, bg="gray")  
points(as.numeric(rownames(N_mean)), N_mean[,2,2], pch=22, bg="gray")  
points(as.numeric(rownames(N_mean)), N_mean[,3,2], pch=15)  
points(as.numeric(rownames(N_mean)), N_mean[,4,2], pch=16)  
points(as.numeric(rownames(N_mean)), N_mean[,5,2], pch=17)  
points(as.numeric(rownames(N_mean)), N_mean[,6,2], pch=18)  
points(as.numeric(rownames(N_mean)), N_mean[,7,2], pch=0) 
points(as.numeric(rownames(N_mean)), N_mean[,8,2], pch=1) 
points(as.numeric(rownames(N_mean)), N_mean[,9,2], pch=2) 
points(as.numeric(rownames(N_mean)), N_mean[,10,2], pch=3) 
points(as.numeric(rownames(N_mean)), N_mean[,11,2], pch=4) 
points(as.numeric(rownames(N_mean)), N_mean[,12,2], pch=5) 
points(as.numeric(rownames(N_mean)), N_mean[,13,2], pch=6) 
legend(x = "topleft", pch = c(21,22,15,16,17,18,0,1,2,3,4,5,6),  pt.bg = "gray", legend = levels(ishodnik$code), ncol = 2)
dev.off()
embedFonts("N_dynamic_Macoma.pdf")



#Cerastoderma
pdf("N_dynamic_cockle.pdf", family = "NimbusSan")
plot(x = as.numeric(rownames(N_mean)), N_mean[,"DZ",1], type = "n", ylim = c(0,max(N_mean[,,1], na.rm=T)), main = "Cerastoderma edule", xlab = "year", ylab = "lgN")
points(as.numeric(rownames(N_mean)), N_mean[,1,1], pch=21, bg="gray")  
points(as.numeric(rownames(N_mean)), N_mean[,2,1], pch=22, bg="gray")  
points(as.numeric(rownames(N_mean)), N_mean[,3,1], pch=15)  
points(as.numeric(rownames(N_mean)), N_mean[,4,1], pch=16)  
points(as.numeric(rownames(N_mean)), N_mean[,5,1], pch=17)  
points(as.numeric(rownames(N_mean)), N_mean[,6,1], pch=18)  
points(as.numeric(rownames(N_mean)), N_mean[,7,1], pch=0) 
points(as.numeric(rownames(N_mean)), N_mean[,8,1], pch=1) 
points(as.numeric(rownames(N_mean)), N_mean[,9,1], pch=2) 
points(as.numeric(rownames(N_mean)), N_mean[,10,1], pch=3) 
points(as.numeric(rownames(N_mean)), N_mean[,11,1], pch=4) 
points(as.numeric(rownames(N_mean)), N_mean[,12,1], pch=5) 
points(as.numeric(rownames(N_mean)), N_mean[,13,1], pch=6) 
legend(x = "topleft", pch = c(21,22,15,16,17,18,0,1,2,3,4,5,6),  pt.bg = "gray", legend = levels(ishodnik$code), ncol=2)
dev.off()
embedFonts("N_dynamic_cockle.pdf")

# Макома дальний пляж динамика
Macoma_DZ <- subset(ishodnik, ishodnik$species == "Macoma balthica" & ishodnik$code == "DZ")

boxplot(Macoma_DZ$N.sqmeter ~ Macoma_DZ$region)

kruskal.test(Macoma_DZ$N.sqmeter ~ Macoma_DZ$year)

anova(lm((Macoma_DZ$N.sqmeter ~ Macoma_DZ$year)))
TukeyHSD(aov(lm(Macoma_DZ$N.sqmeter ~ as.factor(Macoma_DZ$year))))

# средние везде

sink("N_mean.txt")
tapply(ishodnik$N.sqmeter, list(ishodnik$year, ishodnik$code, ishodnik$species), mean)
sink()

sink("N_SEM.txt")
tapply(ishodnik$N.sqmeter, list(ishodnik$year, ishodnik$code, ishodnik$species), sd)/ sqrt(tapply(ishodnik$N.sqmeter, list(ishodnik$year, ishodnik$code, ishodnik$species), length))
sink()
