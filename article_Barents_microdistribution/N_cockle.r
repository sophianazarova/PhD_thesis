setwd("~/Dropbox/PhD_thesis/PhD_thesis/article_Barents_microdistribution/")

ishodnik<-read.table("Cedule_Murman_distribution.csv", header=T, sep=";", dec=",")
str(ishodnik)

ishodnik$N.sqmeter<-ishodnik$N.sample * ishodnik$square

levels(ishodnik$area)

ishodnik$area<-ordered(ishodnik$area, levels=c("Pechenga", "Ura", 
                                               "Pala", "Retinskoe", "Abram", "Nagornoe",   
                                              "Gavrilovo","Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Tryazhina", "Ivanovskaya"))
ishodnik$region<-ordered(ishodnik$region, levels=c("West_Murman", "Kola_bay", "East_Murman"))

#======== summary по пробоотбору ==================================
tapply(ishodnik$sample, ishodnik$area, length)
table(ishodnik$year, ishodnik$area)

# ====== сравнение регионов =======================================================
kruskal.test(ishodnik$N.sqmeter ~ ishodnik$region)

boxplot(ishodnik$N.sqmeter ~ ishodnik$region)

# ============== график обилие с раскраской отдельных регионов ====================
pdf(file="Ncockle_area_Barents_color.pdf", family="NimbusSan")
boxplot(ishodnik$N.sqmeter ~ abbreviate(ishodnik$area),  
        names=abbreviate(levels(ishodnik$area)), ylim=c(0,max(ishodnik$N.sqmeter, na.rm=T)), col=c(3,4,4,4,4,4))
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$area))){
  text(x=seq(1:6)[i], y=150,
       labels=round(as.vector(tapply(ishodnik$N.sqmeter, INDEX=ishodnik$area, FUN=mean, na.rm=T)))[i])
}
dev.off()
embedFonts("Ncockle_area_Barents_color.pdf") #встройка шрифтов в файл

tapply(ishodnik$N.sqmeter, ishodnik$area, mean, na.rm=T)
tapply(ishodnik$N.sqmeter, ishodnik$area, sd, na.rm=T)

length(ishodnik$N.sqmeter[ishodnik$area=="Pala"])
44.9/sqrt(26)

tapply(ishodnik$N.sqmeter, ishodnik$area, median, na.rm=T)

# и черно-белый
pdf(file="Ncockle_area_Barents.pdf", family="NimbusSan")
boxplot(ishodnik$N.sqmeter ~ (ishodnik$area),  
        names=abbreviate(levels(ishodnik$area)), ylim=c(0,max(ishodnik$N.sqmeter, na.rm=T)))
#подпишем на график средние
for (i in 1:length(levels(ishodnik$area))){
  text(x=seq(1:13)[i], y=460,
       labels=round(as.vector(tapply(ishodnik$N.sqmeter, INDEX=ishodnik$area, FUN=mean, na.rm=T)))[i])
}
dev.off()
embedFonts("Ncockle_area_Barents.pdf") #встройка шрифтов в файл


#### Без трящиной
# и черно-белый
ish_tryash <- subset(ishodnik, ishodnik$area!="Tryazhina", drop=T)
ish_tryash$area<-ordered(ish_tryash$area, levels=c("Pechenga", "Ura", 
                                                 "Pala", "Retinskoe", "Abram", "Nagornoe",   
                                                 "Gavrilovo","Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Ivanovskaya"))

#посчитаем средние 
cockle_mean <- round(as.vector(tapply(ish_tryash$N.sqmeter, INDEX=ish_tryash$area, FUN=mean, na.rm=T)))
cockle_mean[cockle_mean==1]<-"<1"

pdf(file="Ncockle_area_Barents_article.pdf", family="NimbusSan", bg = "white")
boxplot(ish_tryash$N.sqmeter ~ (ish_tryash$area),   
        names=c("PC", "UR", "PL", "RT", "AB", "NG", "GV", "YA", "DZ", "SH", "PR", "IV"), ylim=c(0,max(ish_tryash$N.sqmeter, na.rm=T)+10), cex.axis = 1.2, ylab="N, indd./m-2", xlab = "area", cex.lab=1.2)
#подпишем на график средние
for (i in 1:length(levels(ish_tryash$area))){
  text(x=seq(1:13)[i], y=460, cex=1.2,
       labels=cockle_mean[i])
}
dev.off()
embedFonts("Ncockle_area_Barents_article.pdf") #встройка шрифтов в файл

# ggplot
library(ggplot2)

ca <- ggplot(data=ish_tryash, aes(x = area, y = N.sqmeter))

tiff("Ncockle_article.tiff", units = "cm", width = 7.5, height = 7.5, bg = "white", res = 300)
ca + 
  geom_boxplot() + 
  theme_classic() + 
  ylab("N, indd./m-2") +
  scale_x_discrete(breaks=levels(ish_tryash$area), labels=c("PC", "UR", "PL", "RT", "AB", "NG", "GV", "YA", "DZ", "SH", "PR", "IV")) + 
  theme(axis.title.x = element_text(face="bold", size=8), 
        axis.text.x  = element_text(size=8), 
        axis.title.y = element_text(face="bold", size=8), 
        axis.text.y  = element_text(size=8)) +
  annotate("text", label = round(tapply(ish_tryash$N.sqmeter, INDEX=ish_tryash$area, FUN=mean, na.rm=T),0), x=seq(1:12), y=460, size=2.5) 
  

dev.off()




# ================== total distribution ============================
tot_distr<-read.csv2("cockle_total_distr_DZ_2008.csv", header = T)
head(tot_distr)

pdf(file="Ncockle_total_abundance.pdf", family="NimbusSan", bg = "white")
plot(tot_distr$X_sm, tot_distr$Y_sm, xlab = "X, cm", ylab = "Y, cm", cex.axis=1.2)
dev.off()
embedFonts("Ncockle_total_abundance.pdf")


subset(ishodnik, ishodnik$area=="Tryazhina")
