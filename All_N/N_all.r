#setwd("~/Dropbox/PhD_thesis/PhD_thesis/seminar_SPbSU_hydrobiology_2014-02-13/N_all/")
setwd("~/Dropbox/PhD_thesis/PhD_thesis/All_N/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)


ishodnik<-read.table(file="N_all_samples.csv", sep=";", dec=",", head=T)
ishodnik$area<-ordered(ishodnik$area, levels=c("Klyushiha", "Suhaya", "Lisya", "Podpahta",
                                               "YuG", "ZRS", "lomnishniy", 
                                               "goreliy", "estuary", "razrez2", 
                                               "Pechenga", "Ura",
                                               "Pala", "Retinskoe", "Abram", "Nagornoe",    
                                               "Gavrilovo", "Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Ivanovskaya"))
ishodnik$region<-ordered(ishodnik$region, levels=c("Chupa_bay", "North_archipelago", "Luvenga", "West_Murman", "Kola_bay", "East_Murman"))
ishodnik$sea<-ordered(ishodnik$sea, levels=c("White", "Barents"))
ishodnik$mareographic<-ordered(ishodnik$mareographic, levels=c("subtidal","hydrographic_datum", "low", "middle", "high"))

str(ishodnik)
summary(ishodnik$area)
summary(ishodnik$mareographic)

# ============ обилие в морях =================
pdf(file="N2_sea.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter ~ ishodnik$sea)
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$sea))){
  text(x=ishodnik$sea[ishodnik$sea==levels(ishodnik$sea)[i]][1], y=15000,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter, INDEX=ishodnik$sea, FUN=median)))[i])
}
dev.off()
embedFonts("N2_sea.pdf") #встройка шрифтов в файл

pdf(file="N2_region.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter ~ ishodnik$region)
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$region))){
  text(x=ishodnik$region[ishodnik$region==levels(ishodnik$region)[i]][1], y=15000,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter, INDEX=ishodnik$region, FUN=median)))[i])
}
dev.off()
embedFonts("N2_region.pdf") #встройка шрифтов в файл

# ========== обилие на участках ========================
pdf(file="N2_area.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter ~ ishodnik$area,  names=abbreviate(levels(ishodnik$area)), ylim=c(0,10000))
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$area))){
text(x=ishodnik$area[ishodnik$area==levels(ishodnik$area)[i]][1], y=10000,
     labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter, INDEX=ishodnik$area, FUN=median)))[i])
}
dev.off()
embedFonts("N2_area.pdf") #встройка шрифтов в файл


pdf(file="N2_area_White.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter[ishodnik$sea=="White",drop=T] ~ ishodnik$area[ishodnik$sea=="White",drop=T],  
        names=(levels(ishodnik$area[ishodnik$sea=="White",drop=T])), ylim=c(0,10000))
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$area))){
  text(x=ishodnik$area[ishodnik$area==levels(ishodnik$area[ishodnik$sea=="White",drop=T])[i]][1], y=10000,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="White",drop=T], 
                              INDEX=ishodnik$area[ishodnik$sea=="White",drop=T], FUN=median)))[i])
}
dev.off()
embedFonts("N2_area_White.pdf") #встройка шрифтов в файл

pdf(file="N2_area_Barents.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T] ~ ishodnik$area[ishodnik$sea=="Barents",drop=T],  
        names=(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T])), ylim=c(0,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T])))
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
  text(x=seq(1:12)[i], y=3900,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
                                     INDEX=ishodnik$area[ishodnik$sea=="Barents",drop=T], FUN=median)))[i])
}
dev.off()
embedFonts("N2_area_Barents.pdf") #встройка шрифтов в файла

#рисуем со средними на белом фоне для статьи
pdf(file="Macoma_N2_area_Barents_means.pdf", family="NimbusSan", bg = "white")
par(mai = c(0.8,0.5,0.5,0.5))
boxplot(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T] ~ ishodnik$area[ishodnik$sea=="Barents",drop=T],  
        names=c("PC", "UR", "PL", "RT", "AB", "NG", "GV", "YA", "DZ", "SH", "PR", "IV"), ylim=c(0,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T])+10), cex.axis=1.2, ylab="N, indd./m-2", xlab = "area", cex.lab=1.2, main = "Macoma balthica")
#подпишем на график средние
for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
  text(x=seq(1:12)[i], y=3920, cex=1.2,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
                                     INDEX=ishodnik$area[ishodnik$sea=="Barents",drop=T], FUN=mean)))[i])
}
dev.off()
embedFonts("Macoma_N2_area_Barents_means.pdf") #встройка шрифтов в файл

# тифф
tiff(file="Macoma_N2_area_Barents_means.tiff", width = )
boxplot(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T] ~ ishodnik$area[ishodnik$sea=="Barents",drop=T],  
        names=abbreviate(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]),minlength = 2), ylim=c(0,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T])))
#подпишем на график средние
for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
  text(x=seq(1:12)[i], y=3915,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
                                     INDEX=ishodnik$area[ishodnik$sea=="Barents",drop=T], FUN=mean)))[i])
}
dev.off()
embedFonts("Macoma_N2_area_Barents_means.pdf") #встройка шрифтов в файл




#тот же график с раскраской отдельных регионов
pdf(file="N2_area_Barents_color.pdf", family="NimbusSan")
boxplot(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T] ~ abbreviate(ishodnik$area[ishodnik$sea=="Barents",drop=T]),  
        names=(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T])), ylim=c(0,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T])), col=c(2,2,3,3,3,3,4,4,4,4,4,4))
#подпишем на график медианы
for (i in 1:length(levels(ishodnik$area[ishodnik$sea=="Barents",drop=T]))){
  text(x=seq(1:12)[i], y=3900,
       labels=round(as.vector(tapply(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents",drop=T], 
                                     INDEX=ishodnik$area[ishodnik$sea=="Barents",drop=T], FUN=median)))[i])
}
dev.off()
embedFonts("N2_area_Barents_color.pdf") #встройка шрифтов в файл


#tapply(ishodnik$N2.indd.sqmeter, INDEX=ishodnik$area, FUN=median)


#hist(ishodnik$N2.indd.sqmeter[ishodnik$sea=="White"], main="White Sea", xlab="N,экз./кв.м",
#     breaks=c(0,50,100,500,1000,2000,5000,10000,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="White"])))

#hist(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents"], main="Barents Sea", xlab="N,экз./кв.м",
#     breaks=c(0,50,100,500,1000,2000,5000,10000,max(ishodnik$N2.indd.sqmeter[ishodnik$sea=="Barents"])))

## горизонты литорали
attach(ishodnik)

for( i in 1:length(levels(area)))
{
  pdf(file=paste(levels(area)[i],sep="_","vertical.pdf"), family="NimbusSan")
  boxplot(N2.indd.sqmeter[area==(levels(area)[i])] ~ mareographic[area==(levels(area)[i])], horizontal=T, 
          main=paste(levels(area)[i]))
  dev.off()
  embedFonts(paste(levels(area)[i],sep="_","vertical.pdf")) #встройка шрифтов в файл
}

#levels(area)
# выбираем только те участки, где несколько горизонтов, и сравниваем горизонты между собой

sink(file="kruskal_vertikal.txt")
for( i in c(1,2,3,4,8,10,13,16,17,18,19,20))
{
print(levels(area)[i])
print(kruskal.test(N2.indd.sqmeter[area==(levels(area)[i])] ~ mareographic[area==(levels(area)[i]), drop=T]))
}
#Клющиха и Лисья - а если убрать сублитораль??
kruskal.test(N2.indd.sqmeter[area=="Klyushiha" & mareographic!="subtidal"] ~ mareographic[area=="Klyushiha" & mareographic!="subtidal"])
kruskal.test(N2.indd.sqmeter[area=="Lisya" & mareographic!="subtidal"] ~ mareographic[area=="Lisya" & mareographic!="subtidal"])
sink()

sink(file="anova_vertikal.txt")
for( i in c(1,2,3,4,8,10,13,16,17,18,19,20))
{
  print(levels(area)[i])
  print(anova(lm(N2.indd.sqmeter[area==(levels(area)[i])] ~ mareographic[area==(levels(area)[i]), drop=T])))
}
sink()

mean(ishodnik$N2.indd.sqmeter[ishodnik$area=="Klyushiha" & ishodnik$mareographic!="subtidal"])
(sd(ishodnik$N2.indd.sqmeter[ishodnik$area=="Klyushiha" & ishodnik$mareographic!="subtidal"])/sqrt(length((ishodnik$N2.indd.sqmeter[ishodnik$area=="Klyushiha" & ishodnik$mareographic!="subtidal"]))))/mean(ishodnik$N2.indd.sqmeter[ishodnik$area=="Klyushiha" & ishodnik$mareographic!="subtidal"])

wilcox.test(x=ishodnik$N2.indd.sqmeter[ishodnik$region=="West_Murman"], mu=1000)

kruskal.test(N2.indd.sqmeter[area=="Gavrilovo"] ~ mareographic[area=="Gavrilovo"])
tapply(N2.indd.sqmeter[area=="Gavrilovo"], mareographic[area=="Gavrilovo"], summary)
c(3,  5,	4,	4,	7, 1,	2,	0,	0,	1)*30

#======== summary по пробоотбору ==================================
tapply(ishodnik$sample, ishodnik$area, length)
table(ishodnik$year[ ishodnik$sea=="Barents", drop=T], ishodnik$area[ ishodnik$sea=="Barents", drop=T])
length(ishodnik$sample)

subset(ishodnik,ishodnik$area=="Pechenga")

