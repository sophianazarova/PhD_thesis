setwd("~/Dropbox/PhD_thesis/PhD_thesis/article_Barents_microdistribution/")

ishodnik<-read.table("Cedule_Murman_distribution.csv", header=T, sep=";", dec=",")
str(ishodnik)

ishodnik$N.sqmeter<-ishodnik$N.sample * ishodnik$square

ishodnik$area<-ordered(ishodnik$area, levels=c("Pala", 
                                               "Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Tryazhina"))
ishodnik$region<-ordered(ishodnik$region, levels=c("Kola_bay", "East_Murman"))

# ====== сравнение регионов =======================================================
kruskal.test(ishodnik$N.sqmeter ~ ishodnik$region)

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