setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/after_Deryuginskie")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

##
ishodnik<-read.table(file="fauna_DZ_svodka.csv", sep=";", dec=",", header=TRUE)
samples.names<-read.table(file="samples.csv", sep=";", dec=",", head=T)


attach(ishodnik)

str(ishodnik)
str(samples.names)

# ===== считаем численность и биомассы на квадратный метр ===========

ishodnik$N.sqmeter<-ishodnik$N.indd * ishodnik$square
ishodnik$B.sqmeter<-ishodnik$B.mg * ishodnik$square

#считаем сколько проб какой площади было в какой год
#(n.samples<-table(samples.names$square,samples.names$year, samples.names$station))
(n.samples<-table(samples.names$year, samples.names$station))

str(ishodnik)
str(samples.names)

# =========  делаем сводку год-станция-вид =================
# TODO надо учесть какие пробы где брали!
(N_sqmeter_svodka<-tapply(ishodnik$N.sqmeter, INDEX=list(ishodnik$species, ishodnik$year, ishodnik$station), FUN=mean, na.rm=T))

(N_sqmeter_sd<-tapply(ishodnik$N.sqmeter, INDEX=list(ishodnik$species, ishodnik$year, ishodnik$station), FUN=sd, na.rm=T))

#биомасса
(B_sqmeter_svodka<-tapply(ishodnik$B.sqmeter, INDEX=list(ishodnik$species, ishodnik$year, ishodnik$station), FUN=mean, na.rm=T))


# пробы которые есть - сделать нули. которых нет - NA


samples.names$sample[samples.names$year==levels(as.factor(ishodnik$year))[i] & samples.names$square==4]

for (i in 1:length(levels(ishodnik$year)))
s4<-ishodnik$sample[ishodnik$year==levels(as.factor(ishodnik$year))[i] & ishodnik$square==4 & ishodnik$species=="Arenicola marina"] %in% samples.names$sample[samples.names$year==levels(as.factor(ishodnik$year))[i] & samples.names$square==4]


# ====== считаем коэффициент Жаккара - различие между списками видов ==========
#install.packages("prabclus")
library(prabclus)

#install.packages("cluster")
library(cluster)

#делаем матрицу типа 0/1
str(N_sqmeter_svodka)

species_presence_matrix<-N_sqmeter_svodka
species_presence_matrix[is.na(species_presence_matrix)]<-0
species_presence_matrix[species_presence_matrix>0]<-1

#### считаем сходство жаккаром отдельно по годам.
for (i in 1:length(dimnames(species_presence_matrix)[[2]])){
assign(paste("jakkard", dimnames(species_presence_matrix)[[2]][i], sep="_"), jaccard(species_presence_matrix[,i,]))  
}

# ===== рисуем графики кластерного анализа методом Варда по матрицам коэффициентов Жаккара ===============
pdf(file="station_jaccard_2002.pdf", family="NimbusSan") # указываем шрифт подпией
  fit<-hclust(as.dist(jakkard_2002[1:8,1:8]), method="ward.D")
  plot(fit) # display dendogram  
dev.off()
embedFonts("station_jaccard_2002.pdf")

pdf(file="station_jaccard_2003.pdf", family="NimbusSan") # указываем шрифт подпией
fit<-hclust(as.dist(jakkard_2003), method="ward.D")
plot(fit) # display dendogram  
dev.off()
embedFonts("station_jaccard_2003.pdf")

pdf(file="station_jaccard_2004.pdf", family="NimbusSan") # указываем шрифт подпией
fit<-hclust(as.dist(jakkard_2004), method="ward.D")
plot(fit) # display dendogram  
dev.off()
embedFonts("station_jaccard_2004.pdf")

pdf(file="station_jaccard_2005.pdf", family="NimbusSan") # указываем шрифт подпией
fit<-hclust(as.dist(jakkard_2005), method="ward.D")
plot(fit) # display dendogram  
dev.off()
embedFonts("station_jaccard_2005.pdf")

pdf(file="station_jaccard_2006.pdf", family="NimbusSan") # указываем шрифт подпией
fit<-hclust(as.dist(jakkard_2006[1:3,1:3]), method="ward.D")
plot(fit) # display dendogram  
dev.off()
embedFonts("station_jaccard_2006.pdf")

pdf(file="station_jaccard_2007.pdf", family="NimbusSan") # указываем шрифт подпией
fit<-hclust(as.dist(jakkard_2007[1:3,1:3]), method="ward.D")
plot(fit) # display dendogram  
dev.off()
embedFonts("station_jaccard_2007.pdf")


# ====== сходство станций с учетом данных за все годы ===============
species_presence_matrix_year<-tapply(ishodnik$N.sqmeter, INDEX=list(ishodnik$species, ishodnik$station), FUN=mean, na.rm=T)
species_presence_matrix_year[is.na(species_presence_matrix_year)]<-0
species_presence_matrix_year[species_presence_matrix_year>0]<-1

jakkard_all<-jaccard(regmat=species_presence_matrix_year[,1:8])

pdf(file="station_jaccard_all_years.pdf", family="NimbusSan") # указываем шрифт подпией
fit<-hclust(as.dist(jakkard_all), method="ward.D")
plot(fit) # display dendogram  
dev.off()
embedFonts("station_jaccard_all_years.pdf")



# ====== сходство станций по 2002 году строим по количественным данным ==========
str(N_sqmeter_svodka)

#делаем выборку 2002 год, вычеркиваем все виды, которые не было найдено в 2002 году и заменяем NA на 0
N_sqmeter_2002<-N_sqmeter_svodka[rowSums(N_sqmeter_svodka[,1,1:8], na.rm=T)!=0,1,1:8]
N_sqmeter_2002[is.na(N_sqmeter_2002)]<-0

N_sqmeter_2002_sd<-N_sqmeter_sd[rowSums(N_sqmeter_sd[,1,1:8], na.rm=T)!=0,1,1:8]
N_sqmeter_2002_SEM<-t(t(N_sqmeter_2002_sd)/sqrt(n.samples[1,1:8]))


#считаем дистанцию Брей-Кертиса
library(vegan)
bray_2002<-vegdist(t(N_sqmeter_2002), method="bray")

pdf(file="station_bray_2002.pdf", family="NimbusSan") # указываем шрифт подпией
fit<-hclust(bray_2002, method="ward.D")
plot(fit) # display dendogram  
dev.off()
embedFonts("station_bray_2002.pdf")

#SIMPROF
#install.packages("clustsig")
library(clustsig)

is.matrix(N_sqmeter_2002)

SIMPROF_Nmean_2002<-simprof(t(N_sqmeter_2002), num.expected = 999, num.simulated = 999, method.cluster = "ward.D", method.distance = "braycurtis", alpha = 0.05)

pdf("station_bray_2002_SIMPROF.pdf", family="NimbusSan")
simprof.plot(SIMPROF_Nmean_2002)
dev.off()
embedFonts("station_bray_2002_SIMPROF.pdf")

pdf("station_bray_2002_SIMPROF_BW.pdf", family="NimbusSan")
simprof.plot(SIMPROF_Nmean_2002, siglinetype = 5, leafcolors = rep("black", length(SIMPROF_Nmean_2002$significantclusters)))
dev.off()
embedFonts("station_bray_2002_SIMPROF_BW.pdf")

# =====  Доминирующие по численности виды на станциях в 2002 году ===========
barplot(sort(N_sqmeter_2002[,1],decreasing=T),names.arg=abbreviate(names(sort(N_sqmeter_2002[,1],decreasing=T))))
        
,col=as.numeric(names(N_sqmeter_2002)))

#экспортирую список видов за все года
write.table(levels(ishodnik$species), "spicies_all_year.csv", sep=";", dec=",")

# ====== таксономическая структура ===================
species<-table(ishodnik$species, ishodnik$year)
species[species>0]<-1
colSums(species)

#всего найдено таксонов за все года
(species.allyears<-rowSums(species))
species.allyears[species.allyears>0]<-1
sum(species.allyears)

#пишем таксоны в файл
write.table(taxons, file="taxons.csv", sep=";", dec=",")

taxons<-read.table(file="taxons_species.csv", header=T, sep=";", dec=",")
taxons$taxon<-ordered(taxons$taxon, levels=c("Diptera", "Isopoda", "Amphipoda", "Bivalvia", "Gastropoda", "Priapulida", "Oligochaeta", "Polychaeta", "Nemertini"))
(taxons_svodka<-table(taxons$taxon))

pdf(file="taxons_pie.pdf", family="NimbusSan")
pie(taxons_svodka)
dev.off()
embedFonts("taxons_pie.pdf") #встройка шрифтов в файл


#объединяем две таблички в одну - чтобы пририсовать к исходнику таксон
# аналог функции merge из Шипунова и Ко (стр.237)
recode <- function(var, from, to)
{
  x <- as.vector(var)
  x.tmp <- x
  for (i in 1:length(from)) {x <- replace(x, x.tmp == from[i],
                                          to[i])}
  if(is.factor(var)) factor(x) else x
}

taxa<-recode(ishodnik$species, taxons$species, as.character(taxons$taxon))
ishodnik_taxons<-(cbind(taxon=taxa, ishodnik))

(N_svodka_taxons<-tapply(ishodnik_taxons$N.sqmeter, list(ishodnik_taxons$taxon, ishodnik_taxons$year, ishodnik_taxons$station), mean))

str(N_svodka_taxons)
N_svodka_taxons[is.na(N_svodka_taxons)]<-0



pdf(file="N_taxons_dot_2002.pdf", family="NimbusSan")
dotchart(N_svodka_taxons[,1,1])
dev.off()
embedFonts("N_taxons_dot_2002.pdf") #встройка шрифтов в файл

pdf(file="N_taxons_pie_2002.pdf", family="NimbusSan")
pie(N_svodka_taxons[,1,1])
dev.off()
embedFonts("N_taxons_pie_2002.pdf") #встройка шрифтов в файл

#делаем гистограмму
N_2002_svodka_taxons<-N_svodka_taxons[,1,1]
N_2002_svodka_taxons<-as.data.frame(N_2002_svodka_taxons[c(1,2,5,6,8,10,11,12,13)])
colnames(N_2002_svodka_taxons)<-c("N_2002")

rownames(N_2002_svodka_taxons)[order(N_2002_svodka_taxons$N_2002, decreasing = T)]

pdf(file="N_taxons_bar_2002.pdf", family="NimbusSan")
barplot(sort(N_2002_svodka_taxons$N_2002, decreasing = T), names.arg = rownames(N_2002_svodka_taxons)[order(N_2002_svodka_taxons$N_2002, decreasing = T)], ylab = "N, экз./кв.м")
dev.off()
embedFonts("N_taxons_bar_2002.pdf") #встройка шрифтов в файл

# =========== вертикальная структура доминантов ==========================
# Fabricia sabella, Pygospio elegans, Oligochaeta, Capitella capitata
# Arenicola marina
ish_2002 <- subset(ishodnik_taxons, ishodnik_taxons$year==2002)
str(ish_2002)

ish_2002_dominants <- subset(ish_2002, ish_2002$species=="Fabricia sabella" | ish_2002$species=="Pygospio elegans" | ish_2002$species=="Capitella capitata" | ish_2002$taxon=="Oligochaeta" | ish_2002$species=="Arenicola marina", drop = T)

mean_2002_dominants <- tapply(ish_2002_dominants$N.sqmeter[ ish_2002_dominants$taxon!="Oligochaeta"], list(ish_2002_dominants$species[ ish_2002_dominants$taxon!="Oligochaeta"], ish_2002_dominants$station[ ish_2002_dominants$taxon!="Oligochaeta"]), FUN = mean, na.rm=T)
mean_2002_Fabricia <- mean_2002_dominants["Fabricia sabella",]
mean_2002_Pygospio <- mean_2002_dominants["Pygospio elegans",]
mean_2002_Capitella <- mean_2002_dominants["Capitella capitata",]
mean_2002_Arenicola <- mean_2002_dominants["Arenicola marina",]

mean_2002_Oligochaeta <- tapply(ish_2002_dominants$N.sqmeter[ ish_2002_dominants$taxon=="Oligochaeta"], ish_2002_dominants$station[ ish_2002_dominants$taxon=="Oligochaeta"], FUN = mean, na.rm=T)
mean_2002_Oligochaeta <- c(mean_2002_Oligochaeta[1], 0, mean_2002_Oligochaeta[2:7])

mean_2002_dominants_vertical <- data.frame(station=seq(1:8), Oligochaeta=mean_2002_Oligochaeta, Fabricia=mean_2002_Fabricia, Pygospio=mean_2002_Pygospio, Capitella=mean_2002_Capitella, Arenicola=mean_2002_Arenicola)
mean_2002_dominants_vertical$station <- ordered(as.factor(mean_2002_dominants_vertical$station), levels=c(8,5,1,7,6,3,9,4,2))
mean_2002_dominants_vertical <- mean_2002_dominants_vertical[order(mean_2002_dominants_vertical$station),]
mean_2002_dominants_vertical$station_vert<-seq(1:8)

pdf("Nlog_vs_station.pdf", family="NimbusSan")
plot(x=log(mean_2002_dominants_vertical$Oligochaeta,base = 10), y=mean_2002_dominants_vertical$station_vert, type="b", col=2, pch=15, xlim = c(0,5.5), ylab="станция", xlab="logN")
points(x=log(mean_2002_dominants_vertical$Fabricia,base = 10), y=mean_2002_dominants_vertical$station_vert, type="b", col=3, pch=16)
points(x=log(mean_2002_dominants_vertical$Pygospio,base = 10), y=mean_2002_dominants_vertical$station_vert, type="b", col=4, pch=17)
points(x=log(mean_2002_dominants_vertical$Capitella,base = 10), y=mean_2002_dominants_vertical$station_vert, type="b", col=5, pch=18)
points(x=log(mean_2002_dominants_vertical$Arenicola,base = 10), y=mean_2002_dominants_vertical$station_vert, type="b", col=6, pch=19)
legend(x="topleft", legend = c("Oligochaeta", "F.sabella", "P.elegans", "C.capitata", "A.marina"), pch=seq(15,19,1), col=seq(2,6,1))
dev.off()
embedFonts("Nlog_vs_station.pdf")

# ============  смотрим различия между обилием видов. ===================
#Возьмем скажем доминантов по численности, слив олигохет.
# Fabricia sabella, Pygospio elegans, Oligochaeta, Capitella capitata
str(ishodnik_taxons)

dominants_Kruskal_W<-data.frame(trubkostroiteli=rep(NA,5), arenicola=rep(NA,5))
rownames(dominants_Kruskal_W)<-c("Fabricia sabella", "Pygospio elegans", "Capitella capitata", "Arenicola marina", "Oligochaeta")
dominants_Kruskal_pvalue<-data.frame(trubkostroiteli=rep(NA,5), arenicola=rep(NA,5))
rownames(dominants_Kruskal_pvalue)<-c("Fabricia sabella", "Pygospio elegans", "Capitella capitata", "Arenicola marina", "Oligochaeta")

# Fabricia sabella
###
(fabricia_ish<-subset(ishodnik_taxons, ishodnik_taxons$species=="Fabricia sabella"))
fabricia_ish$comm[ fabricia_ish$station==1 | fabricia_ish$station==5 | fabricia_ish$station==8 |  fabricia_ish$station==4 | fabricia_ish$station==7]<-"trubkostroiteli"
fabricia_ish$comm[ fabricia_ish$station==2 |fabricia_ish$station==3 | fabricia_ish$station==6]<-"arenicola"
fabricia_ish$comm<-as.factor(fabricia_ish$comm)
fabricia_ish$station<-as.factor(fabricia_ish$station)

fligner.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="trubkostroiteli" & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="trubkostroiteli" & fabricia_ish$year==2002])
shapiro.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002])
anova(lm(fabricia_ish$N.sqmeter[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002]))
fabricia_trubkostroiteli_kruskal<-kruskal.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002])
dominants_Kruskal_W["Fabricia sabella","trubkostroiteli"]<-fabricia_trubkostroiteli_kruskal$statistic
dominants_Kruskal_pvalue["Fabricia sabella","trubkostroiteli"]<-fabricia_trubkostroiteli_kruskal$p.value

fligner.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="arenicola"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="arenicola"  & fabricia_ish$year==2002])
shapiro.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="arenicola" & fabricia_ish$year==2002])
#anova(lm(fabricia_ish$N.sqmeter[fabricia_ish$comm=="arenicola"] ~ fabricia_ish$station[fabricia_ish$comm=="arenicola"]))
fabricia_arenicola_kruskal<-kruskal.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="arenicola"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="arenicola"  & fabricia_ish$year==2002])
dominants_Kruskal_W["Fabricia sabella","arenicola"]<-fabricia_arenicola_kruskal$statistic
dominants_Kruskal_pvalue["Fabricia sabella","arenicola"]<-fabricia_arenicola_kruskal$p.value



# Pygospio elegans
###
(pygospio_ish<-subset(ishodnik_taxons, ishodnik_taxons$species=="Pygospio elegans"))
pygospio_ish$comm[ pygospio_ish$station==1 | pygospio_ish$station==5 | pygospio_ish$station==8 | pygospio_ish$station==4 | pygospio_ish$station==7] <-"trubkostroiteli"
pygospio_ish$comm[ pygospio_ish$station==2 | pygospio_ish$station==3 | pygospio_ish$station==6]<-"arenicola"
pygospio_ish$comm<-as.factor(pygospio_ish$comm)
pygospio_ish$station<-as.factor(pygospio_ish$station)

fligner.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="trubkostroiteli"  & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002])
shapiro.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002])
anova(lm(pygospio_ish$N.sqmeter[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002]))
pygospio_kruskal_trubkostroiteli<-kruskal.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002])
dominants_Kruskal_W["Pygospio elegans","trubkostroiteli"]<-pygospio_kruskal_trubkostroiteli$statistic
dominants_Kruskal_pvalue["Pygospio elegans","trubkostroiteli"]<-pygospio_kruskal_trubkostroiteli$p.value


fligner.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="arenicola"& pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="arenicola" & pygospio_ish$year==2002])
shapiro.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="arenicola" & pygospio_ish$year==2002])
anova(lm(pygospio_ish$N.sqmeter[pygospio_ish$comm=="arenicola" & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="arenicola" & pygospio_ish$year==2002]))
pygospio_kruskal_arenicola<-kruskal.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="arenicola"] ~ pygospio_ish$station[pygospio_ish$comm=="arenicola"])
dominants_Kruskal_W["Pygospio elegans","arenicola"]<-pygospio_kruskal_arenicola$statistic
dominants_Kruskal_pvalue["Pygospio elegans","arenicola"]<-pygospio_kruskal_arenicola$p.value

# Capitella capitata
###
(capitella_ish<-subset(ishodnik_taxons, ishodnik_taxons$species=="Capitella capitata"))
capitella_ish$comm[ capitella_ish$station==1 | capitella_ish$station==5 | capitella_ish$station==8 | capitella_ish$station==4 | capitella_ish$station==7]<-"trubkostroiteli"
capitella_ish$comm[ capitella_ish$station==3 | capitella_ish$station==6 |  capitella_ish$station==2]<-"arenicola"
capitella_ish$comm<-as.factor(capitella_ish$comm)
capitella_ish$station<-as.factor(capitella_ish$station)

fligner.test(capitella_ish$N.sqmeter[capitella_ish$comm=="trubkostroiteli" & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="trubkostroiteli"  & capitella_ish$year==2002])
shapiro.test(capitella_ish$N.sqmeter[capitella_ish$comm=="trubkostroiteli"  & capitella_ish$year==2002])
#anova(lm(capitella_ish$N.sqmeter[capitella_ish$comm=="trubkostroiteli"] ~ capitella_ish$station[capitella_ish$comm=="trubkostroiteli"]))
capitella_kruskal_trubkostroiteli<-kruskal.test(capitella_ish$N.sqmeter[capitella_ish$comm=="trubkostroiteli"  & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="trubkostroiteli"  & capitella_ish$year==2002])
dominants_Kruskal_W["Capitella capitata","trubkostroiteli"]<-capitella_kruskal_trubkostroiteli$statistic
dominants_Kruskal_pvalue["Capitella capitata","trubkostroiteli"]<-capitella_kruskal_trubkostroiteli$p.value



fligner.test(capitella_ish$N.sqmeter[capitella_ish$comm=="arenicola"  & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="arenicola"  & capitella_ish$year==2002])
shapiro.test(capitella_ish$N.sqmeter[capitella_ish$comm=="arenicola"  & capitella_ish$year==2002])
anova(lm(capitella_ish$N.sqmeter[capitella_ish$comm=="arenicola" & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="arenicola" & capitella_ish$year==2002]))
capitella_kruskal_arenicola<-kruskal.test(capitella_ish$N.sqmeter[capitella_ish$comm=="arenicola"] ~ capitella_ish$station[capitella_ish$comm=="arenicola"])
dominants_Kruskal_W["Capitella capitata","arenicola"]<-capitella_kruskal_arenicola$statistic
dominants_Kruskal_pvalue["Capitella capitata","arenicola"]<-capitella_kruskal_arenicola$p.value

# Arenicola marina
##
(arenicola_ish<-subset(ishodnik_taxons, ishodnik_taxons$species=="Arenicola marina"))
arenicola_ish$comm[ arenicola_ish$station==1 | arenicola_ish$station==5 | arenicola_ish$station==8 |  arenicola_ish$station==4 | arenicola_ish$station==7]<-"trubkostroiteli"
arenicola_ish$comm[ arenicola_ish$station==3 | arenicola_ish$station==6 |  arenicola_ish$station==2]<-"arenicola"
arenicola_ish$comm<-as.factor(arenicola_ish$comm)
arenicola_ish$station<-as.factor(arenicola_ish$station)

fligner.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="trubkostroiteli" & arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="trubkostroiteli"& arenicola_ish$year==2002])
shapiro.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="trubkostroiteli"& arenicola_ish$year==2002])
#anova(lm(arenicola_ish$N.sqmeter[arenicola_ish$comm=="trubkostroiteli"] ~ arenicola_ish$station[arenicola_ish$comm=="trubkostroiteli"]))
arenicola_kruskal_trubkostroiteli<-kruskal.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="trubkostroiteli"& arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="trubkostroiteli"& arenicola_ish$year==2002])
dominants_Kruskal_W["Arenicola marina","trubkostroiteli"]<-arenicola_kruskal_trubkostroiteli$statistic
dominants_Kruskal_pvalue["Arenicola marina","trubkostroiteli"]<-arenicola_kruskal_trubkostroiteli$p.value

fligner.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="arenicola"& arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="arenicola"& arenicola_ish$year==2002])
shapiro.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="arenicola"& arenicola_ish$year==2002])
#anova(lm(arenicola_ish$N.sqmeter[arenicola_ish$comm=="arenicola"] ~ arenicola_ish$station[arenicola_ish$comm=="arenicola"]))
arenicola_kruskal_arenicola<-kruskal.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="arenicola" & arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="arenicola"& arenicola_ish$year==2002])
dominants_Kruskal_W["Arenicola marina","arenicola"]<-arenicola_kruskal_arenicola$statistic
dominants_Kruskal_pvalue["Arenicola marina","arenicola"]<-arenicola_kruskal_arenicola$p.value

boxplot(arenicola_ish$N.sqmeter[arenicola_ish$year==2002] ~ arenicola_ish$comm[arenicola_ish$year==2002])


# Oligochaeta
##
(oligochaeta_ish<-subset(ishodnik_taxons, ishodnik_taxons$taxon=="Oligochaeta"))
(oligochaeta_ish<-(tapply(oligochaeta_ish$N.sqmeter, list(oligochaeta_ish$sample, oligochaeta_ish$station, oligochaeta_ish$year), FUN=sum)))
oligochaeta_ish<-as.data.frame(as.table(oligochaeta_ish))
names(oligochaeta_ish)<-c("samples", "station", "year", "N.sqmeter")
str(oligochaeta_ish)
oligochaeta_ish$comm[ oligochaeta_ish$station==1 | oligochaeta_ish$station==5 | oligochaeta_ish$station==8 | oligochaeta_ish$station==4 | oligochaeta_ish$station==7]<-"trubkostroiteli"
oligochaeta_ish$comm[ oligochaeta_ish$station==3 | oligochaeta_ish$station==6 |  oligochaeta_ish$station==2]<-"arenicola"
oligochaeta_ish$comm<-as.factor(oligochaeta_ish$comm)
oligochaeta_ish$station<-as.factor(oligochaeta_ish$station)

fligner.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="trubkostroiteli" & oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="trubkostroiteli"& oligochaeta_ish$year==2002])
shapiro.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="trubkostroiteli" & oligochaeta_ish$year==2002])
anova(lm(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="trubkostroiteli"& oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="trubkostroiteli"& oligochaeta_ish$year==2002]))
oligochaeta_kruskal_trubkostroiteli<-kruskal.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="trubkostroiteli"] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="trubkostroiteli"])
dominants_Kruskal_W["Oligochaeta","trubkostroiteli"]<-oligochaeta_kruskal_trubkostroiteli$statistic
dominants_Kruskal_pvalue["Oligochaeta","trubkostroiteli"]<-oligochaeta_kruskal_trubkostroiteli$p.value


fligner.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="arenicola"& oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="arenicola"& oligochaeta_ish$year==2002])
shapiro.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="arenicola"& oligochaeta_ish$year==2002])
#anova(lm(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="arenicola"] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="arenicola"]))
oligochaeta_kruskal_arenicola<-kruskal.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="arenicola" & oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="arenicola"& oligochaeta_ish$year==2002])
dominants_Kruskal_W["Oligochaeta","arenicola"]<-oligochaeta_kruskal_arenicola$statistic
dominants_Kruskal_pvalue["Oligochaeta","arenicola"]<-oligochaeta_kruskal_arenicola$p.value


write.table(file="dominants_Kruskal_W.csv", dominants_Kruskal_W, sep=";", dec=",")
write.table(file="dominants_Kruskal_pvalue.csv", dominants_Kruskal_pvalue, sep=";", dec=",")


# =========== вписываем к станциям сообщества. ==============================
community<-read.table(file="community.csv", header=T, sep=";", dec=",")

comm<-recode(ishodnik_taxons$station, community$station, as.character(community$community))
ishodnik_community<-(cbind(comm=comm, ishodnik_taxons))
str(ishodnik_community)

comm<-recode(samples.names$station, community$station, as.character(community$community))
samplenames_community<-cbind(comm=comm, samples.names)
str(samplenames_community)
summary(samplenames_community)


# ========== рисуем картинки по эдификаторам в выделенных зонах ===============
pdf("Arenicola_in_zones.pdf", family="NimbusSan")
boxplot(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$year==2002] ~ ishodnik_community$comm[ishodnik_community$species=="Arenicola marina" & ishodnik_community$year==2002])
dev.off()
embedFonts("Arenicola_in_zones.pdf")

pdf("Fabricia_in_zones.pdf", family="NimbusSan")
boxplot(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$year==2002] ~ ishodnik_community$comm[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$year==2002])
dev.off()
embedFonts("Fabricia_in_zones.pdf")

# ============= динамика доминантов: =============================
#Fabrisia sabella, Pygospio elegans, Oligochaeta varia, Capitella capitata
# + Arenicola marina как эдификатор

# берем данные за 1973 год
dominants_1973<-read.table("dominants_1973_Nsqmeter.csv", header=T, sep=";", dec=",")
data_2000<-read.csv2("data_2000_Strelkov_et_al_2001.csv", header=T)

# ============= динамика доминантов: Fabricia =============================

str(ishodnik_taxons)
fabricia_sqmeter_mean<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245"], list(ishodnik_community$comm[ishodnik_community$species=="Fabricia sabella"& ishodnik_community$square=="245"], ishodnik_community$year[ishodnik_community$species=="Fabricia sabella"& ishodnik_community$square=="245"]), mean, na.rm=T)

fabricia_sqmeter_sd<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245"], list(ishodnik_community$comm[ishodnik_community$species=="Fabricia sabella"& ishodnik_community$square=="245"], ishodnik_community$year[ishodnik_community$species=="Fabricia sabella"& ishodnik_community$square=="245"]), sd, na.rm=T)

n.samples.fabricia<-tapply(samplenames_community$sample[samplenames_community$square=="245"], list(samplenames_community$comm[samplenames_community$square=="245"],samplenames_community$year[samplenames_community$square=="245"]), length)

fabricia_sqmeter_SEM<-fabricia_sqmeter_sd/sqrt(n.samples.fabricia[,c(1,3:6)])

Fabricia_means<-(cbind(c(dominants_1973[1,3], dominants_1973[1,2]), fabricia_sqmeter_mean))
colnames(Fabricia_means)<-c("1973", colnames(fabricia_sqmeter_mean))

pdf(file="Fabricia_N_dynamic.pdf", family="NimbusSan")
barplot(Fabricia_means, beside=T, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 240000), main = "Fabricia sabella")
arrows(x0=seq(1.5,17.5,3),
       y0=(c(NA, fabricia_sqmeter_mean[1,]) + c(NA, fabricia_sqmeter_SEM[1,])),
       x1=seq(1.5,17.5,3),
       y1=(c(NA, fabricia_sqmeter_mean[1,]) - c(NA, fabricia_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,17.5,3),
       y0=(c(NA, fabricia_sqmeter_mean[2,]) + c(NA, fabricia_sqmeter_SEM[2,])),
       x1=seq(2.5,17.5,3),
       y1=(c(NA, fabricia_sqmeter_mean[2,]) - c(NA, fabricia_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Fabricia_means))
dev.off()
embedFonts("Fabricia_N_dynamic.pdf") #встройка шрифтов в файл
#locator()

#или лучше boxplot??
boxplot(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245"] ~ ishodnik_community$comm [ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245"] * ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245"], col = rep(c(2,3),5))


str(ishodnik_community)

#проверяем на наличие тренда
#сначала непараметрика, но уж больно все одинаковое...
kruskal.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"] ~ as.factor(ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"]))

kruskal.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"] ~ as.factor(ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"]))

#проверяем условия для дисперсионки и смотрим на нее.
fligner.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"] ~ as.factor(ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"]))

lapply(split(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"], as.factor(ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"])), shapiro.test)

anova(lm(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"] ~ as.factor(ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"])))

fligner.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"] ~ as.factor(ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"]))

lapply(split(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"], as.factor(ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"])), shapiro.test)

anova(lm((ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"] ~ as.factor(ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"]))))

#смотрим на боксплоты
boxplot(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"] ~ ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"])

boxplot(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"] ~ ishodnik_community$year[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"])

# ============= динамика доминантов: Pygospio ====================

str(ishodnik_taxons)
(pygospio_sqmeter_mean<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245"], list(ishodnik_community$comm[ishodnik_community$species=="Pygospio elegans"& ishodnik_community$square=="245"], ishodnik_community$year[ishodnik_community$species=="Pygospio elegans"& ishodnik_community$square=="245"]), mean, na.rm=T))

(pygospio_sqmeter_sd<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245"], list(ishodnik_community$comm[ishodnik_community$species=="Pygospio elegans"& ishodnik_community$square=="245"], ishodnik_community$year[ishodnik_community$species=="Pygospio elegans"& ishodnik_community$square=="245"]), sd, na.rm=T))

(n.samples.pygospio<-tapply(samplenames_community$sample[samplenames_community$square=="245"], list(samplenames_community$comm[samplenames_community$square=="245"],samplenames_community$year[samplenames_community$square=="245"]), length))

(pygospio_sqmeter_SEM<-pygospio_sqmeter_sd/sqrt(n.samples.pygospio))

Pygospio_means<-(cbind(c(dominants_1973[2,3], dominants_1973[2,2]), (pygospio_sqmeter_mean)))
colnames(Pygospio_means)<-c("1973", colnames(pygospio_sqmeter_mean))
Pygospio_means

pdf(file="Pygospio_N_dynamic.pdf", family="NimbusSan")
barplot((Pygospio_means),beside=T, main="Pygospio elegans", sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 51000))
arrows(x0=seq(1.5,20.5,3),
       y0=(c(NA, pygospio_sqmeter_mean[1,]) + c(NA, pygospio_sqmeter_SEM[1,])),
       x1=seq(1.5,20.5,3),
       y1=(c(NA, pygospio_sqmeter_mean[1,]) - c(NA, pygospio_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,20.5,3),
       y0=(c(NA, pygospio_sqmeter_mean[2,]) + c(NA, pygospio_sqmeter_SEM[2,])),
       x1=seq(2.5,20.5,3),
       y1=(c(NA, pygospio_sqmeter_mean[2,]) - c(NA, pygospio_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Pygospio_means))
dev.off()
embedFonts("Pygospio_N_dynamic.pdf") #встройка шрифтов в файл
#locator()

#проверяем на наличие тренда
#непараметрика
kruskal.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"] ~ ishodnik_community$year[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"])

kruskal.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"] ~ ishodnik_community$year[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"])

#проверяем условия для дисперсионки и пробуем ее
fligner.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"] ~ ishodnik_community$year[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"])

lapply(split(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"], as.factor(ishodnik_community$year[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="arenicola"])), shapiro.test)


fligner.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"] ~ ishodnik_community$year[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"])

# lapply(split(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"], as.factor(ishodnik_community$year[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245" & ishodnik_community$comm=="trubkostroiteli"])), shapiro.test)


# ============= динамика доминантов: Capitella capitata ==================

str(ishodnik_taxons)

(capitella_sqmeter_mean<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30"], list(ishodnik_community$comm[ishodnik_community$species=="Capitella capitata"& ishodnik_community$square=="245"|ishodnik_community$square=="30"], ishodnik_community$year[ishodnik_community$species=="Capitella capitata"& ishodnik_community$square=="245"|ishodnik_community$square=="30"]), mean, na.rm=T))

(capitella_sqmeter_sd<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30"], list(ishodnik_community$comm[ishodnik_community$species=="Capitella capitata"& ishodnik_community$square=="245" |ishodnik_community$square=="30"], ishodnik_community$year[ishodnik_community$species=="Capitella capitata"& ishodnik_community$square=="245"|ishodnik_community$square=="30"]), sd, na.rm=T))

(n.samples.capitella<-tapply(samplenames_community$sample[samplenames_community$square=="245"|samplenames_community$square=="30"], list(samplenames_community$comm[samplenames_community$square=="245"|samplenames_community$square=="30"],samplenames_community$year[samplenames_community$square=="245"|samplenames_community$square=="30"]), length))

(capitella_sqmeter_SEM<-capitella_sqmeter_sd/sqrt(n.samples.capitella[1:6]))

Capitella_means<-(cbind(c(dominants_1973[4,3], dominants_1973[4,2]), (capitella_sqmeter_mean)))
colnames(Capitella_means)<-c("1973", colnames(capitella_sqmeter_mean))
Capitella_means

pdf(file="Capitella_N_dynamic.pdf", family="NimbusSan")
barplot((Capitella_means),beside=T, main="Capitella capitata", sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 3100))
arrows(x0=seq(1.5,20.5,3),
       y0=(c(NA, capitella_sqmeter_mean[1,]) + c(NA, capitella_sqmeter_SEM[1,])),
       x1=seq(1.5,20.5,3),
       y1=(c(NA, capitella_sqmeter_mean[1,]) - c(NA, capitella_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,20.5,3),
       y0=(c(NA, capitella_sqmeter_mean[2,]) + c(NA, capitella_sqmeter_SEM[2,])),
       x1=seq(2.5,20.5,3),
       y1=(c(NA, capitella_sqmeter_mean[2,]) - c(NA, capitella_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
text(x=2.5,y=2800, labels=c("19000"),srt=90)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Capitella_means))
dev.off()
embedFonts("Capitella_N_dynamic.pdf") #встройка шрифтов в файл
#locator()

#проверяем на наличие тренда
#смотрим непараметрику
kruskal.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30" & ishodnik_community$comm=="arenicola"] ~ ishodnik_community$year[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30" & ishodnik_community$comm=="arenicola"])

kruskal.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30" & ishodnik_community$comm=="trubkostroiteli"] ~ ishodnik_community$year[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30" & ishodnik_community$comm=="trubkostroiteli"])

#проверяем условия для дисперсионки и смотрим ее
fligner.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30" & ishodnik_community$comm=="arenicola"] ~ ishodnik_community$year[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30" & ishodnik_community$comm=="arenicola"])

fligner.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30" & ishodnik_community$comm=="trubkostroiteli"] ~ ishodnik_community$year[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30" & ishodnik_community$comm=="trubkostroiteli"])



# ============= динамика доминантов: Arenicola ========================

(arenicola_sqmeter_mean<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4"], list(ishodnik_community$comm[ishodnik_community$species=="Arenicola marina"& ishodnik_community$square=="4"], ishodnik_community$year[ishodnik_community$species=="Arenicola marina"& ishodnik_community$square=="4"]), mean, na.rm=T))

(arenicola_sqmeter_sd<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4"], list(ishodnik_community$comm[ishodnik_community$species=="Arenicola marina"& ishodnik_community$square=="4"], ishodnik_community$year[ishodnik_community$species=="Arenicola marina"& ishodnik_community$square=="4"]), sd, na.rm=T))

(n.samples.arenicola<-tapply(samplenames_community$sample[samplenames_community$square=="4"], list(samplenames_community$comm[samplenames_community$square=="4"],samplenames_community$year[samplenames_community$square=="4"]), length))
n.samples.arenicola<-cbind(n.samples.arenicola, c(15,15)) 

(arenicola_sqmeter_SEM<-arenicola_sqmeter_sd/sqrt(n.samples.arenicola))

Arenicola_means<-(cbind(c(dominants_1973[5,3], dominants_1973[5,2]), (arenicola_sqmeter_mean)))
colnames(Arenicola_means)<-c("1973", colnames(arenicola_sqmeter_mean))
Arenicola_means

pdf(file="Arenicola_N_dynamic.pdf", family="NimbusSan")
barplot((Arenicola_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 250))
arrows(x0=seq(1.5,17.5,3),
       y0=(c(NA, arenicola_sqmeter_mean[1,]) + c(NA, arenicola_sqmeter_SEM[1,])),
       x1=seq(1.5,17.5,3),
       y1=(c(NA, arenicola_sqmeter_mean[1,]) - c(NA, arenicola_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,17.5,3),
       y0=(c(NA, arenicola_sqmeter_mean[2,]) + c(NA, arenicola_sqmeter_SEM[2,])),
       x1=seq(2.5,17.5,3),
       y1=(c(NA, arenicola_sqmeter_mean[2,]) - c(NA, arenicola_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Arenicola_means))
dev.off()
embedFonts("Arenicola_N_dynamic.pdf") #встройка шрифтов в файл
#locator()

#проверяем на наличие тренда
#непараметрика
kruskal.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="arenicola"] ~ ishodnik_community$year[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="arenicola"])

kruskal.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="trubkostroiteli"] ~ ishodnik_community$year[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="trubkostroiteli"])

#првоеряем условия для дисперсионки и смотрим ее
fligner.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="arenicola"] ~ ishodnik_community$year[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="arenicola"])


fligner.test(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="trubkostroiteli"] ~ ishodnik_community$year[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="trubkostroiteli"])

lapply(split(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="trubkostroiteli"], as.factor(ishodnik_community$year[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4" & ishodnik_community$comm=="trubkostroiteli"])), shapiro.test)


# ============= динамика доминантов: Oligochaeta ==================
##
oligochaeta_ish1<-subset(ishodnik_community,ishodnik_community$taxon=="Oligochaeta")
str(oligochaeta_ish1)

oligochaeta_ish2<-as.data.frame(as.table(tapply(oligochaeta_ish1$N.sqmeter, list(oligochaeta_ish1$sample, oligochaeta_ish1$square, oligochaeta_ish1$station, oligochaeta_ish1$year), sum)))
colnames(oligochaeta_ish2)<-c("sample", "square", "station", "year", "N.sqmeter")
comm<-recode(oligochaeta_ish2$station, community$station, as.character(community$community))
oligochaeta_ish2<-(cbind(comm=comm, oligochaeta_ish2))


(n.samples.oligochaeta<-tapply(samplenames_community$sample[samplenames_community$square=="245"], list(samplenames_community$comm[samplenames_community$square=="245"],samplenames_community$year[samplenames_community$square=="245"]), length))

str(oligochaeta_ish2)

(oligochaeta_sqmeter_mean<-tapply(oligochaeta_ish2$N.sqmeter, list( oligochaeta_ish2$comm, oligochaeta_ish2$year), mean, na.rm=T))

(oligochaeta_sqmeter_sd<-tapply(oligochaeta_ish2$N.sqmeter, list( oligochaeta_ish2$comm, oligochaeta_ish2$year), sd, na.rm=T))

oligochaeta_sqmeter_SEM<-oligochaeta_sqmeter_sd/sqrt(n.samples.oligochaeta[,c(1,3:6)])

Oligochaeta_means<-(cbind(c(dominants_1973[3,3], dominants_1973[3,2]), (oligochaeta_sqmeter_mean)))
colnames(Oligochaeta_means)<-c("1973", colnames(oligochaeta_sqmeter_mean))
Oligochaeta_means

pdf(file="Oligochaeta_N_dynamic.pdf", family="NimbusSan")
barplot((Oligochaeta_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 125000))
arrows(x0=seq(1.5,17.5,3),
       y0=(c(NA, oligochaeta_sqmeter_mean[1,]) + c(NA, oligochaeta_sqmeter_SEM[1,])),
       x1=seq(1.5,17.5,3),
       y1=(c(NA, oligochaeta_sqmeter_mean[1,]) - c(NA, oligochaeta_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,17.5,3),
       y0=(c(NA, oligochaeta_sqmeter_mean[2,]) + c(NA, oligochaeta_sqmeter_SEM[2,])),
       x1=seq(2.5,17.5,3),
       y1=(c(NA, oligochaeta_sqmeter_mean[2,]) - c(NA, oligochaeta_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Oligochaeta_means))
dev.off()
embedFonts("Oligochaeta_N_dynamic.pdf") #встройка шрифтов в файл

#проверка на наличие тренда
# непараметрика
kruskal.test(oligochaeta_ish2$N.sqmeter[oligochaeta_ish2$comm=="arenicola"] ~ oligochaeta_ish2$year[oligochaeta_ish2$comm=="arenicola"])

kruskal.test(oligochaeta_ish2$N.sqmeter[oligochaeta_ish2$comm=="trubkostroiteli"] ~ oligochaeta_ish2$year[oligochaeta_ish2$comm=="trubkostroiteli"])

# проверка условия для дисперсионки и смотрим ее
fligner.test(oligochaeta_ish2$N.sqmeter[oligochaeta_ish2$comm=="arenicola"] ~ as.factor(oligochaeta_ish2$year[oligochaeta_ish2$comm=="arenicola"]))

fligner.test(oligochaeta_ish2$N.sqmeter[oligochaeta_ish2$comm=="trubkostroiteli"] ~ as.factor(oligochaeta_ish2$year[oligochaeta_ish2$comm=="trubkostroiteli"]))

#lapply(split(oligochaeta_ish2$N.sqmeter[oligochaeta_ish2$comm=="trubkostroiteli"], oligochaeta_ish2$year[oligochaeta_ish2$comm=="trubkostroiteli"]), shapiro.test)




# ========== Сравнение 1973 года и современности.================
# тренда нет: трубкостроители - Fabricia, Arenicola
# пескожильник: Fabricia, Pygospio, Oligochaeta

#считаем квантили у распределений 2,5% и 97,5% и смотрим, выходит за них или нет

str(ishodnik_community)

#Fabricia trubkostroiteli
quantile(ishodnik_community$N.sqmeter[ ishodnik_community$comm == "trubkostroiteli" & ishodnik_community$species == "Fabricia sabella"], probs = c(0.025,0.5,0.975),na.rm = T)

#Arenicola trubkostroiteli
quantile(ishodnik_community$N.sqmeter[ ishodnik_community$comm == "trubkostroiteli" & ishodnik_community$species == "Arenicola marina"], probs = c(0.025,0.5,0.975),na.rm = T)

#Fabricia arenicola
quantile(ishodnik_community$N.sqmeter[ ishodnik_community$comm == "arenicola" & ishodnik_community$species == "Fabricia sabella"], probs = c(0.025,0.5,0.975),na.rm = T)

#Pygospio arenicola
quantile(ishodnik_community$N.sqmeter[ ishodnik_community$comm == "arenicola" & ishodnik_community$species == "Pygospio elegans"], probs = c(0.025,0.5,0.975),na.rm = T)

#Oligochaeta arenicola
str(oligochaeta_ish2)
quantile(oligochaeta_ish2$N.sqmeter[ ishodnik_community$comm == "arenicola"], probs = c(0.025,0.5,0.975),na.rm = T)
