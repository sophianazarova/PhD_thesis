setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/after_Deryuginskie")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="fauna_DZ_svodka.csv", sep=";", dec=",", header=TRUE)
samples.names<-read.table(file="samples.csv", sep=";", dec=",", head=T)

attach(ishodnik)

str(ishodnik)
str(samples.names)

# считаем численность и биомассы на квадратный метр

ishodnik$N.sqmeter<-ishodnik$N.indd * ishodnik$square
ishodnik$B.sqmeter<-ishodnik$B.mg * ishodnik$square

#считаем сколько проб какой площади было в какой год
#(n.samples<-table(samples.names$square,samples.names$year, samples.names$station))
(n.samples<-table(samples.names$year, samples.names$station))

# БЫла идея работать с разными рамками отдельно, но мы забили, и усредняем по всем рамкам разного размера.
#####
##245ки
ish_245<-subset(ishodnik, square==245)
str(ish_245)

#Mean
N_sqmeter_245<-tapply(ish_245$N.sqmeter, INDEX=list(ish_245$species, ish_245$year, ish_245$station), FUN=sum, na.rm=T)

str(N_sqmeter_245)

for (i in 1:length(levels(as.factor(ish_245$year)))) {
  for (j in 1:length(levels(as.factor(ish_245$station)))) {
    N_sqmeter_245[,i,j]<-N_sqmeter_245[,i,j]/n.samples[4,i,j]
  }
}

#SD=sqrt((sum((x-MEAN)^2))/(n-1))

#####

str(ishodnik)
str(samples.names)

# делаем сводку год-станция-вид
# TODO надо учесть какие пробы где брали!
(N_sqmeter_svodka<-tapply(ishodnik$N.sqmeter, INDEX=list(ishodnik$species, ishodnik$year, ishodnik$station), FUN=mean, na.rm=T))

(N_sqmeter_sd<-tapply(ishodnik$N.sqmeter, INDEX=list(ishodnik$species, ishodnik$year, ishodnik$station), FUN=sd, na.rm=T))


#####
#пробы которые есть - сделать нули. которых нет - NA


samples.names$sample[samples.names$year==levels(as.factor(ishodnik$year))[i] & samples.names$square==4]

for (i in 1:length(levels(ishodnik$year)))
s4<-ishodnik$sample[ishodnik$year==levels(as.factor(ishodnik$year))[i] & ishodnik$square==4 & ishodnik$species=="Arenicola marina"] %in% samples.names$sample[samples.names$year==levels(as.factor(ishodnik$year))[i] & samples.names$square==4]


#for (i in 1:length(levels(size.str.df$year)))
#{ (xxx<-size.str.df$sample[size.str.df$year==levels(size.str.df$year)[i] ]%in%
#     samples.names$sample[samples.names$year==levels(size.str.df$year)[i]])
#  antixxx<-as.logical(1-xxx)
#  size.str.df$Freq[size.str.df$year==levels(size.str.df$year)[i]][antixxx]<-NA
#}
#####

#####

#считаем коэффициент Жаккара - различие между списками видов
#install.packages("prabclus")
library(prabclus)

#install.packages("cluster")
library(cluster)

#делаем матрицу типа 0/1
str(N_sqmeter_svodka)

species_presence_matrix<-N_sqmeter_svodka
species_presence_matrix[is.na(species_presence_matrix)]<-0
species_presence_matrix[species_presence_matrix>0]<-1

# считаем сходство жаккаром отдельно по годам.
for (i in 1:length(dimnames(species_presence_matrix)[[2]])){
assign(paste("jakkard", dimnames(species_presence_matrix)[[2]][i], sep="_"), jaccard(species_presence_matrix[,i,]))  
}

#рисуем графики кластерного анализа методом Варда по матрицам коэффициентов Жаккара
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


#сходство станций с учетом данных за все годы
species_presence_matrix_year<-tapply(ishodnik$N.sqmeter, INDEX=list(ishodnik$species, ishodnik$station), FUN=mean, na.rm=T)
species_presence_matrix_year[is.na(species_presence_matrix_year)]<-0
species_presence_matrix_year[species_presence_matrix_year>0]<-1

jakkard_all<-jaccard(regmat=species_presence_matrix_year[,1:8])

pdf(file="station_jaccard_all_years.pdf", family="NimbusSan") # указываем шрифт подпией
fit<-hclust(as.dist(jakkard_all), method="ward.D")
plot(fit) # display dendogram  
dev.off()
embedFonts("station_jaccard_all_years.pdf")


# по 2002 году строим поколичественным данным
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

# Доминирующие по численности виды на станциях в 2002 году
barplot(sort(N_sqmeter_2002[,1],decreasing=T),names.arg=abbreviate(names(sort(N_sqmeter_2002[,1],decreasing=T))))
        
,col=as.numeric(names(N_sqmeter_2002)))


## таксономическая структура
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

N_svodka_taxons<-tapply(ishodnik_taxons$N.sqmeter, list(ishodnik_taxons$taxon, ishodnik_taxons$year, ishodnik_taxons$station), mean)

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

# смотрим различия между обилием видов. 
#Возьмем скажем доминантов по численности, слив олигохет.
# Fabricia sabella, Pygospio elegans, Oligochaeta, Capitella capitata
str(ishodnik_taxons)

dominants_Kruskal_W<-data.frame(trubkostroiteli=rep(NA,5), perehodnaya=rep(NA,5), arenicola=rep(NA,5))
rownames(dominants_Kruskal_W)<-c("Fabricia sabella", "Pygospio elegans", "Capitella capitata", "Arenicola marina", "Oligochaeta")
dominants_Kruskal_pvalue<-data.frame(trubkostroiteli=rep(NA,5), perehodnaya=rep(NA,5), arenicola=rep(NA,5))
rownames(dominants_Kruskal_pvalue)<-c("Fabricia sabella", "Pygospio elegans", "Capitella capitata", "Arenicola marina", "Oligochaeta")

# Fabricia sabella
#####
(fabricia_ish<-subset(ishodnik_taxons, ishodnik_taxons$species=="Fabricia sabella"))
fabricia_ish$comm[ fabricia_ish$station==1 | fabricia_ish$station==5 | fabricia_ish$station==8]<-"trubkostroiteli"
fabricia_ish$comm[ fabricia_ish$station==4 | fabricia_ish$station==7]<-"perehodnaya"
fabricia_ish$comm[ fabricia_ish$station==3 | fabricia_ish$station==6]<-"arenicola"
fabricia_ish$comm[ fabricia_ish$station==2]<-"verh"
fabricia_ish$comm<-as.factor(fabricia_ish$comm)
fabricia_ish$station<-as.factor(fabricia_ish$station)

fligner.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="trubkostroiteli" & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="trubkostroiteli" & fabricia_ish$year==2002])
shapiro.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002])
anova(lm(fabricia_ish$N.sqmeter[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002]))
fabricia_trubkostroiteli_kruskal<-kruskal.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="trubkostroiteli"  & fabricia_ish$year==2002])
dominants_Kruskal_W["Fabricia sabella","trubkostroiteli"]<-fabricia_trubkostroiteli_kruskal$statistic
dominants_Kruskal_pvalue["Fabricia sabella","trubkostroiteli"]<-fabricia_trubkostroiteli_kruskal$p.value

fligner.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="perehodnaya"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="perehodnaya" & fabricia_ish$year==2002])
shapiro.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="perehodnaya"  & fabricia_ish$year==2002])
anova(lm(fabricia_ish$N.sqmeter[fabricia_ish$comm=="perehodnaya" & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="perehodnaya"  & fabricia_ish$year==2002]))
fabricia_perehodnaya_kruskal<-kruskal.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="perehodnaya" & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="perehodnaya"  & fabricia_ish$year==2002])
dominants_Kruskal_W["Fabricia sabella","perehodnaya"]<-fabricia_perehodnaya_kruskal$statistic
dominants_Kruskal_pvalue["Fabricia sabella","perehodnaya"]<-fabricia_perehodnaya_kruskal$p.value

fligner.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="arenicola"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="arenicola"  & fabricia_ish$year==2002])
shapiro.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="arenicola" & fabricia_ish$year==2002])
#anova(lm(fabricia_ish$N.sqmeter[fabricia_ish$comm=="arenicola"] ~ fabricia_ish$station[fabricia_ish$comm=="arenicola"]))
fabricia_arenicola_kruskal<-kruskal.test(fabricia_ish$N.sqmeter[fabricia_ish$comm=="arenicola"  & fabricia_ish$year==2002] ~ fabricia_ish$station[fabricia_ish$comm=="arenicola"  & fabricia_ish$year==2002])
dominants_Kruskal_W["Fabricia sabella","arenicola"]<-fabricia_arenicola_kruskal$statistic
dominants_Kruskal_pvalue["Fabricia sabella","arenicola"]<-fabricia_arenicola_kruskal$p.value
#####


# Pygospio elegans
#####
(pygospio_ish<-subset(ishodnik_taxons, ishodnik_taxons$species=="Pygospio elegans"))
pygospio_ish$comm[ pygospio_ish$station==1 | pygospio_ish$station==5 | pygospio_ish$station==8]<-"trubkostroiteli"
pygospio_ish$comm[ pygospio_ish$station==4 | pygospio_ish$station==7]<-"perehodnaya"
pygospio_ish$comm[ pygospio_ish$station==3 | pygospio_ish$station==6]<-"arenicola"
pygospio_ish$comm[ pygospio_ish$station==2]<-"verh"
pygospio_ish$comm<-as.factor(pygospio_ish$comm)
pygospio_ish$station<-as.factor(pygospio_ish$station)

fligner.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="trubkostroiteli"  & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002])
shapiro.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002])
anova(lm(pygospio_ish$N.sqmeter[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002]))
pygospio_kruskal_trubkostroiteli<-kruskal.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="trubkostroiteli" & pygospio_ish$year==2002])
dominants_Kruskal_W["Pygospio elegans","trubkostroiteli"]<-pygospio_kruskal_trubkostroiteli$statistic
dominants_Kruskal_pvalue["Pygospio elegans","trubkostroiteli"]<-pygospio_kruskal_trubkostroiteli$p.value

fligner.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="perehodnaya" & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="perehodnaya" & pygospio_ish$year==2002])
shapiro.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="perehodnaya" & pygospio_ish$year==2002])
#anova(lm(pygospio_ish$N.sqmeter[pygospio_ish$comm=="perehodnaya"] ~ pygospio_ish$station[pygospio_ish$comm=="perehodnaya"]))
pygospio_kruskal_perehodnaya<-kruskal.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="perehodnaya"& pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="perehodnaya" & pygospio_ish$year==2002])
dominants_Kruskal_W["Pygospio elegans","perehodnaya"]<-pygospio_kruskal_perehodnaya$statistic
dominants_Kruskal_pvalue["Pygospio elegans","perehodnaya"]<-pygospio_kruskal_perehodnaya$p.value

fligner.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="arenicola"& pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="arenicola" & pygospio_ish$year==2002])
shapiro.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="arenicola" & pygospio_ish$year==2002])
anova(lm(pygospio_ish$N.sqmeter[pygospio_ish$comm=="arenicola" & pygospio_ish$year==2002] ~ pygospio_ish$station[pygospio_ish$comm=="arenicola" & pygospio_ish$year==2002]))
pygospio_kruskal_arenicola<-kruskal.test(pygospio_ish$N.sqmeter[pygospio_ish$comm=="arenicola"] ~ pygospio_ish$station[pygospio_ish$comm=="arenicola"])
dominants_Kruskal_W["Pygospio elegans","arenicola"]<-pygospio_kruskal_arenicola$statistic
dominants_Kruskal_pvalue["Pygospio elegans","arenicola"]<-pygospio_kruskal_arenicola$p.value
#####

# Capitella capitata
#####
(capitella_ish<-subset(ishodnik_taxons, ishodnik_taxons$species=="Capitella capitata"))
capitella_ish$comm[ capitella_ish$station==1 | capitella_ish$station==5 | capitella_ish$station==8]<-"trubkostroiteli"
capitella_ish$comm[ capitella_ish$station==4 | capitella_ish$station==7]<-"perehodnaya"
capitella_ish$comm[ capitella_ish$station==3 | capitella_ish$station==6]<-"arenicola"
capitella_ish$comm[ capitella_ish$station==2]<-"verh"
capitella_ish$comm<-as.factor(capitella_ish$comm)
capitella_ish$station<-as.factor(capitella_ish$station)

fligner.test(capitella_ish$N.sqmeter[capitella_ish$comm=="trubkostroiteli" & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="trubkostroiteli"  & capitella_ish$year==2002])
shapiro.test(capitella_ish$N.sqmeter[capitella_ish$comm=="trubkostroiteli"  & capitella_ish$year==2002])
#anova(lm(capitella_ish$N.sqmeter[capitella_ish$comm=="trubkostroiteli"] ~ capitella_ish$station[capitella_ish$comm=="trubkostroiteli"]))
capitella_kruskal_trubkostroiteli<-kruskal.test(capitella_ish$N.sqmeter[capitella_ish$comm=="trubkostroiteli"  & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="trubkostroiteli"  & capitella_ish$year==2002])
dominants_Kruskal_W["Capitella capitata","trubkostroiteli"]<-capitella_kruskal_trubkostroiteli$statistic
dominants_Kruskal_pvalue["Capitella capitata","trubkostroiteli"]<-capitella_kruskal_trubkostroiteli$p.value


fligner.test(capitella_ish$N.sqmeter[capitella_ish$comm=="perehodnaya"  & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="perehodnaya"  & capitella_ish$year==2002])
shapiro.test(capitella_ish$N.sqmeter[capitella_ish$comm=="perehodnaya"  & capitella_ish$year==2002])
#anova(lm(capitella_ish$N.sqmeter[capitella_ish$comm=="perehodnaya"] ~ capitella_ish$station[capitella_ish$comm=="perehodnaya"]))
capitella_kruskal_perehodnaya<-kruskal.test(capitella_ish$N.sqmeter[capitella_ish$comm=="perehodnaya"  & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="perehodnaya"  & capitella_ish$year==2002])
dominants_Kruskal_W["Capitella capitata","perehodnaya"]<-capitella_kruskal_perehodnaya$statistic
dominants_Kruskal_pvalue["Capitella capitata","perehodnaya"]<-capitella_kruskal_perehodnaya$p.value

fligner.test(capitella_ish$N.sqmeter[capitella_ish$comm=="arenicola"  & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="arenicola"  & capitella_ish$year==2002])
shapiro.test(capitella_ish$N.sqmeter[capitella_ish$comm=="arenicola"  & capitella_ish$year==2002])
anova(lm(capitella_ish$N.sqmeter[capitella_ish$comm=="arenicola" & capitella_ish$year==2002] ~ capitella_ish$station[capitella_ish$comm=="arenicola" & capitella_ish$year==2002]))
capitella_kruskal_arenicola<-kruskal.test(capitella_ish$N.sqmeter[capitella_ish$comm=="arenicola"] ~ capitella_ish$station[capitella_ish$comm=="arenicola"])
dominants_Kruskal_W["Capitella capitata","arenicola"]<-capitella_kruskal_arenicola$statistic
dominants_Kruskal_pvalue["Capitella capitata","arenicola"]<-capitella_kruskal_arenicola$p.value
#####

# Arenicola marina
#####
(arenicola_ish<-subset(ishodnik_taxons, ishodnik_taxons$species=="Arenicola marina"))
arenicola_ish$comm[ arenicola_ish$station==1 | arenicola_ish$station==5 | arenicola_ish$station==8]<-"trubkostroiteli"
arenicola_ish$comm[ arenicola_ish$station==4 | arenicola_ish$station==7]<-"perehodnaya"
arenicola_ish$comm[ arenicola_ish$station==3 | arenicola_ish$station==6]<-"arenicola"
arenicola_ish$comm[ arenicola_ish$station==2]<-"verh"
arenicola_ish$comm<-as.factor(arenicola_ish$comm)
arenicola_ish$station<-as.factor(arenicola_ish$station)

fligner.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="trubkostroiteli" & arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="trubkostroiteli"& arenicola_ish$year==2002])
shapiro.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="trubkostroiteli"& arenicola_ish$year==2002])
#anova(lm(arenicola_ish$N.sqmeter[arenicola_ish$comm=="trubkostroiteli"] ~ arenicola_ish$station[arenicola_ish$comm=="trubkostroiteli"]))
arenicola_kruskal_trubkostroiteli<-kruskal.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="trubkostroiteli"& arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="trubkostroiteli"& arenicola_ish$year==2002])
dominants_Kruskal_W["Arenicola marina","trubkostroiteli"]<-arenicola_kruskal_trubkostroiteli$statistic
dominants_Kruskal_pvalue["Arenicola marina","trubkostroiteli"]<-arenicola_kruskal_trubkostroiteli$p.value

fligner.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="perehodnaya" & arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="perehodnaya"& arenicola_ish$year==2002])
shapiro.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="perehodnaya"& arenicola_ish$year==2002])
#anova(lm(arenicola_ish$N.sqmeter[arenicola_ish$comm=="perehodnaya"] ~ arenicola_ish$station[arenicola_ish$comm=="perehodnaya"]))
arenicola_kruskal_perehodnaya<-kruskal.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="perehodnaya"& arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="perehodnaya"& arenicola_ish$year==2002])
dominants_Kruskal_W["Arenicola marina","perehodnaya"]<-arenicola_kruskal_perehodnaya$statistic
dominants_Kruskal_pvalue["Arenicola marina","perehodnaya"]<-arenicola_kruskal_perehodnaya$p.value

fligner.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="arenicola"& arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="arenicola"& arenicola_ish$year==2002])
shapiro.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="arenicola"& arenicola_ish$year==2002])
#anova(lm(arenicola_ish$N.sqmeter[arenicola_ish$comm=="arenicola"] ~ arenicola_ish$station[arenicola_ish$comm=="arenicola"]))
arenicola_kruskal_arenicola<-kruskal.test(arenicola_ish$N.sqmeter[arenicola_ish$comm=="arenicola" & arenicola_ish$year==2002] ~ arenicola_ish$station[arenicola_ish$comm=="arenicola"& arenicola_ish$year==2002])
dominants_Kruskal_W["Arenicola marina","arenicola"]<-arenicola_kruskal_arenicola$statistic
dominants_Kruskal_pvalue["Arenicola marina","arenicola"]<-arenicola_kruskal_arenicola$p.value

boxplot(arenicola_ish$N.sqmeter[arenicola_ish$year==2002] ~ arenicola_ish$comm[arenicola_ish$year==2002])


#####

# Oligochaeta
#####
(oligochaeta_ish<-subset(ishodnik_taxons, ishodnik_taxons$taxon=="Oligochaeta"))
(oligochaeta_ish<-(tapply(oligochaeta_ish$N.sqmeter, list(oligochaeta_ish$sample, oligochaeta_ish$station, oligochaeta_ish$year), FUN=sum)))
oligochaeta_ish<-as.data.frame(as.table(oligochaeta_ish))
names(oligochaeta_ish)<-c("samples", "station", "year", "N.sqmeter")
str(oligochaeta_ish)
oligochaeta_ish$comm[ oligochaeta_ish$station==1 | oligochaeta_ish$station==5 | oligochaeta_ish$station==8]<-"trubkostroiteli"
oligochaeta_ish$comm[ oligochaeta_ish$station==4 | oligochaeta_ish$station==7]<-"perehodnaya"
oligochaeta_ish$comm[ oligochaeta_ish$station==3 | oligochaeta_ish$station==6]<-"arenicola"
oligochaeta_ish$comm[ oligochaeta_ish$station==2]<-"verh"
oligochaeta_ish$comm<-as.factor(oligochaeta_ish$comm)
oligochaeta_ish$station<-as.factor(oligochaeta_ish$station)

fligner.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="trubkostroiteli" & oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="trubkostroiteli"& oligochaeta_ish$year==2002])
shapiro.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="trubkostroiteli" & oligochaeta_ish$year==2002])
anova(lm(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="trubkostroiteli"& oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="trubkostroiteli"& oligochaeta_ish$year==2002]))
oligochaeta_kruskal_trubkostroiteli<-kruskal.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="trubkostroiteli"] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="trubkostroiteli"])
dominants_Kruskal_W["Oligochaeta","trubkostroiteli"]<-oligochaeta_kruskal_trubkostroiteli$statistic
dominants_Kruskal_pvalue["Oligochaeta","trubkostroiteli"]<-oligochaeta_kruskal_trubkostroiteli$p.value

fligner.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="perehodnaya"& oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="perehodnaya"& oligochaeta_ish$year==2002])
shapiro.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="perehodnaya"& oligochaeta_ish$year==2002])
anova(lm(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="perehodnaya"& oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="perehodnaya"& oligochaeta_ish$year==2002]))
oligochaeta_kruskal_perehodnaya<-kruskal.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="perehodnaya"] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="perehodnaya"])
dominants_Kruskal_W["Oligochaeta","perehodnaya"]<-oligochaeta_kruskal_perehodnaya$statistic
dominants_Kruskal_pvalue["Oligochaeta","perehodnaya"]<-oligochaeta_kruskal_perehodnaya$p.value

fligner.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="arenicola"& oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="arenicola"& oligochaeta_ish$year==2002])
shapiro.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="arenicola"& oligochaeta_ish$year==2002])
#anova(lm(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="arenicola"] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="arenicola"]))
oligochaeta_kruskal_arenicola<-kruskal.test(oligochaeta_ish$N.sqmeter[oligochaeta_ish$comm=="arenicola" & oligochaeta_ish$year==2002] ~ oligochaeta_ish$station[oligochaeta_ish$comm=="arenicola"& oligochaeta_ish$year==2002])
dominants_Kruskal_W["Oligochaeta","arenicola"]<-oligochaeta_kruskal_arenicola$statistic
dominants_Kruskal_pvalue["Oligochaeta","arenicola"]<-oligochaeta_kruskal_arenicola$p.value
#####

write.table(file="dominants_Kruskal_W.csv", dominants_Kruskal_W, sep=";", dec=",")
write.table(file="dominants_Kruskal_pvalue.csv", dominants_Kruskal_pvalue, sep=";", dec=",")

#####


# вписываем к станциям сообщества.
community<-read.table(file="community.csv", header=T, sep=";", dec=",")

comm<-recode(ishodnik_taxons$station, community$station, as.character(community$community))
ishodnik_community<-(cbind(comm=comm, ishodnik_taxons))
str(ishodnik_community)

comm<-recode(samples.names$station, community$station, as.character(community$community))
samplenames_community<-cbind(comm=comm, samples.names)
str(samplenames_community)
summary(samplenames_community)


#рисуем картинки по эдификаторам в выделенных зонах
pdf("Arenicola_in_zones.pdf", family="NimbusSan")
boxplot(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$year==2002] ~ ishodnik_community$comm[ishodnik_community$species=="Arenicola marina" & ishodnik_community$year==2002])
dev.off()
embedFonts("Arenicola_in_zones.pdf")

pdf("Fabricia_in_zones.pdf", family="NimbusSan")
boxplot(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$year==2002] ~ ishodnik_community$comm[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$year==2002])
dev.off()
embedFonts("Fabricia_in_zones.pdf")

#динамика доминантов:
#Fabrisia sabella, Pygospio elegans, Oligochaeta varia, Capitella capitata
# + Arenicola marina как эдификатор

# берем данные за 1973 год
dominants_1973<-read.table("dominants_1973_Nsqmeter.csv", header=T, sep=";", dec=",")

#Fabricia
#####
str(ishodnik_taxons)
fabricia_sqmeter_mean<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245"], list(ishodnik_community$comm[ishodnik_community$species=="Fabricia sabella"& ishodnik_community$square=="245"], ishodnik_community$year[ishodnik_community$species=="Fabricia sabella"& ishodnik_community$square=="245"]), mean, na.rm=T)

fabricia_sqmeter_sd<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Fabricia sabella" & ishodnik_community$square=="245"], list(ishodnik_community$comm[ishodnik_community$species=="Fabricia sabella"& ishodnik_community$square=="245"], ishodnik_community$year[ishodnik_community$species=="Fabricia sabella"& ishodnik_community$square=="245"]), sd, na.rm=T)

n.samples.fabricia<-tapply(samplenames_community$sample[samplenames_community$square=="245"], list(samplenames_community$comm[samplenames_community$square=="245"],samplenames_community$year[samplenames_community$square=="245"]), length)

fabricia_sqmeter_SEM<-fabricia_sqmeter_sd/sqrt(n.samples.fabricia[,c(1,3:6)])

Fabricia_means<-(cbind(c(dominants_1973[1,3],NA, dominants_1973[1,2]), (fabricia_sqmeter_mean)))
colnames(Fabricia_means)<-c("1973", colnames(fabricia_sqmeter_mean))


pdf(file="Fabricia_N_dynamic.pdf", family="NimbusSan")
barplot(Fabricia_means,beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 240000))
arrows(x0=seq(1.5,24.5,4),
       y0=(c(NA, fabricia_sqmeter_mean[1,]) + c(NA, fabricia_sqmeter_SEM[1,])),
       x1=seq(1.5,24.5,4),
       y1=(c(NA, fabricia_sqmeter_mean[1,]) - c(NA, fabricia_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,24.5,4),
       y0=(c(NA, fabricia_sqmeter_mean[2,]) + c(NA, fabricia_sqmeter_SEM[2,])),
       x1=seq(2.5,24.5,4),
       y1=(c(NA, fabricia_sqmeter_mean[2,]) - c(NA, fabricia_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(3.5,24.5,4),
       y0=(c(NA, fabricia_sqmeter_mean[3,]) + c(NA, fabricia_sqmeter_SEM[3,])),
       x1=seq(3.5,24.5,4),
       y1=(c(NA, fabricia_sqmeter_mean[3,]) - c(NA, fabricia_sqmeter_SEM[3,])),
       angle=90, code=3, length=.06)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Fabricia_means))
dev.off()
embedFonts("Fabricia_N_dynamic.pdf") #встройка шрифтов в файл
#locator()
#####

#Pygospio
#####
str(ishodnik_taxons)
(pygospio_sqmeter_mean<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245"], list(ishodnik_community$comm[ishodnik_community$species=="Pygospio elegans"& ishodnik_community$square=="245"], ishodnik_community$year[ishodnik_community$species=="Pygospio elegans"& ishodnik_community$square=="245"]), mean, na.rm=T))

(pygospio_sqmeter_sd<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Pygospio elegans" & ishodnik_community$square=="245"], list(ishodnik_community$comm[ishodnik_community$species=="Pygospio elegans"& ishodnik_community$square=="245"], ishodnik_community$year[ishodnik_community$species=="Pygospio elegans"& ishodnik_community$square=="245"]), sd, na.rm=T))

(n.samples.pygospio<-tapply(samplenames_community$sample[samplenames_community$square=="245"], list(samplenames_community$comm[samplenames_community$square=="245"],samplenames_community$year[samplenames_community$square=="245"]), length))

(pygospio_sqmeter_SEM<-pygospio_sqmeter_sd/sqrt(n.samples.pygospio))

Pygospio_means<-(cbind(c(dominants_1973[2,3],NA, dominants_1973[2,2]), (pygospio_sqmeter_mean)))
colnames(Pygospio_means)<-c("1973", colnames(pygospio_sqmeter_mean))
Pygospio_means


pdf(file="Pygospio_N_dynamic.pdf", family="NimbusSan")
barplot((Pygospio_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 51000))
arrows(x0=seq(1.5,27.5,4),
       y0=(c(NA, pygospio_sqmeter_mean[1,]) + c(NA, pygospio_sqmeter_SEM[1,])),
       x1=seq(1.5,27.5,4),
       y1=(c(NA, pygospio_sqmeter_mean[1,]) - c(NA, pygospio_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,27.5,4),
       y0=(c(NA, pygospio_sqmeter_mean[2,]) + c(NA, pygospio_sqmeter_SEM[2,])),
       x1=seq(2.5,27.5,4),
       y1=(c(NA, pygospio_sqmeter_mean[2,]) - c(NA, pygospio_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(3.5,27.5,4),
       y0=(c(NA, pygospio_sqmeter_mean[3,]) + c(NA, pygospio_sqmeter_SEM[3,])),
       x1=seq(3.5,27.5,4),
       y1=(c(NA, pygospio_sqmeter_mean[3,]) - c(NA, pygospio_sqmeter_SEM[3,])),
       angle=90, code=3, length=.06)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Pygospio_means))
dev.off()
embedFonts("Pygospio_N_dynamic.pdf") #встройка шрифтов в файл
#locator()
#####

#Capitella capitata
#####
str(ishodnik_taxons)

(capitella_sqmeter_mean<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30"], list(ishodnik_community$comm[ishodnik_community$species=="Capitella capitata"& ishodnik_community$square=="245"|ishodnik_community$square=="30"], ishodnik_community$year[ishodnik_community$species=="Capitella capitata"& ishodnik_community$square=="245"|ishodnik_community$square=="30"]), mean, na.rm=T))

(capitella_sqmeter_sd<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Capitella capitata" & ishodnik_community$square=="245" |ishodnik_community$square=="30"], list(ishodnik_community$comm[ishodnik_community$species=="Capitella capitata"& ishodnik_community$square=="245" |ishodnik_community$square=="30"], ishodnik_community$year[ishodnik_community$species=="Capitella capitata"& ishodnik_community$square=="245"|ishodnik_community$square=="30"]), sd, na.rm=T))

(n.samples.capitella<-tapply(samplenames_community$sample[samplenames_community$square=="245"|samplenames_community$square=="30"], list(samplenames_community$comm[samplenames_community$square=="245"|samplenames_community$square=="30"],samplenames_community$year[samplenames_community$square=="245"|samplenames_community$square=="30"]), length))

(capitella_sqmeter_SEM<-capitella_sqmeter_sd/sqrt(n.samples.capitella[1:6]))

Capitella_means<-(cbind(c(dominants_1973[4,3],NA, dominants_1973[4,2]), (capitella_sqmeter_mean)))
colnames(Capitella_means)<-c("1973", colnames(capitella_sqmeter_mean))
Capitella_means

pdf(file="Capitella_N_dynamic.pdf", family="NimbusSan")
barplot((Capitella_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 3100))
arrows(x0=seq(1.5,27.5,4),
       y0=(c(NA, capitella_sqmeter_mean[1,]) + c(NA, capitella_sqmeter_SEM[1,])),
       x1=seq(1.5,27.5,4),
       y1=(c(NA, capitella_sqmeter_mean[1,]) - c(NA, capitella_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,27.5,4),
       y0=(c(NA, capitella_sqmeter_mean[2,]) + c(NA, capitella_sqmeter_SEM[2,])),
       x1=seq(2.5,27.5,4),
       y1=(c(NA, capitella_sqmeter_mean[2,]) - c(NA, capitella_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(3.5,27.5,4),
       y0=(c(NA, capitella_sqmeter_mean[3,]) + c(NA, capitella_sqmeter_SEM[3,])),
       x1=seq(3.5,27.5,4),
       y1=(c(NA, capitella_sqmeter_mean[3,]) - c(NA, capitella_sqmeter_SEM[3,])),
       angle=90, code=3, length=.06)
text(x=3.5,y=2800, labels=c("19000"),srt=90)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Capitella_means))
dev.off()
embedFonts("Capitella_N_dynamic.pdf") #встройка шрифтов в файл
#locator()
#####


#Arenicola
#####

(arenicola_sqmeter_mean<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4"], list(ishodnik_community$comm[ishodnik_community$species=="Arenicola marina"& ishodnik_community$square=="4"], ishodnik_community$year[ishodnik_community$species=="Arenicola marina"& ishodnik_community$square=="4"]), mean, na.rm=T))

(arenicola_sqmeter_sd<-tapply(ishodnik_community$N.sqmeter[ishodnik_community$species=="Arenicola marina" & ishodnik_community$square=="4"], list(ishodnik_community$comm[ishodnik_community$species=="Arenicola marina"& ishodnik_community$square=="4"], ishodnik_community$year[ishodnik_community$species=="Arenicola marina"& ishodnik_community$square=="4"]), sd, na.rm=T))

(n.samples.arenicola<-tapply(samplenames_community$sample[samplenames_community$square=="4"], list(samplenames_community$comm[samplenames_community$square=="4"],samplenames_community$year[samplenames_community$square=="4"]), length))
n.samples.arenicola<-cbind(n.samples.arenicola, c(15,10,15)) 

(arenicola_sqmeter_SEM<-arenicola_sqmeter_sd/sqrt(n.samples.arenicola))

Arenicola_means<-(cbind(c(dominants_1973[5,3],NA, dominants_1973[5,2]), (arenicola_sqmeter_mean)))
colnames(Arenicola_means)<-c("1973", colnames(arenicola_sqmeter_mean))
Arenicola_means

pdf(file="Arenicola_N_dynamic.pdf", family="NimbusSan")
barplot((Arenicola_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 250))
arrows(x0=seq(1.5,23.5,4),
       y0=(c(NA, arenicola_sqmeter_mean[1,]) + c(NA, arenicola_sqmeter_SEM[1,])),
       x1=seq(1.5,23.5,4),
       y1=(c(NA, arenicola_sqmeter_mean[1,]) - c(NA, arenicola_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,23.5,4),
       y0=(c(NA, arenicola_sqmeter_mean[2,]) + c(NA, arenicola_sqmeter_SEM[2,])),
       x1=seq(2.5,23.5,4),
       y1=(c(NA, arenicola_sqmeter_mean[2,]) - c(NA, arenicola_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(3.5,23.5,4),
       y0=(c(NA, arenicola_sqmeter_mean[3,]) + c(NA, arenicola_sqmeter_SEM[3,])),
       x1=seq(3.5,23.5,4),
       y1=(c(NA, arenicola_sqmeter_mean[3,]) - c(NA, arenicola_sqmeter_SEM[3,])),
       angle=90, code=3, length=.06)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Arenicola_means))
dev.off()
embedFonts("Arenicola_N_dynamic.pdf") #встройка шрифтов в файл
#locator()
#####

#Oligochaeta
#####
oligochaeta_ish1<-subset(ishodnik_community,ishodnik_community$taxon=="Oligochaeta")
str(oligochaeta_ish1)

oligochaeta_ish2<-as.data.frame(as.table(tapply(oligochaeta_ish1$N.sqmeter, list(oligochaeta_ish1$sample, oligochaeta_ish1$square, oligochaeta_ish1$station, oligochaeta_ish1$year), sum)))
names(oligochaeta_ish2)<-c("sample", "square", "station", "year", "N.sqmeter")


comm<-recode(oligochaeta_ish2$station, community$station, as.character(community$community))
oligochaeta_ish2<-(cbind(comm=comm, oligochaeta_ish2))

samplenames_community

(n.samples.oligochaeta<-tapply(samplenames_community$sample[samplenames_community$square=="245"], list(samplenames_community$comm[samplenames_community$square=="245"],samplenames_community$year[samplenames_community$square=="245"]), length))

str(oligochaeta_ish2)

(oligochaeta_sqmeter_mean<-tapply(oligochaeta_ish2$N.sqmeter, list( oligochaeta_ish2$comm, oligochaeta_ish2$year), mean, na.rm=T))

(oligochaeta_sqmeter_sd<-tapply(oligochaeta_ish2$N.sqmeter, list( oligochaeta_ish2$comm, oligochaeta_ish2$year), sd, na.rm=T))

oligochaeta_sqmeter_SEM<-oligochaeta_sqmeter_sd/sqrt(n.samples.oligochaeta[,c(1,3:6)])

Oligochaeta_means<-(cbind(c(dominants_1973[3,3],NA, dominants_1973[3,2]), (oligochaeta_sqmeter_mean)))
colnames(Oligochaeta_means)<-c("1973", colnames(oligochaeta_sqmeter_mean))
Oligochaeta_means

pdf(file="Oligochaeta_N_dynamic.pdf", family="NimbusSan")
barplot((Oligochaeta_means),beside=T, main=NULL, sub=NULL, xlab="год", ylab="N, экз./кв.м", ylim=c(0, 125000))
arrows(x0=seq(1.5,23.5,4),
       y0=(c(NA, oligochaeta_sqmeter_mean[1,]) + c(NA, oligochaeta_sqmeter_SEM[1,])),
       x1=seq(1.5,23.5,4),
       y1=(c(NA, oligochaeta_sqmeter_mean[1,]) - c(NA, oligochaeta_sqmeter_SEM[1,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(2.5,23.5,4),
       y0=(c(NA, oligochaeta_sqmeter_mean[2,]) + c(NA, oligochaeta_sqmeter_SEM[2,])),
       x1=seq(2.5,23.5,4),
       y1=(c(NA, oligochaeta_sqmeter_mean[2,]) - c(NA, oligochaeta_sqmeter_SEM[2,])),
       angle=90, code=3, length=.06)
arrows(x0=seq(3.5,23.5,4),
       y0=(c(NA, oligochaeta_sqmeter_mean[3,]) + c(NA, oligochaeta_sqmeter_SEM[3,])),
       x1=seq(3.5,23.5,4),
       y1=(c(NA, oligochaeta_sqmeter_mean[3,]) - c(NA, oligochaeta_sqmeter_SEM[3,])),
       angle=90, code=3, length=.06)
#legend(x=21, y=240000, fill=gray(c(0.3,0.5,0.7)),legend=rownames(Oligochaeta_means))
dev.off()
embedFonts("Oligochaeta_N_dynamic.pdf") #встройка шрифтов в файл
#####


#####
#динамика...
kruskal.test()