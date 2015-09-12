setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/Growth_sravnenie")

detach(ishodnik)

ishodnik<-read.table(file="growth_svodka.csv", sep=";", dec=",", head=T)
str(ishodnik)
ncol(ishodnik)
     
# ========== вводим модель роста Берталанфи =======================
# задаем модель в виде nls(y~f(x)), и задаем стартовые значения коэффициентов из формулы, от которых он методом последовательного приближения (как я понимаю) будет подбирать максимально похожие)
Bertallanfi <- function(L, Age=seq(1,25,1)){
  nls(L~Lmax*(1-exp(-k*Age)), start=list(Lmax=20,k=0.1))
}

Bertallanfi(ishodnik$Rand_Harbor)
Bertallanfi(ishodnik$Wadden_Sea)
Bertallanfi(ishodnik$Hudsons_Bay)
Bertallanfi(ishodnik$MLW)
Bertallanfi(ishodnik$Finland_Gulf_6m)
Bertallanfi(ishodnik$Finland_Gulf_20m) #
Bertallanfi(ishodnik$Finland_Gulf_35m) #
Bertallanfi(ishodnik$St_Laurence_River)
Bertallanfi(ishodnik$Cobscook_Bay)
Bertallanfi(ishodnik$Windsor)
Bertallanfi(ishodnik$Mungo_brook)
Bertallanfi(ishodnik$Pekkelinskaya_Bay)
Bertallanfi(ishodnik$WSBS_MSU)
Bertallanfi(ishodnik$Ermolinskaya_Bay)
Bertallanfi(ishodnik$Sukhaya_mid)
Bertallanfi(ishodnik$Sukhaya_low) #
Bertallanfi(ishodnik$Sukhaya_low_zostera)
Bertallanfi(ishodnik$Klyushiha_mid) #
Bertallanfi(ishodnik$Klyushiha_low)
Bertallanfi(ishodnik$Klyushiha_low_zostera)              
Bertallanfi(ishodnik$St_Laurence_high)
Bertallanfi(ishodnik$St_Laurence_mid)
Bertallanfi(ishodnik$St_Laurence_low)
Bertallanfi(ishodnik$IPB)
Bertallanfi(ishodnik$GN)
Bertallanfi(ishodnik$H75)
Bertallanfi(ishodnik$Tvaren_Bay)
Bertallanfi(ishodnik$North_sea)
Bertallanfi(ishodnik$Kiel_area)#
Bertallanfi(ishodnik$Tvarminne_area)
Bertallanfi(ishodnik$Le_Verdon_high)
Bertallanfi(ishodnik$Le_Verdon_mid)
Bertallanfi(ishodnik$Phare_de_Richard)
Bertallanfi(ishodnik$Abram_mid) #
Bertallanfi(ishodnik$Abram_low)
Bertallanfi(ishodnik$Dalnezeleneckaya_high)
Bertallanfi(ishodnik$Dalnezeleneckaya_mid)
Bertallanfi(ishodnik$Gavrilovo_mid)
Bertallanfi(ishodnik$Gavrilovo_low)
Bertallanfi(ishodnik$Shelpino_high)
Bertallanfi(ishodnik$Shelpino_mid) #
Bertallanfi(ishodnik$Yarnyshnaya_high)#
Bertallanfi(ishodnik$Yarnyshnaya_mid)
Bertallanfi(ishodnik$Yarnyshnaya_low)
Bertallanfi(ishodnik$Pala_mid)#
Bertallanfi(ishodnik$Pala_low) #
Bertallanfi(ishodnik$Porchnikha_mid)
Bertallanfi(ishodnik$Ivanovskaya)#
Bertallanfi(ishodnik$Ythan_estuary)
Bertallanfi(ishodnik$Budle_Bay)
Bertallanfi(ishodnik$Firth_of_Forth)#
Bertallanfi(ishodnik$river_Clwyd)
Bertallanfi(ishodnik$Lynher_estuary)
Bertallanfi(ishodnik$Wash)

# ====== строим картинку по результатам кластеров ==========
# собираем точки в кластеры
attach(ishodnik)
cluster1<-ishodnik$Rand_Harbor
cluster2<-data.frame(Hudsons_Bay,H75,Le_Verdon_mid,Le_Verdon_high,Phare_de_Richard,Tvarminne_area,North_sea,Cobscook_Bay,St_Laurence_low,Finland_Gulf_6m,Klyushiha_low_zostera)
cluster3<-data.frame(Sukhaya_mid,Yarnyshnaya_mid,Sukhaya_low,Sukhaya_low_zostera)
cluster4<-data.frame(Klyushiha_low,Klyushiha_mid,St_Laurence_mid,Dalnezeleneckaya_mid,St_Laurence_high)
cluster5<-data.frame(Ermolinskaya_Bay)
cluster6<-data.frame(St_Laurence_River,Windsor)
cluster7<-data.frame(Tvaren_Bay,Shelpino_mid,Gavrilovo_mid,Shelpino_high)
cluster8<-data.frame(Dalnezeleneckaya_high,Gavrilovo_low)
cluster9<-data.frame(Abram_low,Yarnyshnaya_low)

#делаем матрицы
cluster2m<-as.matrix(cluster2)
cluster3m<-as.matrix(cluster3)
cluster4m<-as.matrix(cluster4)
cluster6m<-as.matrix(cluster6)
cluster7m<-as.matrix(cluster7)
cluster8m<-as.matrix(cluster8)
cluster9m<-as.matrix(cluster9)

# считаем средние по кластерам
cl2_mean<-apply(cluster2m,1,mean,na.rm=T)
cl3_mean<-apply(cluster3m,1,mean,na.rm=T)
cl4_mean<-apply(cluster4m,1,mean,na.rm=T)
cl6_mean<-apply(cluster6m,1,mean,na.rm=T)
cl7_mean<-apply(cluster7m,1,mean,na.rm=T)
cl8_mean<-apply(cluster8m,1,mean,na.rm=T)
cl9_mean<-apply(cluster9m,1,mean,na.rm=T)

#собираем в табличку
clusters_mean<-data.frame(cluster1,cl2_mean,cl3_mean,cl4_mean,cluster5,cl6_mean,cl7_mean,cl8_mean,cl9_mean)

#рисуем график
axe_X<-seq(0,30,5)
axe_Y<-seq(1,30,30/7)
pdf(file="growth_groups.pdf", family="NimbusSan")
plot(x=axe_X,y=axe_Y,type="n", xlab = "возраст, годы", ylab="L, мм")
points(cluster1,pch=1, type = "b")
points(cl2_mean,pch=2, type = "b")
points(cl3_mean,pch=3, type = "b")
points(cl4_mean,pch=4, type = "b")
points(cluster5,pch=5, type = "b")
points(cl6_mean,pch=6, type = "b")
points(cl7_mean,pch=7, type = "b")
points(cl8_mean,pch=8, type = "b")
points(cl9_mean,pch=9, type = "b")
#lines(predict(Bertallanfi(cluster1)))
#lines(predict(Bertallanfi(cl2_mean)))
#lines(predict(Bertallanfi(cl3_mean)))
#lines(predict(Bertallanfi(cl4_mean)))
#lines(predict(Bertallanfi(cluster5)))
#lines(predict(Bertallanfi(cl6_mean)))
#lines(predict(Bertallanfi(cl7_mean)))
#lines(predict(Bertallanfi(cl8_mean)))
#lines(predict(Bertallanfi(cl9_mean)))
dev.off()
embedFonts("growth_groups.pdf")

# ============ Строим картинку широта vs omega = Lmax*k (Beukema, Meehan, 1985) =====
ish_sites<-read.table(file="sample_sites_svodka.csv", sep=";", dec=",", head=T)
str(ish_sites)

pdf(file="long_vs_omega.pdf", family="NimbusSan")
plot(x = ish_sites$lat.[ ish_sites$continent!="Amerika"], y = ish_sites$omega[ ish_sites$continent!="Amerika"], col = c(as.numeric(ish_sites$author[ ish_sites$continent!="Amerika", drop=T]=="author data"))+1, pch=as.numeric(ish_sites$region[ ish_sites$continent!="Amerika", drop=T]), xlab="NL", ylab="omega")
legend(x = "bottomleft", legend = levels(ish_sites$region[ ish_sites$continent!="Amerika", drop=T]), pch=seq(1:8), bty = "n")
dev.off()
embedFonts("long_vs_omega.pdf")


pdf(file="long_vs_omega_big.pdf", family="NimbusSan")
plot(x = ish_sites$lat.[ ish_sites$continent!="Amerika"], y = ish_sites$omega[ ish_sites$continent!="Amerika"], col = c(as.numeric(ish_sites$author[ ish_sites$continent!="Amerika", drop=T]=="author data"))+1, pch=as.numeric(ish_sites$region[ ish_sites$continent!="Amerika", drop=T]), xlab="NL", ylab="omega", cex = 2)
legend(x = "bottomleft", legend = levels(ish_sites$region[ ish_sites$continent!="Amerika", drop=T]), pch=seq(1:8))
dev.off()
embedFonts("long_vs_omega_big.pdf")

# ========== корреляция omega и широты =======================
cor.test(ish_sites$lat., ish_sites$omega, method = "spearman")

# =============== только Европа кластеры ================================
# читаем файл с разбивкой на кластеры
Europe_clusters <- read.csv2("Europe_clusters.csv")
# собираем точки в кластеры
str(ishodnik)
nrow(Europe_clusters)

for (i in c(1,3,4,5)){
  assign(paste("Eclust", i, sep = ""), rowMeans(ishodnik[,c(F, Europe_clusters$X == i)], na.rm=T))  
}
Eclust2 <- ishodnik[,c(F, Europe_clusters$X == 2)]
Eclust6 <- ishodnik[,c(F, Europe_clusters$X == 6)]

#рисуем график
axe_X<-seq(1,6,1)
axe_Y<-seq(1,25,25/6)
pdf(file="Europe_growth_groups.pdf", family="NimbusSan")
plot(x=axe_X,y=axe_Y,type="n", xlab = "возраст, годы", ylab="L, мм")
points(Eclust1[1:6],pch=1, type = "b")
points(Eclust2[1:6],pch=2, type = "b")
points(Eclust3[1:6],pch=3, type = "b")
points(Eclust4[1:6],pch=4, type = "b")
points(Eclust5[1:6],pch=5, type = "b")
points(Eclust6[1:6],pch=6, type = "b")
#lines(predict(Bertallanfi(Eclust1[1:6])))
#lines(predict(Bertallanfi(Eclust2[1:6])))
#lines(predict(Bertallanfi(Eclust3[1:6])))
#lines(predict(Bertallanfi(Eclust4[1:6])))
#lines(predict(Bertallanfi(Eclust5[1:6])))
#lines(predict(Bertallanfi(Eclust6[1:6])))
legend(x="topleft", legend = seq(1:6), pch=seq(1:6), bty = "n", title = "группы")
dev.off()
embedFonts("Europe_growth_groups.pdf")
