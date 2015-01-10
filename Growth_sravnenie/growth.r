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


# ============ Строим картинку широта vs omega = Lmax*k (Beukema, Meehan, 1985) =====
ish_sites<-read.table(file="sample_sites_svodka.csv", sep=";", dec=",", head=T)
str(ish_sites)

pdf(file="long_vs_omega.pdf", family="NimbusSan")
plot(x = ish_sites$lat.[ ish_sites$continent!="Amerika"], y = ish_sites$omega[ ish_sites$continent!="Amerika"], col = c(as.numeric(ish_sites$author[ ish_sites$continent!="Amerika", drop=T]=="author data"))+1, pch=as.numeric(ish_sites$region[ ish_sites$continent!="Amerika", drop=T]), xlab="NL", ylab="omega")
legend(x = "bottomleft", legend = levels(ish_sites$region[ ish_sites$continent!="Amerika", drop=T]), pch=seq(1:8))
dev.off()
embedFonts("long_vs_omega.pdf")


pdf(file="long_vs_omega_big.pdf", family="NimbusSan")
plot(x = ish_sites$lat.[ ish_sites$continent!="Amerika"], y = ish_sites$omega[ ish_sites$continent!="Amerika"], col = c(as.numeric(ish_sites$author[ ish_sites$continent!="Amerika", drop=T]=="author data"))+1, pch=as.numeric(ish_sites$region[ ish_sites$continent!="Amerika", drop=T]), xlab="NL", ylab="omega", cex = 2)
legend(x = "bottomleft", legend = levels(ish_sites$region[ ish_sites$continent!="Amerika", drop=T]), pch=seq(1:8))
dev.off()
embedFonts("long_vs_omega_big.pdf")

# ========== корреляция omega и широты =======================
cor.test(ish_sites$lat., ish_sites$omega, method = "spearman")
