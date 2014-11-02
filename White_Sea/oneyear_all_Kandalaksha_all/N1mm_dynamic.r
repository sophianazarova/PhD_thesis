setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/oneyear_all_Kandalaksha_all/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

ishodnik<-read.table("N_all_Kandalaksha.csv", header=T, sep=";", dec=",")
str(ishodnik)
attach(ishodnik)

#сделаем из линейного датафрейма матрицы
#для годовалых
(N.1year.matrix<-tapply(X=N.1year, list(year,area, tidal_zone), max))
str(N.1year.matrix)

# ========= обилие 1+ насколько определяет общее обилие в заданный год? ========
cairo_pdf("N1y_vs_N2.pdf")
plot(ishodnik$N.1year, ishodnik$N.1mm, xlab = "N1+, экз./кв.м.", ylab = "N, экз./кв.м")
dev.off()

cor.test(ishodnik$N.1year, ishodnik$N.1mm, method = "spearman")


mod1<-lm(ishodnik$N.1mm ~ ishodnik$N.1year)
summary(mod1)
plot(mod1$residuals)
abline(h = 0)
hist(mod1$residuals)
shapiro.test(mod1$residuals)

# ========= кросскорреляции одногодки и половозрелые =========================
ishodnik_shift<-read.table("all_Kandalaksha.csv", header=T, sep=";", dec=",")
str(ishodnik_shift)

plot(ishodnik_shift$polovozrelye_before_tau, ishodnik_shift$oneyear_tau)
cor.test(ishodnik_shift$polovozrelye_before_tau, ishodnik_shift$oneyear_tau, method = "spearman")

# ========= PRCF для N1year - смотрим цикличность пополнений ===================

# Функция для расчетов PRCF по Berryman, Turchin, 2001

PRCF <- function (data)
{
  n <- length(data)
  prcf <- data.frame(lag=c(1:round(n/2)))
  #pracf<- data.frame(lag=c(1:round(n/2)))
  Lt <- log(data)
  R <- Lt[2:n]-Lt[1:(n-1)]
  prcf$acf <- pacf(Lt, lag.max=round(n/2), plot=FALSE)$acf
  prcf$acf[1] <- cor(R,Lt[1:(n-1)])
  prcf$type <- "PRCF"
  #pracf$acf <- pacf(R, lag.max=round(n/2), plot=FALSE)$acf
  #pracf$type <- "PRACF"
  #prcf <- rbind(prcf, pracf)
  Rt <- data.frame(Rt=R, Lt_1=Lt[1:(n-1)])
  return(list(prcf, Rt))
}

PRCF2 <- function (data)
{
  n <- length(data)
  prcf <- data.frame(lag=c(1:round(n/2)))
  #pracf<- data.frame(lag=c(1:round(n/2)))
  Lt <- log(data)
  R <- Lt[2:n]-Lt[1:(n-1)]
  prcf$acf <- pacf(Lt, lag.max=round(n/2), plot=FALSE)$acf
  prcf$acf[1] <- cor(R,Lt[1:(n-1)])
  prcf$type <- "PRCF"
  #pracf$acf <- pacf(R, lag.max=round(n/2), plot=FALSE)$acf
  #pracf$type <- "PRACF"
  #prcf <- rbind(prcf, pracf)
  Rt <- data.frame(Rt=R, Lt_1=Lt[1:(n-1)])
  return(prcf$acf)
}

perm_PRCF <- function(data)
{
  prcf <- PRCF(data)[[1]]
  n <- nrow(prcf)
  longiv <- length(data)
  signif_prcf <- data.frame(lag=prcf$lag, prcf=prcf$acf)
  signif_prcf$p <- 0
  for(i in 1:999)
  {
    data <- sample(data, longiv) 
    prcf_perm <- PRCF(data)[[1]]
    for (j in 1:n) if(abs(prcf_perm$acf[j])>=abs(prcf$acf[j])) signif_prcf$p [j] <- signif_prcf$p[j] +1
    cat("Permutation ", i, "\n")                  
  }
  signif_prcf$p <- (signif_prcf$p + 1)/1000
  return(signif_prcf)
  
}

str(N.1year.matrix)
dimnames(N.1year.matrix)[3]

#Эстуарий
(perm_prcf_Estuary_N1y<-perm_PRCF(N.1year[area=="Estuary"]+1))
perm_prcf_Estuary_N1y$signif<-NA
perm_prcf_Estuary_N1y$signif[perm_prcf_Estuary_N1y$p>0.1]<-1
perm_prcf_Estuary_N1y$signif[perm_prcf_Estuary_N1y$p<=0.1 & perm_prcf_Estuary_N1y$p>0.05]<-0.1
perm_prcf_Estuary_N1y$signif[perm_prcf_Estuary_N1y$p<=0.05]<-0.05
perm_prcf_Estuary_N1y$signif<-as.factor(perm_prcf_Estuary_N1y$signif)

cairo_pdf("perm_PRCF_Estuary_N1y.pdf")
ggplot(perm_prcf_Estuary_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Estuary_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_Estuary_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез верхний пляж
(perm_prcf_razrez2_high_beatch_N1y<-perm_PRCF(N.1year[area=="razrez2" & tidal_zone=="high_beatch"]+1))

perm_prcf_razrez2_high_beatch_N1y$signif<-NA
perm_prcf_razrez2_high_beatch_N1y$signif[perm_prcf_razrez2_high_beatch_N1y$p<=0.05]<-0.005
perm_prcf_razrez2_high_beatch_N1y$signif[perm_prcf_razrez2_high_beatch_N1y$p>0.05 & perm_prcf_razrez2_high_beatch_N1y$p<=0.1]<-0.1
perm_prcf_razrez2_high_beatch_N1y$signif[perm_prcf_razrez2_high_beatch_N1y$p>0.1]<-1
perm_prcf_razrez2_high_beatch_N1y$signif<-as.factor(perm_prcf_razrez2_high_beatch_N1y$signif)

cairo_pdf("perm_PRCF_razrez2_high_beatch_N1y.pdf")
ggplot(perm_prcf_razrez2_high_beatch_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_high_beatch_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_high_beatch_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез пояс фукоидов
(perm_prcf_razrez2_fucus_zone_N1y<-perm_PRCF(N.1year[area=="razrez2" & tidal_zone=="fucus_zone"]+1))

perm_prcf_razrez2_fucus_zone_N1y$signif<-NA
perm_prcf_razrez2_fucus_zone_N1y$signif[perm_prcf_razrez2_fucus_zone_N1y$p<=0.05]<-0.005
perm_prcf_razrez2_fucus_zone_N1y$signif[perm_prcf_razrez2_fucus_zone_N1y$p>0.05 & perm_prcf_razrez2_fucus_zone_N1y$p<=0.1]<-0.1
perm_prcf_razrez2_fucus_zone_N1y$signif[perm_prcf_razrez2_fucus_zone_N1y$p>0.1]<-1
perm_prcf_razrez2_fucus_zone_N1y$signif<-as.factor(perm_prcf_razrez2_fucus_zone_N1y$signif)

cairo_pdf("perm_PRCF_razrez2_fucus_zone_N1y.pdf")
ggplot(perm_prcf_razrez2_fucus_zone_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_fucus_zone_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_fucus_zone_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез нижний пляж
(perm_prcf_razrez2_low_beatch_N1y<-perm_PRCF(N.1year[area=="razrez2" & tidal_zone=="low_beatch"]+1))

perm_prcf_razrez2_low_beatch_N1y$signif<-NA
perm_prcf_razrez2_low_beatch_N1y$signif[perm_prcf_razrez2_low_beatch_N1y$p<=0.05]<-0.05
perm_prcf_razrez2_low_beatch_N1y$signif[perm_prcf_razrez2_low_beatch_N1y$p>0.05 & perm_prcf_razrez2_low_beatch_N1y$p<=0.1]<-0.1
perm_prcf_razrez2_low_beatch_N1y$signif[perm_prcf_razrez2_low_beatch_N1y$p>0.1]<-1
perm_prcf_razrez2_low_beatch_N1y$signif<-as.factor(perm_prcf_razrez2_low_beatch_N1y$signif)

cairo_pdf("perm_PRCF_razrez2_low_beatch_N1y.pdf")
ggplot(perm_prcf_razrez2_low_beatch_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_low_beatch_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_low_beatch_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез пояс зостеры
(perm_prcf_razrez2_zostera_zone_N1y<-perm_PRCF(N.1year[area=="razrez2" & tidal_zone=="zostera_zone"]+1))

perm_prcf_razrez2_zostera_zone_N1y$signif<-NA
perm_prcf_razrez2_zostera_zone_N1y$signif[perm_prcf_razrez2_zostera_zone_N1y$p<=0.05]<-0.05
perm_prcf_razrez2_zostera_zone_N1y$signif[perm_prcf_razrez2_zostera_zone_N1y$p>0.05 & perm_prcf_razrez2_zostera_zone_N1y$p<=0.1]<-0.1
perm_prcf_razrez2_zostera_zone_N1y$signif[perm_prcf_razrez2_zostera_zone_N1y$p>0.1]<-1
perm_prcf_razrez2_zostera_zone_N1y$signif<-as.factor(perm_prcf_razrez2_zostera_zone_N1y$signif)

cairo_pdf("perm_PRCF_razrez2_zostera_zone_N1y.pdf")
ggplot(perm_prcf_razrez2_zostera_zone_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_zostera_zone_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_zostera_zone_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# ЮГ
(perm_prcf_YuG_N1y<-perm_PRCF(N.1year[area=="YuG"]+1))

perm_prcf_YuG_N1y$signif<-NA
perm_prcf_YuG_N1y$signif[perm_prcf_YuG_N1y$p>0.1]<-1
perm_prcf_YuG_N1y$signif[perm_prcf_YuG_N1y$p<=0.1 & perm_prcf_YuG_N1y$p>0.05]<-0.1
perm_prcf_YuG_N1y$signif[perm_prcf_YuG_N1y$p<=0.05]<-0.05
perm_prcf_YuG_N1y$signif<-as.factor(perm_prcf_YuG_N1y$signif)

cairo_pdf("perm_PRCF_YuG_N1y.pdf")
ggplot(perm_prcf_YuG_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_YuG_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_YuG_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()


# Ломнишный
(perm_prcf_Lomnishniy_N1y<-perm_PRCF(N.1year[area=="Lomnishniy"]+1))

perm_prcf_Lomnishniy_N1y$signif<-NA
perm_prcf_Lomnishniy_N1y$signif[perm_prcf_Lomnishniy_N1y$p>0.1]<-1
perm_prcf_Lomnishniy_N1y$signif[perm_prcf_Lomnishniy_N1y$p<=0.1 & perm_prcf_Lomnishniy_N1y$p>0.05]<-0.1
perm_prcf_Lomnishniy_N1y$signif[perm_prcf_Lomnishniy_N1y$p<=0.05]<-0.05
perm_prcf_Lomnishniy_N1y$signif<-as.factor(perm_prcf_Lomnishniy_N1y$signif)

cairo_pdf("perm_PRCF_Lomnishniy_N1y.pdf")
ggplot(perm_prcf_Lomnishniy_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Lomnishniy_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_Lomnishniy_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# ЗРС
(perm_prcf_ZRS_N1y<-perm_PRCF(N.1year[area=="ZRS"]+1))

perm_prcf_ZRS_N1y$signif<-NA
perm_prcf_ZRS_N1y$signif[perm_prcf_ZRS_N1y$p>0.1]<-1
perm_prcf_ZRS_N1y$signif[perm_prcf_ZRS_N1y$p<=0.1 & perm_prcf_ZRS_N1y$p>0.05]<-0.1
perm_prcf_ZRS_N1y$signif[perm_prcf_ZRS_N1y$p<=0.05]<-0.05
perm_prcf_ZRS_N1y$signif<-as.factor(perm_prcf_ZRS_N1y$signif)

cairo_pdf("perm_PRCF_ZRS_N1y.pdf")
ggplot(perm_prcf_ZRS_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_ZRS_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_ZRS_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Горелый верх
(perm_prcf_Goreliy_high_N1y<-perm_PRCF(N.1year[area=="Goreliy" & tidal_zone=="high"]+1))

perm_prcf_Goreliy_high_N1y$signif<-NA
perm_prcf_Goreliy_high_N1y$signif[perm_prcf_Goreliy_high_N1y$p>0.1]<-1
perm_prcf_Goreliy_high_N1y$signif[perm_prcf_Goreliy_high_N1y$p<=0.1 & perm_prcf_Goreliy_high_N1y$p>0.05]<-0.1
perm_prcf_Goreliy_high_N1y$signif[perm_prcf_Goreliy_high_N1y$p<=0.05]<-0.05
perm_prcf_Goreliy_high_N1y$signif<-as.factor(perm_prcf_Goreliy_high_N1y$signif)

cairo_pdf("perm_PRCF_Goreliy_high_N1y.pdf")
ggplot(perm_prcf_Goreliy_high_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_high_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_high_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Горелый middle
(perm_prcf_Goreliy_middle_N1y<-perm_PRCF(N.1year[area=="Goreliy" & tidal_zone=="middle"]+1))

perm_prcf_Goreliy_middle_N1y$signif<-NA
perm_prcf_Goreliy_middle_N1y$signif[perm_prcf_Goreliy_middle_N1y$p>0.1]<-1
perm_prcf_Goreliy_middle_N1y$signif[perm_prcf_Goreliy_middle_N1y$p<=0.1 & perm_prcf_Goreliy_middle_N1y$p>0.05]<-0.1
perm_prcf_Goreliy_middle_N1y$signif[perm_prcf_Goreliy_middle_N1y$p<=0.05]<-0.05
perm_prcf_Goreliy_middle_N1y$signif<-as.factor(perm_prcf_Goreliy_middle_N1y$signif)

cairo_pdf("perm_PRCF_Goreliy_middle_N1y.pdf")
ggplot(perm_prcf_Goreliy_middle_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_middle_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_middle_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Горелый midlow
(perm_prcf_Goreliy_midlow_N1y<-perm_PRCF(N.1year[area=="Goreliy" & tidal_zone=="low"]+1))

perm_prcf_Goreliy_midlow_N1y$signif<-NA
perm_prcf_Goreliy_midlow_N1y$signif[perm_prcf_Goreliy_midlow_N1y$p>0.1]<-1
perm_prcf_Goreliy_midlow_N1y$signif[perm_prcf_Goreliy_midlow_N1y$p<=0.1 & perm_prcf_Goreliy_midlow_N1y$p>0.05]<-0.1
perm_prcf_Goreliy_midlow_N1y$signif[perm_prcf_Goreliy_midlow_N1y$p<=0.05]<-0.05
perm_prcf_Goreliy_midlow_N1y$signif<-as.factor(perm_prcf_Goreliy_midlow_N1y$signif)

cairo_pdf("perm_PRCF_Goreliy_midlow_N1y.pdf")
ggplot(perm_prcf_Goreliy_midlow_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_midlow_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_midlow_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Горелый low
(perm_prcf_Goreliy_low_N1y<-perm_PRCF(N.1year[area=="Goreliy" & tidal_zone=="hydrographicum_datum"]+1))

perm_prcf_Goreliy_low_N1y$signif<-NA
perm_prcf_Goreliy_low_N1y$signif[perm_prcf_Goreliy_low_N1y$p>0.1]<-1
perm_prcf_Goreliy_low_N1y$signif[perm_prcf_Goreliy_low_N1y$p<=0.1 & perm_prcf_Goreliy_low_N1y$p>0.05]<-0.1
perm_prcf_Goreliy_low_N1y$signif[perm_prcf_Goreliy_low_N1y$p<=0.05]<-0.05
perm_prcf_Goreliy_low_N1y$signif<-as.factor(perm_prcf_Goreliy_low_N1y$signif)

cairo_pdf("perm_PRCF_Goreliy_low_N1y.pdf")
ggplot(perm_prcf_Goreliy_low_N1y, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_low_N1y$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_low_N1y$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()


# ============= Смотрим синхронность пополнений ==========================
#####

library(vegan)
str(ishodnik)
attach(ishodnik)
rm(year)

#сделаем дата фрейм без разделения на горизонты
(N.1year.df<-as.data.frame(tapply(X=N.1year, list(year,area), sum)))
N.1year.df$years<-rownames(N.1year.df)

#делаем пустые матрицы для результатов
(N.1year.mantel.signif<-matrix(nrow=(ncol(N.1year.df)-1), ncol=ncol(N.1year.df)-1, dimnames=list(colnames(N.1year.df[1:(ncol(N.1year.df)-1)]),colnames(N.1year.df[1:(ncol(N.1year.df)-1)]))))
(N.1year.mantel.statistic<-matrix(nrow=(ncol(N.1year.df)-1), ncol=ncol(N.1year.df)-1, dimnames=list(colnames(N.1year.df[1:(ncol(N.1year.df)-1)]),colnames(N.1year.df[1:(ncol(N.1year.df)-1)]))))
#для каждой пары участков i и j
for (i in 1:(ncol(N.1year.df)-1)) {
  for (j in 1:(ncol(N.1year.df)-1)) {
    # делаем модельную матрицу по годам
    year.period<-N.1year.df$years[!is.na(N.1year.df[,i]) &  !is.na(N.1year.df[,j])]
    if (length(year.period)==0) j<-j+1
    year.period<-N.1year.df$years[!is.na(N.1year.df[,i]) &  !is.na(N.1year.df[,j])]
    if (length(year.period)==0) j<-j+1
    year.m<-vegdist(year.period, method="euclidean")
    # делаем матрицу по численностям
    area1.m<-vegdist(N.1year.df[,i][N.1year.df$years%in%N.1year.df$years[!is.na(N.1year.df[,i])] &  !is.na(N.1year.df[,j])], method="euclidean")
    area2.m<-vegdist(N.1year.df[,j][N.1year.df$years%in%N.1year.df$years[!is.na(N.1year.df[,i])] &  !is.na(N.1year.df[,j])], method="euclidean")
    #считаем частные корреляции мантеля с учетом детрендинга
    cormant<-mantel.partial(area1.m, area2.m, year.m, permutations=999)
    N.1year.mantel.signif[i, j]<-cormant$signif
    N.1year.mantel.statistic[i, j]<-round(cormant$statistic, digits=3)
  }
}
write.table(N.1year.mantel.statistic, file="N_1year_mantel_statistic.csv", sep=";", dec=",")
write.table(N.1year.mantel.signif, file="N_1_year_mantel_signif.csv", sep=";", dec=",")


# ============== зависимость от расстояний ================================
#Считаем сходство с матрицей расстояний
#####

#читаем матрицу расстояний
distance_N1_km<-read.table("coordinates_N_1year.csv", sep=";", dec=",", header =T)
rownames(distance_N1_km)<-distance_N1_km[,1]


# считаем мантеля между матрицей расстояний и корреляциями динамики
mantel(xdis=distance_N1_km, N1.mantel.statistic, na.rm=T)
