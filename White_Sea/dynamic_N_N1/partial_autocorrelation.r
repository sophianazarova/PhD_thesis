setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/dynamic_N_N1")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="All_Kandalaksha_N2mean.csv", sep=";", dec=",", head=T)

str(ishodnik)

#из Хайтова
# ЧАСТЬ 4. Анализ PACF и PRCF. 
#-------------------------------------------

# Detrending of time series и анализ PACF
#install.packages("ggplot2")
#install.packages("boot")
library(ggplot2)
library(boot)

#проверяем на наличие тренда - есть в Эстуарии, 2 разрез фукусы, 2 разрез нижний пляж, 2 разрез зостера, ЮГ, 
#####
sink("trends_avova.txt")
for (i in 2:ncol(ishodnik)){
  print(colnames(ishodnik)[i])
  print(anova(lm(ishodnik[,i] ~ ishodnik[,1])))
}
sink()
#  Хайтов считает, что дисперсионку плохо, потому что у нас не выполняются условия для линейных моделей... 

#kraskel-wallis - ничего не дает, слишком слабый
sink("trends_kruskal.txt")
for (i in 2:ncol(ishodnik)){
  print(colnames(ishodnik)[i])
  print(kruskal.test(ishodnik[,i] ~ ishodnik[,1]))
}
sink()
#####

# Проводим анализ с помощью Мантеловской коррелограммы
#####
#install.packages("vegan")
library(vegan)

# Формируем матрицу расстояний между годами Внимание! эта матрица является также и модельной матрицей для направленного тренда
(dist_year <- vegdist(ishodnik[,1], method="euclidean"))

# считаем эвклидово расстояне между точками и считаем корреляции мантеля между модельной матрицей для тренда 
# (рассчитано по годам - равномерное увеличение) и матрицей для каждого участка.
# и собираем сразу в табличку.

trend_mantel<-data.frame(area=rep(NA, (ncol(ishodnik)-1)), mantel=rep(NA, (ncol(ishodnik)-1)), p=rep(NA, (ncol(ishodnik)-1)))  
for (j in 2:ncol(ishodnik)){
  trend_mantel$area[j-1] <- colnames(ishodnik)[j]
  mt<-mantel(xdis=vegdist(as.vector(ishodnik[,1][!is.na(ishodnik[,j])]), method="euclidean"),
               ydis=vegdist(as.vector(na.omit(ishodnik[,j])), method="euclidean"))
  trend_mantel$mantel[j-1]<-round(mt$statistic, digits=4)
  trend_mantel$p[j-1]<-mt$signif
}
trend_mantel
# запишем в табличку. 
# тренд есть в Эстуарии, 2 разрез фукусы, 2 разрез зостера, ЮГ, разре2_весь, Сельдяная
write.table(trend_mantel, file="trend_mantel.csv", sep=";",dec=",")




#детрендирование (2, 8, 10, 11, 12)
head(ishodnik)
for(i in c(2,8,10,11,12)){
  assign(paste(colnames(ishodnik)[i], "detr", sep="_"),(lm(ishodnik[,i] ~ ishodnik[,1])$residuals + mean(ishodnik[,i], na.rm=T)))
}

#детредним все - для единообразности

for(i in 2:ncol(ishodnik)){
  assign(paste(colnames(ishodnik)[i], "detr", sep="_"),(lm(ishodnik[,i] ~ ishodnik[,1])$residuals + mean(ishodnik[,i], na.rm=T)))
}

#detrend_mac <- lm(N ~ as.numeric(year), data=mac)$residuals + mean(mac$N)
#mac$detrendN <- detrend_mac 

#pl <- ggplot(mac, aes(x=year, y=detrendN))
#pl+geom_line()

nrow(ishodnik)

#pacf_detrend <- pacf(detrend_mac, lag.max=10)

#####


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

## рассчеты с PRCF
#####
# Анализы без детрендинга

#pacf(mac$N, lag.max=10)
# Считаем prcf для всех учатсков

for (i in 2:ncol(ishodnik)) { 
 assign(paste("prcf", colnames(ishodnik)[i], sep="_"), PRCF(as.vector(na.omit(ishodnik[,i])))[[1]]) 
}

#вписываем в каждый датафрейм что это за участок
# как это автоматизировать не придумала :(
prcf_Estuary$area<-"Estuary"
prcf_Goreliy_high$area<-"Goreliy_high"
prcf_Goreliy_middle$area<-"Goreliy_middle"
prcf_Goreliy_midlow$area<-"Goreliy_midlow"
prcf_Goreliy_low$area<-"Goreliy_low"
prcf_Lomnishniy$area<-"Lomnishniy"
prcf_razrez2_fucus_zone$area<-"razrez2_fucus_zone"
prcf_razrez2_high_beatch$area<-"razrez2_high_beatch"
prcf_razrez2_zostera_zone$area<-"razrez2_zostera_zone"
prcf_razrez2_low_beatch$area<-"razrez2_low_beatch"
prcf_YuG$area<-"YuG"
prcf_ZRS$area<-"ZRS"

# детреднированные данные
# Эстуарии, 2 разрез фукусы, 2 разрез нижний пляж, 2 разрез зостера, ЮГ, 
#Эстуарий
prcf_Estuary_detrend<-PRCF(Estuary_detr)[[1]]

cairo_pdf("PRCF_Estuary_detrend.pdf")
ggplot(prcf_Estuary_detrend, aes(x=as.factor(lag), y=acf, fill=type)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  scale_fill_manual(values=c("PRCF" = "grey")) + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(prcf_Estuary_detrend$acf)),
                          2/sqrt(length(prcf_Estuary_detrend$acf))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

  dev.off()

#2 разрез пояс фукоидов
prcf_razrez2_fucus_zone_detrend<-PRCF(razrez2_fucus_zone_detr)[[1]]

cairo_pdf("PRCF_razrez2_fucus_zone_detrend.pdf")
ggplot(prcf_razrez2_fucus_zone_detrend, aes(x=as.factor(lag), y=acf, fill=type)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  scale_fill_manual(values=c("PRCF" = "grey")) + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(prcf_razrez2_fucus_zone_detrend$acf)),
                          2/sqrt(length(prcf_razrez2_fucus_zone_detrend$acf))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

#2 разрез нижний пляж
prcf_razrez2_low_beatch_detrend<-PRCF(razrez2_low_beatch_detr)[[1]]

cairo_pdf("PRCF_razrez2_low_beatch_detrend.pdf")
ggplot(prcf_razrez2_low_beatch_detrend, aes(x=as.factor(lag), y=acf, fill=type)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  scale_fill_manual(values=c("PRCF" = "grey")) + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(prcf_razrez2_low_beatch_detrend$acf)),
                          2/sqrt(length(prcf_razrez2_low_beatch_detrend$acf))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

#2 разрез пояс зостеры
prcf_razrez2_zostera_zone_detrend<-PRCF(razrez2_zostera_zone_detr)[[1]]

cairo_pdf("PRCF_razrez2_zostera_zone_detrend.pdf")
ggplot(prcf_razrez2_zostera_zone_detrend, aes(x=as.factor(lag), y=acf, fill=type)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  scale_fill_manual(values=c("PRCF" = "grey")) + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(prcf_razrez2_zostera_zone_detrend$acf)),
                          2/sqrt(length(prcf_razrez2_zostera_zone_detrend$acf))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

# ЮГ
prcf_YuG_detrend<-PRCF(YuG_detr)[[1]]

cairo_pdf("PRCF_YuG_detrend.pdf")
ggplot(prcf_YuG_detrend, aes(x=as.factor(lag), y=acf, fill=type)) + 
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  scale_fill_manual(values=c("PRCF" = "grey")) + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(prcf_YuG_detrend$acf)),
                          2/sqrt(length(prcf_YuG_detrend$acf))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

# продолжаем с данными без детрендирования
#склеиваем все в один датафрейм
prcf_all <- rbind(prcf_Estuary, 
                  prcf_Goreliy_high, 
                  prcf_Goreliy_middle,
                  prcf_Goreliy_midlow,
                  prcf_Goreliy_low,
                  prcf_Lomnishniy,
                  prcf_razrez2_fucus_zone,
                  prcf_razrez2_high_beatch,
                  prcf_razrez2_zostera_zone,
                  prcf_razrez2_low_beatch,
                  prcf_YuG,
                  prcf_ZRS)
prcf_all$lag<-as.factor(prcf_all$lag)
prcf_all$type<-as.factor(prcf_all$type)
prcf_all$area<-as.factor(prcf_all$area)

# Рисуем гистограммы для PRCF все на одном графике - и получаем отстой 
pl_PRCF <- ggplot(prcf_all, aes(x=as.factor(lag), y=acf, fill=type))

pl_PRCF + geom_bar(colour="black", stat="identity", position="dodge")  + scale_fill_manual(values=c("PRCF" = "grey")) + geom_hline(yintercept=0) + geom_hline(yintercept=c(-2/sqrt(length(prcf_all$acf)),2/sqrt(length(prcf_all$acf))), linetype=2) + xlab("Time lag") + ylab("Partial autocorrelations") + theme_bw()


# рисуем отдельно
#for (i in 2:ncol(ishodnik)) { 
 # pl_PRCF <- ggplot(subset(prcf_all, prcf_all$area==colnames(ishodnik)[i]), aes(x=as.factor(lag), y=acf, fill=type))
  #assign("prcf_plot", ggplot(subset(prcf_all, prcf_all$area==colnames(ishodnik)[2]), aes(x=as.factor(lag), y=acf, fill=type)))
  
# я не понимаю, почему не генерится файл!!  прогнал руками, но зело извращение
# ggplot не замыкается в цикл
  for (i in 2:ncol(ishodnik)) { 
    cairo_pdf(paste("PRCF", colnames(ishodnik)[i], ".pdf", sep="_"))
    ggplot(subset(prcf_all, prcf_all$area==colnames(ishodnik)[i]), aes(x=as.factor(lag), y=acf, fill=type)) +
    geom_bar(colour="black", stat="identity", position="dodge")  + 
    scale_fill_manual(values=c("PRCF" = "grey")) + 
    geom_hline(yintercept=0) + 
    geom_hline(yintercept=c(-2/sqrt(length(prcf_all$acf[prcf_all$area==colnames(ishodnik)[i]])),
                            2/sqrt(length(prcf_all$acf[prcf_all$area==colnames(ishodnik)[i]]))), linetype=2) + 
    xlab("Time lag") + 
    ylab("Partial autocorrelations") + 
    theme_bw()
  dev.off()
}

#ну или картинка без ggplot2
for (i in 2:ncol(ishodnik)) {
  pdf(file=paste("prcf_plot", colnames(ishodnik)[i], ".pdf",sep="_"))
  barplot(prcf_all$acf[prcf_all$area==colnames(ishodnik)[i]],names.arg=as.numeric(prcf_all$lag[prcf_all$area==colnames(ishodnik)[i]]))
  abline(h=-2/sqrt(length(prcf_all$acf[prcf_all$area==colnames(ishodnik)[i]])), lty="dashed")
  abline(h=2/sqrt(length(prcf_all$acf[prcf_all$area==colnames(ishodnik)[i]])), lty="dashed")
  dev.off()
  embedFonts(file=paste("prcf_plot", colnames(ishodnik)[i], ".pdf",sep="_"))
}
#####

## ВМ: играю с бутстрепом для оценки ошибок в PRCF
#####
boot_PRCF <- function (data, n=99, dl=3, confid=0.95)
{
  set.seed(1234)
  data<-as.ts(data)
  longiv <- length(data)
  boot_res <- tsboot(data, PRCF2, n, sim="fixed", l=round(longiv/dl))
  result <- data.frame(lag=1:length(as.numeric(boot_res$t0)), PRCF=as.numeric(boot_res$t0))
  for (i in 1:round(longiv/dl)) result$se[i] <- sd(boot_res$t[,i])
  for(i in 1:length(as.numeric(boot_res$t0)))
  {
    result$low.ci[i] <- boot.ci(boot_res, type="basic", conf=confid, index=i)$basic[4]
    result$upper.ci[i] <- boot.ci(boot_res, type="basic",conf=confid, index=i)$basic[5]
  }   
  return(result)
}


# Считаем boot-prcf для всех учатсков

for (i in 2:ncol(ishodnik)) { 
  assign(paste("boot_prcf", colnames(ishodnik)[i], sep="_"), boot_PRCF(as.vector(na.omit(ishodnik[,i])), n=999, confid=0.95))
}


#вписываем в каждый датафрейм что это за участок
# как это автоматизировать не придумала :(
boot_prcf_Estuary$area<-"Estuary"
boot_prcf_Goreliy_high$area<-"Goreliy_high"
boot_prcf_Goreliy_middle$area<-"Goreliy_middle"
boot_prcf_Goreliy_midlow$area<-"Goreliy_midlow"
boot_prcf_Goreliy_low$area<-"Goreliy_low"
boot_prcf_Lomnishniy$area<-"Lomnishniy"
boot_prcf_razrez2_fucus_zone$area<-"razrez2_fucus_zone"
boot_prcf_razrez2_high_beatch$area<-"razrez2_high_beatch"
boot_prcf_razrez2_zostera_zone$area<-"razrez2_zostera_zone"
boot_prcf_razrez2_low_beatch$area<-"razrez2_low_beatch"
boot_prcf_YuG$area<-"YuG"
boot_prcf_ZRS$area<-"ZRS"


#склеиваем все в один датафрейм
boot_prcf_all <- rbind(boot_prcf_Estuary, 
                  boot_prcf_Goreliy_high, 
                  boot_prcf_Goreliy_middle,
                  boot_prcf_Goreliy_midlow,
                  boot_prcf_Goreliy_low,
                  boot_prcf_Lomnishniy,
                  boot_prcf_razrez2_fucus_zone,
                  boot_prcf_razrez2_high_beatch,
                  boot_prcf_razrez2_zostera_zone,
                  boot_prcf_razrez2_low_beatch,
                  boot_prcf_YuG,
                  boot_prcf_ZRS)
boot_prcf_all$lag<-as.factor(boot_prcf_all$lag)
boot_prcf_all$area<-as.factor(boot_prcf_all$area)

str(boot_prcf_all)

# #Рисуем диаграммы PRCF с бутстреповыми доверительными интервалами все на одном графике - и получаем отстой 
pl_PRCF_ci <- ggplot(boot_prcf_all, aes(x=lag, y=PRCF))

pl_PRCF_ci + 
  geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge") + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(boot_prcf_all$PRCF[boot_prcf_all$area==colnames(ishodnik)[i]])),
                          2/sqrt(length(boot_prcf_all$PRCF[boot_prcf_all$area==colnames(ishodnik)[i]]))), linetype=2) + 
  geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
# рисуем отдельно

# я не понимаю, почему не генерится файл!!  прогнал руками, но зело извращение

for (i in 2:13){
cairo_pdf(paste("boot_PRCF", colnames(ishodnik)[i], ".pdf", sep="_"))
  ggplot(subset(boot_prcf_all, boot_prcf_all$area==colnames(ishodnik)[i]), aes(x=lag, y=PRCF)) + 
  geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(boot_prcf_all$PRCF[boot_prcf_all$area==colnames(ishodnik)[i]])),
                           2/sqrt(length(boot_prcf_all$PRCF[boot_prcf_all$area==colnames(ishodnik)[i]]))), linetype=2) + 
  geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()
}

# детреднированные данные
# Эстуарии, 2 разрез фукусы, 2 разрез нижний пляж, 2 разрез зостера, ЮГ, 
#Эстуарий
(boot_prcf_Estuary_detrend<-boot_PRCF(Estuary_detr, n=999, confid=0.95))

plot(boot_prcf_Estuary_detrend$lag, boot_prcf_Estuary_detrend$PRCF, type="b", pch=15, 
     ylim=c(-1,1))
arrows(x0=boot_prcf_Estuary_detrend$lag, x1=boot_prcf_Estuary_detrend$lag,
       y0=boot_prcf_Estuary_detrend$low.ci, y1=boot_prcf_Estuary_detrend$upper.ci, angle=90, code=3, length=0.1)

cairo_pdf("boot_PRCF_Estuary_detrend.pdf")
  ggplot(boot_prcf_Estuary_detrend, aes(x=lag, y=PRCF)) + 
  geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(boot_prcf_Estuary_detrend$PRCF)),
                          2/sqrt(length(boot_prcf_Estuary_detrend$PRCF))), linetype=2) + 
  geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

#2 разрез пояс фукоидов
boot_PRCF_razrez2_fucus_zone_detrend<-boot_PRCF(razrez2_fucus_zone_detr, n=999, confid=0.95)

cairo_pdf("boot_PRCF_razrez2_fucus_zone_detrend.pdf")
ggplot(boot_PRCF_razrez2_fucus_zone_detrend, aes(x=lag, y=PRCF)) + 
  geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(boot_PRCF_razrez2_fucus_zone_detrend$PRCF)),
                          2/sqrt(length(boot_PRCF_razrez2_fucus_zone_detrend$PRCF))), linetype=2) + 
  geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

#2 разрез нижний пляж
boot_PRCF_razrez2_low_beatch_detrend<-boot_PRCF(razrez2_low_beatch_detr, n=999, confid=0.95)

cairo_pdf("boot_PRCF_razrez2_low_beatch_detrend.pdf")
ggplot(boot_PRCF_razrez2_low_beatch_detrend, aes(x=lag, y=PRCF)) + 
  geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(boot_PRCF_razrez2_low_beatch_detrend$PRCF)),
                          2/sqrt(length(boot_PRCF_razrez2_low_beatch_detrend$PRCF))), linetype=2) + 
  geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

#2 разрез пояс зостеры
boot_PRCF_razrez2_zostera_zone_detrend<-boot_PRCF(razrez2_zostera_zone_detr, n=999, confid=0.95)

cairo_pdf("boot_PRCF_razrez2_zostera_zone_detrend.pdf")
ggplot(boot_PRCF_razrez2_zostera_zone_detrend, aes(x=lag, y=PRCF)) + 
  geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(boot_PRCF_razrez2_zostera_zone_detrend$PRCF)),
                          2/sqrt(length(boot_PRCF_razrez2_zostera_zone_detrend$PRCF))), linetype=2) + 
  geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

# ЮГ
boot_PRCF_YuG_detrend<-boot_PRCF(YuG_detr, n=999, confid=0.95)

cairo_pdf("boot_PRCF_YuG_detrend.pdf")
ggplot(boot_PRCF_YuG_detrend, aes(x=lag, y=PRCF)) + 
  geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(boot_PRCF_YuG_detrend$PRCF)),
                          2/sqrt(length(boot_PRCF_YuG_detrend$PRCF))), linetype=2) + 
  geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()

dev.off()

#####

## ВМ: Играю с возможностью пермутационной оценки достоверности PRCF
#####
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

# Анализы без детрендинга

# Считаем perm-prcf для всех учатсков

for (i in 2:ncol(ishodnik)) { 
  assign(paste("prcf_perm", colnames(ishodnik)[i], sep="_"), perm_PRCF(as.vector(na.omit(ishodnik[,i])))) 
}


#вписываем в каждый датафрейм что это за участок
# как это автоматизировать не придумала :(
prcf_perm_Estuary$area<-"Estuary"
prcf_perm_Goreliy_high$area<-"Goreliy_high"
prcf_perm_Goreliy_middle$area<-"Goreliy_middle"
prcf_perm_Goreliy_midlow$area<-"Goreliy_midlow"
prcf_perm_Goreliy_low$area<-"Goreliy_low"
prcf_perm_Lomnishniy$area<-"Lomnishniy"
prcf_perm_razrez2_fucus_zone$area<-"razrez2_fucus_zone"
prcf_perm_razrez2_high_beatch$area<-"razrez2_high_beatch"
prcf_perm_razrez2_zostera_zone$area<-"razrez2_zostera_zone"
prcf_perm_razrez2_low_beatch$area<-"razrez2_low_beatch"
prcf_perm_YuG$area<-"YuG"
prcf_perm_ZRS$area<-"ZRS"
#####

# детреднированные данные - по всем
#Эстуарий
(perm_prcf_Estuary_detrend<-perm_PRCF(Estuary_detr))
perm_prcf_Estuary_detrend$signif<-NA
perm_prcf_Estuary_detrend$signif[perm_prcf_Estuary_detrend$p>0.1]<-1
perm_prcf_Estuary_detrend$signif[perm_prcf_Estuary_detrend$p<=0.1 & perm_prcf_Estuary_detrend$p>0.05]<-0.1
perm_prcf_Estuary_detrend$signif[perm_prcf_Estuary_detrend$p<=0.05]<-0.05
perm_prcf_Estuary_detrend$signif<-as.factor(perm_prcf_Estuary_detrend$signif)

cairo_pdf("perm_PRCF_Estuary_detrend.pdf")
ggplot(perm_prcf_Estuary_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Estuary_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Estuary_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез верхний пляж
(perm_prcf_razrez2_high_beatch_detrend<-perm_PRCF(razrez2_high_beatch_detr))

perm_prcf_razrez2_high_beatch_detrend$signif<-NA
perm_prcf_razrez2_high_beatch_detrend$signif[perm_prcf_razrez2_high_beatch_detrend$p<=0.05]<-0.005
perm_prcf_razrez2_high_beatch_detrend$signif[perm_prcf_razrez2_high_beatch_detrend$p>0.05 & perm_prcf_razrez2_high_beatch_detrend$p<=0.1]<-0.1
perm_prcf_razrez2_high_beatch_detrend$signif[perm_prcf_razrez2_high_beatch_detrend$p>0.1]<-1
perm_prcf_razrez2_high_beatch_detrend$signif<-as.factor(perm_prcf_razrez2_high_beatch_detrend$signif)

cairo_pdf("perm_PRCF_razrez2_high_beatch_detrend.pdf")
ggplot(perm_prcf_razrez2_high_beatch_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_high_beatch_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_high_beatch_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез пояс фукоидов
(perm_prcf_razrez2_fucus_zone_detrend<-perm_PRCF(razrez2_fucus_zone_detr))

perm_prcf_razrez2_fucus_zone_detrend$signif<-NA
perm_prcf_razrez2_fucus_zone_detrend$signif[perm_prcf_razrez2_fucus_zone_detrend$p<=0.05]<-0.005
perm_prcf_razrez2_fucus_zone_detrend$signif[perm_prcf_razrez2_fucus_zone_detrend$p>0.05 & perm_prcf_razrez2_fucus_zone_detrend$p<=0.1]<-0.1
perm_prcf_razrez2_fucus_zone_detrend$signif[perm_prcf_razrez2_fucus_zone_detrend$p>0.1]<-1
perm_prcf_razrez2_fucus_zone_detrend$signif<-as.factor(perm_prcf_razrez2_fucus_zone_detrend$signif)

cairo_pdf("perm_PRCF_razrez2_fucus_zone_detrend.pdf")
ggplot(perm_prcf_razrez2_fucus_zone_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_fucus_zone_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_fucus_zone_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез нижний пляж
(perm_prcf_razrez2_low_beatch_detrend<-perm_PRCF(razrez2_low_beatch_detr))

perm_prcf_razrez2_low_beatch_detrend$signif<-NA
perm_prcf_razrez2_low_beatch_detrend$signif[perm_prcf_razrez2_low_beatch_detrend$p<=0.05]<-0.05
perm_prcf_razrez2_low_beatch_detrend$signif[perm_prcf_razrez2_low_beatch_detrend$p>0.05 & perm_prcf_razrez2_low_beatch_detrend$p<=0.1]<-0.1
perm_prcf_razrez2_low_beatch_detrend$signif[perm_prcf_razrez2_low_beatch_detrend$p>0.1]<-1
perm_prcf_razrez2_low_beatch_detrend$signif<-as.factor(perm_prcf_razrez2_low_beatch_detrend$signif)

cairo_pdf("perm_PRCF_razrez2_low_beatch_detrend.pdf")
ggplot(perm_prcf_razrez2_low_beatch_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_low_beatch_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_low_beatch_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез пояс зостеры
(perm_prcf_razrez2_zostera_zone_detrend<-perm_PRCF(razrez2_zostera_zone_detr))

perm_prcf_razrez2_zostera_zone_detrend$signif<-NA
perm_prcf_razrez2_zostera_zone_detrend$signif[perm_prcf_razrez2_zostera_zone_detrend$p<=0.05]<-0.05
perm_prcf_razrez2_zostera_zone_detrend$signif[perm_prcf_razrez2_zostera_zone_detrend$p>0.05 & perm_prcf_razrez2_zostera_zone_detrend$p<=0.1]<-0.1
perm_prcf_razrez2_zostera_zone_detrend$signif[perm_prcf_razrez2_zostera_zone_detrend$p>0.1]<-1
perm_prcf_razrez2_zostera_zone_detrend$signif<-as.factor(perm_prcf_razrez2_zostera_zone_detrend$signif)

cairo_pdf("perm_PRCF_razrez2_zostera_zone_detrend.pdf")
ggplot(perm_prcf_razrez2_zostera_zone_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_zostera_zone_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_zostera_zone_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

#2 разрез_весь
(perm_prcf_razrez2_all_detrend<-perm_PRCF(razrez2_all_detr))

perm_prcf_razrez2_all_detrend$signif<-NA
perm_prcf_razrez2_all_detrend$signif[perm_prcf_razrez2_all_detrend$p<=0.05]<-0.05
perm_prcf_razrez2_all_detrend$signif[perm_prcf_razrez2_all_detrend$p>0.05 & perm_prcf_razrez2_all_detrend$p<=0.1]<-0.1
perm_prcf_razrez2_all_detrend$signif[perm_prcf_razrez2_all_detrend$p>0.1]<-1
perm_prcf_razrez2_all_detrend$signif<-as.factor(perm_prcf_razrez2_all_detrend$signif)

cairo_pdf("perm_PRCF_razrez2_all_detrend.pdf")
ggplot(perm_prcf_razrez2_all_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_razrez2_all_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_razrez2_all_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# ЮГ
(perm_prcf_YuG_detrend<-perm_PRCF(YuG_detr))

perm_prcf_YuG_detrend$signif<-NA
perm_prcf_YuG_detrend$signif[perm_prcf_YuG_detrend$p>0.1]<-1
perm_prcf_YuG_detrend$signif[perm_prcf_YuG_detrend$p<=0.1 & perm_prcf_YuG_detrend$p>0.05]<-0.1
perm_prcf_YuG_detrend$signif[perm_prcf_YuG_detrend$p<=0.05]<-0.05
perm_prcf_YuG_detrend$signif<-as.factor(perm_prcf_YuG_detrend$signif)

cairo_pdf("perm_PRCF_YuG_detrend.pdf")
ggplot(perm_prcf_YuG_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_YuG_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_YuG_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

colnames(ishodnik)

# Ломнишный
(perm_prcf_Lomnishniy_detrend<-perm_PRCF(Lomnishniy_detr))

perm_prcf_Lomnishniy_detrend$signif<-NA
perm_prcf_Lomnishniy_detrend$signif[perm_prcf_Lomnishniy_detrend$p>0.1]<-1
perm_prcf_Lomnishniy_detrend$signif[perm_prcf_Lomnishniy_detrend$p<=0.1 & perm_prcf_Lomnishniy_detrend$p>0.05]<-0.1
perm_prcf_Lomnishniy_detrend$signif[perm_prcf_Lomnishniy_detrend$p<=0.05]<-0.05
perm_prcf_Lomnishniy_detrend$signif<-as.factor(perm_prcf_Lomnishniy_detrend$signif)

cairo_pdf("perm_PRCF_Lomnishniy_detrend.pdf")
ggplot(perm_prcf_Lomnishniy_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Lomnishniy_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Lomnishniy_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# ЗРС
(perm_prcf_ZRS_detrend<-perm_PRCF(ZRS_detr))

perm_prcf_ZRS_detrend$signif<-NA
perm_prcf_ZRS_detrend$signif[perm_prcf_ZRS_detrend$p>0.1]<-1
perm_prcf_ZRS_detrend$signif[perm_prcf_ZRS_detrend$p<=0.1 & perm_prcf_ZRS_detrend$p>0.05]<-0.1
perm_prcf_ZRS_detrend$signif[perm_prcf_ZRS_detrend$p<=0.05]<-0.05
perm_prcf_ZRS_detrend$signif<-as.factor(perm_prcf_ZRS_detrend$signif)

cairo_pdf("perm_PRCF_ZRS_detrend.pdf")
ggplot(perm_prcf_ZRS_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_ZRS_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_ZRS_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Горелый верх
(perm_prcf_Goreliy_high_detrend<-perm_PRCF(Goreliy_high_detr))

perm_prcf_Goreliy_high_detrend$signif<-NA
perm_prcf_Goreliy_high_detrend$signif[perm_prcf_Goreliy_high_detrend$p>0.1]<-1
perm_prcf_Goreliy_high_detrend$signif[perm_prcf_Goreliy_high_detrend$p<=0.1 & perm_prcf_Goreliy_high_detrend$p>0.05]<-0.1
perm_prcf_Goreliy_high_detrend$signif[perm_prcf_Goreliy_high_detrend$p<=0.05]<-0.05
perm_prcf_Goreliy_high_detrend$signif<-as.factor(perm_prcf_Goreliy_high_detrend$signif)

cairo_pdf("perm_PRCF_Goreliy_high_detrend.pdf")
ggplot(perm_prcf_Goreliy_high_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_high_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_high_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Горелый middle
(perm_prcf_Goreliy_middle_detrend<-perm_PRCF(Goreliy_middle_detr))

perm_prcf_Goreliy_middle_detrend$signif<-NA
perm_prcf_Goreliy_middle_detrend$signif[perm_prcf_Goreliy_middle_detrend$p>0.1]<-1
perm_prcf_Goreliy_middle_detrend$signif[perm_prcf_Goreliy_middle_detrend$p<=0.1 & perm_prcf_Goreliy_middle_detrend$p>0.05]<-0.1
perm_prcf_Goreliy_middle_detrend$signif[perm_prcf_Goreliy_middle_detrend$p<=0.05]<-0.05
perm_prcf_Goreliy_middle_detrend$signif<-as.factor(perm_prcf_Goreliy_middle_detrend$signif)

cairo_pdf("perm_PRCF_Goreliy_middle_detrend.pdf")
ggplot(perm_prcf_Goreliy_middle_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_middle_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_middle_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Горелый midlow
(perm_prcf_Goreliy_midlow_detrend<-perm_PRCF(Goreliy_midlow_detr))

perm_prcf_Goreliy_midlow_detrend$signif<-NA
perm_prcf_Goreliy_midlow_detrend$signif[perm_prcf_Goreliy_midlow_detrend$p>0.1]<-1
perm_prcf_Goreliy_midlow_detrend$signif[perm_prcf_Goreliy_midlow_detrend$p<=0.1 & perm_prcf_Goreliy_midlow_detrend$p>0.05]<-0.1
perm_prcf_Goreliy_midlow_detrend$signif[perm_prcf_Goreliy_midlow_detrend$p<=0.05]<-0.05
perm_prcf_Goreliy_midlow_detrend$signif<-as.factor(perm_prcf_Goreliy_midlow_detrend$signif)

cairo_pdf("perm_PRCF_Goreliy_midlow_detrend.pdf")
ggplot(perm_prcf_Goreliy_midlow_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_midlow_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_midlow_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Горелый low
(perm_prcf_Goreliy_low_detrend<-perm_PRCF(as.vector(Goreliy_low_detr)))

perm_prcf_Goreliy_low_detrend$signif<-NA
perm_prcf_Goreliy_low_detrend$signif[perm_prcf_Goreliy_low_detrend$p>0.1]<-1
perm_prcf_Goreliy_low_detrend$signif[perm_prcf_Goreliy_low_detrend$p<=0.1 & perm_prcf_Goreliy_low_detrend$p>0.05]<-0.1
perm_prcf_Goreliy_low_detrend$signif[perm_prcf_Goreliy_low_detrend$p<=0.05]<-0.05
perm_prcf_Goreliy_low_detrend$signif<-as.factor(perm_prcf_Goreliy_low_detrend$signif)

cairo_pdf("perm_PRCF_Goreliy_low_detrend.pdf")
ggplot(perm_prcf_Goreliy_low_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_low_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_low_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()


# Горелый весь
(perm_prcf_Goreliy_all_detrend<-perm_PRCF(as.vector(Goreliy_all_detr)))

perm_prcf_Goreliy_all_detrend$signif<-NA
perm_prcf_Goreliy_all_detrend$signif[perm_prcf_Goreliy_all_detrend$p>0.1]<-1
perm_prcf_Goreliy_all_detrend$signif[perm_prcf_Goreliy_all_detrend$p<=0.1 & perm_prcf_Goreliy_all_detrend$p>0.05]<-0.1
perm_prcf_Goreliy_all_detrend$signif[perm_prcf_Goreliy_all_detrend$p<=0.05]<-0.05
perm_prcf_Goreliy_all_detrend$signif<-as.factor(perm_prcf_Goreliy_all_detrend$signif)

cairo_pdf("perm_PRCF_Goreliy_all_detrend.pdf")
ggplot(perm_prcf_Goreliy_all_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Goreliy_all_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Goreliy_all_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()


# Медвежья
(perm_prcf_Medvezhya_detrend<-perm_PRCF(Medvezhya_detr))

perm_prcf_Medvezhya_detrend$signif<-NA
perm_prcf_Medvezhya_detrend$signif[perm_prcf_Medvezhya_detrend$p>0.1]<-1
perm_prcf_Medvezhya_detrend$signif[perm_prcf_Medvezhya_detrend$p<=0.1 & perm_prcf_Medvezhya_detrend$p>0.05]<-0.1
perm_prcf_Medvezhya_detrend$signif[perm_prcf_Medvezhya_detrend$p<=0.05]<-0.05
perm_prcf_Medvezhya_detrend$signif<-as.factor(perm_prcf_Medvezhya_detrend$signif)

cairo_pdf("perm_PRCF_Medvezhya_detrend.pdf")
ggplot(perm_prcf_Medvezhya_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Medvezhya_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Medvezhya_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()

# Сельдяная
Seldyanaya_detr<-lm(ishodnik$Seldyanaya ~ 0 + ishodnik[,1])$residuals+mean(ishodnik$Seldyanaya)
(perm_prcf_Seldyanaya_detrend<-perm_PRCF(Seldyanaya_detr))

perm_prcf_Seldyanaya_detrend$signif<-NA
perm_prcf_Seldyanaya_detrend$signif[perm_prcf_Seldyanaya_detrend$p>0.1]<-1
perm_prcf_Seldyanaya_detrend$signif[perm_prcf_Seldyanaya_detrend$p<=0.1 & perm_prcf_Seldyanaya_detrend$p>0.05]<-0.1
perm_prcf_Seldyanaya_detrend$signif[perm_prcf_Seldyanaya_detrend$p<=0.05]<-0.05
perm_prcf_Seldyanaya_detrend$signif<-as.factor(perm_prcf_Seldyanaya_detrend$signif)

cairo_pdf("perm_PRCF_Seldyanaya_detrend.pdf")
ggplot(perm_prcf_Seldyanaya_detrend, aes(x=as.factor(lag), y=prcf, fill=signif)) + 
  scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
  geom_bar(colour="black", stat="identity", position="dodge")  + 
  geom_hline(yintercept=0) + 
  geom_hline(yintercept=c(-2/sqrt(length(perm_prcf_Seldyanaya_detrend$PRCF)),
                          2/sqrt(length(perm_prcf_Seldyanaya_detrend$PRCF))), linetype=2) + 
  xlab("Time lag") + 
  ylab("Partial autocorrelations") + 
  theme_bw()
dev.off()


# продолжаем с данными без детрендирования
#склеиваем все в один датафрейм
prcf_perm_all <- rbind(prcf_perm_Estuary, 
                  prcf_perm_Goreliy_high, 
                  prcf_perm_Goreliy_middle,
                  prcf_perm_Goreliy_midlow,
                  prcf_perm_Goreliy_low,
                  prcf_perm_Lomnishniy,
                  prcf_perm_razrez2_fucus_zone,
                  prcf_perm_razrez2_high_beatch,
                  prcf_perm_razrez2_zostera_zone,
                  prcf_perm_razrez2_low_beatch,
                  prcf_perm_YuG,
                  prcf_perm_ZRS)
prcf_perm_all$lag<-as.factor(prcf_perm_all$lag)
prcf_perm_all$area<-as.factor(prcf_perm_all$area)

prcf_perm_all$signif<-NA
prcf_perm_all$signif[prcf_perm_all$p>0.1]<-1
prcf_perm_all$signif[prcf_perm_all$p<=0.1 & prcf_perm_all$p>0.05]<-0.1
prcf_perm_all$signif[prcf_perm_all$p<=0.05]<-0.05
prcf_perm_all$signif<-as.factor(prcf_perm_all$signif)

# я не понимаю, почему не генерится файл!!  прогнал руками, но зело извращение
# ggplot не замыкается в цикл
for (i in 2:ncol(ishodnik)) { 
  cairo_pdf(paste("perm_PRCF", colnames(ishodnik)[i], ".pdf", sep="_"))
  ggplot(subset(prcf_all, prcf_all$area==colnames(ishodnik)[i]), aes(x=as.factor(lag), y=acf, fill=signif)) + 
    scale_fill_manual(values=c("1"="white", "0.1"="gray", "0.05"="black")) +
    geom_bar(colour="black", stat="identity", position="dodge")  + 
    geom_hline(yintercept=0) + 
    geom_hline(yintercept=c(-2/sqrt(length(prcf_all$acf[prcf_all$area==colnames(ishodnik)[i]])),
                            2/sqrt(length(prcf_all$acf[prcf_all$area==colnames(ishodnik)[i]]))), linetype=2) + 
    xlab("Time lag") + 
    ylab("Partial autocorrelations") + 
    theme_bw()
dev.off()  
  }


#####