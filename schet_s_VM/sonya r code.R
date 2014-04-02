setwd("~/Dropbox/PhD_thesis/PhD_thesis/schet_s_VM/")

install.packages("vegan")

library(ggplot2)
library(vegan)

mac <- read.table("est1.csv", header=TRUE, sep=";", dec=",")

pl <- ggplot(mac, aes(x=year, y=N))
pl+geom_line()



#Функция для вычисления соотвествия наблюдаемой динамики циклическим модельным матрицам

circus <- function(n,p, met="euclidean"){
  # n - число точек
  # p - период
  factor<- points <- data.frame(X=c(1:n), Y=c(1:n))
  k <- 0
  for (i in 1:n){
    factor$X[i] <- (i-1)/p - k
    if ((i/p - trunc(i/p))==0) k <- k + 1
  }
  factor$Y <- factor$X
  
  for (i in 1:n){
    points$X[i] <- cos(2*pi*factor$X[i])
    points$Y[i] <- sin(2*pi*factor$Y[i])
  }
  dist <- dist(points, method=met, diag=TRUE, upper=TRUE)
  #dist <- as.dist(matrix(rank(as.matrix(round(dist, digits=3))), ncol=n))
  return(dist)
}



Mantel_cycles <- function(m = "euclidean", r="spearman", partial=FALSE, perm=999)
{
  
  data_uni <- mac$detrendN
  longiv <- length(mac$N)
  
  dist_uni <- vegdist(data_uni, method = "euclidean")
  
  result_uni <- data.frame(period=rep(NA, (longiv-2)), mantel=rep(NA, (longiv-2)), p=rep(NA, (longiv-2)))
  
  if(!partial)
  {
    for (i in 3:longiv)
    {
      mt<-mantel(dist_uni, circus(longiv, i), method=r, permutations=perm)
      result_uni$period[i-2] <- i
      result_uni$mantel[i-2] <- round(mt$statistic, digits=4)
      result_uni$p[i-2] <- mt$signif
    }  
    
  } 
  else
  {
    for (i in 3:longiv)
    {
      
      mt<-mantel.partial(dist_uni, circus(longiv, i), dist_year, method=r, permutations=perm)
      result_uni$period[i-2] <- i
      result_uni$mantel[i-2] <- round(mt$statistic, digits=4)
      result_uni$p[i-2] <- mt$signif
      cat("cycle ", i, "\n")
    }  
    
  }
  return(list(result_uni))
}


dist_year <- vegdist(mac$year, method="euclidean")

dist_mac <- vegdist(mac$N, method="euclidean")

mantel(dist_mac,dist_year)


mantel_result <- Mantel_cycles(partial=TRUE)


# Соединяем в один датафрейм результаты Mantel_cycles
vor2 <- rbind(vor2_mantel_result[[1]], vor2_mantel_result[[2]])
vor2$type[1:16] <- "Univariate case"
vor2$type[17:32] <- "Multivariate case"
vor2$bank <- "vor2"

vor4 <- rbind(vor4_mantel_result[[1]], vor4_mantel_result[[2]])
vor4$type[1:16] <- "Univariate case"
vor4$type[17:32] <- "Multivariate case"
vor4$bank <- "vor4"

vor5 <- rbind(vor5_mantel_result[[1]], vor5_mantel_result[[2]])
vor5$type[1:16] <- "Univariate case"
vor5$type[17:32] <- "Multivariate case"
vor5$bank <- "vor5"

mantel_result <-rbind(vor2, vor4, vor5)
mantel_result$signif[mantel_result$p >= 0.1] <- "NS"
mantel_result$signif[mantel_result$p < 0.1 & mantel_result$p >=0.05] <- "p<0.1"
mantel_result$signif[mantel_result$p < 0.05] <- "p<0.05"

# Рисуем результаты Mantel_cycles

pl_mantel_result <- ggplot(mantel_result, aes(x=factor(period),y=mantel, group=type, ymax=0.15))

cairo_pdf("mantel cyclic vodel.pdf")

pl_mantel_result + geom_line(aes(linetype=type), position=position_dodge(width=0.2)) + geom_point(size=3, shape=21, aes(fill=signif), position=position_dodge(width=0.2)) + geom_hline(yintercept=0, linetype=2) + facet_grid(bank~.) + scale_fill_manual(values=c("p<0.05" = "black", "p<0.1" = "gray", "NS"="white"))+xlab("Cyclic model matrix period")+ylab("Partial Mantel correlation") 

dev.off()


#Альтернатива частной Мантеловской уорреляции по Оксанену
# Вариант 1 оцениваем коэффициенты линейной модели
lmcycles <- function (b="vor4", m="euclidean", perm=999)
{
  data_multi <- log(subset(ascam, bank == b)[,3:8]+1)
  data_uni <- log(subset(averaN, bank == b) [,3]+1)
  longiv <- length(data_multi[,1])
  
  
  result_multi <- result_uni <- data.frame(period=rep(NA, (longiv-2)), coef_trend=rep(NA, (longiv-2)), p_trend=rep(NA, (longiv-2)), coef_cycle=rep(NA, (longiv-2)), p_cycle=rep(NA, (longiv-2)))
  
  
  y_m <- as.vector(vegdist(data_multi, method = m))
  y_u <- as.vector(vegdist(data_uni, method = "euclidean")
  )
  z <- as.vector(dist_year)
  
  for (i in 3:longiv)
  {
    z <- as.vector(dist_year)
    x <- as.vector (circus(longiv, i))
    
    result_uni$period[i-2] <-i
    result_multi$period[i-2] <-i
    
    fit_u <- lm(y_u ~ z + x)
    result_uni$coef_trend[i-2] <- fit_u$coefficients [2]
    result_uni$coef_cycle[i-2] <- fit_u$coefficients [3]
    
    fit_m <- lm(y_m ~ z + x)
    result_multi$coef_trend[i-2] <- round(fit_m$coefficients [2], digits=4)
    result_multi$coef_cycle[i-2] <- round(fit_m$coefficients [3], digits=4)
    
    #Permutations
    perm_trend_u <- rep(NA, (perm+1))
    perm_cycle_u <- rep(NA, (perm+1))
    
    perm_trend_m <- rep(NA, (perm+1))
    perm_cycle_m <- rep(NA, (perm+1))
    
    for(j in 1:perm)
    {
      ind <- sample(1:longiv, longiv)
      y_m_p <- as.vector(vegdist(data_multi[ind, ], method = m))
      y_u_p <- as.vector(vegdist(data_uni[ind], method = "euclidean"))
      fit_p <- lm(y_u_p ~ z + x)
      perm_trend_u[j] <- fit_p$coefficients [2]
      perm_cycle_u[j] <- fit_p$coefficients [3]
      
      fit_p <- lm(y_m_p ~ z + x)
      perm_trend_m[j] <- fit_p$coefficients [2]
      perm_cycle_m[j] <- fit_p$coefficients [3]
    }  
    
    perm_trend_u[perm+1] <- result_uni$coef_trend[i-2]
    perm_cycle_u[perm+1] <- result_uni$coef_cycle[i-2]
    perm_trend_m[perm+1] <- result_multi$coef_trend[i-2]
    perm_cycle_m[perm+1] <- result_multi$coef_cycle[i-2]
    
    result_uni$p_cycle[i-2] <- length(perm_cycle_u[perm_cycle_u >= perm_cycle_u[perm+1]])/(perm+1)
    
    result_uni$p_trend[i-2] <- length(perm_trend_u[perm_trend_u >= perm_trend_u[perm+1]])/(perm+1)
    
    result_multi$p_cycle[i-2] <- length(perm_cycle_m[perm_cycle_m >= perm_cycle_m[perm+1]])/(perm+1)
    
    result_multi$p_trend[i-2] <- length(perm_trend_m[perm_trend_m >= perm_trend_m[perm+1]])/(perm+1)
    
    
    cat("Period ", i,"\n", sep="") 
  }
  
  cat("\nUnivariate case for Mussel bed ", b, "\n", sep="")  
  print(result_uni)
  
  cat("\nMultivariate case for Mussel bed ", b, "\n", sep="")  
  print(result_multi)
  
  return(list(result_uni, result_multi))
}    



# Ваирмнт 2. Находим частную корреляцию с помощью функции pcor.test() из пакета {ppcor}

library(ppcor)

pcorcycles <- function (b="vor4", m="euclidean", rm="spearman", perm=999)
{
  data_multi <- log(subset(ascam, bank == b)[,3:8]+1)
  data_uni <- log(subset(averaN, bank == b) [,3]+1)
  longiv <- length(data_multi[,1])
  
  
  result_multi <- result_uni <- data.frame(period=rep(NA, (longiv-2)), mantel=rep(NA, (longiv-2)), p=rep(NA, (longiv-2)))
  
  
  y_m <- as.vector(vegdist(data_multi, method = m))
  y_u <- as.vector(vegdist(data_uni, method = "euclidean")
  )
  z <- as.vector(dist_year)
  
  for (i in 3:longiv)
  {
    
    x <- as.vector (circus(longiv, i))
    
    result_uni$period[i-2] <-i
    result_multi$period[i-2] <-i
    
    fit_u <- pcor.test(y_u, x, z, method=rm)
    result_uni$mantel[i-2] <- fit_u$estimate
    
    fit_m <- pcor.test(y_m, x, z, method=rm)
    result_multi$mantel[i-2] <- fit_m$estimate
    
    
    #Permutations
    perm_cycle_u <- rep(NA, (perm+1))
    
    perm_cycle_m <- rep(NA, (perm+1))
    
    for(j in 1:perm)
    {
      ind <- sample(1:longiv, longiv)
      y_m_p <- as.vector(vegdist(data_multi[ind, ], method = m))
      y_u_p <- as.vector(vegdist(data_uni[ind], method = "euclidean"))
      
      fit_p <- pcor.test(y_m_p, x, z, method=rm)
      perm_cycle_m[j] <- fit_p$estimate
      
      fit_p <-pcor.test(y_u_p, x, z, method=rm)
      
      perm_cycle_u[j] <- fit_p$estimate
    }  
    
    perm_cycle_u[perm+1] <- result_uni$mantel[i-2]
    perm_cycle_m[perm+1] <- result_multi$mantel[i-2]
    
    result_uni$p[i-2] <- length(perm_cycle_u[perm_cycle_u >= perm_cycle_u[perm+1]])/(perm+1)
    
    result_multi$p[i-2] <- length(perm_cycle_m[perm_cycle_m >= perm_cycle_m[perm+1]])/(perm+1)
    
    
    cat("Period ", i,"\n", sep="") 
  }
  
  #cat("\nUnivariate case for Mussel bed ", b, "\n", sep="")  
  #print(result_uni)
  
  #cat("\nMultivariate case for Mussel bed ", b, "\n", sep="")  
  #print(result_multi)
  
  return(list(result_uni, result_multi))
}    

vor2_pcorcycles <- pcorcycles("vor2")
vor4_pcorcycles <- pcorcycles("vor4")
vor5_pcorcycles <- pcorcycles("vor5")

#Соединяем в один датафрейм данные по pcorcycles

vor2 <- rbind(vor2_pcorcycles[[1]], vor2_pcorcycles[[2]])
vor2$type[1:16] <- "Univariate case"
vor2$type[17:32] <- "Multivariate case"
vor2$bank <- "vor2"

vor4 <- rbind(vor4_pcorcycles[[1]], vor4_pcorcycles[[2]])
vor4$type[1:16] <- "Univariate case"
vor4$type[17:32] <- "Multivariate case"
vor4$bank <- "vor4"

vor5 <- rbind(vor5_pcorcycles[[1]], vor5_pcorcycles[[2]])
vor5$type[1:16] <- "Univariate case"
vor5$type[17:32] <- "Multivariate case"
vor5$bank <- "vor5"

mantel_result <-rbind(vor2, vor4, vor5)
mantel_result$signif[mantel_result$p >= 0.1] <- "NS"
mantel_result$signif[mantel_result$p < 0.1 & mantel_result$p >=0.05] <- "p<0.1"
mantel_result$signif[mantel_result$p < 0.05] <- "p<0.05"

# Рисуем результаты pcorcycles

pl_mantel_result <- ggplot(mantel_result, aes(x=factor(period),y=mantel, group=type, ymax=0.15))

cairo_pdf("mantel cyclic model2.pdf")

pl_mantel_result + geom_line(aes(linetype=type), position=position_dodge(width=0.2)) + geom_point(size=3, shape=21, aes(fill=signif), position=position_dodge(width=0.2)) + geom_hline(yintercept=0, linetype=2) + facet_grid(bank~.) + scale_fill_manual(values=c("p<0.05" = "black", "p<0.1" = "gray", "NS"="white"))+xlab("Cyclic model matrix period")+ylab("Partial Mantel correlation") 

dev.off()


# ЧАСТЬ 4. Анализ PACF и PRCF. 
#-------------------------------------------

# Detrending of time series и анализ PACF

#install.packages("boot")
library(boot)

detrend_mac <- lm(N ~ as.numeric(year), data=mac)$residuals + mean(mac$N)
mac$detrendN <- detrend_mac 

pl <- ggplot(mac, aes(x=year, y=detrendN))
pl+geom_line()

nrow(mac)

pacf_detrend <- pacf(detrend_mac, lag.max=10)


# Анализы без детрендинга

pacf(mac$N, lag.max=10)


# Функция для расчетов PRCF по Berryman, Turchin, 2001
PRCF <- function (data=mac$N)
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

# Создаем датафрейм с PRCF и PRACF для всех банок
prcf_mac <- PRCF(mac$N)[[1]]


prcf_all <- prcf_mac


# Рисуем гистограммы для PRCF 
pl_PRCF <- ggplot(prcf_all, aes(x=as.factor(lag), y=acf, fill=type))



pl_PRCF + geom_bar(colour="black", stat="identity", position="dodge")  + scale_fill_manual(values=c("PRCF" = "grey")) + geom_hline(yintercept=0) + geom_hline(yintercept=c(-2/sqrt(21),2/sqrt(21)), linetype=2) + xlab("Time lag") + ylab("Partial autocorrelations") + theme_bw()


#Играю с возможностью пермутационной оценки достоверности PRCF
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

perm_prcf_mac <- perm_PRCF(mac$N)
perm_prcf_mac

perm_prcf_mac_detr <- perm_PRCF(mac$detrendN)
perm_prcf_mac_detr



# играю с бутстрепом для оценки ошибок в PRCF
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

boot_PRCF_mac <- boot_PRCF(mac$N, n=999, confid=0.99)

#Рисуем диаграммы PRCF с бутстреповыми доверительными интервалами
boot_prcf_all <-boot_PRCF_mac

pl_PRCF_ci <- ggplot(boot_prcf_all, aes(x=lag, y=PRCF))


pl_PRCF_ci + geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge")  + geom_hline(yintercept=0) + geom_hline(yintercept=c(-2/sqrt(21),2/sqrt(21)), linetype=2) + geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ xlab("Time lag") + ylab("Partial autocorrelations") + theme_bw()

pdf(file="Rstuary_N2_PRCF.pdf", family="NimbusSan")
pl_PRCF_ci + geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge")  + geom_hline(yintercept=0) + geom_hline(yintercept=c(-2/sqrt(21),2/sqrt(21)), linetype=2) + geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ xlab("Time lag") + ylab("Partial autocorrelations") + theme_bw()
dev.off()
embedFonts("Rstuary_N2_PRCF.pdf") #встройка шрифтов в файл