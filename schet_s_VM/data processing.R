#ЧАСТЬ 1. Подготовка данных

myt <- read.csv("all measurements.csv", header=TRUE, sep=";")

#Формируем SCAM с классовым шагом 5 мм

classes <- seq(55, 1, -5)
myt$L<-myt$length
for (i in classes) myt$L[myt$length <= i] <- i

d <- as.data.frame(table(myt$year, myt$bank, myt$sample, myt$L))

library(reshape)

d <- melt(d)
head(d)

scam <- cast(d, Var1 + Var2 + Var3 ~ Var4)

names(scam) <- c("year", "bank", "sample", "L3", "L8", "L13", "L18", "L23", "L28", "L33", "L38", "L43", "L48", "L53")

head(scam)

#чистим SCAM

scam[scam$sample==4 & scam$bank=="vor4" & scam$year==1998, 4:14] <-NA

scam[scam$bank=="vor5" & scam$year==1998, 4:14] <-NA


scam[scam$sample==6 & scam$year==1996, 4:14] <-NA

write.table(scam,"clipboard", sep="\t")

head(scam)

# Формируем ASCAM
library (doBy)

#Пишем функции, для обсчетов
means <- function(x) round(mean(x, na.rm=TRUE), digits=1)
sds <- function(x) round(sd(x, na.rm=TRUE), digits=2)
lengths <-function(x) length(x[complete.cases(x)])


ascam <- summaryBy (.~year+bank,data=scam, FUN=means)

#Заполняем данные 1998 года м банке vor5 средними значениями по 1997 и 1999 гг.

ascam[ascam$year == 1998 & ascam$bank == "vor5", 3:13] <- (ascam[ascam$year == 1997 & ascam$bank == "vor5", 3:13] + ascam[ascam$year == 1999 & ascam$bank == "vor5", 3:13])/2

averaN[averaN$year == 1998 & averaN$bank == "vor5", 3] <- (averaN[averaN$year == 1997 & averaN$bank == "vor5", 3] + averaN[averaN$year == 1999 & averaN$bank == "vor5", 3])/2

write.table(ascam,"clipboard", sep="\t")




#-------------------------------------------------------
#Формируем SCAM с классовым шагом 10 мм

classes <- seq(60, 1, -10)
myt$L<-myt$length
for (i in classes) myt$L[myt$length <= i] <- i

d <- as.data.frame(table(myt$year, myt$bank, myt$sample, myt$L))

library(reshape)

d <- melt(d)
head(d)

scam <- cast(d, Var1 + Var2 + Var3 ~ Var4)

names(scam) <- c("year", "bank", "sample", "L10", "L20", "L30", "L40", "L50", "L60")

head(scam)

#чистим SCAM

scam[scam$sample==4 & scam$bank=="vor4" & scam$year==1998, 4:9] <-NA

scam[scam$bank=="vor5" & scam$year==1998, 4:9] <-NA


scam[scam$sample==6 & scam$year==1996, 4:9] <-NA

write.table(scam,"clipboard", sep="\t")

head(scam)

# Формируем ASCAM
library (doBy)

#Пишем функции, для обсчетов
means <- function(x) round(mean(x, na.rm=TRUE), digits=1)
sds <- function(x) round(sd(x, na.rm=TRUE), digits=2)
lengths <-function(x) length(x[complete.cases(x)])


ascam <- summaryBy (.~year+bank,data=scam, FUN=means)

#Заполняем данные 1998 года м банке vor5 средними значениями по 1997 и 1999 гг.

ascam[ascam$year == 1998 & ascam$bank == "vor5", 3:8] <- (ascam[ascam$year == 1997 & ascam$bank == "vor5", 3:8] + ascam[ascam$year == 1999 & ascam$bank == "vor5", 3:8])/2

write.table(ascam,"clipboard", sep="\t")

head(ascam)
#-------------------------------------------------





# Формируем датафрейм N, содержащий суммарные численности мидий в каждой пробе
N <- data.frame(year=scam$year, bank=scam$bank)

N$abundance <- apply(scam[,4:9], 1, sum)

write.table(N,"clipboard", sep="\t")


# Формируем датафрейм averaN со средними обилиями по каждому году на каждой банке

averaN <- as.data.frame(summaryBy(abundance ~ bank + year, data=N, FUN=c(means, sds, lengths)))
names(averaN) <- c("bank", "year", "meanN", "sd", "n")
averaN$se <- round(with(averaN, sd/sqrt(n)), digits=2) 


write.table(averanN,"clipboard", sep="\t")

#Заполняем данные 1998 года м банке vor5 средними значениями по 1997 и 1999 гг.
averaN2 <- averaN
averaN2[averaN$year == 1998 & averaN$bank == "vor5", 3] <- (averaN[averaN$year == 1997 & averaN$bank == "vor5", 3] + averaN[averaN$year == 1999 & averaN$bank == "vor5", 3])/2


#------------------------------------
# Читаем данные по среденмесячным температурам
temp <- read.csv("temperatures.csv", header=TRUE, sep=";" )

#Создаем датафрейм со среднегодовыми температурами
averaT<-data.frame(year=temp$year)
averaT$all <- apply(temp[,2:13],1, FUN=mean)
averaT$anomal <- averaT$all - mean(averaT$all)

# Att! За зимние месяцы взяты December of previous year, January - March

averaT$winter <- apply(temp[,5:8],1, FUN=mean)
averaT$winter_anomal <- averaT$winter - mean(averaT$winter)

# Летние месяцы June-August
averaT$summer <- apply(temp[,11:13],1, FUN=mean)
averaT$summer_anomal <- averaT$summer - mean(averaT$summer) 



# ЧАСТЬ 2. Обработка матриала
#______________________________________________

# Строим графики динамики обилия мидий
library(ggplot2)
theme_set(theme_bw())
Ndynam <- ggplot(averaN, aes(x=year, y=meanN))

cairo_pdf("abundance dynamics.pdf")

Ndynam <- Ndynam + geom_line(aes(group=3))+ geom_point(size=2) + facet_grid(bank~., scales="free") + geom_errorbar(aes(ymin=meanN-se, ymax=meanN+se), width=0.2) + theme(axis.text.x=element_text(angle=90)) + xlab("Years") + ylab("Mean abundance")

dev.off()





# Проводим анализ с помощью Мантеловской коррелограммы
library(vegan)

# Формируем матрицу расстояний между годами Внимание! эта матрица является также и модельной матрицей для направленного тренда
dist_year <- vegdist(ascam[ascam$bank=="vor5",1], method="euclidean")


# Формируем матрицы расстояний для каждой банки постфикс _multi для многмерного случая (ASCAM) _uni - для одномерного случая (averaN)

dist_vor2_mult <- vegdist(log(ascam[ascam$bank=="vor2",3:8]+1), method="euclidean")
dist_vor2_uni <- vegdist(log(averaN[averaN$bank=="vor2",3] +1), method="euclidean")


dist_vor4_mult <- vegdist((ascam[ascam$bank=="vor4",3:8]), method="euclidean")
dist_vor4_uni <- vegdist(log(averaN[averaN$bank=="vor4",3] +1), method="euclidean")



dist_vor5_mult <- vegdist(log(ascam[ascam$bank=="vor5",3:8]+1), method="euclidean")
dist_vor5_uni <- vegdist(log(averaN2[averaN2$bank=="vor5",3] +1), method="euclidean")



plot(mantel.correlog(dist_vor2_mult, dist_year))
plot(mantel.correlog(dist_vor2_uni, dist_year))

plot(mantel.correlog(dist_vor4_mult, dist_year))
plot(mantel.correlog(dist_vor4_uni, dist_year))

     
plot(mantel.correlog(dist_vor5_mult, dist_year))
plot(mantel.correlog(dist_vor5_uni, dist_year))


# Проверка соответствия динамики тренду
trend <- function(x) { 
  write.table(c(mantel(x, dist_year)$statistic,mantel(x, dist_year)$signif), "clipboard", sep="\t")
  cat(c(mantel(x, dist_year)$statistic,mantel(x, dist_year)$signif))
}


trend(dist_vor2_mult)
trend(dist_vor2_uni)

trend(dist_vor4_mult)
trend(dist_vor4_uni)

trend(dist_vor5_mult)
trend(dist_vor5_uni)


# Пишем функцию для вычисления координат точек на круге

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



#Функция для вычисления соотвествия наблюдаемой динамики циклическим модельным матрицам

Mantel_cycles <- function(b="vor4", m = "euclidean", r="spearman", partial=FALSE, perm=999)
{
  
    data_multi <- subset(ascam, bank == b)[,3:8]
    data_uni <- subset(averaN, bank == b) [,3]
    longiv <- length(data_multi[,1])
    
    dist_multi <- vegdist(data_multi, method = m)
  
    dist_uni <- vegdist(data_uni, method = "euclidean")
    
    result_multi <- result_uni <- data.frame(period=rep(NA, (longiv-2)), mantel=rep(NA, (longiv-2)), p=rep(NA, (longiv-2)))
    
  if(!partial)
  {
    for (i in 3:longiv)
       {
          mt<-mantel(dist_multi, circus(longiv, i), method=r, permutations=perm)
          cat("period ", i,"\n")
          result_multi$period[i-2] <- i
          result_multi$mantel[i-2] <- round(mt$statistic, digits=4)
          result_multi$p[i-2] <- mt$signif
          
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
      mt<-mantel.partial(dist_multi, circus(longiv, i), dist_year, method=r, permutations=perm)
      cat("period ", i,"\n")
      result_multi$period[i-2] <- i
      result_multi$mantel[i-2] <- round(mt$statistic, digits=4)
      result_multi$p[i-2] <- mt$signif
      
      mt<-mantel.partial(dist_uni, circus(longiv, i), dist_year, method=r, permutations=perm)
      result_uni$period[i-2] <- i
      result_uni$mantel[i-2] <- round(mt$statistic, digits=4)
      result_uni$p[i-2] <- mt$signif
    }  
   
  }
        return(list(result_uni, result_multi))
}

vor2_mantel_result <- Mantel_cycles("vor2", partial=TRUE)
vor4_mantel_result <- Mantel_cycles("vor4", partial=TRUE)
vor5_mantel_result <- Mantel_cycles("vor5", partial=TRUE)

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


#-------------------------------------------------------
#Рисуем диаграммы с MDS и размерными структурами

library(grid)
library(gridExtra)
library(ggplot2)
library (vegan)

mdsplot <- function(b="vor4", m="euclidean", logar=TRUE, alpha=0)
{
  set.seed(1234)
  ascam_work <- ascam[ascam$bank==b,]
  if (logar) ascam_work[,3:8] <- log(ascam_work[,3:8]+1)
  mds<-monoMDS(vegdist(ascam_work[,3:8], method=m))
  mds_points <- as.data.frame(mds$points)
  mds_points$bank <- ascam_work$bank
  mds_points$year <- ascam_work$year
  
  new_MDS1 <- mds_points$MDS1*cos(alpha) + mds_points$MDS2*sin(alpha)
  new_MDS2 <- (-1)*mds_points$MDS1*sin(alpha) + mds_points$MDS2*cos(alpha)
  mds_points$MDS1 <- new_MDS1
  mds_points$MDS2 <- new_MDS2
  
  
  longiv <- nrow(mds_points)
  
  #Начальная и конечная точки
  begin <-(c(round(as.numeric(as.character(mds_points[1,4]))), mds_points[1,1], mds_points[1,2]))
  end <- (c(as.numeric(as.character(mds_points[longiv,4])), mds_points[longiv,1], mds_points[longiv,2]))

 # точки с минимальными и максимальными координатами в MDS
  
  MDS1_min <- (c(round(as.numeric(as.character(mds_points[which.min(mds_points$MDS1),4]))), mds_points[which.min(mds_points$MDS1),1], mds_points[which.min(mds_points$MDS1),2])) 
  MDS1_max <- (c(round(as.numeric(as.character(mds_points[which.max(mds_points$MDS1),4]))), mds_points[which.max(mds_points$MDS1),1], mds_points[which.max(mds_points$MDS1),2]))
  MDS2_min <- (c(round(as.numeric(as.character(mds_points[which.min(mds_points$MDS2),4]))), mds_points[which.min(mds_points$MDS2),1], mds_points[which.min(mds_points$MDS2),2])) 
  MDS2_max <- (c(round(as.numeric(as.character(mds_points[which.max(mds_points$MDS2),4]))), mds_points[which.max(mds_points$MDS2),1], mds_points[which.max(mds_points$MDS2),2]))
  
 
  theme_set(theme_bw())
  pl <- ggplot(mds_points, aes(x=MDS1, y=MDS2))
  pl <- pl + 
    geom_point(size=2, shape=21, fill="gray") + 
    geom_path(linejoin = "round", lineend = "butt", arrow=arrow(angle = 15,  length = unit(0.1, "inches"), type="closed")) + 
    theme(axis.title.x=element_blank(), axis.title.y=element_blank(), axis.ticks=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.text.x = element_blank(), axis.text.y=element_blank()) +
    annotate("text", x=(begin[2] + 0.1), y=(begin[3] + 0.1), label = as.character(begin[1]), size=3) +
    annotate("text", x=(end[2] + 0.2), y=(end[3]), label = as.character(end[1]), size=3) + 
    annotate ("point", x=(MDS1_min[2]), y=(MDS1_min[3]), size=3) +
    annotate ("point", x=(MDS2_min[2]), y=(MDS2_min[3]), size=3) +
    annotate ("point", x=(MDS1_max[2]), y=(MDS1_max[3]), size=3) +
    annotate ("point", x=(MDS2_max[2]), y=(MDS2_max[3]), size=3) +
    ggtitle(paste("Stress = ", round(mds$stress, digits=2)))

   

  hist_mds1_min <- ggplot(myt[myt$bank == b & myt$year == (MDS1_min[1]),], aes(x=length))
  hist_mds1_min <- hist_mds1_min + geom_histogram(bin=5, fill="gray", colour="black") + xlab(" ") + ggtitle (paste(as.character (MDS1_min[1]))) + theme(axis.title.y=element_blank())
  
  

  hist_mds1_max <- ggplot(myt[myt$bank == b & myt$year == (MDS1_max[1]),], aes(x=length))
  hist_mds1_max <- hist_mds1_max + geom_histogram(bin=5, fill="gray", colour="black") + xlab(" ") + ggtitle (paste(as.character (MDS1_max[1])))+ theme(axis.title.y=element_blank())
  
  
  
  hist_mds2_min <- ggplot(myt[myt$bank == b & myt$year == (MDS2_min[1]),], aes(x=length))
  hist_mds2_min <- hist_mds2_min + geom_histogram(bin=5, fill="gray", colour="black") + xlab(" ") + ggtitle (paste(as.character (MDS2_min[1])))+ theme(axis.title.y=element_blank())
  
  
  
  hist_mds2_max <- ggplot(myt[myt$bank == b & myt$year == (MDS2_max[1]),], aes(x=length))
  hist_mds2_max <- hist_mds2_max + geom_histogram(bin=5, fill="gray", colour="black") + xlab(" ") + ggtitle (paste(as.character (MDS2_max[1])))+ theme(axis.title.y=element_blank())
  
  empty_graph <- ggplot(myt, aes(x=length, y=L)) + geom_blank() + theme_bw() + theme(axis.title.x=element_text(colour="white"), axis.title.y=element_text(colour="white"), axis.ticks=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), axis.text.x = element_text(colour="white"), axis.text.y=element_text(colour="white"), panel.border=element_rect(colour="white"))
  
  
 grid.arrange(empty_graph, hist_mds2_max, empty_graph, hist_mds1_min, pl, hist_mds1_max, empty_graph, hist_mds2_min,empty_graph, nrow=3)
  
  
}

cairo_pdf("vor2 mds.pdf")
mdsplot("vor2", alpha=0.2)
dev.off()

cairo_pdf("vor4 mds.pdf")
mdsplot("vor4", alpha=0.2)
dev.off()

cairo_pdf("vor5 mds.pdf")
mdsplot("vor5", alpha=0)
dev.off()



# ЧАСТЬ 3
# Обрабатываем данные по температуре

#Компоеннтный анализ данных по тмпературе
install.packages("GPArotation")
library (psych)
str(principal(temp[, 2:13], rotate="none",nfactors=2))

principal(temp[, 2:13], rotate="varimax",nfactors=2)

# Формируем матрмцу расстояний для данных по температуре
dist_temp_mult <- vegdist(temp[, 2:13], method="euclidean")
dist_temp_winter_multi <- vegdist(temp[, 5:8], method="euclidean")
dist_temp_uni <- vegdist(averaT[, 2], method="euclidean")
dist_temp_winter_uni <- vegdist(averaT[, 3], method="euclidean")


# Проверяем данные по температуре на наличие тренда
trend(dist_temp_mult)
trend(dist_temp_winter_multi)
trend(dist_temp_uni)
trend(dist_temp_winter_uni)

# Строим график динамики среднегодовой температуры
pl_temp <- ggplot(averaT, aes(x=as.factor(year), y=all))

cairo_pdf("temperature dynamics.pdf")

pl_temp <- pl_temp + geom_point(shape=21, size=3, fill="black") + geom_line(aes(group=1)) + theme(axis.text.x=element_text(angle=90)) + geom_hline(yintercept=mean(averaT$all), linetype=2) + xlab("Years") + ylab("Temperature")

dev.off()

# строим систему графиков с динамикой обилия мидий и динамикой температуры

temperatures <- data.frame(bank=rep("Temperature", longiv), year=averaT$year, meanN=averaT$all, sd=0, n=0, se=0)
all_means <- rbind(temperatures,averaN)
all_means[57,3]<-NA
theme_set(theme_bw())
Ndynam <- ggplot(all_means, aes(x=year, y=meanN))

cairo_pdf("All means dynamics.pdf")

Ndynam + geom_line(aes(group=3))+ geom_point(size=2) + facet_grid(bank~., scales="free") + geom_errorbar(aes(ymin=meanN-se, ymax=meanN+se), width=0.2) + theme(axis.text.x=element_text(angle=90)) + xlab("Years") + ylab("Mean abundance")

dev.off()



library(ppcor)
pcorcycles_t <- function (m="euclidean", rm="spearman", perm=999)
{
    
  
  result_multi <- result_uni <- data.frame(period=rep(NA, (longiv-2)), mantel=rep(NA, (longiv-2)), p=rep(NA, (longiv-2)))
  
  
  y_m <- as.vector(dist_temp_mult)
  y_u <- as.vector(dist_temp_uni)
  
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
  
   
  return(list(result_uni, result_multi))
}    

cycles_temp<-pcorcycles_t()

cycles_temperature <- rbind(cycles_temp[[1]], cycles_temp[[2]])
cycles_temperature$type[1:16] <- "Univariate case"
cycles_temperature$type[17:32] <- "Multivariate case"
cycles_temperature$signif[cycles_temperature$p >= 0.1] <- "NS"
cycles_temperature$signif[cycles_temperature$p < 0.1 & cycles_temperature$p >=0.05] <- "p<0.1"
cycles_temperature$signif[cycles_temperature$p < 0.05] <- "p<0.05"



#Рисуем Мантеловские корреляции для разных периодов для данных по температуре

pl_mantel_result_temperature <- ggplot(cycles_temperature, aes(x=factor(period),y=mantel, group=type, ymax=0.15))

cairo_pdf("mantel temperature cycles.pdf")

pl_mantel_result_temperature + geom_line(aes(linetype=type), position=position_dodge(width=0.2)) + geom_point(size=3, shape=21, aes(fill=signif), position=position_dodge(width=0.2)) + geom_hline(yintercept=0, linetype=2) + scale_fill_manual(values=c("p<0.05" = "black", "p<0.1" = "gray", "NS"="white"))+xlab("Cyclic model matrix period")+ylab("Partial Mantel correlation") 

dev.off()


#Рисуем мантеловские частные корреляции для всех банок и для температуры

cycles_temp <- cycles_temperature
mantel_result
cycles_temp$bank <- "Temperature"
all_cycles <- rbind(mantel_result,cycles_temp)


pl_mantel_result_all <- ggplot(all_cycles, aes(x=factor(period),y=mantel, group=type, ymax=0.15))

cairo_pdf("mantel cycles for all time series.pdf")

pl_mantel_result_all + geom_line(aes(linetype=type), position=position_dodge(width=0.2)) + geom_point(size=3, shape=21, aes(fill=signif), position=position_dodge(width=0.2)) + geom_hline(yintercept=0, linetype=2) + facet_grid(bank~.) + scale_fill_manual(values=c("p<0.05" = "black", "p<0.1" = "gray", "NS"="white"))+xlab("Cyclic model matrix period")+ylab("Partial Mantel correlation") 


dev.off()


# Смотрим на мантеловские корреляции между температурой и размерной структурой

mantel(dist_vor2_mult, dist_temp_mult, method="spearman")
mantel(dist_vor4_mult, dist_temp_mult, method="spearman")
mantel(dist_vor5_mult, dist_temp_mult, method="spearman")

# Смотрим на кросскорреляции 

cross_vor2 <- ccf(averaT$all, averaN$meanN[averaN$bank=="vor2"], type="correlation")
cross_vor4 <- ccf(averaT$all, averaN$meanN[averaN$bank=="vor4"], type="correlation")
cross_vor5 <- ccf(averaT$all, averaN$meanN[averaN$bank=="vor5"], type="correlation")

#Формируем датафрейм из кросс-корреляций между температурой и обилием мидий
crosscorrelations <- data.frame(Lag=cross_vor2$lag, r2=round(cross_vor2$acf, digits=3), r4=round(cross_vor4$acf, digits=3), r5=round(cross_vor5$acf, digits=3))

write.table(crosscorrelations, "clipboard", sep="\t", row.names=F)


# ЧАСТЬ 4. Анализ PACF и PRCF. 
#-------------------------------------------

# Detrending of time series и анализ PACF
detrend_vor2 <- lm((meanN[bank=="vor2"]) ~ as.numeric(year[bank=="vor2"]), data=averaN)$residuals + mean(averaN$meanN[averaN$bank=="vor2"])

pacf_detrend_vor2 <- pacf(detrend_vor2, lag.max=9)

detrend_vor4 <- lm((meanN[bank=="vor4"]) ~ as.numeric(year[bank=="vor4"]), data=averaN)$residuals + mean(averaN$meanN[averaN$bank=="vor4"])
pacf(detrend_vor4, lag.max=9)

detrend_vor5 <- lm((meanN[bank=="vor5"]) ~ as.numeric(year[bank=="vor5"]), data=averaN)$residuals + mean(averaN$meanN[averaN$bank=="vor5"])
pacf(detrend_vor5, lag.max=9)

# Анализы без детрендинга
N_vor2 <- (averaN$meanN[averaN$bank=="vor2"])
pacf(N_vor2, lag.max=9, plot=FALSE)

N_vor4 <- (averaN$meanN[averaN$bank=="vor4"])
pacf(N_vor4, lag.max=9, plot=FALSE)

N_vor5 <- (averaN$meanN[averaN$bank=="vor5"])
pacf(N_vor5, lag.max=9, plot=FALSE)

# Функция для расчетов PRCF по Berryman, Turchin, 2001
PRCF <- function (data=N_vor2)
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
prcf_vor2 <- (PRCF(detrend_vor2))[[1]])
prcf_vor2$bank <- "vor2"

prcf_vor4 <- (PRCF(detrend_vor4)[[1]])
prcf_vor4$bank <- "vor4"

prcf_vor5 <- (PRCF(detrend_vor5)[[1]])
prcf_vor5$bank <- "vor5"

prcf_all <- rbind(prcf_vor2, prcf_vor4, prcf_vor5)
prcf_all$lag<-as.factor(prcf_all$lag)
prcf_all$type<-as.factor(prcf_all$type)
prcf_all$bank<-as.factor(prcf_all$bank)



# Рисуем гистограммы для PRCF 
pl_PRCF <- ggplot(prcf_all, aes(x=as.factor(lag), y=acf, fill=type))

cairo_pdf("PRCF.pdf")

pl_PRCF + geom_bar(colour="black", stat="identity", position="dodge") + facet_grid(bank~.) + scale_fill_manual(values=c("PRCF" = "grey")) + geom_hline(yintercept=0) + geom_hline(yintercept=c(-2/sqrt(longiv),2/sqrt(longiv)), linetype=2) + xlab("Time lag") + ylab("Partial autocorrelations") + theme_bw()

dev.off()

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

perm_prcf_vor2 <- perm_PRCF(detrend_vor2)
perm_prcf_vor2

perm_prcf_vor4 <- perm_PRCF(detrend_vor4)
perm_prcf_vor4

perm_prcf_vor5<- perm_PRCF(detrend_vor5)
perm_prcf_vor5


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

boot_PRCF_vor2 <- boot_PRCF(detrend_vor2, n=999, confid=0.99)
boot_PRCF_vor2$bank="vor2"
boot_PRCF_vor2

boot_PRCF_vor4 <- boot_PRCF(detrend_vor4, n=999, confid=0.99)
boot_PRCF_vor4$bank="vor4"
boot_PRCF_vor4

boot_PRCF_vor5 <- boot_PRCF(detrend_vor5, n=999, confid=0.99)
boot_PRCF_vor5$bank="vor5"
boot_PRCF_vor5

#Рисуем диаграммы PRCF с бутстреповыми доверительными интервалами
boot_prcf_all <- rbind(boot_PRCF_vor2, boot_PRCF_vor4, boot_PRCF_vor5)
boot_prcf_all$lag<-as.factor(boot_prcf_all$lag)
boot_prcf_all$bank<-as.factor(boot_prcf_all$bank)
boot_prcf_all

pl_PRCF_ci <- ggplot(boot_prcf_all, aes(x=lag, y=PRCF))

cairo_pdf("PRCF with confidence intervals.pdf")

pl_PRCF_ci + geom_bar(colour="black", fill="darkgray", stat="identity", position="dodge") + facet_grid(bank~.) + geom_hline(yintercept=0) + geom_hline(yintercept=c(-2/sqrt(longiv),2/sqrt(longiv)), linetype=2) + geom_errorbar(aes(ymin=low.ci, ymax=upper.ci),width=0.1)+ xlab("Time lag") + ylab("Partial autocorrelations") + theme_bw()

dev.off()

#Применяем ANOVA и PERMANOVA для проверки взаимодействия факторов 
anova_result <- anova(lm(log(abundance+1)~year*bank, data=N[N$year!="1996" & N$year !="1998",]))
anova_result

permanova_result<-adonis(vegdist(scam[scam$year!="1996" & scam$year !="1998",4:8], method="euclidean")~as.factor(year)*bank, data=scam[scam$year!="1996" & scam$year !="1998",])
permanova_result


#Выявляем гетеросцедастичность в Nt vs Nt-1 с помощью Breusch-Pagan Test

install.packages("lmtest")
library(lmtest)

Lt_1_vor2 <- log(N_vor2[1:(longiv-1)]+1)
Lt_vor2 <- log(N_vor2[2:(longiv)]+1)
lm_Lt_Lt1_vor2 <- lm(Lt_vor2~Lt_1_vor2)
bptest(lm_Lt_Lt1_vor2)
qplot(x=Lt_1_vor2, y=Lt_vor2, geom="point")


Lt_1_vor4 <- log(N_vor4[1:(longiv-1)]+1)
Lt_vor4 <- log(N_vor4[2:(longiv)]+1)
lm_Lt_Lt1_vor4 <- lm(Lt_vor4~Lt_1_vor4)
bptest(lm_Lt_Lt1_vor4)
qplot(x=Lt_1_vor4, y=Lt_vor4, geom="point")

Lt_1_vor5 <- log(N_vor5[1:(longiv-1)]+1)
Lt_vor5 <- log(N_vor5[2:(longiv)]+1)
lm_Lt_Lt1_vor5 <- lm(Lt_vor5~Lt_1_vor5)
bptest(lm_Lt_Lt1_vor5)
qplot(x=Lt_1_vor5, y=Lt_vor5, geom="point")

all_Lt <- c(Lt_vor2, Lt_vor4,Lt_vor5)
all_Lt_1 <- c(Lt_1_vor2, Lt_1_vor4, Lt_1_vor5)
qplot(x=all_Lt_1, y=all_Lt, geom="point")