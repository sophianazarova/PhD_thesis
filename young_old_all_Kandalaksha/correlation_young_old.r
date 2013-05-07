setwd("~/Dropbox/PhD_thesis/PhD_thesis/young_old_all_Kandalaksha/")


#в процентах - это какая-то лажа получается
percents.young.old<-read.table(file="young_old_all_Kandalaksha_percent.csv", head=T, sep=";", dec=",")
str(percents.young.old)

hist(percents.young.old$young)
hist(percents.young.old$old)

#корреляция - молодь и половозрелые в %
(spearman.young.old.all.percent<-cor.test(percents.young.old$young, percents.young.old$old, method="spearman"))


pdf(file="young_old_all.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=percents.young.old$young, x=percents.young.old$old, 
     xlab="доля половозрелых моллюсков, %", 
     ylab="доля годовалых моллюсков, %", 
     main="Влияние доли половозрелых особей
     в поселении на обилие молоди через год")
dev.off()
embedFonts("young_old_all.pdf") #встройка шрифтов в файл

pdf(file="young_old_all1.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=percents.young.old$young, x=percents.young.old$old, 
     xlab="доля половозрелых моллюсков, %", 
     ylab="доля годовалых моллюсков, %", 
     main="Влияние доли половозрелых особей
     в поселении на обилие молоди через год",
     col=c(as.numeric(percents.young.old$area)*10+as.numeric(percents.young.old$tidal_zone)))
dev.off()
embedFonts("young_old_all1.pdf") #встройка шрифтов в файл


#средние на квадратный метр

means.young.old<-read.table(file="young_old_all_Kandalaksha_mean.csv", head=T, sep=";", dec=",")
str(means.young.old)

hist(means.young.old$young)
hist(means.young.old$old)

#корреляция - молодь и половозрелые в %
(spearman.young.old.all.percent<-cor.test(means.young.old$young, means.young.old$old, method="spearman"))


pdf(file="young_old_all.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=means.young.old$young, x=means.young.old$old, 
     xlab="доля половозрелых моллюсков, %", 
     ylab="доля годовалых моллюсков, %", 
     main="Влияние доли половозрелых особей
     в поселении на обилие молоди через год")
dev.off()
embedFonts("young_old_all.pdf") #встройка шрифтов в файл

pdf(file="young_old_all1.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=means.young.old$young, x=means.young.old$old, 
     xlab="доля половозрелых моллюсков, %", 
     ylab="доля годовалых моллюсков, %", 
     main="Влияние доли половозрелых особей
     в поселении на обилие молоди через год",
     col=c(as.numeric(means.young.old$area)*10+as.numeric(means.young.old$tidal_zone)))
dev.off()
embedFonts("young_old_all1.pdf") #встройка шрифтов в файл