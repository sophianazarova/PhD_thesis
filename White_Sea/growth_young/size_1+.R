setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/growth_young/")
ishodnik<-read.table(file="growth_young_Kandalaksha.csv", sep=";", dec=",", head=T)
str(ishodnik)
hist(ishodnik$k1)
hist(ishodnik$k1[ishodnik$age==1])
summary(ishodnik)

# гистограмма 1+ и 2+ все участки вместе
pdf(file="hist_1year_2year.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm[ishodnik$age==1], breaks=seq(min(ishodnik$Length.mm), max(ishodnik$Length.mm), 0.1), main="1+", xlab="L,мм", ylab="N, экз.")
hist(ishodnik$Length.mm[ishodnik$age==2], breaks=seq(min(ishodnik$Length.mm), max(ishodnik$Length.mm), 0.1), main="2+", xlab="L,мм", ylab="N, экз.")
dev.off()
embedFonts("hist_1year_2year.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))

# гистограмма 1+ и 2+ эстуарий
pdf(file="hist_Estuary_1year_2year.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Estuary"], breaks=seq(min(ishodnik$Length.mm), max(ishodnik$Length.mm), 0.1), main="1+", xlab="L,мм", ylab="N, экз.")
hist(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="Estuary"], breaks=seq(min(ishodnik$Length.mm), max(ishodnik$Length.mm), 0.1), main="2+", xlab="L,мм", ylab="N, экз.")
dev.off()
embedFonts("hist_Estuary_1year_2year.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))

# гистограмма 1+ и 2+ горелый
pdf(file="hist_Goreliy_1year_2year.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Goreliy"], breaks=seq(min(ishodnik$Length.mm), max(ishodnik$Length.mm), 0.1), main="1+", xlab="L,мм", ylab="N, экз.")
hist(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="Goreliy"], breaks=seq(min(ishodnik$Length.mm), max(ishodnik$Length.mm), 0.1), main="2+", xlab="L,мм", ylab="N, экз.")
dev.off()
embedFonts("hist_Goreliy_1year_2year.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))

# гистограмма 1+ и 2+ ЗРС
pdf(file="hist_ZRS_1year_2year.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="ZRS"], breaks=seq(min(ishodnik$Length.mm), max(ishodnik$Length.mm), 0.1), main="1+", xlab="L,мм", ylab="N, экз.")
hist(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="ZRS"], breaks=seq(min(ishodnik$Length.mm), max(ishodnik$Length.mm), 0.1), main="2+", xlab="L,мм", ylab="N, экз.")
dev.off()
embedFonts("hist_ZRS_1year_2year.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))


#boxplot по годам
pdf(file="boxplot_length_in_year.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(ishodnik$Length.mm ~ ishodnik$age, main="длина раковины Macoma balthica разного возраста в июле 2012 года", xlab="возраст", ylab="L, мм")
dev.off()
embedFonts("boxplot_length_in_year.pdf") #встройка шрифтов в файл

#boxplot по участкам (1+)
pdf(file="boxplot_length_1+_area.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1], main="длина раковины Macoma balthica на разных участках", xlab="участок", ylab="L,мм")
dev.off()
embedFonts("boxplot_length_1+_area.pdf") #встройка шрифтов в файл

#boxplot по участкам (кольцо1)
pdf(file="boxplot_length_1kolco_area.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(ishodnik$k1 ~ ishodnik$area, main="длина первого кольца Macoma balthica на разных участках", xlab="участок", ylab="L,мм")
dev.off()
embedFonts("boxplot_length_1kolco_area.pdf") #встройка шрифтов в файл

#boxplot по возрасту 
pdf(file="boxplot_length_1kolco_age.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(ishodnik$k1 ~ ishodnik$age, main="длина первого кольца у разных генераций Macoma balthica", xlab="возраст", ylab="L,мм")
dev.off()
embedFonts("boxplot_length_1kolco_age.pdf") #встройка шрифтов в файл

#надо какую-то дисперсионку... Влияние на длину 1 кольца - участка, генерации (возраста)
shapiro.test(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area == "Estuary"])
shapiro.test(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area == "ZRS"])
shapiro.test(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area == "Goreliy"])

write.table(anova(lm(ishodnik$k1 ~ ishodnik$area * ishodnik$age)), file="anova_k1.csv", sep=";", dec=",")

#friedman.test()

# и дисперсионка влияние учатска на длину раковины 1+ в июле

write.table(anova(lm(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1])), file="anova_1+.csv", sep=";", dec=",")

#oneway.test(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1])
#kruskal.test(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1])

#пробую отделить 1+

#qnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==1]), sd=sd(ishodnik$Length.mm[ishodnik$age==1]),p=0.90)
#qnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==2]), sd=sd(ishodnik$Length.mm[ishodnik$age==2]),p=0.1281114)

pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==2]), sd=sd(ishodnik$Length.mm[ishodnik$age==2]),q=2)
  pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==1]), sd=sd(ishodnik$Length.mm[ishodnik$age==1]),q=2)

pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="Goreliy"]), sd=sd(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="Goreliy"]),q=2)
pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Goreliy"]), sd=sd(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Goreliy"]),q=2)

pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="Estuary"]), sd=sd(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="Estuary"]),q=2)
pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Estuary"]), sd=sd(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Estuary"]),q=2)

pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="ZRS"]), sd=sd(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="ZRS"]),q=2)
pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="ZRS"]), sd=sd(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="ZRS"]),q=2)

#pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==2]), sd=sd(ishodnik$Length.mm[ishodnik$age==2]),q=1.9)
#pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==1]), sd=sd(ishodnik$Length.mm[ishodnik$age==1]),q=1.9)

# проверяем равенство дисперсий
var.test(ishodnik$Length.mm[ishodnik$age==1], ishodnik$Length.mm[ishodnik$age==2])

# проверить нормальность распределений
shapiro.test(ishodnik$Length.mm[ishodnik$age==1])
shapiro.test(ishodnik$Length.mm[ishodnik$age==2])
