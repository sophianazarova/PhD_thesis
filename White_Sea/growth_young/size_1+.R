setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/growth_young/")
ishodnik<-read.table(file="growth_young_Kandalaksha.csv", sep=";", dec=",", head=T)
ishodnik<-subset(ishodnik, subset=ishodnik$Length.mm<=3)
ishodnik$tidal_level<-ordered(ishodnik$tidal_level, levels=c("high", "middle", "Mid-low", "low"))

str(ishodnik)
hist(ishodnik$k1)
hist(ishodnik$k1[ishodnik$age==1])
summary(ishodnik)

#смотрим, сколько измерено штук молоди на разных участках всего и разного возраста
table(ishodnik$area, ishodnik$tidal_level)
table(ishodnik$area[ishodnik$age==0], ishodnik$tidal_level[ishodnik$age==0])
table(ishodnik$area[ishodnik$age==1], ishodnik$tidal_level[ishodnik$age==1])
table(ishodnik$area[ishodnik$age==2], ishodnik$tidal_level[ishodnik$age==2])


length(ishodnik$Length.mm)

#гистограмма  по возрастам все участки
pdf(file="hist_obili_po_godam.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm, breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="A", xlab="L, мм", ylab="N, экз.")
hist(ishodnik$age, breaks=seq(min(ishodnik$age, na.rm=T), max(ishodnik$age, na.rm=T), 1), main="B", xlab="возраст, годы", ylab="N, экз.")
dev.off()
embedFonts("hist_obili_po_godam.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))

# на Горелом надо сравнить 1+ особей по размерам. 
# Наверное, правильно будет сравнить, принадлежат ли они одной генеральной совокупности...
# то есть нужен хи-квадрат - нет, хи-квадрат нельзя, т.к. надо в каждой ячейке больше 5 и без нулей.:(
#отбираем только Горелый
(Goreliy.ish<-subset(ishodnik, ishodnik$area=="Goreliy"))
summary(Goreliy.ish$Length.mm[Goreliy.ish$age==1])
#делаем матрицу с размерной структурой годовалых
#Goreliy.size.str.1<-as.matrix(table(Goreliy.ish$Length.mm[Goreliy.ish$age==1], Goreliy.ish$tidal_level[Goreliy.ish$age==1]))

# считаем дисперсионку по горизонтам
str(Goreliy.ish)
# проверка на нормальность, ничерта не нормально - почему-то. Может быть, маленькие выборки?
shapiro.test(Goreliy.ish$Length.mm[Goreliy.ish$age==1 & Goreliy.ish$tidal_level=="high"])
shapiro.test(Goreliy.ish$Length.mm[Goreliy.ish$age==1 & Goreliy.ish$tidal_level=="middle"])
shapiro.test(Goreliy.ish$Length.mm[Goreliy.ish$age==1 & Goreliy.ish$tidal_level=="Mid-low"])
shapiro.test(Goreliy.ish$Length.mm[Goreliy.ish$age==1 & Goreliy.ish$tidal_level=="low"])

# посчитаем непараметрику
kruskal.test(Goreliy.ish$Length.mm[Goreliy.ish$age==1] ~ Goreliy.ish$tidal_level[Goreliy.ish$age==1])

# 0+ и 2+
kruskal.test(Goreliy.ish$Length.mm[Goreliy.ish$age==0] ~ Goreliy.ish$tidal_level[Goreliy.ish$age==0])
kruskal.test(Goreliy.ish$Length.mm[Goreliy.ish$age==2] ~ Goreliy.ish$tidal_level[Goreliy.ish$age==2])

#anova(lm(Goreliy.ish$Length.mm[Goreliy.ish$age==1] ~ Goreliy.ish$tidal_level[Goreliy.ish$age==1]))

#boxplot Горелый по горизонтам (1+)
pdf(file="boxplot_Goreliy_length_1+_tidal.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(Goreliy.ish$Length.mm[Goreliy.ish$age==1] ~ Goreliy.ish$tidal_level[Goreliy.ish$age==1], main="длина раковины Macoma balthica на разных горизонтах литорали о. Горелый", xlab="горизонт литорали", ylab="L,мм")
dev.off()
embedFonts("boxplot_Goreliy_length_1+_tidal.pdf") #встройка шрифтов в файл

#проверим на нормальность весь Горелый вместе и другие участки
shapiro.test(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area == "Estuary"])
shapiro.test(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area == "ZRS"])
shapiro.test(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area == "Goreliy"])

#опять надо непараметрику
kruskal.test(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1])
TukeyHSD(aov(lm(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1])))

#0+ и 2+
kruskal.test(ishodnik$Length.mm[ishodnik$age==0] ~ ishodnik$area[ishodnik$age==0])
kruskal.test(ishodnik$Length.mm[ishodnik$age==2] ~ ishodnik$area[ishodnik$age==2])

#boxplot по участкам (1+)
pdf(file="boxplot_length_1age_area.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1], main="длина раковины Macoma balthica на разных участках", xlab="участок", ylab="L,мм")
dev.off()
embedFonts("boxplot_length_1age_area.pdf") #встройка шрифтов в файл


##разделение для Эстуария
#Эстуарий деалем исходник для размрной структуры
Estuary_size_str<-as.matrix(table(ishodnik$Length.mm[ishodnik$area=="Estuary"], ishodnik$age[ishodnik$area=="Estuary"]))

# Эстуарий гистограмма размерная структура по возрастам
pdf(file="hist_012+_estuary.pdf", family="NimbusSan") # указываем шрифт подпией
barplot(t(Estuary_size_str[,1:3]), beside=T,space=c(0,0.2),col=c("grey","black","white"))
legend(legend=c("0+", "1+", "2+"),x=41, y=32, fill=c("grey","black","white"))
dev.off()
embedFonts("hist_012+_estuary.pdf") #встройка шрифтов в файл
#locator()

# Эстуарий считаем средние размеры
(L_mean_age_Estuary<-tapply(ishodnik$Length.mm[ishodnik$area=="Estuary"], ishodnik$age[ishodnik$area=="Estuary"], mean))
(L_sem_age_Estuary<-tapply(ishodnik$Length.mm[ishodnik$area=="Estuary"], ishodnik$age[ishodnik$area=="Estuary"], sd)/
  tapply(ishodnik$Length.mm[ishodnik$area=="Estuary"], ishodnik$age[ishodnik$area=="Estuary"], length))

##разделение для Горелого
#Горелый деалем исходник для размрной структуры
Goreliy_size_str<-as.matrix(table(ishodnik$Length.mm[ishodnik$area=="Goreliy"], ishodnik$age[ishodnik$area=="Goreliy"]))

# Горелый гистограмма размерная структура по возрастам
pdf(file="hist_012+_Goreliy.pdf", family="NimbusSan") # указываем шрифт подпией
barplot(t(Goreliy_size_str[,1:3]), beside=T,space=c(0,0.2),col=c("grey","black","white"))
legend(legend=c("0+", "1+", "2+"),x=41, y=32, fill=c("grey","black","white"))
dev.off()
embedFonts("hist_012+_Goreliy.pdf") #встройка шрифтов в файл
#locator()

# Горелый считаем средние размеры
(L_mean_age_Goreliy<-tapply(ishodnik$Length.mm[ishodnik$area=="Goreliy"], ishodnik$age[ishodnik$area=="Goreliy"], mean))
(L_sem_age_Goreliy<-tapply(ishodnik$Length.mm[ishodnik$area=="Goreliy"], ishodnik$age[ishodnik$area=="Goreliy"], sd)/
   tapply(ishodnik$Length.mm[ishodnik$area=="Goreliy"], ishodnik$age[ishodnik$area=="Goreliy"], length))

##разделение для ЗРС
#ЗРСй деалем исходник для размрной структуры
ZRS_size_str<-as.matrix(table(ishodnik$Length.mm[ishodnik$area=="ZRS"], ishodnik$age[ishodnik$area=="ZRS"]))

# ЗРС гистограмма размерная структура по возрастам
pdf(file="hist_012+_ZRS.pdf", family="NimbusSan") # указываем шрифт подпией
barplot(t(ZRS_size_str[,1:3]), beside=T,space=c(0,0.2),col=c("grey","black","white"))
legend(legend=c("0+", "1+", "2+"),x=41, y=32, fill=c("grey","black","white"))
dev.off()
embedFonts("hist_012+_ZRS.pdf") #встройка шрифтов в файл
#locator()

# ЗРС считаем средние размеры
(L_mean_age_ZRS<-tapply(ishodnik$Length.mm[ishodnik$area=="ZRS"], ishodnik$age[ishodnik$area=="ZRS"], mean))
(L_sem_age_ZRS<-tapply(ishodnik$Length.mm[ishodnik$area=="ZRS"], ishodnik$age[ishodnik$area=="ZRS"], sd)/
   tapply(ishodnik$Length.mm[ishodnik$area=="ZRS"], ishodnik$age[ishodnik$area=="ZRS"], length))


# гистограмма 1+ и 2+ все участки вместе
pdf(file="hist_1year_2year.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm[ishodnik$age==1], breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="1+", xlab="L,мм", ylab="N, экз.")
hist(ishodnik$Length.mm[ishodnik$age==2], breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="2+", xlab="L,мм", ylab="N, экз.")
dev.off()
embedFonts("hist_1year_2year.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))

# гистограмма 1+ и 2+ эстуарий
pdf(file="hist_Estuary_1year_2year.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Estuary"], breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="1+", xlab="L,мм", ylab="N, экз.")
hist(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="Estuary"], breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="2+", xlab="L,мм", ylab="N, экз.")
dev.off()
embedFonts("hist_Estuary_1year_2year.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))

# гистограмма 1+ и 2+ горелый
pdf(file="hist_Goreliy_1year_2year.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Goreliy"], breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="1+", xlab="L,мм", ylab="N, экз.")
hist(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="Goreliy"], breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="2+", xlab="L,мм", ylab="N, экз.")
dev.off()
embedFonts("hist_Goreliy_1year_2year.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))

# гистограмма 1+ и 2+ ЗРС
pdf(file="hist_ZRS_1year_2year.pdf", family="NimbusSan") # указываем шрифт подпией
par(mfrow=c(2,1))
hist(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="ZRS"], breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="1+", xlab="L,мм", ylab="N, экз.")
hist(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$area=="ZRS"], breaks=seq(min(ishodnik$Length.mm, na.rm=T), max(ishodnik$Length.mm, na.rm=T), 0.1), main="2+", xlab="L,мм", ylab="N, экз.")
dev.off()
embedFonts("hist_ZRS_1year_2year.pdf") #встройка шрифтов в файл
par(mfrow=c(1,1))


#boxplot по годам
pdf(file="boxplot_length_in_year.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(ishodnik$Length.mm ~ ishodnik$age, main="длина раковины Macoma balthica разного возраста в июле", xlab="возраст", ylab="L, мм")
dev.off()
embedFonts("boxplot_length_in_year.pdf") #встройка шрифтов в файл



#boxplot по участкам + горизонтам (1+)
pdf(file="boxplot_length_1+_tidal_area.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1] + ishodnik$tidal_level[ishodnik$age==1], main="длина раковины Macoma balthica на разных участках", xlab="участок", ylab="L,мм")
dev.off()
embedFonts("boxplot_length_1+_tidal_area.pdf") #встройка шрифтов в файл

#boxplot по горизонтам (1+)
pdf(file="boxplot_length_1+_tidal.pdf", family="NimbusSan") # указываем шрифт подпией
boxplot(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$tidal_level[ishodnik$age==1], main="длина раковины Macoma balthica на разных горизонтах литорали", xlab="горизонт литорали", ylab="L,мм")
dev.off()
embedFonts("boxplot_length_1+_tidal.pdf") #встройка шрифтов в файл


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


write.table(anova(lm(ishodnik$k1 ~ ishodnik$area * ishodnik$age)), file="anova_k1.csv", sep=";", dec=",")

#friedman.test()

# и дисперсионка влияние учатска на длину раковины 1+ в июле

write.table(anova(lm(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1])), file="anova_1+.csv", sep=";", dec=",")

# и дисперсионка влияние учатска и горизонта на длину раковины 1+ в июле
write.table(anova(lm(ishodnik$Length.mm[ishodnik$age==1] ~ ishodnik$area[ishodnik$age==1] * ishodnik$tidal_level[ishodnik$age==1])), file="anova_1+.csv", sep=";", dec=",")

# и дисперсионка влияние горизонта на длину раковины 1+ отдельно на Горелом
write.table(anova(lm(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$area=="Goreliy"] ~ ishodnik$tidal_level[ishodnik$age==1 & ishodnik$area=="Goreliy"])), file="anova_1+.csv", sep=";", dec=",")


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
