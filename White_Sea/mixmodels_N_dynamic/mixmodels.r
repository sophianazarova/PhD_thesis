setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/mixmodels_N_dynamic/")

ishodnik<-read.table("Kandalaksha_N_for_mixmodels.csv", header=T, sep=";", dec=",")
str(ishodnik)
attach(ishodnik)

# area - случайный  фактор
# возможные автокорреляции - для каждого сайта
# 
library(nlme)

# посчитаем средние численности для участков где больше одного горизонта
#install.packages("doBy")
library(doBy)

mean_N<-summaryBy(.~area+year, data=ishodnik, FUN=mean)
str(mean_N)
names(mean_N)<-names(ishodnik)[-2]
mean_N$winter<-mean(mean_N$January, mean_N$Febryary, mean_N$December
mean_N$
  
# строим модель для фиксированной части
f1 <- formula(N.1year ~ N.old + January + Febryary + Marts + April + May + June + July + August + September + October + November + December)

f2 <- formula(N.1year ~ N.old + January + May + August + November)

model_fix<-gls(f2, data=mean_N)
summary(model_fix)

# недостоверное влияние - кандидаты на выброс: February, Marts, April, June, August, September, October, November, December

# строим модель с учетом автокорреляций
model_autocorr<-gls(f2, data=mean_N, correlation = corAR1(form= ~year|area))
summary(model_autocorr)

plot(model_autocorr)

# сравниваем модели
AIC(model_fix, model_autocorr)

# добавляем случайный фактор
model_random1<-lme(f2, data=mean_N, random=~1|area, correlation=corAR1(form= ~year|area), method="REML")

model_random2<-lme(f2, data=mean_N, random=~1+N.old|area, correlation=corAR1(form= ~year|area), method="REML")

AIC(model_random1, model_random2, model_autocorr) # не имеет смысла


# убираем гетеростохастичность
plot(y=model_autocorr$residuals, x=mean_N$N.old)

vf1Fixed <- varFixed(~N.old + January + May + August + November)

model_autocorr_var1<-gls(f2, data=mean_N, correlation = corAR1(form= ~year|area), weights=vf1Fixed)
AIC(model_autocorr, model_autocorr_var1)
plot(model_autocorr_var1)

vf2_1<-varIdent(form= ~ 1 | January)
lmc <- lmeControl(niterEM = 2200, msMaxIter = 2200)
model_autocorr_var2_1<-gls(f2, data=mean_N, correlation = corAR1(form= ~year|area), weights=vf2_1, control=lmc)
plot(model_autocorr_var2_1)

vf2_5<-varIdent(form= ~ 1 | May)
lmc <- lmeControl(niterEM = 2200, msMaxIter = 2200)
model_autocorr_var2_5<-gls(f2, data=mean_N, correlation = corAR1(form= ~year|area), weights=vf2_5, control=lmc)
plot(model_autocorr_var2_5)

vf2_8<-varIdent(form= ~ 1 | August)
lmc <- lmeControl(niterEM = 2200, msMaxIter = 2200)
model_autocorr_var2_8<-gls(f2, data=mean_N, correlation = corAR1(form= ~year|area), weights=vf2_8, control=lmc)
plot(model_autocorr_var2_8)

vf2_11<-varIdent(form= ~ 1 | November)
lmc <- lmeControl(niterEM = 2200, msMaxIter = 2200)
model_autocorr_var2_11<-gls(f2, data=mean_N, correlation = corAR1(form= ~year|area), weights=vf2_11, control=lmc)
plot(model_autocorr_var2_11)

AIC(model_autocorr_var2_1, model_autocorr_var2_5, model_autocorr_var2_8, model_autocorr_var2_11)

install.packages("car")
library(car)

Anova(model_autocorr_var2_1)
summary(model_autocorr_var2_1)

#среднезимняя - ноябрь t-1 по 