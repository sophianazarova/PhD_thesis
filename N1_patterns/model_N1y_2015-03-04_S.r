setwd("~/Dropbox/PhD_thesis/PhD_thesis/N1_patterns/")
ishodnik <- read.csv2("N1year_samples.csv")

library(gridExtra)
library(ggplot2)
library(mgcv)
 

# Полная модель 
mod_full <- gam(N1y ~ 1 + s(year, by = as.numeric(site == "Estuary"), bs = "cr") + s(year, by = as.numeric(site == "Goreliy"), bs = "cr") + s(year, by = as.numeric(site == "Lomnishniy"), bs = "cr") + s(year, by = as.numeric(site == "razrez2"), bs = "cr") + s(year, by = as.numeric(site == "YuG"), bs = "cr") + s(year, by = as.numeric(site == "ZRS"), bs = "cr")+ s(year, bs = "cr") + site  , data = ishodnik, family = "nb")

summary(mod_full)


# Редуцированная модель - выкидываем взаимодействия между year и  site
mod_red1 <- gam(N1y ~ 1 + s(year, bs = "cr") + site  , data = ishodnik, family = "nb")

summary(mod_red1)

AIC(mod_full, mod_red1)

anova(mod_full, mod_red1, test = "Chi")

# mod_red1 - более простая, и не отличается достоверно от полной модели, следовательно можно выкинуть взаимодействия!!! 

plot(mod_red1, page=1) # рисуем smoothers

# Рисуем предсказания редуцированой модели

ishodnik$mod_red1_pred <- (predict.gam(mod_red1, newdata = ishodnik, type = "response"))

ggplot(ishodnik, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=mod_red1_pred)) + facet_wrap(~site, ncol=2, scales = "free_y" )


# Рисуем предсказания полной модели

ishodnik$mod_full_pred <- (predict.gam(mod_full, newdata = ishodnik, type = "response"))

ggplot(ishodnik, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=mod_full_pred)) + facet_wrap(~site, ncol=2, scales = "free_y" )


# Обе модели на одном графике

ggplot(ishodnik, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=mod_full_pred), color = "blue") + geom_line(aes(x=year, y=mod_red1_pred)) + facet_wrap(~site, ncol=2, scales = "free_y" )


# Очень похожие предсказания моделей, следовательно останавливаемся на более простой.



# Проверяем валидность редуцировнной модели
gam.check(mod_red1, type = "pearson")

ggplot(data.frame(fit = fitted(mod_red1), res = resid(mod_red1, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = fit, y =res)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red1), res = resid(mod_red1, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = year, y =res)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red1), res = resid(mod_red1, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = fit, y =response)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red1), res = resid(mod_red1, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = site, y = res)) + geom_boxplot()


# Не сказать, чтобы best, но паттерна нет в первом графике нет, гетероскедастичность тоже не бросается в глаза.... 


# Итого, я бы сказал, что миром пополнения маком правят какие-то глобальные факторы. Надо теперь посмотреть что будет если в модель встроить температуру или что-то иное глобальное. 


# Можно еще поиграть с моделью, напимер, выкинув из нее сайт. Возможно, что это ничего не даст.

# убираем сайт

mod_red2 <- gam(N1y ~ 1 + s(year, bs = "cr"), data = ishodnik, family = "nb")

summary(mod_red2)

AIC(mod_full, mod_red1, mod_red2)

anova(mod_full, mod_red2, test = "Chi")

# не отличается от полной.

plot(mod_red2, page=1) # рисуем smoothers

# Рисуем предсказания редуцированой модели

ishodnik$mod_red2_pred <- (predict.gam(mod_red2, newdata = ishodnik, type = "response"))

ggplot(ishodnik, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=mod_red2_pred)) + facet_wrap(~site, ncol=2, scales = "free_y" )


# red2 & full models at one plot

ggplot(ishodnik, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=mod_full_pred), color = "blue") + geom_line(aes(x=year, y=mod_red2_pred)) + facet_wrap(~site, ncol=2, scales = "free_y" )

# вроде по ANOVA не отличается, но на графике все сильно съезжает, т.е. я бы не стала выкидывать сайт.


# ========= добавляем температуру =================
temp <- read.csv2("temp_Kandalaksha.csv")

#клеим температуры в год наблюдения (tau1) и за год до (tau)
ishodnik$tau <- ishodnik$year - 1
temp$tau <- temp$year - 1

ish_temp <- merge(ishodnik, temp, by = "tau", suffixes = c("", "_tau"))
str(ish_temp)

ish_temp$tau1 <- ish_temp$year
temp$tau1 <- temp$year

ish_temp <- merge(ish_temp, temp, by = "tau1", suffixes = c("_tau", "_tau1"))
str(ish_temp)
ish_temp$year <- ish_temp$year_tau

# вставляем температуры в модель : от весны, когда формировались гонады у родителей этих маком до момента наблюдения: от марта tau до июля tau1

temp_mod_full <- gam(N1y ~ 1 + s(year, by = as.numeric(site == "Estuary"), bs = "cr") + s(year, by = as.numeric(site == "Goreliy"), bs = "cr") + s(year, by = as.numeric(site == "Lomnishniy"), bs = "cr") + s(year, by = as.numeric(site == "razrez2"), bs = "cr") + s(year, by = as.numeric(site == "YuG"), bs = "cr") + s(year, by = as.numeric(site == "ZRS"), bs = "cr")+ s(year, bs = "cr") + site + MAR_tau + APR_tau + MAY_tau + JUN_tau + JUL_tau + AUG_tau + SEP_tau + OCT_tau + NOV_tau + DEC_tau + JAN_tau1 + FEB_tau1 + MAR_tau1 + APR_tau1 + MAY_tau1 + JUN_tau1 + JUL_tau1 , data = ish_temp, family = "nb")

summary(temp_mod_full)

# редуцируем модель - убираем взаимодействие между year и  site

temp_mod_red1 <- gam(N1y ~ 1 + s(year, bs = "cr") + site + MAR_tau + APR_tau + MAY_tau + JUN_tau + JUL_tau + AUG_tau + SEP_tau + OCT_tau + NOV_tau + DEC_tau + JAN_tau1 + FEB_tau1 + MAR_tau1 + APR_tau1 + MAY_tau1 + JUN_tau1 + JUL_tau1 , data = ish_temp, family = "nb")

summary(temp_mod_red1)

AIC(temp_mod_full, temp_mod_red1)

anova(temp_mod_full, temp_mod_red1, test = "Chi")


#оставляем только те месяцы, которые достоверно влияют: MAY_tau, JUL_tau, AUG_tau, SEP_tau, OCT_tau, NOV_tau, DEC_tau, MAR_tau1

temp_mod_red2 <- gam(N1y ~ 1 + s(year, bs = "cr") + site + MAY_tau + JUL_tau + AUG_tau + SEP_tau + OCT_tau + NOV_tau + DEC_tau + MAR_tau1  , data = ish_temp, family = "nb")

summary(temp_mod_red2)

AIC(temp_mod_full, temp_mod_red2)

anova(temp_mod_full, temp_mod_red2, test = "Chi")

# не отличается от полной.

plot(temp_mod_red2, page=1) # рисуем smoothers

# Рисуем предсказания редуцированой модели

ish_temp$temp_mod_red2_pred <- (predict.gam(temp_mod_red2, newdata = ish_temp, type = "response"))

str(ish_temp)
ggplot(ish_temp, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=temp_mod_red2_pred)) + facet_wrap(~site, ncol=2, scales = "free_y" )


# Рисуем предсказания полной модели

ish_temp$temp_mod_full_pred <- (predict.gam(temp_mod_full, newdata = ish_temp, type = "response"))

ggplot(ish_temp, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=temp_mod_full_pred)) + facet_wrap(~site, ncol=2, scales = "free_y" )


# Обе модели на одном графике

ggplot(ish_temp, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=temp_mod_full_pred), color = "blue") + geom_line(aes(x=year, y=temp_mod_red2_pred)) + facet_wrap(~site, ncol=2, scales = "free_y" )


# Тут вроде как достаточно похоже остается

# Проверяем валидность редуцировнной модели
gam.check(temp_mod_red2, type = "pearson")

ggplot(data.frame(fit = fitted(mod_red1), res = resid(mod_red1, type = "pearson"), year = ish_temp$year, response = ish_temp$N1y, site = ish_temp$site), aes (x = fit, y =res)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red1), res = resid(mod_red1, type = "pearson"), year = ish_temp$year, response = ish_temp$N1y, site = ish_temp$site), aes (x = year, y =res)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red1), res = resid(mod_red1, type = "pearson"), year = ish_temp$year, response = ish_temp$N1y, site = ish_temp$site), aes (x = fit, y =response)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red1), res = resid(mod_red1, type = "pearson"), year = ish_temp$year, response = ish_temp$N1y, site = ish_temp$site), aes (x = site, y = res)) + geom_boxplot()

# сравниваем модели без температуры и с температурой
AIC(mod_red1, temp_mod_red2) #вроде с температурой чуть-чуть лучше

anova(mod_red1, temp_mod_red2, test = "Chi") # хотя так они как будто бы и не отличаются...
