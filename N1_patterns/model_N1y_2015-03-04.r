setwd("~/Dropbox/PhD_thesis/PhD_thesis/N1_patterns/")
ishodnik <- read.csv2("N1year_samples.csv")

install.packages("gridExtra")
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

