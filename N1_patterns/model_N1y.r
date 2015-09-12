
ishodnik <- read.csv2("N1year_samples.csv")

library(gridExtra)
library(ggplot2)
library(mgcv)
library(lme4)
library(car)

# Модель динамики численност 1+
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


# Не сказать, чтобы best, но паттерна в первом графике нет, гетероскедастичность тоже не бросается в глаза.... 


# Итого, я бы сказал, что миром пополнения маком правят какие-то глобальные факторы. Надо теперь посмотреть что будет если в модель встроить температуру или что-то иное глобальное. 


# Можно еще поиграть с моделью, напимер, выкинув из нее сайт. Возможно, что это ничего не даст.


mod_red2 <- gam(N1y ~ 1 + s(year, bs = "cr") , data = ishodnik, family = "nb")

plot(mod_red2)

anova(mod_red2, mod_red1, test ="Chi")


ishodnik$mod_red2_pred <- (predict.gam(mod_red2, newdata = ishodnik, type = "response"))


# Три модели на одном графике

ggplot(ishodnik, aes(x=year, y =N1y)) + geom_point() + geom_line(aes(x=year, y=mod_full_pred), color = "blue") + geom_line(aes(x=year, y=mod_red1_pred)) + geom_line(aes(x=year, y=mod_red2_pred), color = "green") + facet_wrap(~site, ncol=2, scales = "free_y" )




# Проверяем валидность еще более редуцировнной модели
gam.check(mod_red2, type = "pearson")

ggplot(data.frame(fit = fitted(mod_red2), res = resid(mod_red2, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = fit, y =res)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red2), res = resid(mod_red2, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = year, y =res)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red2), res = resid(mod_red2, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = fit, y =response)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_red2), res = resid(mod_red2, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = site, y = res)) + geom_boxplot()



# Нее... отменить! Сайт оставляем





# ЧАСТЬ 2. Играем с температурой


temp <- read.csv("temp_Kandalaksha_corrected.csv", sep=";", header=TRUE)
str(temp)
temp <- numeric(temp)

NT <- merge(ishodnik, temp, by="year")
NT_scaled <- NT
NT_scaled[, 5:16] <- scale(NT[, 5:16])

head(NT_scaled)

summary(NT_scaled[,15])

# Удаляем мультколлинеарные предикторы

vif(lm(N1y ~ JAN +  FEB  + MAR + APR + MAY + JUN + JUL  + AUG + SEP_py + OCT_py + NOV_py + DEC_py, data = NT_scaled))

vif(lm(N1y ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + SEP_py + OCT_py + NOV_py + DEC_py, data = NT_scaled))

vif(lm(N1y ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + OCT_py + NOV_py + DEC_py, data = NT_scaled))

vif(lm(N1y ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + OCT_py + DEC_py, data = NT_scaled))




# Подбираем оптимальную случайную часть модели

mod_NT_full <- glmer.nb(N1y ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + OCT_py + DEC_py + (1|site) + (1|year) , data = NT_scaled)



mod_NT_full2 <- glmer.nb(N1y ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + OCT_py + DEC_py + (1|site), data = NT_scaled)


mod_NT_full3 <- glmer.nb(N1y ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + OCT_py + DEC_py + (1|year), data = NT_scaled)



AIC(mod_NT_full, mod_NT_full2, mod_NT_full3)

anova(mod_NT_full3, mod_NT_full)

# Случайную часть модели сокращать нельзя!


summary(mod_NT_full)


ggplot(data.frame(res = residuals(mod_NT_full, type ="pearson"), fit = fitted(mod_NT_full), NT), aes(x=fit, y=res)) + geom_point()+geom_smooth(se=F) 

ggplot(data.frame(res = residuals(mod_NT_full, type ="pearson"), fit = fitted(mod_NT_full), NT), aes(x=year, y=res)) + geom_point()+geom_smooth(se=F) 


ggplot(data.frame(res = residuals(mod_NT_full, type ="pearson"), fit = fitted(mod_NT_full), NT), aes(x=DEC_py, y=res)) + geom_point()+geom_smooth(se=F) 


ggplot(data.frame(res = residuals(mod_NT_full, type ="pearson"), fit = fitted(mod_NT_full), NT), aes(x=JUL, y=res)) + geom_point()+geom_smooth(se=F) 

ggplot(data.frame(res = residuals(mod_NT_full, type ="pearson"), fit = fitted(mod_NT_full), NT), aes(x=MAY, y=res)) + geom_point()+geom_smooth(se=F) 


ggplot(data.frame(res = residuals(mod_NT_full, type ="pearson"), fit = fitted(mod_NT_full), NT), aes(x=site, y=res)) + geom_boxplot() 



# Сокращаем фиксированную часть модели

mod_NT_red1 <- glmer.nb(N1y ~ JAN +  FEB + MAY + JUN + JUL  + AUG + OCT_py + DEC_py + (1|site) + (1|year) , data = NT_scaled)


anova(mod_NT_full, mod_NT_red1)
summary(mod_NT_red1)


mod_NT_red2 <- glmer.nb(N1y ~ JAN +  FEB + MAY + JUL  + AUG + OCT_py + DEC_py + (1|site) + (1|year) , data = NT_scaled)

anova(mod_NT_red1, mod_NT_red2)

summary(mod_NT_red2)


mod_NT_red3 <- glmer.nb(N1y ~ JAN +  MAY + JUL  + AUG + OCT_py + DEC_py + (1|site) + (1|year) , data = NT_scaled)

anova(mod_NT_red2, mod_NT_red3)

summary(mod_NT_red3)

mod_NT_red4 <- glmer.nb(N1y ~ JAN +  MAY + JUL  + OCT_py + DEC_py + (1|site) + (1|year) , data = NT_scaled)

anova(mod_NT_red3, mod_NT_red4)

summary(mod_NT_red4)


# Проверяем валидность модели mod_NT_red4

ggplot(data.frame(fit = fitted(mod_NT_red4), res = resid(mod_NT_red4, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = fit, y =res)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_NT_red4), res = resid(mod_NT_red4, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = year, y =res)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_NT_red4), res = resid(mod_NT_red4, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = fit, y =response)) + geom_point() + geom_smooth(se=FALSE)

ggplot(data.frame(fit = fitted(mod_NT_red4), res = resid(mod_NT_red4, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, site = ishodnik$site), aes (x = site, y = res)) + geom_boxplot()


ggplot(data.frame(fit = fitted(mod_NT_red4), res = resid(mod_NT_red4, type = "pearson"), year = ishodnik$year, response = ishodnik$N1y, JUL = NT_scaled$JUL, site = ishodnik$site), aes (x = JUL, y = response)) + geom_point() + geom_smooth(method = "lm", se=FALSE)



# Проводим анализ по log трансформироаннным данным

NT_scaled$logN <- log(NT_scaled$N1y + 1)

mod_logNT_full <- lme(logN ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + OCT_py + DEC_py, random = list(year = ~ 1, site = ~ 1), data = NT_scaled)





mod_logNT_full2 <- lme(logN ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + OCT_py + DEC_py, random = list( site = ~ 1), data = NT_scaled)



mod_logNT_full3 <- lme(logN ~ JAN +  FEB  + MAR + MAY + JUN + JUL  + AUG + OCT_py + DEC_py, random = list(year = ~ 1), data = NT_scaled)


AIC(mod_logNT_full, mod_logNT_full2, mod_logNT_full3)


ggplot(data.frame(res = residuals(mod_logNT_full, type ="pearson"), fit = fitted(mod_logNT_full), NT), aes(x=fit, y=res)) + geom_point()+geom_smooth(se=F) 

ggplot(data.frame(res = residuals(mod_logNT_full, type ="pearson"), fit = fitted(mod_logNT_full), NT), aes(x=year, y=res)) + geom_point()+geom_smooth(se=F) 


ggplot(data.frame(res = residuals(mod_logNT_full, type ="pearson"), fit = fitted(mod_logNT_full), NT), aes(x=DEC_py, y=res)) + geom_point()+geom_smooth(se=F) 


ggplot(data.frame(res = residuals(mod_logNT_full, type ="pearson"), fit = fitted(mod_logNT_full), NT), aes(x=JUL, y=res)) + geom_point()+geom_smooth(se=F) 

ggplot(data.frame(res = residuals(mod_logNT_full, type ="pearson"), fit = fitted(mod_logNT_full), NT), aes(x=MAY, y=res)) + geom_point()+geom_smooth(se=F) 


ggplot(data.frame(res = residuals(mod_logNT_full, type ="pearson"), fit = fitted(mod_logNT_full), NT), aes(x=site, y=res)) + geom_boxplot() 



summary(mod_logNT_full)

mod_logNT_full_ML <- update(mod_logNT_full, method = "ML")

drop1(mod_logNT_full_ML, test = "Chi")


mod_logNT_red1_ML <- update(mod_logNT_full_ML, .~.-FEB)

drop1(mod_logNT_red1_ML, test = "Chi")


mod_logNT_red2_ML <- update(mod_logNT_red1_ML, .~.-JUN)

drop1(mod_logNT_red2_ML, test = "Chi")


mod_logNT_red3_ML <- update(mod_logNT_red2_ML, .~.-MAR)
drop1(mod_logNT_red3_ML, test = "Chi")


mod_logNT_red4_ML <- update(mod_logNT_red3_ML, .~.-AUG)
drop1(mod_logNT_red4_ML, test = "Chi")

summary(mod_logNT_red4_ML)

mod_logNT_red4 <- update(mod_logNT_red4_ML, method = "REML")

plot(mod_logNT_red4)

qqPlot(resid(mod_logNT_red4, type = "pearson"))
hist(resid(mod_logNT_red4, type = "pearson"))




# Смотрим на соотвествие динамики и хода температуры 

ggplot(ishodnik, aes(x=year, y =log(N1y+1))) + geom_point() + geom_line(aes(x=year, y=log(mod_red1_pred+1))) + geom_line(data = temp, aes(x=year, y=JUL), color = "blue") + facet_wrap(~site, ncol=2, scales = "free_y" )



# Строим модель для среднесезонных температур

winter <- c( "JAN",    "FEB",  "DEC_py")
spring <- c( "MAR", "APR" ,   "MAY")
summer <- c("JUN" ,   "JUL" ,   "AUG")
fall <-  c( "SEP_py" , "OCT_py", "NOV_py")

season_temp <- data.frame(year= temp$year, winter = apply(temp[, winter], 1, mean), spring = apply(temp[, spring], 1, mean), summer = apply(temp[, summer], 1, mean), fall = apply(temp[, fall], 1, mean))
temp[, winter]



NT_season <- merge(ishodnik, season_temp, by="year")
NT_season_scaled <- NT_season

head(NT_season_scaled)

NT_season_scaled[, 5:11] <- scale(NT_season[, 5:11])

head(NT_season_scaled)


# Смотрим есть ли мультколлинеарные предикторы

vif(lm(N1y ~ winter + spring + summer + fall, data = NT_season_scaled))



# Подбираем оптимальную случайную часть модели

mod_NT_season_full <- glmer.nb(N1y ~ winter + spring + summer + fall + (1|site) + (1|year) , data = NT_season_scaled)



mod_NT_season_full2 <- glmer.nb(N1y ~ winter + spring + summer + fall + (1|site) , data = NT_season_scaled)


mod_NT_season_full3 <- glmer.nb(N1y ~ winter + spring + summer + fall  + (1|year) , data = NT_season_scaled)


AIC(mod_NT_season_full, mod_NT_season_full2, mod_NT_season_full3)

summary(mod_NT_season_full)

mod_NT_season_red1 <- glmer.nb(N1y ~ winter + spring + summer + (1|site) + (1|year) , data = NT_season_scaled)

anova(mod_NT_season_full, mod_NT_season_red1)
summary(mod_NT_season_red1)

mod_NT_season_red2 <- glmer.nb(N1y ~ spring + summer + (1|site) + (1|year) , data = NT_season_scaled)

anova( mod_NT_season_red1, mod_NT_season_red2)
summary( mod_NT_season_red2)

mod_NT_season_red3 <- glmer.nb(N1y ~ summer + (1|site) + (1|year) , data = NT_season_scaled)

anova( mod_NT_season_red2, mod_NT_season_red3)
summary( mod_NT_season_red3)


# Смотрим на соотвествие динамики и хода температуры 
head(ishodnik)

ggplot(ishodnik, aes(x=year, y =log(N1y+1))) + geom_point() + geom_line(aes(x=year, y=log(mod_red1_pred+1))) + geom_line(data = season_temp, aes(x=year, y=(winter)), color = "blue") + facet_wrap(~site, ncol=2, scales = "free_y" )
