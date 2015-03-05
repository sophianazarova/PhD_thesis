setwd("~/Dropbox/PhD_thesis/PhD_thesis/N1_patterns/")

ishodnik <- read.csv2("N1year_samples.csv")
str(ishodnik)
summary(ishodnik$N1y)

library(mgcv)

ishodnik$lnN <- log(ishodnik$N1y + 1)
levels(ishodnik$site)


mod_full <- gam(N1y ~ 1 + s(year, by = as.numeric(site == "Estuary"), bs = "cr") + s(year, by = as.numeric(site == "Goreliy"), bs = "cr") + s(year, by = as.numeric(site == "Lomnishniy"), bs = "cr") + s(year, by = as.numeric(site == "razrez2"), bs = "cr") + s(year, by = as.numeric(site == "YuG"), bs = "cr") + s(year, by = as.numeric(site == "ZRS"), bs = "cr") + s(year, bs = "cr") + site, data = ishodnik, family = "nb", correlation = corAR1(form = ~ year | site))

summary(mod_full)

plot(mod_full, page=1) # рисуем smoothers

gam.check(mod_full) # все плохо - см. residuals

plot(ishodnik$year, residuals(mod_full, type = "pearson"))

plot(ishodnik$site, residuals(mod_full, type = "pearson"))

#dispersion
E1 <- residuals(mod_full, type = "pearson") #остатки
N <- nrow(ishodnik) #число точек в модели
p <- length(coef(mod_full)) #число параметров
sum(E1^2) / (N-p)

mod_full_res <- data.frame(year = ishodnik$year, site = ishodnik$site, fit = fitted(mod_full), resid = residuals(mod_full, type = "pearson"))

library(ggplot2)

ggplot(mod_full_res, aes(x = fit, y = resid)) + geom_point() + geom_smooth(se = F)

ggplot(mod_full_res, aes(x = year, y = resid)) + geom_point() + geom_smooth(se = F)

ggplot(mod_full_res, aes(x = site, y = resid)) + geom_boxplot()


mod_full2 <- gamm(N1y ~ 1 + s(year, bs = "cr"), data = ishodnik, family = "nb", correlation = corAR1(form = ~ year | site), random = list(site=~1))
