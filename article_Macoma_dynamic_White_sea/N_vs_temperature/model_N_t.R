setwd("~/Dropbox/PhD_thesis/PhD_thesis/article_Macoma_dynamic_White_sea/N_vs_temperature/")

ishodnik <- read.csv2("Luvenga_N_temp.csv")
str(ishodnik)


# ========== весь массив вместе =================
library(car)
scatterplotMatrix(ishodnik)


mod1 <-lm(log(ishodnik$Nt1.sqmeter) ~ log(ishodnik$Nt.sqmeter) + ishodnik$wint_temp_t + ishodnik$summ_temp_t + ishodnik$site + ishodnik$year)

summary(mod1)

mod2 <- lm(log(ishodnik$Nt1.sqmeter) ~ log(ishodnik$Nt.sqmeter) + ishodnik$wint_temp_t + ishodnik$summ_temp_t + ishodnik$site)

summary(mod2)

anova(mod1, mod2)

mod3 <- lm(log(ishodnik$Nt1.sqmeter) ~ log(ishodnik$Nt.sqmeter) + ishodnik$wint_temp_t + ishodnik$summ_temp_t)
summary(mod3)
anova(mod4)

plot(mod3)

anova(mod1, mod3)

mod4 <- lm(log(ishodnik$Nt1.sqmeter) ~ log(ishodnik$Nt.sqmeter) + ishodnik$wint_temp_t)
summary(mod4)

#график


pdf("Twt1_vs_logNt1.pdf", family="NimbusSan")
ggplot(data=ishodnik, aes(x=wint_temp_t1, y=log(Nt1.sqmeter))) + geom_point(shape=1) + geom_smooth(method="lm", level=0.95) + theme_bw()
dev.off()
embedFonts("Twt1_vs_logNt1.pdf")

pdf("lodNt_vs_logNt1.pdf", family="NimbusSan")
ggplot(data=ishodnik, aes(x=log(Nt.sqmeter), y=log(Nt1.sqmeter))) + geom_point(shape=1) + geom_smooth(method="lm", level=0.95) + theme_bw()
dev.off()
embedFonts("lodNt_vs_logNt1.pdf")

anova(mod1, mod4)

durbinWatsonTest (mod4)

#валидность:
library(ggplot2)
c3_diag <- fortify(mod4)

pl_resid <- ggplot(c3_diag, aes(x = .fitted, y = .stdresid, size = .cooksd)) + 
  geom_point() + 
  geom_smooth(se=FALSE) + 
  geom_hline(eintercept=0)

pl_resid

# считаем Di = 4/(N−k−1)
4/length(ishodnik$Nt1.sqmeter-2-1)

#Проверяем на нормальность

qqPlot(mod4)

shapiro.test(mod4$residuals)


#Проверяем на гетероскедастичность

library(lmtest)
bptest(mod3)

# мультиколлинеарность
vif(mod3)


# проверка допущений автоматическими средствами R
#install.packages("gvlma")
library(gvlma)
gvlma(mod3)
pdf("gvlma.pdf")
plot.gvlma(gvlma(mod3))
dev.off()


# =============== Горелый =================
Gor <- subset(ishodnik, ishodnik$site == "Goreliy")

mod_gor <- lm(log(Nt1.sqmeter) ~ log(Nt.sqmeter) + wint_temp_t + summ_temp_t, data = Gor)

summary(mod_gor)

# =============== Горелый =================
Luv <- subset(ishodnik, ishodnik$site == "razrez2")

mod_luv <- lm(log(Nt1.sqmeter) ~ log(Nt.sqmeter) + wint_temp_t + summ_temp_t, data = Luv)

summary(mod_luv)

# =============== Эстуарий =================
Est <- subset(ishodnik, ishodnik$site == "Estuary")

mod_est <- lm(log(Nt1.sqmeter) ~ log(Nt.sqmeter) + wint_temp_t + summ_temp_t, data = Est)

summary(mod_est)
