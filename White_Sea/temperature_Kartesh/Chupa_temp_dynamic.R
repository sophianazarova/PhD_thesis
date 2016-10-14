setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/temperature_Kartesh/")

ishodnik <- read.csv2("temperature_1979-2010.csv")

str(ishodnik)

# =========== делаем даты датами ===============
# склеиваем из в форму, читаемую функцией as.Date
ishodnik$date <- as.factor(paste(ishodnik$year, ishodnik$month, ishodnik$day, sep = "-"))
ishodnik$date <- as.Date(ishodnik$date, "%Y-%m-%d")
str(ishodnik$date)

#в другой формат 
ishodnik$date <- as.POSIXlt(ishodnik$date, format = "%Y-%m-%d", tz = "") 

# ============= среднемесячные температуры ================

#делаем колонку месяц-год
ishodnik$ym <- format(ishodnik$date,"%Y-%m")

#считаем средние
m0_month_mean <- aggregate(m0 ~ ym, data = ishodnik, FUN = mean, na.rm=T)

#дописываем даты, чтобы перевести в дата-формат
m0_month_mean$date <- paste(m0_month_mean$ym, "-01", sep="")
m0_month_mean$date <- factor(m0_month_mean$date)

m0_month_mean$date <- as.POSIXlt(m0_month_mean$date, format = "%Y-%m-%d") 

m0_month_mean$date <- as.Date(m0_month_mean$date, format = "%Y-%m-%d") 

str(m0_month_mean)

# рисуем среднемесячные
pdf("t_mean_month.pdf", fonts = "NimbusSan")
plot(m0_month_mean$date, m0_month_mean$m0, type="l", xlab = "year", ylab = "t, C")
dev.off()
embedFonts("t_mean_month.pdf")


# ========== выясняем про самые холодные месяцы =========
str(m0_month_mean)
#делаем колонку месяц
m0_month_mean$month <- format(m0_month_mean$date,"%m")

# смотрим на график по месяцам - вроде как холодные самые январь-февраль
plot(m0_month_mean$date[m0_month_mean$month == "01"], m0_month_mean$m0[m0_month_mean$month == "01"], type="l", xlab = "year", ylab = "t, C", ylim = c(-2,2))
lines(m0_month_mean$date[m0_month_mean$month == "03"], m0_month_mean$m0[m0_month_mean$month == "03"], col = "cyan")
lines(m0_month_mean$date[m0_month_mean$month == "04"], m0_month_mean$m0[m0_month_mean$month == "04"], col = "orange")
lines(m0_month_mean$date[m0_month_mean$month == "12"], m0_month_mean$m0[m0_month_mean$month == "12"], col = "red")

#считаем средние за два самых холодных месяца - январь и февраль

mo_coldest_monthes <- subset(m0_month_mean, subset = m0_month_mean$month%in%c("01", "02"))
mo_coldest_monthes$y <- format(mo_coldest_monthes$date,"%Y")

m0_coldest_mean <- aggregate(m0 ~ y, data = mo_coldest_monthes, FUN = mean, na.rm=T)
write.table(m0_coldest_mean, file = "Chupa_mean_temp_Jan_Feb.csv", dec = ",", sep = ";")

# ============= среднегодовые температуры ================

#считаем средние
m0_year_mean <- aggregate(m0 ~ year, data = ishodnik, FUN = mean, na.rm=T)

#дописываем даты, чтобы перевести в дата-формат
m0_year_mean$date <- paste(m0_year_mean$year, "-01-01", sep="")
m0_year_mean$date <- factor(m0_year_mean$date)

m0_year_mean$date <- as.POSIXlt(m0_year_mean$date, format = "%Y-%m-%d") 

# рисуем среднегодовые
pdf("t_mean_year.pdf", fonts = "NimbusSan")
plot(m0_year_mean$date, m0_year_mean$m0, type="l", xlab = "year", ylab = "t, C")
dev.off()
embedFonts("t_mean_year.pdf")

# со среднемноголетней
plot(m0_year_mean$date, m0_year_mean$m0, type="l")
abline(h = mean(ishodnik$m0, na.rm=T), col=2)
abline(h = mean(m0_year_mean$m0, na.rm=T),col=3)

#пишем в файл
write.table(m0_year_mean, "Chupa_temp_year_mean.csv", sep = ";", dec = ",")

# =============== сезонные температуры ======================
#install.packages("seas")
library(seas)

str(ishodnik)
ishodnik$date <- as.Date(ishodnik$date, "%Y-%m-%d")
ishodnik$season <- mkseas(x = ishodnik, width = "DJF")

m0_season_mean <- aggregate(m0 ~ season + year, data = ishodnik, FUN = mean, na.rm=T)

m0_season_nobs <- tapply(ishodnik$m0, list(ishodnik$season, ishodnik$year), FUN = length)

# winter
plot(m0_season_mean$year[ m0_season_mean$season == "DJF"], m0_season_mean$m0[ m0_season_mean$season == "DJF"], type="l")

#summer
plot(m0_season_mean$year[ m0_season_mean$season == "JJA"], m0_season_mean$m0[ m0_season_mean$season == "JJA"], type="l")

#all seasons
pdf("t_mean_season_year.pdf", fonts = "NimbusSan")
plot(m0_season_mean$year, m0_season_mean$m0, type = "n", xlab = "годы", ylab = "t, C")
lines(m0_season_mean$year[ m0_season_mean$season == "DJF"], m0_season_mean$m0[ m0_season_mean$season == "DJF"], col="blue")
lines(m0_season_mean$year[ m0_season_mean$season == "MAM"], m0_season_mean$m0[ m0_season_mean$season == "MAM"], col="green")
lines(m0_season_mean$year[ m0_season_mean$season == "JJA"], m0_season_mean$m0[ m0_season_mean$season == "JJA"], col="red")
lines(m0_season_mean$year[ m0_season_mean$season == "SON"], m0_season_mean$m0[ m0_season_mean$season == "SON"], col="orange")
lines(m0_year_mean$year, m0_year_mean$m0)
#n <- c("зима", "весна", "лето", "осень", "среднегодовая")
#legend(x="topleft", legend = n, lty=1, col = c("blue", "green", "red", "orange", "black"))
dev.off()
embedFonts("t_mean_season_year.pdf")

# пишем в файл
write.table(m0_season_mean, "Chupa_t_season_mean.csv", dec=",", sep=";")

# ============= средняя для слоя 0-50 метров для сравнения с Баренцевым ===========
ishodnik$m0_50 <- rowMeans(ishodnik[,4:9], na.rm = T)

# ============= среднегодовые температуры  0-50 m ================

#считаем средние
m0_50_year_mean <- aggregate(m0_50 ~ year, data = ishodnik, FUN = mean, na.rm=T)

#дописываем даты, чтобы перевести в дата-формат
m0_50_year_mean$date <- paste(m0_50_year_mean$year, "-01-01", sep="")
m0_50_year_mean$date <- factor(m0_50_year_mean$date)

m0_50_year_mean$date <- as.POSIXlt(m0_50_year_mean$date, format = "%Y-%m-%d") 

#пишем в файл
write.table(m0_50_year_mean, "Chupa_temp_year_mean_0_50.csv", sep = ";", dec = ",")

# =============== сезонные температуры ======================
#install.packages("seas")
library(seas)

str(ishodnik)
ishodnik$date <- as.Date(ishodnik$date, "%Y-%m-%d")
ishodnik$season <- mkseas(x = ishodnik, width = "DJF")

m0_50_season_mean <- aggregate(m0_50 ~ season + year, data = ishodnik, FUN = mean, na.rm=T)

# пишем в файл
write.table(m0_50_season_mean, "Chupa_t_season_mean_0_50.csv", dec=",", sep=";")
