setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/temperatures_Kandalaksha/")

ishodnik <- read.csv2("Kandalaksha_temp_air_1986-2013.csv")

str(ishodnik)

# форматируем в ts
ish_ts <- ts(as.vector(t(as.matrix(ishodnik)[,-1])),
        start=c(1986, 1), # начинаем в 1-й месяц 1961 года
        frequency=12) # помесячно

plot(ish_ts)


# форматруем в POSIXlt
ish_posix<-data.frame(year = sort(rep(seq(1986, 2013,1),12)), month = rep(seq(01:12), 28), temp_air = as.vector(t(as.matrix(ishodnik)[,-1])))
ish_posix$date <- paste(ish_posix$year, ish_posix$month, "01", sep="-")

ish_posix$date <- as.POSIXlt(ish_posix$date, format = "%Y-%m-%d", tz = "") 

# ============= среднегодовые температуры ================

#считаем средние
temp_year_mean <- aggregate(temp_air ~ year, data = ish_posix, FUN = mean, na.rm=T)

#дописываем даты, чтобы перевести в дата-формат
temp_year_mean$date <- paste(temp_year_mean$year, "-01-01", sep="")
temp_year_mean$date <- factor(temp_year_mean$date)

temp_year_mean$date <- as.POSIXlt(temp_year_mean$date, format = "%Y-%m-%d") 

# рисуем среднегодовые
pdf("t_air_mean_year.pdf", fonts = "NimbusSan")
plot(temp_year_mean$date, temp_year_mean$temp_air, type="l", xlab = "year", ylab = "t, C")
dev.off()
embedFonts("t_air_mean_year.pdf")

#пишем в файл
write.table(temp_year_mean, "Kandalaksha_temp_year_mean.csv", sep = ";", dec = ",")

# =============== сезонные температуры ======================
#install.packages("seas")
library(seas)


ish_posix$date <- as.Date(ish_posix$date)
ish_posix$season <- mkseas(x = ish_posix, width = "DJF")

temp_season_mean <- aggregate(temp_air ~ season + year, data = ish_posix, FUN = mean, na.rm=T)

#all seasons
pdf("t_air_mean_season_year.pdf", fonts = "NimbusSan")
plot(temp_season_mean$year, temp_season_mean$temp_air, type = "n", xlab = "year", ylab = "t, C")
lines(temp_season_mean$year[ temp_season_mean$season == "DJF"], temp_season_mean$temp_air[ temp_season_mean$season == "DJF"], col="blue")
lines(temp_season_mean$year[ temp_season_mean$season == "MAM"], temp_season_mean$temp_air[ temp_season_mean$season == "MAM"], col="green")
lines(temp_season_mean$year[ temp_season_mean$season == "JJA"], temp_season_mean$temp_air[ temp_season_mean$season == "JJA"], col="red")
lines(temp_season_mean$year[ temp_season_mean$season == "SON"], temp_season_mean$temp_air[ temp_season_mean$season == "SON"], col="orange")
lines(temp_year_mean$year, temp_season_mean$temp_air)
dev.off()
embedFonts("t_air_mean_season_year.pdf")

# пишем в файл
write.table(temp_season_mean, "Kandalaksha_t_air_season_mean.csv", dec=",", sep=";")
