temp <- read.csv2("2013_2016.csv")

str(temp)
str(temp$Time)

temp$Time <- as.POSIXlt(x = temp$Time, format = "%d.%m.%Y %H:%M", tz = "MSK")
str(temp$Time)

#делаем колонку месяц-год
temp$ym <- format(temp$Time,"%Y.%m")

str(temp$ym)

#считаем среднемесячные
Temp_month_mean <- aggregate( Temp ~ ym, data = temp, FUN = mean, na.rm=T)

write.csv2(Temp_month_mean, "Temp_Kandalaksha_mean_month_2013_2016.csv")
