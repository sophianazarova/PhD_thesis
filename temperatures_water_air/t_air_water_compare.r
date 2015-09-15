setwd("~/Dropbox/PhD_thesis/PhD_thesis/temperatures_water_air/")

# ======== сезонная темепература воздуха в Кандалакше vs температура воды в Чупе ========

water_air <- read.csv2("t_air_water_season_mean.csv")

str(water_air)

tapply(water_air$Chupa_water, water_air$season, mean, na.rm=T)

# делаем вектор для раскраски точек на графике
ish_colors <- character(length = length(water_air$season))
ish_colors[water_air$season == "DJF"] <- "blue"
ish_colors[water_air$season == "MAM"] <- "green"
ish_colors[water_air$season == "JJA"] <- "red"
ish_colors[water_air$season == "SON"] <- "orange"

pdf("temp_air_water.pdf", family = "NimbusSan")
plot(water_air$Kanda_air, water_air$Chupa_water, pch=15, col=ish_colors, xlab = "t воздуха, C", ylab = "t воды, C")
n <- c("зима", "весна", "лето", "осень")
legend(x="topleft", legend = n, pch=15, col = c("blue", "green", "red", "orange"))
dev.off()
embedFonts("temp_air_water.pdf")

#считаем корреляцию
cor.test(water_air$Kanda_air, water_air$Chupa_water, method = "spearman")

sink("spearman_air_water.txt")
cor.test(water_air$Kanda_air, water_air$Chupa_water, method = "spearman")
sink()

# рисуем воду и воздух на одном графике среднегодовые
White_temp <- read.csv2("White_temp_year_mean_air_water.csv")

# динамика
pdf("White_temp_air_water_dynamic.pdf", family = "NimbusSan")
plot(White_temp$year[ White_temp$year >=1991], White_temp$temp_air[ White_temp$year >=1991], type = "b", pch=15, lty = 1, ylim = c(min(c(White_temp$temp_water_m0,White_temp$temp_air), na.rm=T), max(c(White_temp$temp_water_m0,White_temp$temp_air), na.rm=T)), xlab = "годы", ylab = "t, C")
lines(White_temp$year[ White_temp$year >=1991], White_temp$temp_water_m0[ White_temp$year >=1991], type = "b", pch=16, lty = 2)
lines(White_temp$year[ White_temp$year >=1991], White_temp$temp_water_m0_50[ White_temp$year >=1991], type = "b", pch=17, lty = 3)
n <- c("воздух", "вода 0 м", "вода 0-50 м")
legend(x="topright", legend = n, lty = seq(1:3), pch=seq(15,17,1))
dev.off()
embedFonts("White_temp_air_water_dynamic.pdf")

# корреляция
pdf("White_temp_air_water_year.pdf", family = "NimbusSan")
plot(White_temp$temp_air[ White_temp$year >=1991], White_temp$temp_water[ White_temp$year >=1991], xlab = "t воздуха, C", ylab = "t воды, C", pch=15)
dev.off()
embedFonts("White_temp_air_water_year.pdf")

# поверхность
cor.test(White_temp$temp_air[ White_temp$year >=1991], White_temp$temp_water_m0[ White_temp$year >=1991], method = "spearman")

# 50 метровый слой
cor.test(White_temp$temp_air[ White_temp$year >=1991], White_temp$temp_water_m0_50[ White_temp$year >=1991], method = "spearman")

# ==== сезонная температура воды в Белом м Баренцевом =================
White_Barents <- read.csv2("Barents_White_season_mean.csv")
str(White_Barents)

tapply(White_Barents$Barents, White_Barents$season, mean, na.rm=T)
tapply(White_Barents$White, White_Barents$season, mean, na.rm=T)

WB_colors <- character(length = length(White_Barents$season))
WB_colors[White_Barents$season == "DJF"] <- "blue"
WB_colors[White_Barents$season == "MAM"] <- "green"
WB_colors[White_Barents$season == "JJA"] <- "red"
WB_colors[White_Barents$season == "SON"] <- "orange"

pdf("temp_White_Barents.pdf", family = "NimbusSan")
plot(White_Barents$Barents, White_Barents$White, pch=15, col=WB_colors, xlab = "t воды, Баренцево море, C", ylab = "t воды, Белое море, C", xlim = c(0,max(White_Barents$Barents, White_Barents$White)), ylim = c(0,max(White_Barents$Barents, White_Barents$White)))
dev.off()
embedFonts("temp_White_Barents.pdf")

#считаем корреляцию
cor.test(White_Barents$Barents, White_Barents$White, method = "spearman")

sink("spearman_White_Barents.txt")
cor.test(White_Barents$Barents, White_Barents$White, method = "spearman")
sink()
