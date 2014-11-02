setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/dynamic_N_N1/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Luvenga_Goreliy//")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

# ======= размерная структура средние по годам по горизонтам ====================
ishodnik<-read.table(file="dynamic_N_all_White.csv", sep=";", dec=",", head=T)
attach(ishodnik)

str(ishodnik)

table(year, area)

# сделаем из линейного data.frame две матрицы
N2.matrix<-tapply(X=N2.mean, list(year,area), max)
(N2.df<-data.frame(years=as.numeric(dimnames(N2.matrix)[[1]]), as.data.frame(N2.matrix[,!is.na(colMeans(N2.matrix, na.rm=T))])))
N.matrix<-tapply(X=N.mean, list(year,area), max)
(N.df<-data.frame(years=as.numeric(dimnames(N.matrix)[[1]]), as.data.frame(N.matrix[,!is.na(colMeans(N.matrix, na.rm=T))])))

names(N2.df)

# ============== crosscorrelation  ==============================================
#Кажется это некоторая хрень - кросскорреляции между участками.


## численность N, участки, где мыли на сите 0,5.

#N Эстуарий vs Горелый, Сельдяная, Медвежья 1992-2012
pdf(file="crosscorr_N_Estury_Goreliy_1992_2012.pdf", family="NimbusSan")
ccf(x=N.df$Estuary[rownames(N.df)%in%seq(1992,2012,1)], N.df$Goreliy[rownames(N.df)%in%seq(1992,2012,1)])
dev.off()
embedFonts("crosscorr_N_Estury_Goreliy_1992_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N_Estury_Seldyanaya_1992_2012.pdf", family="NimbusSan")
ccf(x=N.df$Estuary[rownames(N.df)%in%seq(1992,2012,1)], N.df$Seldyanaya[rownames(N.df)%in%seq(1992,2012,1)])
dev.off()
embedFonts("crosscorr_N_Estury_Seldyanaya_1992_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N_Estury_Medvezhya_1992_2012.pdf", family="NimbusSan")
ccf(x=N.df$Estuary[rownames(N.df)%in%seq(1992,2012,1)], N.df$Medvezhya[rownames(N.df)%in%seq(1992,2012,1)])
dev.off()
embedFonts("crosscorr_N_Estury_Medvezhya_1992_2012.pdf") #встройка шрифтов в файл

#N Эстуарий vs 2разрез 1992-2000
pdf(file="crosscorr_N_Estury_2razrez_1992_2000.pdf", family="NimbusSan")
ccf(x=N.df$Estuary[rownames(N.df)%in%seq(1992,2000,1)], N.df$razrez2[rownames(N.df)%in%seq(1992,2000,1)])
dev.off()
embedFonts("crosscorr_N_Estury_2razrez_1992_2000.pdf") #встройка шрифтов в файл

#N Эстуарий vs ЗРС 1994-2012
pdf(file="crosscorr_N_Estury_ZRS_1994_2012.pdf", family="NimbusSan")
ccf(x=N.df$Estuary[rownames(N.df)%in%seq(1994,2012,1)], N.df$ZRS[rownames(N.df)%in%seq(1994,2012,1)])
dev.off()
embedFonts("crosscorr_N_Estury_ZRS_1994_2012.pdf") #встройка шрифтов в файл

#N Горелый vs Сельдяная, Медвежья 1992-2012
pdf(file="crosscorr_N_Goreliy_Seldyanaya_1992_2012.pdf", family="NimbusSan")
ccf(x=N.df$Goreliy[rownames(N.df)%in%seq(1992,2012,1)], N.df$Seldyanaya[rownames(N.df)%in%seq(1992,2012,1)])
dev.off()
embedFonts("crosscorr_N_Goreliy_Seldyanaya_1992_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N_Goreliy_Medvezhya_1992_2012.pdf", family="NimbusSan")
ccf(x=N.df$Goreliy[rownames(N.df)%in%seq(1992,2012,1)], N.df$Medvezhya[rownames(N.df)%in%seq(1992,2012,1)])
dev.off()
embedFonts("crosscorr_N_Goreliy_Medvezhya_1992_2012.pdf") #встройка шрифтов в файл

#N Горелый vs 2разрез 1992-2000
pdf(file="crosscorr_N_Goreliy_2razrez_1992_2000.pdf", family="NimbusSan")
ccf(x=N.df$Goreliy[rownames(N.df)%in%seq(1992,2000,1)], N.df$razrez2[rownames(N.df)%in%seq(1992,2000,1)])
dev.off()
embedFonts("crosscorr_N_Goreliy_2razrez_1992_2000.pdf") #встройка шрифтов в файл

#N Горелый vs ЗРС 1994-2012
pdf(file="crosscorr_N_Goreliy_ZRS_1994_2012.pdf", family="NimbusSan")
ccf(x=N.df$Goreliy[rownames(N.df)%in%seq(1994,2012,1)], N.df$ZRS[rownames(N.df)%in%seq(1994,2012,1)])
dev.off()
embedFonts("crosscorr_N_Goreliy_ZRS_1994_2012.pdf") #встройка шрифтов в файл

#N Сельдяная vs Медвежья 1987-2013
pdf(file="crosscorr_N_Medvezhya_Seldyanaya_1987_2013.pdf", family="NimbusSan")
ccf(x=N.df$Medvezhya[rownames(N.df)%in%seq(1987,2013,1)], N.df$Seldyanaya[rownames(N.df)%in%seq(1987,2013,1)])
dev.off()
embedFonts("crosscorr_N_Medvezhya_Seldyanaya_1987_2013.pdf") #встройка шрифтов в файл

#N 2 разрез vs Сельдяная, Медвежья 1992-2000
pdf(file="crosscorr_N_Seldyanaya_2razrez_1992_2000.pdf", family="NimbusSan")
ccf(x=N.df$Seldyanaya[rownames(N.df)%in%seq(1992,2000,1)], N.df$razrez2[rownames(N.df)%in%seq(1992,2000,1)])
dev.off()
embedFonts("crosscorr_N_Seldyanaya_2razrez_1992_2000.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N_Medvezhya_2razrez_1992_2000.pdf", family="NimbusSan")
ccf(x=N.df$Medvezhya[rownames(N.df)%in%seq(1992,2000,1)], N.df$razrez2[rownames(N.df)%in%seq(1992,2000,1)])
dev.off()
embedFonts("crosscorr_N_Medvezhya_2razrez_1992_2000.pdf") #встройка шрифтов в файл

#N ЗРС vs Сельдяная, Медвежья 1994-2012
pdf(file="crosscorr_N_Seldyanaya_ZRS_1994_2012.pdf", family="NimbusSan")
ccf(x=N.df$Seldyanaya[rownames(N.df)%in%seq(1994,2012,1)], N.df$ZRS[rownames(N.df)%in%seq(1994,2012,1)])
dev.off()
embedFonts("crosscorr_N_Seldyanaya_ZRS_1994_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N_Medvezhya_ZRS_1994_2012.pdf", family="NimbusSan")
ccf(x=N.df$Medvezhya[rownames(N.df)%in%seq(1994,2012,1)], N.df$ZRS[rownames(N.df)%in%seq(1994,2012,1)])
dev.off()
embedFonts("crosscorr_N_Medvezhya_ZRS_1994_2012.pdf") #встройка шрифтов в файл

#N 2 разрез vs ЗРС 1994-2000
pdf(file="crosscorr_N_ZRS_2razrez_1994_2000.pdf", family="NimbusSan")
ccf(x=N.df$ZRS[rownames(N.df)%in%seq(1994,2000,1)], N.df$razrez2[rownames(N.df)%in%seq(1994,2000,1)])
dev.off()
embedFonts("crosscorr_N_ZRS_2razrez_1994_2000.pdf") #встройка шрифтов в файл


# ==== численность N2, участки, где мыли на сите 1 + обрезанные по 1 мм ЛЭМБовские мониторинги. =============================================================

#N2 Эстуарий vs Горелый 1992-2012
pdf(file="crosscorr_N2_Estury_Goreliy_1992_2012.pdf", family="NimbusSan")
ccf(x=N2.df$Estuary[rownames(N.df)%in%seq(1992,2012,1)], N2.df$Goreliy[rownames(N.df)%in%seq(1992,2012,1)])
dev.off()
embedFonts("crosscorr_N2_Estury_Goreliy_1992_2012.pdf") #встройка шрифтов в файл

#N2 Клющиха vs Эстуарий, Горелый 1995-2000
pdf(file="crosscorr_N2_Klyushiha_Goreliy_1995_2000.pdf", family="NimbusSan")
ccf(x=N2.df$Klyushuha[rownames(N.df)%in%seq(1995,2000,1)], N2.df$Goreliy[rownames(N.df)%in%seq(1995,2000,1)])
dev.off()
embedFonts("crosscorr_N2_Klyushiha_Goreliy_1995_2000.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_Klyushiha_Estuary_1995_2000.pdf", family="NimbusSan")
ccf(x=N2.df$Klyushuha[rownames(N.df)%in%seq(1995,2000,1)], N2.df$Estuary[rownames(N.df)%in%seq(1995,2000,1)])
dev.off()
embedFonts("crosscorr_N2_Klyushiha_Estuary_1995_2000.pdf") #встройка шрифтов в файл


#N2 Клющиха vs Эстуарий, Горелый 2008-2012
pdf(file="crosscorr_N2_Klyushiha_Goreliy_2008_2012.pdf", family="NimbusSan")
ccf(x=N2.df$Klyushuha[rownames(N.df)%in%seq(2008,2012,1)], N2.df$Goreliy[rownames(N.df)%in%seq(2008,2012,1)])
dev.off()
embedFonts("crosscorr_N2_Klyushiha_Goreliy_2008_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_Klyushiha_Estuary_2008_2012.pdf", family="NimbusSan")
ccf(x=N2.df$Klyushuha[rownames(N.df)%in%seq(2008,2012,1)], N2.df$Estuary[rownames(N.df)%in%seq(2008,2012,1)])
dev.off()
embedFonts("crosscorr_N2_Klyushiha_Estuary_2008_2012.pdf") #встройка шрифтов в файл

#N2 Ломнишный vs Эстуарий, Горелый 2007-2012
pdf(file="crosscorr_N2_Lomnishniy_Goreliy_2007_2012.pdf", family="NimbusSan")
ccf(x=N2.df$Lomnishniy[rownames(N.df)%in%seq(2007,2012,1)], N2.df$Goreliy[rownames(N.df)%in%seq(2007,2012,1)])
dev.off()
embedFonts("crosscorr_N2_Lomnishniy_Goreliy_2007_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_Lomnishniy_Estuary_2007_2012.pdf", family="NimbusSan")
ccf(x=N2.df$Lomnishniy[rownames(N.df)%in%seq(2007,2012,1)], N2.df$Estuary[rownames(N.df)%in%seq(2007,2012,1)])
dev.off()
embedFonts("crosscorr_N2_Lomnishniy_Estuary_2007_2012.pdf") #встройка шрифтов в файл

#N2 Ломнишный vs Клющиха 2008-2012
pdf(file="crosscorr_N2_Lomnishniy_Klyushiha_2008_2012.pdf", family="NimbusSan")
ccf(x=N2.df$Lomnishniy[rownames(N.df)%in%seq(2008,2012,1)], N2.df$Klyushuha[rownames(N.df)%in%seq(2008,2012,1)])
dev.off()
embedFonts("crosscorr_N2_Lomnishniy_Klyushiha_2008_2012.pdf") #встройка шрифтов в файл

#N2 2 разрез vs Эстуарий, Горелый 1992-2000
pdf(file="crosscorr_N2_2razrez_Estuary_1992_2000.pdf", family="NimbusSan")
ccf(x=N2.df$razrez2[rownames(N.df)%in%seq(1992,2000,1)], N2.df$Estuary[rownames(N.df)%in%seq(1992,2000,1)])
dev.off()
embedFonts("crosscorr_N2_2razrez_Estuary_1992_2000.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_2razrez_Goreliy_1992_2000.pdf", family="NimbusSan")
ccf(x=N2.df$razrez2[rownames(N.df)%in%seq(1992,2000,1)], N2.df$Goreliy[rownames(N.df)%in%seq(1992,2000,1)])
dev.off()
embedFonts("crosscorr_N2_2razrez_Goreliy_1992_2000.pdf") #встройка шрифтов в файл

#N2 2 разрез vs Клющиха, Сухая 1995-2000
pdf(file="crosscorr_N2_2razrez_Klyushuha_1995_2000.pdf", family="NimbusSan")
ccf(x=N2.df$razrez2[rownames(N.df)%in%seq(1995,2000,1)], N2.df$Klyushuha[rownames(N.df)%in%seq(1995,2000,1)])
dev.off()
embedFonts("crosscorr_N2_2razrez_Klyushuha_1995_2000.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_2razrez_Suhaya_1995_2000.pdf", family="NimbusSan")
ccf(x=N2.df$razrez2[rownames(N.df)%in%seq(1995,2000,1)], N2.df$Klyushuha[rownames(N.df)%in%seq(1995,2000,1)])
dev.off()
embedFonts("crosscorr_N2_2razrez_Suhaya_1995_2000.pdf") #встройка шрифтов в файл

#N2 Сухая vs Эстуарий, Горелый 1995-2001
pdf(file="crosscorr_N2_Suhaya_Estuary_1995_2001.pdf", family="NimbusSan")
ccf(x=N2.df$Suhaya[rownames(N.df)%in%seq(1995,2001,1)], N2.df$Estuary[rownames(N.df)%in%seq(1995,2001,1)])
dev.off()
embedFonts("crosscorr_N2_Suhaya_Estuary_1995_2001.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_Suhaya_Goreliy_1995_2001.pdf", family="NimbusSan")
ccf(x=N2.df$Suhaya[rownames(N.df)%in%seq(1995,2001,1)], N2.df$Goreliy[rownames(N.df)%in%seq(1995,2001,1)])
dev.off()
embedFonts("crosscorr_N2_Suhaya_Goreliy_1995_2001.pdf") #встройка шрифтов в файл

#N2 Сухая vs Клющиха 1982-1984
pdf(file="crosscorr_N2_Suhaya_Klyushuha_1982_1984.pdf", family="NimbusSan")
ccf(x=N2.df$Suhaya[rownames(N.df)%in%seq(1982,1984,1)], N2.df$Klyushuha[rownames(N.df)%in%seq(1982,1984,1)])
dev.off()
embedFonts("crosscorr_N2_Suhaya_Klyushuha_1982_1984.pdf") #встройка шрифтов в файл

#N2 Сухая vs Клющиха 1987-1992
pdf(file="crosscorr_N2_Suhaya_Klyushuha_1989_1992.pdf", family="NimbusSan")
ccf(x=N2.df$Suhaya[rownames(N.df)%in%seq(1989,1992,1)], N2.df$Klyushuha[rownames(N.df)%in%seq(1989,1992,1)])
dev.off()
embedFonts("crosscorr_N2_Suhaya_Klyushuha_1989_1992.pdf") #встройка шрифтов в файл

#N2 Сухая vs Клющиха 1995-2000
pdf(file="crosscorr_N2_Suhaya_Klyushuha_1995_2000.pdf", family="NimbusSan")
ccf(x=N2.df$Suhaya[rownames(N.df)%in%seq(1995,2000,1)], N2.df$Klyushuha[rownames(N.df)%in%seq(1995,2000,1)])
dev.off()
embedFonts("crosscorr_N2_Suhaya_Klyushuha_1995_2000.pdf") #встройка шрифтов в файл

#N2 Клющиха vs ЮГ, ЗРС 2008-2012
pdf(file="crosscorr_N2_Klyushiha_YuG_2008_2012.pdf", family="NimbusSan")
ccf(x=N2.df$Klyushuha[rownames(N.df)%in%seq(2008,2012,1)], N2.df$YuG[rownames(N.df)%in%seq(2008,2012,1)])
dev.off()
embedFonts("crosscorr_N2_Klyushiha_YuG_2008_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_Klyushiha_ZRS_2008_2012.pdf", family="NimbusSan")
ccf(x=N2.df$Klyushuha[rownames(N.df)%in%seq(2008,2012,1)], N2.df$ZRS[rownames(N.df)%in%seq(2008,2012,1)])
dev.off()
embedFonts("crosscorr_N2_Klyushiha_ZRS_2008_2012.pdf") #встройка шрифтов в файл

#N2 Клющиха vs ЗРС 1995-2000
pdf(file="crosscorr_N2_Klyushiha_ZRS_1995_2000.pdf", family="NimbusSan")
ccf(x=N2.df$Klyushuha[rownames(N.df)%in%seq(1995,2000,1)], N2.df$ZRS[rownames(N.df)%in%seq(1995,2000,1)])
dev.off()
embedFonts("crosscorr_N2_Klyushiha_ZRS_1995_2000.pdf") #встройка шрифтов в файл

#N2 ЮГ vs Эстуарий, Горелый, ЗРС 2001-2012
pdf(file="crosscorr_N2_YuG_Estuary_2001_2012.pdf", family="NimbusSan")
ccf(x=N2.df$YuG[rownames(N.df)%in%seq(2001,2012,1)], N2.df$Estuary[rownames(N.df)%in%seq(2001,2012,1)])
dev.off()
embedFonts("crosscorr_N2_YuG_Estuary_2001_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_YuG_Goreliy_2001_2012.pdf", family="NimbusSan")
ccf(x=N2.df$YuG[rownames(N.df)%in%seq(2001,2012,1)], N2.df$Goreliy[rownames(N.df)%in%seq(2001,2012,1)])
dev.off()
embedFonts("crosscorr_N2_YuG_Goreliy_2001_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_YuG_ZRS_2001_2012.pdf", family="NimbusSan")
ccf(x=N2.df$YuG[rownames(N.df)%in%seq(2001,2012,1)], N2.df$ZRS[rownames(N.df)%in%seq(2001,2012,1)])
dev.off()
embedFonts("crosscorr_N2_YuG_ZRS_2001_2012.pdf") #встройка шрифтов в файл

#N2 ЮГ vs Ломнишный 2007-2012
pdf(file="crosscorr_N2_YuG_Lomnishniy_2007_2012.pdf", family="NimbusSan")
ccf(x=N2.df$YuG[rownames(N.df)%in%seq(2007,2012,1)], N2.df$Lomnishniy[rownames(N.df)%in%seq(2007,2012,1)])
dev.off()
embedFonts("crosscorr_N2_YuG_Lomnishniy_2007_2012.pdf") #встройка шрифтов в файл

#N2 ЗРС vs Ломнишный 2007-2012
pdf(file="crosscorr_N2_ZRS_Lomnishniy_2007_2012.pdf", family="NimbusSan")
ccf(x=N2.df$ZRS[rownames(N.df)%in%seq(2007,2012,1)], N2.df$Lomnishniy[rownames(N.df)%in%seq(2007,2012,1)])
dev.off()
embedFonts("crosscorr_N2_ZRS_Lomnishniy_2007_2012.pdf") #встройка шрифтов в файл

#N2 ЗРС vs Эстуарий, Горелый 1994-2012
pdf(file="crosscorr_N2_ZRS_Estuary_1994_2012.pdf", family="NimbusSan")
ccf(x=N2.df$ZRS[rownames(N.df)%in%seq(1994,2012,1)], N2.df$Estuary[rownames(N.df)%in%seq(1994,2012,1)])
dev.off()
embedFonts("crosscorr_N2_ZRS_Estuary_1994_2012.pdf") #встройка шрифтов в файл

pdf(file="crosscorr_N2_ZRS_Goreliy_1994_2012.pdf", family="NimbusSan")
ccf(x=N2.df$ZRS[rownames(N.df)%in%seq(1994,2012,1)], N2.df$Goreliy[rownames(N.df)%in%seq(1994,2012,1)])
dev.off()
embedFonts("crosscorr_N2_ZRS_Goreliy_1994_2012.pdf") #встройка шрифтов в файл

#N2 ЗРС vs 2 разрез 1994-2000
pdf(file="crosscorr_N2_ZRS_2razrez_1994_2000.pdf", family="NimbusSan")
ccf(x=N2.df$ZRS[rownames(N.df)%in%seq(1994,2000,1)], N2.df$razrez2[rownames(N.df)%in%seq(1994,2000,1)])
dev.off()
embedFonts("crosscorr_N2_ZRS_2razrez_1994_2000.pdf") #встройка шрифтов в файл

#N2 ЗРС vs Сухая 1995-2001
pdf(file="crosscorr_N2_ZRS_Suhaya_1995_2005.pdf", family="NimbusSan")
ccf(x=N2.df$ZRS[rownames(N.df)%in%seq(1995,2001,1)], N2.df$Suhaya[rownames(N.df)%in%seq(1995,2001,1)])
dev.off()
embedFonts("crosscorr_N2_ZRS_Suhaya_1995_2005.pdf") #встройка шрифтов в файл


# ==== просто корреляции всего со всем... По численности между всеми участками ===============================================================================

#install.packages("psych")
library(psych)
N_pearson<-corr.test(N.matrix, method="pearson", use="pairwise")
N2_pearson<-corr.test(N2.matrix, method="pearson", use="pairwise")
#для N: Пишем в файл значения корреляции Пирсона и значения p
write.table(corr.test(N.matrix, method="pearson", use="pairwise")$r, file="corr_N_pearson_r.csv", sep=";", dec=",")
write.table(corr.test(N.matrix, method="pearson", use="pairwise")$p, file="corr_N_pearson_p_value.csv", sep=";", dec=",")
#для N2: Пишем в файл значения корреляции Пирсона и значения p
write.table(corr.test(N2.matrix, method="pearson", use="pairwise")$r, file="corr_N2_pearson_r.csv", sep=";", dec=",")
write.table(corr.test(N2.matrix, method="pearson", use="pairwise")$p, file="corr_N2_pearson_p_value.csv", sep=";", dec=",")


# не станет ли лучше если посчитать Спирмена, т.к. он ранговый??
N_spearman<-corr.test(N.matrix, method="spearman", use="pairwise")
N2_spearman<-corr.test(N.matrix, method="spearman", use="pairwise")
#для N: Пишем в файл значения корреляции Пирсона и значения p
write.table(corr.test(N.matrix, method="spearman", use="pairwise")$r, file="corr_N_spearman_r.csv", sep=";", dec=",")
write.table(corr.test(N.matrix, method="spearman", use="pairwise")$p, file="corr_N_spearman_p_value.csv", sep=";", dec=",")
#для N2: Пишем в файл значения корреляции Пирсона и значения p
write.table(corr.test(N2.matrix, method="spearman", use="pairwise")$r, file="corr_N2_spearman_r.csv", sep=";", dec=",")
write.table(corr.test(N2.matrix, method="spearman", use="pairwise")$p, file="corr_N2_spearman_p_value.csv", sep=";", dec=",")
 

# ====== теперь считаем Мантеля между расстояниями и корреляциями. =================


library(vegan)
#сделаем матрицу корреляций для участков где мыли на 1 мм
N_spearman.matrix<-N_spearman$r[c(1,2,5,6,7,10),c(1,2,5,6,7,10)]
# и соответствующую ей матрицу расстояний
rasstoyanie_km_N<-read.table("coordinates_N.csv", sep=";", dec=",", header=T, )
rownames(rasstoyanie_km_N)<-rasstoyanie_km_N$X
rasstoyanie_km_N<-rasstoyanie_km_N[,2:7]
rasstoyanie_km_N<-as.matrix(rasstoyanie_km_N)
str(rasstoyanie_km_N)

mantel(xdis=rasstoyanie_km_N, ydis=N_spearman.matrix)

# соответствие динамик через Мантелевские корреляции


library(vegan)
str(ishodnik)
attach(ishodnik)


str(N.df)

# ===== считаем частные корреляции мантеля по участкам, мытым на сите 0,5 мм =======
#делаем пустые матрицы для результатов
(N.mantel.signif<-matrix(nrow=(ncol(N.df)-1), ncol=ncol(N.df)-1, dimnames=list(colnames(N.df[2:ncol(N.df)]),colnames(N.df[2:ncol(N.df)]))))
(N.mantel.statistic<-matrix(nrow=(ncol(N.df)-1), ncol=ncol(N.df)-1, dimnames=list(colnames(N.df[2:ncol(N.df)]),colnames(N.df[2:ncol(N.df)]))))
#для каждой пары участков i и j
for (i in 2:ncol(N.df)) {
  for (j in 2:ncol(N.df)) {
    # делаем модельную матрицу по годам
    year.period<-N.df$years[!is.na(N.df[,i]) &  !is.na(N.df[,j])]
    if (length(year.period)==0) j<-j+1
    year.period<-N.df$years[!is.na(N.df[,i]) &  !is.na(N.df[,j])]
    if (length(year.period)==0) j<-j+1
    year.m<-vegdist(year.period, method="euclidean")
    # делаем матрицу по численностям
    area1.m<-vegdist(N.df[,i][N.df$years%in%N.df$years[!is.na(N.df[,i])] &  !is.na(N.df[,j])], method="euclidean")
    area2.m<-vegdist(N.df[,j][N.df$years%in%N.df$years[!is.na(N.df[,i])] &  !is.na(N.df[,j])], method="euclidean")
    #считаем частные корреляции мантеля с учетом детрендинга
    cormant<-mantel.partial(area1.m, area2.m, year.m, permutations=999)
    N.mantel.signif[i-1, j-1]<-cormant$signif
    N.mantel.statistic[i-1, j-1]<-round(cormant$statistic, digits=3)
  }
}
write.table(N.mantel.statistic, file="N_mantel_statistic.csv", sep=";", dec=",")
write.table(N.mantel.signif, file="N_mantel_signif.csv", sep=";", dec=",")

# ===== считаем частные корреляции мантеля для участков где мыли на 1 мм. =====
(N2.mantel.signif<-matrix(nrow=(ncol(N2.df)-1), ncol=ncol(N2.df)-1, dimnames=list(colnames(N2.df[2:ncol(N2.df)]),colnames(N2.df[2:ncol(N2.df)]))))
(N2.mantel.statistic<-matrix(nrow=(ncol(N2.df)-1), ncol=ncol(N2.df)-1, dimnames=list(colnames(N2.df[2:ncol(N2.df)]),colnames(N2.df[2:ncol(N2.df)]))))
for (i in 2:ncol(N2.df)) {
  for (j in 2:ncol(N2.df)) {
    year.period<-N2.df$years[!is.na(N2.df[,i]) &  !is.na(N2.df[,j])]
    if (length(year.period)==0) j<-j+1
    year.period<-N2.df$years[!is.na(N2.df[,i]) &  !is.na(N2.df[,j])]
    if (length(year.period)==0) j<-j+1
    year.m<-vegdist(year.period, method="euclidean")
    area1.m<-vegdist(N2.df[,i][N2.df$years%in%year.period], method="euclidean")
    area2.m<-vegdist(N2.df[,j][N2.df$years%in%year.period], method="euclidean")
    cormant<-mantel.partial(area1.m, area2.m, year.m, permutations=999)
    N2.mantel.signif[i-1, j-1]<-cormant$signif
    N2.mantel.statistic[i-1, j-1]<-round(cormant$statistic, digits=3)
  }
}
write.table(N2.mantel.statistic, file="N2_mantel_statistic.csv", sep=";", dec=",")
write.table(N2.mantel.signif, file="N2_mantel_signif.csv", sep=";", dec=",")



# ===== Считаем сходство с матрицей расстояний ================================


#читаем матрицу расстояний
distance_N_km<-read.table("coordinates_N.csv", sep=";", dec=",", header =T)
rownames(distance_N_km)<-distance_N_km[,1]
distance_N_km<-as.matrix(distance_N_km[,2:9])
colnames(distance_N_km)<-rownames(distance_N_km)

distance_N2_km<-read.table("coordinates_N2.csv", sep=";", dec=",", header =T)
rownames(distance_N2_km)<-distance_N2_km[,1]
distance_N2_km<-as.matrix(distance_N2_km[,2:9])
colnames(distance_N2_km)<-rownames(distance_N2_km)

# считаем мантеля между матрицей расстояний и корреляциями динамики
mantel(xdis=distance_N_km, N.mantel.statistic, na.rm=T)
mantel(xdis=distance_N2_km, N2.mantel.statistic, na.rm=T)

#попробую убрать 2 разрез и посчитать без na.rm
N2.mantel.razrez2.rm<-N2.mantel.statistic[c(1:4,6:8),c(1:4,6:8)]
distance_N2_km.razrez2.rm<-distance_N2_km[c(1:4,6:8),c(1:4,6:8)]

mantel(xdis=distance_N2_km.razrez2.rm, N2.mantel.razrez2.rm)



#картинки расстояние vs корреляция мантеля
as.vector(distance_N_km)
as.vector(N.mantel.statistic)
plot(distance_N_km, N.mantel.statistic)
