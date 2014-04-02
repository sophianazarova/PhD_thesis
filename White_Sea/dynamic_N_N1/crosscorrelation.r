setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/dynamic_N_N1/")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Luvenga_Goreliy//")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="dynamic_N_all_White.csv", sep=";", dec=",", head=T)
attach(ishodnik)

str(ishodnik)

table(year, area)

##crosscorrelation 
# сделаем из линейного data.frame две матрицы
N2.matrix<-tapply(X=N2.mean, list(year,area), max)
N2.df<-as.data.frame(N2.matrix)
N.matrix<-tapply(X=N.mean, list(year,area), max)
N.df<-as.data.frame(N.matrix)

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

## численность N2, участки, где мыли на сите 1 + обрезанные по 1 мм ЛЭМБовские мониторинги.
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
