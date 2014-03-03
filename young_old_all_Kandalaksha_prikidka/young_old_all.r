setwd("~/Dropbox/PhD_thesis/PhD_thesis/young_old_all_Kandalaksha/")
ishodnik<-read.table(file="young_old_all_Kandalaksha.csv", sep=";", dec=",", head=T)
ishodnik$year<-factor(ishodnik$year)
str(ishodnik)
attach(ishodnik)


## молодь и общая численность 
#!! Хрень. надо >2mm, иначе не независимые величины

#график
pdf(file="young_N2_Kandalaksha.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=N2_all, 
     y=young, 
     pch=as.numeric(year), 
     col=10*as.numeric(area)+as.numeric(tidal_zone),
     xlab="N, экз./кв.м", ylab="N1+, экз./кв.м")
dev.off()
embedFonts("young_N2_Kandalaksha.pdf") #встройка шрифтов в файл

# убираем отскок - максимум. Это в ЗРС. и рисуем снова график
pdf(file="young_N2__without_max_Kandalaksha.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=N2_all[c(1:5, 7:length(N2_all))], 
     y=young[c(1:5, 7:length(young))], 
     pch=as.numeric(year)[c(1:5, 7:length(year))], 
     col=10*as.numeric(area)[c(1:5, 7:length(area))]+as.numeric(tidal_zone)[c(1:5, 7:length(tidal_zone))],
     xlab="N, экз./кв.м", ylab="N1+, экз./кв.м")
dev.off()
embedFonts("young_N2__without_max_Kandalaksha.pdf") #встройка шрифтов в файл

# корреляция Спирмена
cor.test(x=N2_all[c(1:5, 7:length(N2_all))], y=young[c(1:5, 7:length(young))], method="spearman")

## молодь и половозрелые в предыдущий год
#график
pdf(file="young_N8_Kandalaksha.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=N8_previous, 
     y=young, 
     pch=as.numeric(year), 
     col=10*as.numeric(area)+as.numeric(tidal_zone),
     xlab="Nad, экз./кв.м", ylab="N1+, экз./кв.м")
dev.off()
embedFonts("young_N8_Kandalaksha.pdf") #встройка шрифтов в файл

# убираем отскок - максимум. Это в ЗРС. и рисуем снова график
pdf(file="young_N8__without_max_Kandalaksha.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=N8_previous[c(1:5, 7:length(N8_previous))], 
     y=young[c(1:5, 7:length(young))], 
     pch=as.numeric(year)[c(1:5, 7:length(year))], 
     col=10*as.numeric(area)[c(1:5, 7:length(area))]+as.numeric(tidal_zone)[c(1:5, 7:length(tidal_zone))],
     xlab="Nad, экз./кв.м", ylab="N1+, экз./кв.м")
dev.off()
embedFonts("young_N8__without_max_Kandalaksha.pdf") #встройка шрифтов в файл

# корреляция Спирмена
cor.test(x=N8_previous[c(1:5, 7:length(N8_previous))], y=young[c(1:5, 7:length(young))], method="spearman")

## молодь и биомасса половозрелых в предыдущий год
#график
pdf(file="young_B8_Kandalaksha.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=B8_previous, 
     y=young, 
     pch=as.numeric(year), 
     col=10*as.numeric(area)+as.numeric(tidal_zone),
     xlab="Bad, г/кв.м", ylab="N1+, экз./кв.м")
dev.off()
embedFonts("young_B8_Kandalaksha.pdf") #встройка шрифтов в файл

# убираем отскок - максимум. Это в ЗРС. и рисуем снова график
pdf(file="young_B8__without_max_Kandalaksha.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=B8_previous[c(1:5, 7:length(B8_previous))], 
     y=young[c(1:5, 7:length(young))], 
     pch=as.numeric(year)[c(1:5, 7:length(year))], 
     col=10*as.numeric(area)[c(1:5, 7:length(area))]+as.numeric(tidal_zone)[c(1:5, 7:length(tidal_zone))],
     xlab="Bad, г/кв.м", ylab="N1+, экз./кв.м")
dev.off()
embedFonts("young_B8__without_max_Kandalaksha.pdf") #встройка шрифтов в файл

# корреляция Спирмена
cor.test(x=B8_previous[c(1:5, 7:length(B8_previous))], y=young[c(1:5, 7:length(young))], method="spearman")

## молодь все на одном графике
young_kanda_mean<-read.table(file="young_Kandalaksha.csv", sep=";", dec=",", head=T)
str(young_kanda_mean)
attach(young_kanda_mean)

pdf(file="young_dynamic_Kandalaksha.pdf", family="NimbusSan") # указываем шрифт подпией
plot(x=year, y=, type="n",
     xlab="год", ylab="N, экз./кв.м")
lines(x=as.numeric(levels(year)), y=young[area=="Estuary"], pch=15, type="b", col=1) #Эстуарий
lines(x=as.numeric(levels(year)), y=young[area=="Lomnishniy"], pch=16, type="b", col=2) #Ломнишный
dev.off()
embedFonts("young_dynamic_Kandalaksha.pdf") #встройка шрифтов в файл