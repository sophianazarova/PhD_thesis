setwd("~/Dropbox/PhD_thesis/PhD_thesis/White_Sea/dynamic_N_N1")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="N2_mean_SEM.csv", sep=";", dec=",", head=T)
str(ishodnik)
ishodnik$area<-ordered(ishodnik$area,levels=c("Klyushuha", "Suhaya", "Medvezhya", "Seldyanaya", "Lomnishniy", "YuG", "ZRS", "Goreliy", "Estuary", "razrez2"))
ishodnik$region<-ordered(ishodnik$region, levels=c("Chupa", "North_arckipelago", "Luvenga"))

levels(ishodnik$region)

#пробую рисовать lattice, но какая-то фигня с разделением
library(lattice)
xyplot(ishodnik$N2.mean.sqmeter ~ ishodnik$year | ishodnik$region, pch=20, col=ishodnik$area)


as.numeric(ishodnik$area)
ishodnik$area

# картинка про Лувеньгу
pdf(file="N2_dynamic_Luvenga_all.pdf", family="NimbusSan") # указываем шрифт подпией
#plot(x=ishodnik$year[ ishodnik$area=="Suhaya"], y=ishodnik$N2.mean.sqmeter[shodnik$area=="Suhaya"], type="n", xlim=c(min(ishodnik$year), max(ishodnik$year)), ylim=c(min(ishodnik$N2.mean.sqmeter[ ishodnik$region=="Luvenga"]), max(ishodnik$N2.mean.sqmeter[ ishodnik$region=="Luvenga"])), main="Лувеньгские шхеры", xlab="годы", ylab="N, экз./кв.м")

plot(x=ishodnik$year[ ishodnik$area=="Suhaya"], y=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Suhaya"], type="n", xlim=c(min(ishodnik$year), max(ishodnik$year)), ylim=c(min(ishodnik$N2.mean.sqmeter), max(ishodnik$N2.mean.sqmeter)), main="Лувеньгские шхеры", xlab="годы", ylab="N, экз./кв.м")
#2 разрез
lines(ishodnik$year[ ishodnik$area=="razrez2"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="razrez2"], type="b", pch=20, col=2)
arrows(x0=ishodnik$year[ ishodnik$area=="razrez2"], 
       x1=ishodnik$year[ ishodnik$area=="razrez2"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="razrez2"] + ishodnik$N2.sem[ ishodnik$area=="razrez2"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="razrez2"] - ishodnik$N2.sem[ ishodnik$area=="razrez2"], 
       angle=90, code=3, length=.1, col=2)
#Горелый
lines(ishodnik$year[ ishodnik$area=="Goreliy"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="Goreliy"], type="b", pch=20, col=3)
arrows(x0=ishodnik$year[ ishodnik$area=="Goreliy"], 
       x1=ishodnik$year[ ishodnik$area=="Goreliy"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Goreliy"] + ishodnik$N2.sem[ ishodnik$area=="Goreliy"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Goreliy"] - ishodnik$N2.sem[ ishodnik$area=="Goreliy"], 
       angle=90, code=3, length=.1, col=3)
#Эстуарий
lines(ishodnik$year[ ishodnik$area=="Estuary"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="Estuary"], type="b", pch=20, col=4)
arrows(x0=ishodnik$year[ ishodnik$area=="Estuary"], 
       x1=ishodnik$year[ ishodnik$area=="Estuary"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Estuary"] + ishodnik$N2.sem[ ishodnik$area=="Estuary"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Estuary"] - ishodnik$N2.sem[ ishodnik$area=="Estuary"], 
       angle=90, code=3, length=.1, col=4)

dev.off()
embedFonts("N2_dynamic_Luvenga_all.pdf") #встройка шрифтов в файл

# картинка про Чупу
pdf(file="N2_dynamic_Chupa_all.pdf", family="NimbusSan")
plot(x=ishodnik$year[ ishodnik$area=="Suhaya"], y=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Suhaya"], type="n", xlim=c(min(ishodnik$year), max(ishodnik$year)), ylim=c(min(ishodnik$N2.mean.sqmeter), max(ishodnik$N2.mean.sqmeter)), main="район губы Чупа", xlab="годы", ylab="N, экз./кв.м")
#сухая
lines(ishodnik$year[ ishodnik$area=="Suhaya"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="Suhaya"], type="b", pch=20, col=2)
arrows(x0=ishodnik$year[ ishodnik$area=="Suhaya"], 
       x1=ishodnik$year[ ishodnik$area=="Suhaya"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Suhaya"] + ishodnik$N2.sem[ ishodnik$area=="Suhaya"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Suhaya"] - ishodnik$N2.sem[ ishodnik$area=="Suhaya"], 
       angle=90, code=3, length=.1, col=2)
#клющиха
lines(ishodnik$year[ ishodnik$area=="Klyushuha"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="Klyushuha"], type="b", pch=20, col=3)
arrows(x0=ishodnik$year[ ishodnik$area=="Klyushuha"], 
       x1=ishodnik$year[ ishodnik$area=="Klyushuha"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Klyushuha"] + ishodnik$N2.sem[ ishodnik$area=="Klyushuha"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Klyushuha"] - ishodnik$N2.sem[ ishodnik$area=="Klyushuha"], 
       angle=90, code=3, length=.1, col=3)
#Медвежья
lines(ishodnik$year[ ishodnik$area=="Medvezhya"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="Medvezhya"], type="b", pch=20, col=4)
arrows(x0=ishodnik$year[ ishodnik$area=="Medvezhya"], 
       x1=ishodnik$year[ ishodnik$area=="Medvezhya"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Medvezhya"] + ishodnik$N2.sem[ ishodnik$area=="Medvezhya"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Medvezhya"] - ishodnik$N2.sem[ ishodnik$area=="Medvezhya"], 
       angle=90, code=3, length=.1, col=4)
#сельдяная
lines(ishodnik$year[ ishodnik$area=="Seldyanaya"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="Seldyanaya"], type="b", pch=20, col=5)
arrows(x0=ishodnik$year[ ishodnik$area=="Seldyanaya"], 
       x1=ishodnik$year[ ishodnik$area=="Seldyanaya"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Seldyanaya"] + ishodnik$N2.sem[ ishodnik$area=="Seldyanaya"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Seldyanaya"] - ishodnik$N2.sem[ ishodnik$area=="Seldyanaya"], 
       angle=90, code=3, length=.1, col=5)

dev.off()
embedFonts("N2_dynamic_Chupa_all.pdf") #встройка шрифтов в файл

# картинка про Лувеньгу
pdf(file="N2_dynamic_North_all.pdf", family="NimbusSan") # указываем шрифт подпией

plot(x=ishodnik$year[ ishodnik$area=="Suhaya"], y=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Suhaya"], type="n", xlim=c(min(ishodnik$year), max(ishodnik$year)), ylim=c(min(ishodnik$N2.mean.sqmeter), max(ishodnik$N2.mean.sqmeter)), main="Северный архипелаг", xlab="годы", ylab="N, экз./кв.м")
#2 разрез
lines(ishodnik$year[ ishodnik$area=="ZRS"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="ZRS"], type="b", pch=20, col=2)
arrows(x0=ishodnik$year[ ishodnik$area=="ZRS"], 
       x1=ishodnik$year[ ishodnik$area=="ZRS"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="ZRS"] + ishodnik$N2.sem[ ishodnik$area=="ZRS"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="ZRS"] - ishodnik$N2.sem[ ishodnik$area=="ZRS"], 
       angle=90, code=3, length=.1, col=2)
#Горелый
lines(ishodnik$year[ ishodnik$area=="YuG"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="YuG"], type="b", pch=20, col=3)
arrows(x0=ishodnik$year[ ishodnik$area=="YuG"], 
       x1=ishodnik$year[ ishodnik$area=="YuG"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="YuG"] + ishodnik$N2.sem[ ishodnik$area=="YuG"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="YuG"] - ishodnik$N2.sem[ ishodnik$area=="YuG"], 
       angle=90, code=3, length=.1, col=3)
#Эстуарий
lines(ishodnik$year[ ishodnik$area=="Lomnishniy"], ishodnik$N2.mean.sqmeter[ ishodnik$area=="Lomnishniy"], type="b", pch=20, col=4)
arrows(x0=ishodnik$year[ ishodnik$area=="Lomnishniy"], 
       x1=ishodnik$year[ ishodnik$area=="Lomnishniy"], 
       y0=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Lomnishniy"] + ishodnik$N2.sem[ ishodnik$area=="Lomnishniy"], 
       y1=ishodnik$N2.mean.sqmeter[ ishodnik$area=="Lomnishniy"] - ishodnik$N2.sem[ ishodnik$area=="Lomnishniy"], 
       angle=90, code=3, length=.1, col=4)

dev.off()
embedFonts("N2_dynamic_North_all.pdf") #встройка шрифтов в файл
