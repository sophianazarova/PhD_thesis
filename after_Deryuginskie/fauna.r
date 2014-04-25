setwd("/home/sonya/Dropbox/PhD_thesis/PhD_thesis/after_Deryuginskie")
#setwd("~/note_backup_2013-04-13/PhD_thesis/White_Sea/Luvenga_Goreliy//")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

## размерная структура средние по годам по горизонтам
ishodnik<-read.table(file="fauna_DZ_svodka.csv", sep=";", dec=",", header=TRUE)

attach(ishodnik)

str(ishodnik)

# считаем численность и биомассы на квадратный метр

ishodnik$N.sqmeter<-ishodnik$N.indd * ishodnik$square
ishodnik$B.sqmeter<-ishodnik$B.mg * ishodnik$square

# делаем сводку год-станция-вид
# TODO надо учесть какие пробы где брали!
N_sqmeter_svodka<-tapply(ishodnik$N.sqmeter, INDEX=list(ishodnik$species, ishodnik$year, ishodnik$station), FUN=mean, na.rm=T)

str(N_sqmeter_svodka)
N_sqmeter_svodka[,,1]
