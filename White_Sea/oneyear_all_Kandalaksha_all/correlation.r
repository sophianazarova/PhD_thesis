setwd("~/Dropbox/PhD_thesis/PhD_thesis/oneyear_all_Kandalaksha_all/")

#на всякий случай отключили исходний от предыдущего файла
detach(ishodnik)

ishodnik<-read.table(file="all_Kandalaksha.csv", sep=";", dec=",", head=T)

str(ishodnik)

#рисуем картинку
plot(ishodnik$all_before_tau, ishodnik$oneyear_tau)


#отсекли выбросы, чтобы понять что в нижней части графика
plot(ishodnik$all_before_tau, ishodnik$oneyear_tau, ylim=c(0,2000), xlim=c(0,5000))

cor.test(ishodnik$all_before_tau, ishodnik$oneyear_tau, method="spearman")

#а если поделить на массовые и немассовые и посмотреть дисперсионку?..
hist(ishodnik$all_before_tau, breaks=c(0,100,500,1000,2000,5000,max(ishodnik$all_before_tau)))
summary(ishodnik$all_before_tau)

#а если разбить на интервалы, то ощущение что таки растет плотность. Но почему-то положительно.
boxplot(ishodnik$oneyear_tau[ishodnik$all_before_tau<=500], 
        ishodnik$oneyear_tau[ishodnik$all_before_tau>500 & ishodnik$all_before_tau<=2000],
        ishodnik$oneyear_tau[ishodnik$all_before_tau>2000 & ishodnik$all_before_tau<=5000],
        ishodnik$oneyear_tau[ishodnik$all_before_tau>5000])

# а если разбить на более дробные интервалы и посмотреть...
(all_before_tau.int<-cut(ishodnik$all_before_tau, breaks=c(0,50,100,500,1000,2000,3000,4000,5000,6000,max(ishodnik$all_before_tau))))
boxplot(ishodnik$oneyear_tau ~ all_before_tau.int)

#а если отдельно по участкам? 
# получается что где-то до 1000-2000 экз. крупных идет + граиент, а дальше разнобой, но это не на всех участках
boxplot(ishodnik$oneyear_tau[ishodnik$area=="goreliy"] ~ all_before_tau.int[ishodnik$area=="goreliy"])
boxplot(ishodnik$oneyear_tau[ishodnik$area=="estuary"] ~ all_before_tau.int[ishodnik$area=="estuary"])
boxplot(ishodnik$oneyear_tau[ishodnik$area=="razrez2"] ~ all_before_tau.int[ishodnik$area=="razrez2"])
boxplot(ishodnik$oneyear_tau[ishodnik$area=="ZRS"] ~ all_before_tau.int[ishodnik$area=="ZRS"])
boxplot(ishodnik$oneyear_tau[ishodnik$area=="YuG"] ~ all_before_tau.int[ishodnik$area=="YuG"])
boxplot(ishodnik$oneyear_tau[ishodnik$area=="lomnishniy"] ~ all_before_tau.int[ishodnik$area=="lomnishniy"])

#можеть быть на Горелом и 2 разрезе это эффект того что мы смешиваем несколько горизонтов вместе? смотрим.
# по горизонтам?
#и красивая картинка тут же теряется, конечно же
boxplot(ishodnik$oneyear_tau[ishodnik$area=="goreliy" & ishodnik$tidal_level=="high"] ~ 
          all_before_tau.int[ishodnik$area=="goreliy" & ishodnik$tidal_level=="high"])

boxplot(ishodnik$oneyear_tau[ishodnik$area=="goreliy" & ishodnik$tidal_level=="middle"] ~ 
          all_before_tau.int[ishodnik$area=="goreliy" & ishodnik$tidal_level=="middle"])

boxplot(ishodnik$oneyear_tau[ishodnik$area=="goreliy" & ishodnik$tidal_level=="midlow"] ~ 
          all_before_tau.int[ishodnik$area=="goreliy" & ishodnik$tidal_level=="midlow"])

boxplot(ishodnik$oneyear_tau[ishodnik$area=="goreliy" & ishodnik$tidal_level=="low"] ~ 
          all_before_tau.int[ishodnik$area=="goreliy" & ishodnik$tidal_level=="low"])

