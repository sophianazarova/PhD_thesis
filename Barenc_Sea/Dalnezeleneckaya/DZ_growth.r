ishodnik<-read.table(file="growth.csv", head=T, sep=";", dec=",")
str(ishodnik)

## средний размер 1+
mean(ishodnik$Length.mm[ishodnik$age==1], na.rm=T)
mean(ishodnik$Length.mm[ishodnik$age==2], na.rm=T)

sd(ishodnik$Length.mm[ishodnik$age==1], na.rm=T)
sd(ishodnik$Length.mm[ishodnik$age==2], na.rm=T)

#ничерта оно не нормальное :(
shapiro.test(ishodnik$Length.mm[ishodnik$age==1])
shapiro.test(ishodnik$Length.mm[ishodnik$age==2])


boxplot(ishodnik$Length.mm ~ishodnik$age)

pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==1], na.rm=T), sd=sd(ishodnik$Length.mm[ishodnik$age==1], na.rm=T), q=4.1)
pnorm(mean=mean(ishodnik$Length.mm[ishodnik$age==2], na.rm=T), sd=sd(ishodnik$Length.mm[ishodnik$age==2], na.rm=T), q=4.1)
#и вообще они хреново разделяются :(

# бррр, а вообще 2007 на 2008 год поход по измерениям колец??
mean(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$year==2007], na.rm=T)
mean(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$year==2007], na.rm=T)

mean(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$year==2008], na.rm=T)
mean(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$year==2008], na.rm=T)

wilcox.test(ishodnik$Length.mm[ishodnik$age==1 & ishodnik$year==2007], ishodnik$Length.mm[ishodnik$age==1 & ishodnik$year==2008]) 
#не отличаются
wilcox.test(ishodnik$Length.mm[ishodnik$age==2 & ishodnik$year==2007], ishodnik$Length.mm[ishodnik$age==2 & ishodnik$year==2008])
# отличаются

summary(ishodnik)


## размер колец
kolca.mean<-colMeans(ishodnik[,7:18], na.rm=T)
kolca.sd<-apply(ishodnik[,7:18], 2, sd, na.rm=T)
kolca.sem<-kolca.sd/sqrt(colSums(!is.na(ishodnik[,7:18])))

Age<-seq(1,12,1)
Bertallanfi <- nls(kolca.mean~Lmax*(1-exp(-k*Age)), start=list(Lmax=20,k=0.1)) # задаем модель в виде nls(y~f(x)), и задаем стартовые значения коэффициентов из формулы, от которых он методом последовательного приближения (как я понимаю) будет подбирать максимально похожие)
summary(Bertallanfi)
Agemod <- seq(0,12,0.1)
Lmod <- predict(Bertallanfi, list(Age=Agemod))

pdf(file="L_kolec.pdf", family="NimbusSan") # указываем шрифт подпией
plot(y=kolca.mean, x=Age, pch=15, main="Рост Macoma balthica в г. Дальнезеленецкой", 
#     xlim=seq(1,12,1),
     xlab="возраст", ylab="L, мм")
arrows(x0=Age, x1=Age, y0=kolca.mean+kolca.sem, y1=kolca.mean-kolca.sem, angle=90, code=3, length=0.1)
lines(x=Agemod, y=Lmod)
dev.off()
embedFonts("L_kolec.pdf") #встройка шрифтов в файл
