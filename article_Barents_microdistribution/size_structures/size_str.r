setwd("~/Dropbox/PhD_thesis/PhD_thesis/article_Barents_microdistribution/size_structures/")

ishodnik<-read.table("size_age_structures.csv", header=T, sep=";", dec=",")

levels(ishodnik$site)

ishodnik$site<-ordered(ishodnik$site, levels=c("Pechenga", "Ura", "Pala", "Retinskoe", "Abram", "Nagornoe", "Volokovaya", "Gavrilovo", "Yarnyshnaya", "DZ", "Shelpino", "Porchnikha", "Tryashina", "Ivanovskaya"))

levels(ishodnik$code)
ishodnik$code <- reorder(ishodnik$code, as.numeric(ishodnik$site))

str(ishodnik)

# ========== делаем таблички по отдельным видам ================
ish_macoma <- subset(ishodnik, ishodnik$species == "Macoma balthica", drop=T)
ish_cockle <- subset(ishodnik, ishodnik$species == "Cerastoderma edule", drop=T)


library(lattice)
# ======== size structure Macoma ===================
histogram(~ish_macoma$Length.mm | ish_macoma$site, breaks=seq(0, max(ish_macoma$Length.mm+1, na.rm=T),1))

table(ish_macoma$code, ish_macoma$year)

min(ish_macoma$Length.mm, na.rm=T)

macoma_reference <- rbind(subset(ish_macoma, ish_macoma$code=="PG" & ish_macoma$year==2005), subset(ish_macoma, ish_macoma$code=="UR"), subset(ish_macoma, ish_macoma$code=="PL" & ish_macoma$year==2007), subset(ish_macoma, ish_macoma$code=="RT" & ish_macoma$year==2005), subset(ish_macoma, ish_macoma$code=="AB" & ish_macoma$year==2008), subset(ish_macoma, ish_macoma$code=="NG" & ish_macoma$year==2005), subset(ish_macoma, ish_macoma$code=="GV" & ish_macoma$year==2008), subset(ish_macoma, ish_macoma$code=="YA" & ish_macoma$year==2008), subset(ish_macoma, ish_macoma$code=="DZ" & ish_macoma$year==2008), subset(ish_macoma, ish_macoma$code=="SH" & ish_macoma$year==2008), subset(ish_macoma, ish_macoma$code=="PR" & ish_macoma$year==2007), subset(ish_macoma, ish_macoma$code=="IV" & ish_macoma$year==2008))

macoma_reference$id<-paste(macoma_reference$code, macoma_reference$year, sep=" ")

pdf("macoma_sizestr_ref.pdf", family="NimbusSan")
histogram(~macoma_reference$Length.mm | macoma_reference$id, breaks=seq(0, max(ish_macoma$Length.mm+1, na.rm=T),1), xlab = "length, mm", main = "Macoma balthica")
dev.off()
embedFonts("macoma_sizestr_ref.pdf")



# =========== size structure cockle ======================

histogram(~ish_cockle$Length.mm | ish_cockle$site, breaks=seq(0, max(ish_cockle$Length.mm+1, na.rm=T),2))

table(ish_cockle$code, ish_cockle$year)

cockle_reference <- rbind(subset(ish_cockle, ish_cockle$code == "PG" & ish_cockle$year == 2005), subset(ish_cockle, ish_cockle$code == "UR" & ish_cockle$year == 2005), subset(ish_cockle, ish_cockle$code == "PL" & ish_cockle$year == 2007), subset(ish_cockle, ish_cockle$code == "YA" & ish_cockle$year == 2008), subset(ish_cockle, ish_cockle$code == "DZ" & ish_cockle$year == 2008), subset(ish_cockle, ish_cockle$code == "SH" & ish_cockle$year == 2005), subset(ish_cockle, ish_cockle$code == "PR" & ish_cockle$year == 2005), subset(ish_cockle, ish_cockle$code == "TR" & ish_cockle$year == 2008))

cockle_reference$id<-paste(cockle_reference$code, cockle_reference$year, sep=" ")

pdf("cockle_sizestr_ref.pdf", family="NimbusSan")
histogram(~cockle_reference$Length.mm | cockle_reference$id, breaks=seq(0, max(ish_cockle$Length.mm+1, na.rm=T),2), xlab = "length, mm", , main = "Cerastoderma edule")
dev.off()
embedFonts("cockle_sizestr_ref.pdf")


# ============ варьирование размеров Macoma ==================
boxplot(ish_macoma$Length.mm ~ ish_macoma$site)

# ============ варьирование размеров cockle ================
boxplot(ish_cockle$Length.mm ~ ish_cockle$site)

# ================== главные компоненты Macoma ===============
# строим размерно-частотную струткуру
ish_macoma$Length_int <- cut(ish_macoma$Length.mm, breaks=seq(0, max(ish_macoma$Length.mm+1, na.rm=T),1))
ish_macoma$id <- paste(ish_macoma$code, ish_macoma$year, sep="_")
macoma_sizestr_t <- table(ish_macoma$id, ish_macoma$Length_int)

macoma_prcomp <- prcomp((macoma_sizestr_t))
plot(macoma_prcomp)

str(macoma_prcomp)

pdf("macoma_biplot.pdf", family="NimbusSan")
biplot(macoma_prcomp)
dev.off()
embedFonts("macoma_biplot.pdf")

macoma_p <- predict(macoma_prcomp)

pdf("macoma_PCA.pdf", family="NimbusSan")
plot(macoma_p[,1:2],col=as.numeric(ish_macoma$site)+1, pch=16, type = "n")
text(macoma_p[,1:2],labels = rownames(macoma_p))
dev.off()
embedFonts("macoma_PCA.pdf")

# ================== главные компоненты Cerastoderma ===============
# строим размерно-частотную струткуру
ish_cockle$Length_int <- cut(ish_cockle$Length.mm, breaks=seq(0, max(ish_cockle$Length.mm+1, na.rm=T),1))
ish_cockle$id <- paste(ish_cockle$code, ish_cockle$year, sep="_")
cockle_sizestr_t <- table(ish_cockle$id, ish_cockle$Length_int)

cockle_prcomp <- prcomp((cockle_sizestr_t))
plot(cockle_prcomp)

str(cockle_prcomp)

pdf("cockle_biplot.pdf", family="NimbusSan")
biplot(cockle_prcomp)
dev.off()
embedFonts("cockle_biplot.pdf")

cockle_p <- predict(cockle_prcomp)

pdf("cockle_PCA.pdf", family="NimbusSan")
plot(cockle_p[,1:2],col=as.numeric(ish_cockle$site)+1, pch=16, type = "n")
text(cockle_p[,1:2],labels = rownames(cockle_p))
dev.off()
embedFonts("cockle_PCA.pdf")


#######
hist_m<-hist(ish_macoma$Length.mm, breaks = seq(0, max(ish_macoma$Length.mm, na.rm=T)+1), plot = F)

