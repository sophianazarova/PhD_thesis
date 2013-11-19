setwd("~/Dropbox/PhD_thesis/PhD_thesis/Barenc_Sea/distribution_Moran")

#Пала-губа 2008
Pala.08<-read.table("Pala_autumn_2008.csv", header=T, sep=";", dec=",")
str(Pala.08)

for (i in 2:3) {
  for (j in 4:ncol(Pala.08)) {
  assign(paste("Pala_kendall",colnames(Pala.08)[i],colnames(Pala.08)[j], sep="_"), cor.test(method="kendall",x=Pala.08[,i], y=Pala.08[,j]))
}}

Pala_kendall_X_B_Cerastoderma_edule
Pala_kendall_X_N_Cerastoderma_edule
Pala_kendall_X_B_Macoma_balthica
Pala_kendall_X_N_Macoma_balthica
Pala_kendall_X_B_Priapulus_caudatus
Pala_kendall_X_N_Priapulus_caudatus
Pala_kendall_X_N_Crangon_crangon
Pala_kendall_Y_B_Cerastoderma_edule
Pala_kendall_Y_N_Cerastoderma_edule
Pala_kendall_Y_B_Macoma_balthica
Pala_kendall_Y_N_Macoma_balthica
Pala_kendall_Y_B_Priapulus_caudatus
Pala_kendall_Y_N_Priapulus_caudatus
Pala_kendall_Y_N_Crangon_crangon

#Дальний Пляж 2007
Plyazh.07<-read.table(file="Dalnezeleneckaya_2007.csv", header=T, sep=";", dec=",")
str(Plyazh.07)


for (i in 2:3) {
  for (j in 4:ncol(Plyazh.07)) {
    assign(paste("Plyazh_07_kendall",colnames(Plyazh.07)[i],colnames(Plyazh.07)[j], sep="_"), cor.test(method="kendall",x=Plyazh.07[,i], y=Plyazh.07[,j]))
  }}

Plyazh_07_kendall_X_B_Cerastoderma_edule
Plyazh_07_kendall_X_N_Cerastoderma_edule
Plyazh_07_kendall_X_B_Macoma_balthica
Plyazh_07_kendall_X_N_Macoma_balthica
Plyazh_07_kendall_X_B_Mya_arenaria
Plyazh_07_kendall_X_N_Mya_arenaria
Plyazh_07_kendall_X_B_Mytilus_edulis
Plyazh_07_kendall_X_N_Mytilus_edulis
Plyazh_07_kendall_X_B_Pseudolibrotus_littoralis
Plyazh_07_kendall_X_N_Pseudolibrotus_littoralis
Plyazh_07_kendall_X_N_Gammarus_sp.
Plyazh_07_kendall_Y_B_Cerastoderma_edule
Plyazh_07_kendall_Y_N_Cerastoderma_edule
Plyazh_07_kendall_Y_B_Macoma_balthica
Plyazh_07_kendall_Y_N_Macoma_balthica
Plyazh_07_kendall_Y_B_Mya_arenaria
Plyazh_07_kendall_Y_N_Mya_arenaria
Plyazh_07_kendall_Y_B_Mytilus_edulis
Plyazh_07_kendall_Y_N_Mytilus_edulis
Plyazh_07_kendall_Y_B_Pseudolibrotus_littoralis
Plyazh_07_kendall_Y_N_Pseudolibrotus_littoralis
Plyazh_07_kendall_Y_N_Gammarus_sp.

#Дальний Пляж 2008 квадрат1
Plyazh.081<-read.table(file="Dalnezeleneckaya_kv1_2008.csv", header=T, sep=";", dec=",")
str(Plyazh.081)

for (i in 2:3) {
  for (j in 4:ncol(Plyazh.081)) {
    assign(paste("Plyazh_08_1_kendall",colnames(Plyazh.081)[i],colnames(Plyazh.081)[j], sep="_"), cor.test(method="kendall",x=Plyazh.081[,i], y=Plyazh.081[,j]))
  }}

Plyazh_08_1_kendall_X_B_Cerastoderma_edule
Plyazh_08_1_kendall_X_N_Cerastoderma_edule
Plyazh_08_1_kendall_X_B_Macoma_balthica
Plyazh_08_1_kendall_X_N_Macoma_balthica
Plyazh_08_1_kendall_X_B_Mya_arenaria
Plyazh_08_1_kendall_X_N_Mya_arenaria
Plyazh_08_1_kendall_X_B_Mytilus_edulis
Plyazh_08_1_kendall_X_N_Mytilus_edulis
Plyazh_08_1_kendall_X_B_Pseudolibrotus_littoralis
Plyazh_08_1_kendall_X_N_Pseudolibrotus_littoralis
Plyazh_08_1_kendall_X_N_Gammarus_sp.
Plyazh_08_1_kendall_X_B_Gammarus_sp.
Plyazh_08_1_kendall_X_Priapulus_N_caudatus
Plyazh_08_1_kendall_X_B_Priapulus_caudatus
Plyazh_08_1_kendall_Y_B_Cerastoderma_edule
Plyazh_08_1_kendall_Y_N_Cerastoderma_edule
Plyazh_08_1_kendall_Y_B_Macoma_balthica
Plyazh_08_1_kendall_Y_N_Macoma_balthica
Plyazh_08_1_kendall_Y_B_Mya_arenaria
Plyazh_08_1_kendall_Y_N_Mya_arenaria
Plyazh_08_1_kendall_Y_B_Mytilus_edulis
Plyazh_08_1_kendall_Y_N_Mytilus_edulis
Plyazh_08_1_kendall_Y_B_Pseudolibrotus_littoralis
Plyazh_08_1_kendall_Y_N_Pseudolibrotus_littoralis
Plyazh_08_1_kendall_Y_N_Gammarus_sp.
Plyazh_08_1_kendall_Y_B_Gammarus_sp.
Plyazh_08_1_kendall_Y_Priapulus_N_caudatus
Plyazh_08_1_kendall_Y_B_Priapulus_caudatus

#Дальний Пляж 2008 квадрат2
Plyazh.082<-read.table(file="Dalnezeleneckaya_kv2_2008.csv", header=T, sep=";", dec=",")
str(Plyazh.082)

for (i in 2:3) {
  for (j in 4:ncol(Plyazh.082)) {
    assign(paste("Plyazh_08_2_kendall",colnames(Plyazh.082)[i],colnames(Plyazh.082)[j], sep="_"), cor.test(method="kendall",x=Plyazh.082[,i], y=Plyazh.082[,j]))
  }}

Plyazh_08_2_kendall_X_B_Cerastoderma_edule
Plyazh_08_2_kendall_X_N_Cerastoderma_edule
Plyazh_08_2_kendall_X_B_Macoma_balthica
Plyazh_08_2_kendall_X_N_Macoma_balthica
Plyazh_08_2_kendall_X_B_Mya_arenaria
Plyazh_08_2_kendall_X_N_Mya_arenaria
Plyazh_08_2_kendall_X_B_Mytilus_edulis
Plyazh_08_2_kendall_X_N_Mytilus_edulis
Plyazh_08_2_kendall_X_B_Pseudolibrotus_littoralis
Plyazh_08_2_kendall_X_N_Pseudolibrotus_littoralis
Plyazh_08_2_kendall_Y_B_Cerastoderma_edule
Plyazh_08_2_kendall_Y_N_Cerastoderma_edule
Plyazh_08_2_kendall_Y_B_Macoma_balthica
Plyazh_08_2_kendall_Y_N_Macoma_balthica
Plyazh_08_2_kendall_Y_B_Mya_arenaria
Plyazh_08_2_kendall_Y_N_Mya_arenaria
Plyazh_08_2_kendall_Y_B_Mytilus_edulis
Plyazh_08_2_kendall_Y_N_Mytilus_edulis
Plyazh_08_2_kendall_Y_B_Pseudolibrotus_littoralis
Plyazh_08_2_kendall_Y_N_Pseudolibrotus_littoralis

#Дальний Пляж 2008 квадраты 1+2
Plyazh.0812<-read.table(file="Dalnezeleneckaya_kv12_2008.csv", header=T, sep=";", dec=",")
str(Plyazh.08122)

for (i in 2:3) {
  for (j in 4:ncol(Plyazh.0812)) {
    assign(paste("Plyazh_08_12_kendall",colnames(Plyazh.0812)[i],colnames(Plyazh.0812)[j], sep="_"), cor.test(method="kendall",x=Plyazh.0812[,i], y=Plyazh.0812[,j]))
  }}

Plyazh_08_12_kendall_X_B_Cerastoderma_edule
Plyazh_08_12_kendall_X_N_Cerastoderma_edule
Plyazh_08_12_kendall_X_B_Macoma_balthica
Plyazh_08_12_kendall_X_N_Macoma_balthica
Plyazh_08_12_kendall_X_B_Mya_arenaria
Plyazh_08_12_kendall_X_N_Mya_arenaria
Plyazh_08_12_kendall_X_B_Mytilus_edulis
Plyazh_08_12_kendall_X_N_Mytilus_edulis
Plyazh_08_12_kendall_X_B_Pseudolibrotus_littoralis
Plyazh_08_12_kendall_X_N_Pseudolibrotus_littoralis
Plyazh_08_12_kendall_X_N_Gammarus_sp.
Plyazh_08_12_kendall_X_B_Gammarus_sp.
Plyazh_08_12_kendall_X_Priapulus_N_caudatus
Plyazh_08_12_kendall_X_B_Priapulus_caudatus
Plyazh_08_12_kendall_Y_B_Cerastoderma_edule
Plyazh_08_12_kendall_Y_N_Cerastoderma_edule
Plyazh_08_12_kendall_Y_B_Macoma_balthica
Plyazh_08_12_kendall_Y_N_Macoma_balthica
Plyazh_08_12_kendall_Y_B_Mya_arenaria
Plyazh_08_12_kendall_Y_N_Mya_arenaria
Plyazh_08_12_kendall_Y_B_Mytilus_edulis
Plyazh_08_12_kendall_Y_N_Mytilus_edulis
Plyazh_08_12_kendall_Y_B_Pseudolibrotus_littoralis
Plyazh_08_12_kendall_Y_N_Pseudolibrotus_littoralis
Plyazh_08_12_kendall_Y_N_Gammarus_sp.
Plyazh_08_12_kendall_Y_B_Gammarus_sp.
Plyazh_08_12_kendall_Y_Priapulus_N_caudatus
Plyazh_08_12_kendall_Y_B_Priapulus_caudatus


# Ярнышная 2007 
Yarn.07<-read.table(file="Yarnyshnaya_2007.csv", header=T, sep=";", dec=",")
str(Yarn.07)

for (i in 2:3) {
  for (j in 4:ncol(Yarn.07)) {
    assign(paste("Yarn_07_kendall",colnames(Yarn.07)[i],colnames(Yarn.07)[j], sep="_"), cor.test(method="kendall",x=Yarn.07[,i], y=Yarn.07[,j]))
  }}


Yarn_07_kendall_X_B_Cerastoderma_edule
Yarn_07_kendall_X_N_Cerastoderma_edule
Yarn_07_kendall_X_B_Macoma_balthica
Yarn_07_kendall_X_N_Macoma_balthica
Yarn_07_kendall_X_B_Mya_arenaria
Yarn_07_kendall_X_N_Mya_arenaria
Yarn_07_kendall_X_B_Mytilus_edulis
Yarn_07_kendall_X_N_Mytilus_edulis
Yarn_07_kendall_Y_B_Cerastoderma_edule
Yarn_07_kendall_Y_N_Cerastoderma_edule
Yarn_07_kendall_Y_B_Macoma_balthica
Yarn_07_kendall_Y_N_Macoma_balthica
Yarn_07_kendall_Y_B_Mya_arenaria
Yarn_07_kendall_Y_N_Mya_arenaria
Yarn_07_kendall_Y_B_Mytilus_edulis
Yarn_07_kendall_Y_N_Mytilus_edulis
