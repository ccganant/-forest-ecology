
library(readxl)
library(tidyverse)

datos <- read_xlsx("parcial1.xlsx")

datos<- datos %>% select(Parcela, 
                       Finca, ...4) %>% 
  rename(DAP_cm= Parcela,
         Parcela= Finca,
         Finca= ...4) %>% 
  mutate(Parcela = as.factor(Parcela),
         Finca= as.factor(Finca))


grupos<- datos[datos$Finca == "finca0",]
area<- with(grupos, tapply(((pi/4)*(DAP_cm/100)^2), Parcela, sum))*10000/1000
Dcm<- sqrt(with(grupos, tapply(DAP_cm^2, Parcela, mean)))
N_ind<- with(grupos , tapply(DAP_cm, Parcela, length))*10000/1000

area<- area[!is.na(area)]
Dcm<- Dcm[!is.na(Dcm)]
N_ind<- N_ind[!is.na(N_ind)]

gr<- cbind(area, Dcm, N_ind)
gr<- data.frame(gr)
gr$Finca<- ifelse(max(gr$area) > 1, "finca0", "NA")


grupos<- datos[datos$Finca == "finca1",]
area<- with(grupos, tapply(((pi/4)*(DAP_cm/100)^2), Parcela, sum))*10000/1000
Dcm<- sqrt(with(grupos, tapply(DAP_cm^2, Parcela, mean)))
N_ind<- with(grupos , tapply(DAP_cm, Parcela, length))*10000/1000

area<- area[!is.na(area)]
Dcm<- Dcm[!is.na(Dcm)]
N_ind<- N_ind[!is.na(N_ind)]

gr1<- cbind(area, Dcm, N_ind)
gr1<- data.frame(gr1)
gr1$Finca<- ifelse(max(gr1$area) > 1, "finca1", "NA")


grupos<- datos[datos$Finca == "finca2",]
area<- with(grupos, tapply(((pi/4)*(DAP_cm/100)^2), Parcela, sum))*10000/1000
Dcm<- sqrt(with(grupos, tapply(DAP_cm^2, Parcela, mean)))
N_ind<- with(grupos , tapply(DAP_cm, Parcela, length))*10000/1000

area<- area[!is.na(area)]
Dcm<- Dcm[!is.na(Dcm)]
N_ind<- N_ind[!is.na(N_ind)]

gr2<- cbind(area, Dcm, N_ind)
gr2<- data.frame(gr2)
gr2$Finca<- ifelse(max(gr2$area) > 1, "finca2", "NA")


grupos<- datos[datos$Finca == "finca3",]
area<- with(grupos, tapply(((pi/4)*(DAP_cm/100)^2), Parcela, sum))*10000/2500
Dcm<- sqrt(with(grupos, tapply(DAP_cm^2, Parcela, mean)))
N_ind<- with(grupos , tapply(DAP_cm, Parcela, length))*10000/2500

area<- area[!is.na(area)]
Dcm<- Dcm[!is.na(Dcm)]
N_ind<- N_ind[!is.na(N_ind)]

gr3<- cbind(area, Dcm, N_ind)
gr3<- data.frame(gr3)
gr3$Finca<- ifelse(max(gr3$area) > 1, "finca3", "NA")


grupos<- datos[datos$Finca == "finca4",]
area<- with(grupos, tapply(((pi/4)*(DAP_cm/100)^2), Parcela, sum))*10000/2000
Dcm<- sqrt(with(grupos, tapply(DAP_cm^2, Parcela, mean)))
N_ind<- with(grupos , tapply(DAP_cm, Parcela, length))*10000/2000

area<- area[!is.na(area)]
Dcm<- Dcm[!is.na(Dcm)]
N_ind<- N_ind[!is.na(N_ind)]

gr4<- cbind(area, Dcm, N_ind)
gr4<- data.frame(gr4)
gr4$Finca<- ifelse(max(gr4$area) > 1, "finca4", "NA")



grupos<- datos[datos$Finca == "finca5",]
area<- with(grupos, tapply(((pi/4)*(DAP_cm/100)^2), Parcela, sum))*10000/1000
Dcm<- sqrt(with(grupos, tapply(DAP_cm^2, Parcela, mean)))
N_ind<- with(grupos , tapply(DAP_cm, Parcela, length))*10000/1000

area<- area[!is.na(area)]
Dcm<- Dcm[!is.na(Dcm)]
N_ind<- N_ind[!is.na(N_ind)]

gr5<- cbind(area, Dcm, N_ind)
gr5<- data.frame(gr5)
gr5$Finca<- ifelse(max(gr5$area) > 1, "finca5", "NA")


base<- rbind(gr, gr1, gr2, gr3, gr4, gr5)

data2<- base[base$Finca == "finca0" | base$Finca == "finca2" | 
               base$Finca == "finca4", ]

a<- cor.test(data2$area, data2$N_ind)
b<- cor.test(data2$Dcm, data2$area)
c<- cor.test(data2$Dcm, data2$N_ind)

a
b
c




finca_0<-droplevels(subset(datos,Finca=="finca0"))

###Histograma de Distribuciones diametricas
hist_f0<-hist(finca_0$DAP_cm,breaks=c(seq(10,140,5)),ylab="Numero de individuos",
              xlab="DAPcm",
              main="Distribuciones diametricas Finca 0",
              ylim = c(0,35))
names(hist_f0$counts) <- hist_f0$mids
barplot(hist_f0$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,35),
        main="Distribuciones diametricas Finca 0")

##Histograma de Distribuciones para Area basal
##Numero de clases con la ley de Sturges
finca_0$abasal_cm2<-with(finca_0,(pi/4)*DAP_cm^2)
n_clases<-round(1 + 3.322 * log10(length(finca_0$abasal_cm2)))
abhist_f0<-hist(finca_0$abasal_cm2,breaks=seq(20,13000,1442),ylab="Numero de individuos",
                xlab="Area basal",
                main="Distribuciones Area basal Finca 0")
ampl.intervalo<- (13000 - 20)/ 9
marca.clase <- seq(20+(1442/2),13000-(1442/2), 1442)
lim.interv<-seq(20,13000,(13000-20)/9)
names(abhist_f0$counts) <- abhist_f0$mids
barplot(abhist_f0$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,100),
        main="Distribuciones Area basal Finca 0")


finca_1<-droplevels(subset(datos,Finca=="finca1"))

###Histograma de Distribuciones diametricas
hist_f1<-hist(finca_1$DAP_cm,breaks=c(seq(10,140,5)),ylab="Numero de individuos",
              xlab="DAPcm",
              main="Distribuciones diametricas Finca 1",
              ylim = c(0,60))
names(hist_f1$counts) <- hist_f1$mids
barplot(hist_f1$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,60),
        main="Distribuciones diametricas Finca 1")

##Histograma de Distribuciones para Area basal
##Numero de clases con la ley de Sturges

finca_1$abasal_cm2<-with(finca_1,(pi/4)*DAP_cm^2)

n_clases<-round(1 + 3.322 * log10(length(finca_1$abasal_cm2)))
abhist_f1<-hist(finca_1$abasal_cm2,breaks=seq(80,12000,1324),ylab="Numero de individuos",
                xlab="Area basal",
                main="Distribuciones Area basal Finca 1")
ampl.intervalo<- (12000 - 80)/ 9
marca.clase <- seq(80+(1324/2),12000-(1324/2), 1324)
lim.interv<-seq(80,12000,(12000-80)/9)
names(abhist_f1$counts) <- abhist_f1$mids
barplot(abhist_f1$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,250),
        main="Distribuciones Area basal Finca 1")


finca_2<-droplevels(subset(datos,Finca=="finca2"))

##Histograma de Distribuciones diametricas
hist_f2<-hist(finca_2$DAP_cm,breaks=c(seq(5,35,5)),ylab="Numero de individuos",
              xlab="DAPcm",
              main="Distribuciones diametricas Finca 2",
              ylim = c(0,200))
names(hist_f2$counts) <- hist_f2$mids
barplot(hist_f2$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,200),
        main="Distribuciones diametricas Finca 2")

##Histograma de Distribuciones para Area basal
##Numero de clases con la ley de Sturges

finca_2$abasal_cm2<-with(finca_2,(pi/4)*DAP_cm^2)
n_clases<-round(1 + 3.322 * log10(length(finca_2$abasal_cm2)))
abhist_f2<-hist(finca_2$abasal_cm2,breaks=seq(20,800,78),ylab="Numero de individuos",
                xlab="Area basal",
                main="Distribuciones Area basal Finca 1")
ampl.intervalo<- (800 - 20)/ 10
marca.clase <- seq(20+(78/2),800-(78/2), 78)
lim.interv<-seq(20,800,(800-20)/10)
names(abhist_f2$counts) <- abhist_f2$mids
barplot(abhist_f2$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,250),
        main="Distribuciones Area basal Finca 2")


finca_3<-droplevels(subset(datos,Finca=="finca3"))

##Histograma de Distribuciones diametricas
hist_f3<-hist(finca_3$DAP_cm,breaks=c(seq(10,30,5)),ylab="Numero de individuos",
              xlab="DAPcm",
              main="Distribuciones diametricas Finca 3",
              ylim = c(0,150))
names(hist_f3$counts) <- hist_f3$mids
barplot(hist_f3$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,150),
        main="Distribuciones diametricas Finca 3")

##Histograma de Distribuciones para Area basal
##Numero de clases con la ley de Sturges

finca_3$abasal_cm2<-with(finca_3,(pi/4)*DAP_cm^2)
n_clases<-round(1 + 3.322 * log10(length(finca_3$abasal_cm2)))
abhist_f3<-hist(finca_3$abasal_cm2,breaks=seq(70,580,63),ylab="Numero de individuos",
                xlab="Area basal",
                main="Distribuciones Area basal Finca 3")
ampl.intervalo<- (580 - 70)/ 8
marca.clase <- seq(70+(63/2),580-(63/2), 63)
lim.interv<-seq(70,580,(580-70)/8)
names(abhist_f3$counts) <- abhist_f3$mids
barplot(abhist_f3$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,120),
        main="Distribuciones Area basal Finca 3")



finca_4<-droplevels(subset(datos,Finca=="finca4"))

##Histograma de Distribuciones diametricas
hist_f4<-hist(finca_4$DAP_cm,breaks=c(seq(25,70,5)),ylab="Numero de individuos",
              xlab="DAPcm",
              main="Distribuciones diametricas Finca 3",
              ylim = c(0,250))
names(hist_f4$counts) <- hist_f4$mids
barplot(hist_f4$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,250),
        main="Distribuciones diametricas Finca 4")

##Histograma de Distribuciones para Area basal
##Numero de clases con la ley de Sturges

finca_4$abasal_cm2<-with(finca_4,(pi/4)*DAP_cm^2)
n_clases<-round(1 + 3.322 * log10(length(finca_4$abasal_cm2)))
abhist_f4<-hist(finca_4$abasal_cm2,breaks=seq(620,4000,307),ylab="Numero de individuos",
                xlab="Area basal",
                main="Distribuciones Area basal Finca 4")
ampl.intervalo<- (4000 - 620)/ 11
marca.clase <- seq(620+(307/2),4000-(307/2), 307)
lim.interv<-seq(620,4000,(4000-620)/11)
names(abhist_f4$counts) <- abhist_f4$mids
barplot(abhist_f4$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,200),
        main="Distribuciones Area basal Finca 4")


finca_5<-droplevels(subset(datos,Finca=="finca5"))

##Histgrama de Distribuciones diametricas
hist_f5<-hist(finca_5$DAP_cm,breaks=c(seq(10,30,5)),ylab="Numero de individuos",
              xlab="DAPcm",
              main="Distribuciones diametricas Finca 5",
              ylim = c(0,400))
names(hist_f5$counts) <- hist_f5$mids
barplot(hist_f5$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,400),
        main="Distribuciones diametricas Finca 5")

##Histograma de Distribuciones para Area basal
##Numero de clases con la ley de Sturges

finca_5$abasal_cm2<-with(finca_5,(pi/4)*DAP_cm^2)
n_clases<-round(1 + 3.322 * log10(length(finca_5$abasal_cm2)))
abhist_f5<-hist(finca_5$abasal_cm2,breaks=seq(75,710,63.5),ylab="Numero de individuos",
                xlab="Area basal",
                main="Distribuciones Area basal Finca 5")
ampl.intervalo<- (710 - 75)/ 10
marca.clase <- seq(75+(63.5/2),710-(63.5/2), 63.5)
lim.interv<-seq(75,710,(710-75)/10)
names(abhist_f5$counts) <- abhist_f5$mids
barplot(abhist_f5$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,300),
        main="Distribuciones Area basal Finca 5")


##GRAFICAS
windows()
##Diametro
par(mfrow=c(3,2))
barplot(hist_f0$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,35),
        main="Distribuciones diametricas Finca 0")
barplot(hist_f1$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,60),
        main="Distribuciones diametricas Finca 1")
barplot(hist_f2$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,200),
        main="Distribuciones diametricas Finca 2")
barplot(hist_f3$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,150),
        main="Distribuciones diametricas Finca 3")
barplot(hist_f4$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,250),
        main="Distribuciones diametricas Finca 4")
barplot(hist_f5$counts,las=2, xlab= "DAP (cm)", ylab="Numero de individuos",ylim=c(0,400),
        main="Distribuciones diametricas Finca 5")

windows()
##area basal
par(mfrow=c(3,2))
barplot(abhist_f0$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,100),
        main="Distribuciones Area basal Finca 0")
barplot(abhist_f1$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,250),
        main="Distribuciones Area basal Finca 1")
barplot(abhist_f2$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,250),
        main="Distribuciones Area basal Finca 2")
barplot(abhist_f3$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,120),
        main="Distribuciones Area basal Finca 3")
barplot(abhist_f4$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,200),
        main="Distribuciones Area basal Finca 4")
barplot(abhist_f5$counts,las=2, xlab= "Area basal(cm2)", ylab="Numero de individuos",ylim=c(0,300),
        main="Distribuciones Area basal Finca 5")



##Punto 2

finca_0<-droplevels(subset(datos,Finca=="finca0"))


ab_m2_ha_f0 <- (with(finca_0,tapply(abasal_cm2/10000,Parcela,sum)))/0.1
ab_m2_ha_f0<-data.frame(ab_m2_ha_f0)
ab_m2_ha_f0$Nombre_parc<-row.names(ab_m2_ha_f0)


f0 <- unique(finca_0[,c("Parcela","Finca")])
ab_m2_ha_f0<-merge(ab_m2_ha_f0,f0,by="Parcela",all=FALSE)
ab_m2_ha_f0
ab_m2_ha_f0_mean<-with(ab_m2_ha_f0, tapply(ab_m2_ha_f0,Finca,mean))
ab_m2_ha_f0_sd<-with(ab_m2_ha_f0, tapply(ab_m2_ha_f0,Finca,sd))


Dq_cm_f0 <- sqrt(with(finca_0,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f0<-data.frame(Dq_cm_f0)
Dq_cm_f0$Nombre_parc<-row.names(Dq_cm_f0)
fin0<-merge(ab_m2_ha_f0,Dq_cm_f0,by="Nombre_parc",all=FALSE)