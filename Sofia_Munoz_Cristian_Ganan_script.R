######################### PARCIAL 1 DASOMETRIA #############################
### Presentado por: Cristian Camilo Gañan y Sofia Muñoz Duque

library(readxl)
library(tidyverse)
datos<-read_xlsx("parcial1.xlsx")

datos<- datos %>% select(Parcela, 
                         Finca, ...4) %>% 
  rename(DAP_cm= Parcela,
         Parcela= Finca,
         Finca= ...4) %>% 
  mutate(Parcela = as.factor(Parcela),
         Finca= as.factor(Finca))

str(datos)

##PUNTO 1
##Analice la distribución de los DAP (cm) y área basal (cm2) de los árboles
##muestreados en cada finca mediante el uso de un histograma en cada una de
##ellas. Concluya y explique que diferencias estructurales generales se visualizan
#entre fincas, para cada una de las variables estudiadas. Analice los histogramas
#usando intervalos de clase de 5 cm (para el DAP) y usando el número de
#intervalos de clase que se defina bajo la regla de Sturges para el área basal (20%)

datos$abasal_cm2<-with(datos,(pi/4)*DAP_cm^2)

###FINCA 0
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

###FINCA 1
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

###FINCA 2
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

###FINCA 3
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

###FINCA 4
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

###FINCA 5
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



### Punto 2

datos$Nombre_parc<-with(datos,paste(Parcela,Finca,sep=""))

##FINCA 0
finca_0<-droplevels(subset(datos,Finca=="finca0"))

ab_m2_ha <- (with(finca_0,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.1
ab_m2_ha_f0<-data.frame(ab_m2_ha)
ab_m2_ha_f0$Nombre_parc<-row.names(ab_m2_ha_f0)

Dq_cm <- sqrt(with(finca_0,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f0<-data.frame(Dq_cm)
Dq_cm_f0$Nombre_parc<-row.names(Dq_cm_f0)
fin0<-merge(ab_m2_ha_f0,Dq_cm_f0,by="Nombre_parc",all=FALSE)


f0 <- unique(finca_0[,c("Nombre_parc","Finca")])
ab_m2_ha_f0<-merge(ab_m2_ha_f0,f0,by="Nombre_parc",all=FALSE)

n_ind<- with(finca_0,tapply(DAP_cm,Nombre_parc,length))
nind_ha<-n_ind/0.1
nind_ha_f0<-data.frame(nind_ha)
nind_ha_f0$Nombre_parc<-row.names(nind_ha_f0)
fin_0<-merge(fin0,nind_ha_f0,by="Nombre_parc",all=FALSE)

#Finca 1

finca_1<-droplevels(subset(datos,Finca=="finca1"))

ab_m2_ha <- (with(finca_1,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.1
ab_m2_ha_f1<-data.frame(ab_m2_ha)
ab_m2_ha_f1$Nombre_parc<-row.names(ab_m2_ha_f1)


f1 <- unique(finca_1[,c("Nombre_parc","Finca")])
ab_m2_ha_f1<-merge(ab_m2_ha_f1,f1,by="Nombre_parc",all=FALSE)

Dq_cm <- sqrt(with(finca_1,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f1<-data.frame(Dq_cm)
Dq_cm_f1$Nombre_parc<-row.names(Dq_cm_f1)
fin1<-merge(ab_m2_ha_f1,Dq_cm_f1,by="Nombre_parc",all=FALSE)


n_ind<- with(finca_1,tapply(DAP_cm,Nombre_parc,length))
nind_ha<-n_ind/0.1
nind_ha_f1<-data.frame(nind_ha)
nind_ha_f1$Nombre_parc<-row.names(nind_ha_f1)
fin_1<-merge(fin1,nind_ha_f1,by="Nombre_parc",all=FALSE)

##FINCA 2
finca_2<-droplevels(subset(datos,Finca=="finca2"))

ab_m2_ha <- (with(finca_2,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.1
ab_m2_ha_f2<-data.frame(ab_m2_ha)
ab_m2_ha_f2$Nombre_parc<-row.names(ab_m2_ha_f2)


f2 <- unique(finca_2[,c("Nombre_parc","Finca")])
ab_m2_ha_f2<-merge(ab_m2_ha_f2,f2,by="Nombre_parc",all=FALSE)

Dq_cm <- sqrt(with(finca_2,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f2<-data.frame(Dq_cm)
Dq_cm_f2$Nombre_parc<-row.names(Dq_cm_f2)
fin2<-merge(ab_m2_ha_f2,Dq_cm_f2,by="Nombre_parc",all=FALSE)


n_ind<- with(finca_2,tapply(DAP_cm,Nombre_parc,length))
nind_ha<-n_ind/0.1
nind_ha_f2<-data.frame(nind_ha)
nind_ha_f2$Nombre_parc<-row.names(nind_ha_f2)
fin_2<-merge(fin2,nind_ha_f2,by="Nombre_parc",all=FALSE)

#Finca 3

finca_3<-droplevels(subset(datos,Finca=="finca3"))

ab_m2_ha <- (with(finca_3,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.25
ab_m2_ha_f3<-data.frame(ab_m2_ha)
ab_m2_ha_f3$Nombre_parc<-row.names(ab_m2_ha_f3)



f3 <- unique(finca_3[,c("Nombre_parc","Finca")])
ab_m2_ha_f3<-merge(ab_m2_ha_f3,f3,by="Nombre_parc",all=FALSE)

Dq_cm <- sqrt(with(finca_3,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f3<-data.frame(Dq_cm)
Dq_cm_f3$Nombre_parc<-row.names(Dq_cm_f3)
fin3<-merge(ab_m2_ha_f3,Dq_cm_f3,by="Nombre_parc",all=FALSE)


n_ind<- with(finca_3,tapply(DAP_cm,Nombre_parc,length))
nind_ha<-n_ind/0.25
nind_ha_f3<-data.frame(nind_ha)
nind_ha_f3$Nombre_parc<-row.names(nind_ha_f3)
fin_3<-merge(fin3,nind_ha_f3,by="Nombre_parc",all=FALSE)


##FINCA 4
finca_4<-droplevels(subset(datos,Finca=="finca4"))

ab_m2_ha <- (with(finca_4,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.2

ab_m2_ha_f4<-data.frame(ab_m2_ha)
ab_m2_ha_f4$Nombre_parc<-row.names(ab_m2_ha_f4)


f4 <- unique(finca_4[,c("Nombre_parc","Finca")])
ab_m2_ha_f4<-merge(ab_m2_ha_f4,f4,by="Nombre_parc",all=FALSE)


Dq_cm <- sqrt(with(finca_4,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f4<-data.frame(Dq_cm)
Dq_cm_f4$Nombre_parc<-row.names(Dq_cm_f4)
fin4<-merge(ab_m2_ha_f4,Dq_cm_f4,by="Nombre_parc",all=FALSE)


n_ind<- with(finca_4,tapply(DAP_cm,Nombre_parc,length))
nind_ha<-n_ind/0.2
nind_ha_f4<-data.frame(nind_ha)
nind_ha_f4$Nombre_parc<-row.names(nind_ha_f4)
fin_4<-merge(fin4,nind_ha_f4,by="Nombre_parc",all=FALSE)

##FINCA 5
finca_5<-droplevels(subset(datos,Finca=="finca5"))

ab_m2_ha <- (with(finca_5,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.1
ab_m2_ha_f5<-data.frame(ab_m2_ha)
ab_m2_ha_f5$Nombre_parc<-row.names(ab_m2_ha_f5)


f5 <- unique(finca_5[,c("Nombre_parc","Finca")])
ab_m2_ha_f5<-merge(ab_m2_ha_f5,f5,by="Nombre_parc",all=FALSE)

Dq_cm <- sqrt(with(finca_5,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f5<-data.frame(Dq_cm)
Dq_cm_f5$Nombre_parc<-row.names(Dq_cm_f5)

n_ind<- with(finca_5,tapply(DAP_cm,Nombre_parc,length))
nind_ha<-n_ind/0.1
nind_ha_f5<-data.frame(nind_ha)
nind_ha_f5$Nombre_parc<-row.names(nind_ha_f5)
fin_5<-merge(fin5,nind_ha_f5,by="Nombre_parc",all=FALSE)

base<- rbind(fin_0, fin_1, fin_2, fin_3, fin_4, fin_5)


data2<- base[base$Finca == "finca0" | base$Finca == "finca2" | 
               base$Finca == "finca4", ]

a<- cor.test(data2$ab_m2_ha, data2$nind_ha)
b<- cor.test(data2$Dq_cm, data2$ab_m2_ha)
c<- cor.test(data2$Dq_cm, data2$nind_ha)

a
b
c



#----------------------------------------------------------------------------------------------------
###PUNTO 3
##Calcule el promedio y la desviación estándar del número de individuos (ha),
##área basal (m 2 /ha) y diámetro cuadrático medio (cm) de cada una de las fincas.
##Mediante un análisis de varianza (ANAVA) de una vía, defina si hay diferencias
##significativas entre fincas para las tres variables definidas. Gráficamente,
##usando un box-plot (diagrama de cajas), muestre y explique las diferencias en
##aquellos casos en los que entre fincas sean significativas (50%).

datos$Nombre_parc<-with(datos,paste(Parcela,Finca,sep=""))

##FINCA 0
finca_0<-droplevels(subset(datos,Finca=="finca0"))

ab_m2_ha_f0 <- (with(finca_0,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.1
ab_m2_ha_f0<-data.frame(ab_m2_ha_f0)
ab_m2_ha_f0$Nombre_parc<-row.names(ab_m2_ha_f0)


f0 <- unique(finca_0[,c("Nombre_parc","Finca")])
ab_m2_ha_f0<-merge(ab_m2_ha_f0,f0,by="Nombre_parc",all=FALSE)
ab_m2_ha_f0
ab_m2_ha_f0_mean<-with(ab_m2_ha_f0, tapply(ab_m2_ha_f0,Finca,mean))
ab_m2_ha_f0_sd<-with(ab_m2_ha_f0, tapply(ab_m2_ha_f0,Finca,sd))

  
Dq_cm_f0 <- sqrt(with(finca_0,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f0<-data.frame(Dq_cm_f0)
Dq_cm_f0$Nombre_parc<-row.names(Dq_cm_f0)
fin0<-merge(ab_m2_ha_f0,Dq_cm_f0,by="Nombre_parc",all=FALSE)

Dq_cm_f0_mean<-with(fin0, tapply(Dq_cm_f0,Finca,mean))
Dq_cm_f0_sd<-with(fin0, tapply(Dq_cm_f0,Finca,sd))

n_ind_f0<- with(finca_0,tapply(DAP_cm,Nombre_parc,length))
nind_ha_f0<-n_ind_f0/0.1
nind_ha_f0
nind_ha_f0<-data.frame(nind_ha_f0)
nind_ha_f0$Nombre_parc<-row.names(nind_ha_f0)
fin_0<-merge(fin0,nind_ha_f0,by="Nombre_parc",all=FALSE)
nind_ha_f0_mean<-with(fin_0, tapply(nind_ha_f0,Finca,mean))
nind_ha_f0_sd<-with(fin_0, tapply(nind_ha_f0,Finca,sd))

ab_m2_ha_f0_mean
ab_m2_ha_f0_sd
Dq_cm_f0_mean
Dq_cm_f0_sd
nind_ha_f0_mean
nind_ha_f0_sd

##FINCA 1
finca_1<-droplevels(subset(datos,Finca=="finca1"))

ab_m2_ha_f1 <- (with(finca_1,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.1
ab_m2_ha_f1<-data.frame(ab_m2_ha_f1)
ab_m2_ha_f1$Nombre_parc<-row.names(ab_m2_ha_f1)


f1 <- unique(finca_1[,c("Nombre_parc","Finca")])
ab_m2_ha_f1<-merge(ab_m2_ha_f1,f1,by="Nombre_parc",all=FALSE)
ab_m2_ha_f1
ab_m2_ha_f1_mean<-with(ab_m2_ha_f1, tapply(ab_m2_ha_f1,Finca,mean))
ab_m2_ha_f1_sd<-with(ab_m2_ha_f1, tapply(ab_m2_ha_f1,Finca,sd))


Dq_cm_f1 <- sqrt(with(finca_1,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f1<-data.frame(Dq_cm_f1)
Dq_cm_f1$Nombre_parc<-row.names(Dq_cm_f1)
fin1<-merge(ab_m2_ha_f1,Dq_cm_f1,by="Nombre_parc",all=FALSE)

Dq_cm_f1_mean<-with(fin1, tapply(Dq_cm_f1,Finca,mean))
Dq_cm_f1_sd<-with(fin1, tapply(Dq_cm_f1,Finca,sd))

n_ind_f1<- with(finca_1,tapply(DAP_cm,Nombre_parc,length))
nind_ha_f1<-n_ind_f1/0.1
nind_ha_f1
nind_ha_f1<-data.frame(nind_ha_f1)
nind_ha_f1$Nombre_parc<-row.names(nind_ha_f1)
fin_1<-merge(fin1,nind_ha_f1,by="Nombre_parc",all=FALSE)
nind_ha_f1_mean<-with(fin_1, tapply(nind_ha_f1,Finca,mean))
nind_ha_f1_sd<-with(fin_1, tapply(nind_ha_f1,Finca,sd))

ab_m2_ha_f1_mean
ab_m2_ha_f1_sd
Dq_cm_f1_mean
Dq_cm_f1_sd
nind_ha_f1_mean
nind_ha_f1_sd

##FINCA 2
finca_2<-droplevels(subset(datos,Finca=="finca2"))

ab_m2_ha_f2 <- (with(finca_2,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.1
ab_m2_ha_f2<-data.frame(ab_m2_ha_f2)
ab_m2_ha_f2$Nombre_parc<-row.names(ab_m2_ha_f2)


f2 <- unique(finca_2[,c("Nombre_parc","Finca")])
ab_m2_ha_f2<-merge(ab_m2_ha_f2,f2,by="Nombre_parc",all=FALSE)
ab_m2_ha_f2
ab_m2_ha_f2_mean<-with(ab_m2_ha_f2, tapply(ab_m2_ha_f2,Finca,mean))
ab_m2_ha_f2_sd<-with(ab_m2_ha_f2, tapply(ab_m2_ha_f2,Finca,sd))



Dq_cm_f2 <- sqrt(with(finca_2,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f2<-data.frame(Dq_cm_f2)
Dq_cm_f2$Nombre_parc<-row.names(Dq_cm_f2)
fin2<-merge(ab_m2_ha_f2,Dq_cm_f2,by="Nombre_parc",all=FALSE)

Dq_cm_f2_mean<-with(fin2, tapply(Dq_cm_f2,Finca,mean))
Dq_cm_f2_sd<-with(fin2, tapply(Dq_cm_f2,Finca,sd))


n_ind_f2<- with(finca_2,tapply(DAP_cm,Nombre_parc,length))
nind_ha_f2<-n_ind_f2/0.1
nind_ha_f2
nind_ha_f2<-data.frame(nind_ha_f2)
nind_ha_f2$Nombre_parc<-row.names(nind_ha_f2)
fin_2<-merge(fin2,nind_ha_f2,by="Nombre_parc",all=FALSE)
nind_ha_f2_mean<-with(fin_2, tapply(nind_ha_f2,Finca,mean))
nind_ha_f2_sd<-with(fin_2, tapply(nind_ha_f2,Finca,sd))

ab_m2_ha_f2_mean
ab_m2_ha_f2_sd
Dq_cm_f2_mean
Dq_cm_f2_sd
nind_ha_f2_mean
nind_ha_f2_sd

##FINCA 3
finca_3<-droplevels(subset(datos,Finca=="finca3"))

ab_m2_ha_f3 <- (with(finca_3,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.25
ab_m2_ha_f3<-data.frame(ab_m2_ha_f3)
ab_m2_ha_f3$Nombre_parc<-row.names(ab_m2_ha_f3)



f3 <- unique(finca_3[,c("Nombre_parc","Finca")])
ab_m2_ha_f3<-merge(ab_m2_ha_f3,f3,by="Nombre_parc",all=FALSE)
ab_m2_ha_f3
ab_m2_ha_f3_mean<-with(ab_m2_ha_f3, tapply(ab_m2_ha_f3,Finca,mean))
ab_m2_ha_f3_sd<-with(ab_m2_ha_f3, tapply(ab_m2_ha_f3,Finca,sd))


Dq_cm_f3 <- sqrt(with(finca_3,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f3<-data.frame(Dq_cm_f3)
Dq_cm_f3$Nombre_parc<-row.names(Dq_cm_f3)
fin3<-merge(ab_m2_ha_f3,Dq_cm_f3,by="Nombre_parc",all=FALSE)

Dq_cm_f3_mean<-with(fin3, tapply(Dq_cm_f3,Finca,mean))
Dq_cm_f3_sd<-with(fin3, tapply(Dq_cm_f3,Finca,sd))

n_ind_f3<- with(finca_3,tapply(DAP_cm,Nombre_parc,length))
nind_ha_f3<-n_ind_f3/0.25
nind_ha_f3
nind_ha_f3<-data.frame(nind_ha_f3)
nind_ha_f3$Nombre_parc<-row.names(nind_ha_f3)
fin_3<-merge(fin3,nind_ha_f3,by="Nombre_parc",all=FALSE)
nind_ha_f3_mean<-with(fin_3, tapply(nind_ha_f3,Finca,mean))
nind_ha_f3_sd<-with(fin_3, tapply(nind_ha_f3,Finca,sd))

ab_m2_ha_f3_mean
ab_m2_ha_f3_sd
Dq_cm_f3_mean


Dq_cm_f3_sd
nind_ha_f3_mean
nind_ha_f3_sd

##FINCA 4
finca_4<-droplevels(subset(datos,Finca=="finca4"))

ab_m2_ha_f4 <- (with(finca_4,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.2

ab_m2_ha_f4<-data.frame(ab_m2_ha_f4)
ab_m2_ha_f4$Nombre_parc<-row.names(ab_m2_ha_f4)


f4 <- unique(finca_4[,c("Nombre_parc","Finca")])
ab_m2_ha_f4<-merge(ab_m2_ha_f4,f4,by="Nombre_parc",all=FALSE)
ab_m2_ha_f4
ab_m2_ha_f4_mean<-with(ab_m2_ha_f4, tapply(ab_m2_ha_f4,Finca,mean))
ab_m2_ha_f4_sd<-with(ab_m2_ha_f4, tapply(ab_m2_ha_f4,Finca,sd))


Dq_cm_f4 <- sqrt(with(finca_4,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f4<-data.frame(Dq_cm_f4)
Dq_cm_f4$Nombre_parc<-row.names(Dq_cm_f4)
fin4<-merge(ab_m2_ha_f4,Dq_cm_f4,by="Nombre_parc",all=FALSE)

Dq_cm_f4_mean<-with(fin4, tapply(Dq_cm_f4,Finca,mean))
Dq_cm_f4_sd<-with(fin4, tapply(Dq_cm_f4,Finca,sd))

n_ind_f4<- with(finca_4,tapply(DAP_cm,Nombre_parc,length))
nind_ha_f4<-n_ind_f4/0.2
nind_ha_f4
nind_ha_f4<-data.frame(nind_ha_f4)
nind_ha_f4$Nombre_parc<-row.names(nind_ha_f4)
fin_4<-merge(fin4,nind_ha_f4,by="Nombre_parc",all=FALSE)
nind_ha_f4_mean<-with(fin_4, tapply(nind_ha_f4,Finca,mean))
nind_ha_f4_sd<-with(fin_4, tapply(nind_ha_f4,Finca,sd))

ab_m2_ha_f4_mean
ab_m2_ha_f4_sd
Dq_cm_f4_mean
Dq_cm_f4_sd
nind_ha_f4_mean
nind_ha_f4_sd

##FINCA 5
finca_5<-droplevels(subset(datos,Finca=="finca5"))

ab_m2_ha_f5 <- (with(finca_5,tapply(abasal_cm2/10000,Nombre_parc,sum)))/0.1
ab_m2_ha_f5<-data.frame(ab_m2_ha_f5)
ab_m2_ha_f5$Nombre_parc<-row.names(ab_m2_ha_f5)


f5 <- unique(finca_5[,c("Nombre_parc","Finca")])
ab_m2_ha_f5<-merge(ab_m2_ha_f5,f5,by="Nombre_parc",all=FALSE)
ab_m2_ha_f5
ab_m2_ha_f5_mean<-with(ab_m2_ha_f5, tapply(ab_m2_ha_f5,Finca,mean))
ab_m2_ha_f5_sd<-with(ab_m2_ha_f5, tapply(ab_m2_ha_f5,Finca,sd))


Dq_cm_f5 <- sqrt(with(finca_5,tapply(DAP_cm^2,Nombre_parc,mean)))
Dq_cm_f5<-data.frame(Dq_cm_f5)
Dq_cm_f5$Nombre_parc<-row.names(Dq_cm_f5)
fin5<-merge(ab_m2_ha_f5,Dq_cm_f5,by="Nombre_parc",all=FALSE)

Dq_cm_f5_mean<-with(fin5, tapply(Dq_cm_f5,Finca,mean))
Dq_cm_f5_sd<-with(fin5, tapply(Dq_cm_f5,Finca,sd))

n_ind_f5<- with(finca_5,tapply(DAP_cm,Nombre_parc,length))
nind_ha_f5<-n_ind_f5/0.1
nind_ha_f5
nind_ha_f5<-data.frame(nind_ha_f5)
nind_ha_f5$Nombre_parc<-row.names(nind_ha_f5)
fin_5<-merge(fin5,nind_ha_f5,by="Nombre_parc",all=FALSE)
nind_ha_f5_mean<-with(fin_5, tapply(nind_ha_f5,Finca,mean))
nind_ha_f5_sd<-with(fin_5, tapply(nind_ha_f5,Finca,sd))

ab_m2_ha_f5_mean
ab_m2_ha_f5_sd
Dq_cm_f5_mean
Dq_cm_f5_sd
nind_ha_f5_mean
nind_ha_f5_sd

aov.abasal.region<-aov(A_basal_m2_ha~Region,est_congl2)
