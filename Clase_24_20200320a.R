
# validacion modelos de regresion
# evaluar la capacidad de un modelo para predecir la variable respuesta

# validacion simple
## dividir las observaciones de forma aleatoria en dos grupos, 
## uno se emplea como set de entrenamiento y otro para la validacion

bd <- read.csv2("dap-h.csv")

set.seed(1)
d1 <- sample(1:dim(bd)[1], dim(bd)[1]/2)

# Ajusto el modelo empleando la mitad de los datos
m2 <- lm(Altura_m ~ DAP_cm, data = bd, subset = d1)
summary(m2)

# estimacion de las alturas empleando los datos no usados en el modelo
h.m2 <- predict(m2, newdata = bd[-d1,])

# error de prediccion del modelo: cuadrado medio del error (MSE)
res.m2 <- (bd$Altura_m[-d1] - h.m2)^2
mean(res.m2)

# el error varia en funcion del set de datos seleccionado
# para ajustar el modelo y el empleado para la validacion

mse1 <- rep(NA, 999)
for(i in 1:999){
  d.i <- sample(1:dim(bd)[1], dim(bd)[1]/2)
  m.i <- lm(Altura_m ~ DAP_cm, data = bd, subset = d.i)
  p.i <- predict(m.i, newdata = bd[-d.i,])
  mse1[i] <-  mean((bd$Altura_m[-d.i] - p.i)^2)
}
summary(mse1)
hist(mse1)
#


###
# Leave One Out Cross-Validation (LOOCV)
# Metodo iterativo que consiste en ajustar el modelo con todas las
# observaciones disponibles excepto una, que se usa para validar.

m3 <- lm(Altura_m ~ DAP_cm, data = bd, subset = -1)
summary(m3)

(h.m3 <- predict(m3, newdata = bd[1,]))
# error de prediccion del modelo: cuadrado medio del error (MSE)
(res.m3 <- (bd$Altura_m[1] - h.m3)^2)

####
# Como generar un loop para este analisis??
####


###
# K-fold Cross-Validation
# Consiste en dividir los datos de forma aleatoria en k grupos del mismo tamaño.
# k-1 grupos se emplean para entrenar el modelo y uno de los grupos se emplea
# como test, este proceso se repite k veces utilizando un grupo distinto como
# test en cada iteracion. El proceso genera k estimaciones del test error
# cuyo promedio se emplea como estimacion final.

# aleatorizo la seleccion de los arboles
set.seed(1)
aa <- sample(1:dim(bd)[1], dim(bd)[1])

# numero de grupos (k)= 10
(nk <- round(length(aa)/10))

# intervalos seleccion grupos aleatorios
(ki <- seq(0, 290, nk))
(k1 <- aa[seq(ki[1]+1, ki[2])])

# ajusto el modelo excluyendo un grupo
m4 <- lm(Altura_m ~ DAP_cm, data = bd, subset = -k1)
summary(m4)

# valores predichos por el modelo
h.m4 <- predict(m4, newdata = bd[k1,])

# error de prediccion del modelo: cuadrado medio del error (MSE)
res.m4 <- (bd$Altura_m[k1] - h.m4)^2
mean(res.m4)

####
# Como generar un loop para este analisis??
####


##
# Bootstrap en la estimacion de la precision de un modelo de regresion.
# Permite analizar la variabilidad en la estimacion de los coeficientes de regresion

# El proceso de bootstraping consiste en generar de forma iterativa
# diferentes modelos lineales, empleando en cada caso una bootstrap-sample
# creada mediante resampling del mismo tamaño que la muestra inicial.
# Para cada modelo ajustado se registran los valores de los coeficientes B0 y B1
# y finalmente se estudia su distribucion.

b.0 <- b.1 <- rep(NA, 9999)
for(i in 1:9999){
  s.i <- sample(1:dim(bd)[1], dim(bd)[1], replace = T)
  m.i <- lm(Altura_m ~ DAP_cm, data = bd, subset = s.i)
  b.0[i] <- coefficients(m.i)[1]
  b.1[i] <- coefficients(m.i)[2]
}
summary(b.0)
hist(b.0)
summary(b.1)
hist(b.1)

##
# Bootstrapping
# Calcular intervalos de confianza de un parametro poblacional
# Calcular significancia estadistica (p-value) para la diferencia entre dos poblaciones
# Calcular intervalos de confianza para la diferencia entre dos poblaciones
# 

# Modelo lineal multiple --------------------------------------------------

bd <- read.csv2("biomasa.csv")
str(bd)
summary(bd)

pairs(bd[,c(6,2:5)], panel=panel.smooth)

panel.cor <- function(x, y, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- (cor(x, y, method = "pearson"))
  txt <- format(c(r, 0.123456789), digits=2)[1]
  ra <- cor.test(x, y, method = "pearson")$p.value
  prefix <- ""
  if(ra <= 0.1) prefix <- "."
  if(ra <= 0.05) prefix <- "*"
  if(ra <= 0.01) prefix <- "**"
  if(ra <= 0.001) prefix <- "***"
  txt <- paste(txt, prefix, sep="\n")
  text(0.5, 0.5, txt)
}

pairs(bd[,c(6,2:5)], lower.panel = panel.cor)

# 
m1 <- lm(AGB_kg ~ DAP_cm + Altura_m + WDtrunk_gcm3 + WDbranch_gcm3, data = bd)
summary(m1)

# seleccion de variables
# existe un total 2p modelos que contengan subgrupos de p predictores,
# por lo que evaluar cada combinacion posible resulta inviable

# Metodo Forward: se parte del modelo nulo, luego se ajustan p modelos simples
# y se añade al modelo nulo la variable con la que se obtiene el menor RSS.
m1.f <- step(m1, direction  = "forward")
summary(m1.f)

# Metodo Backward: se parte del modelo completo con todos los predictores,
# y se elimina el que tenga el mayor p-value. El nuevo modelo (p-1) se reajusta
# y se continuan eliminando variables no significativas
m1.b <- step(m1, direction  = "backward")
summary(m1.b)

# seleccion manual
summary(m1)
summary(m2 <- update(m1, ~. -WDbranch_gcm3))
AIC(m2)
AIC(m1)
# plot(m2)

# inclusion interacciones
m3 <- lm(AGB_kg ~  DAP_cm*Altura_m, data = bd)
summary(m3)

m4 <- lm(AGB_kg ~  DAP_cm:Altura_m, data = bd)
summary(m4)


# transformacion variables
m5 <- lm(log(AGB_kg) ~ log(DAP_cm) + log(Altura_m) + log(WDtrunk_gcm3) +
           log(WDbranch_gcm3), data = bd)
summary(m5)

summary(m6 <- update(m5, ~. -log(WDbranch_gcm3)))
AIC(m6)

# Validacion de condiciones
# Analisis de los residuos (distribucion, variabilidad)
plot(m6)

# analisis de normalidad
shapiro.test(m6$residuals)

# Residuos estudentizados para deteccion de outliers o puntos influyentes
plot(predict(m6), rstudent(m6)) 
abline(h=0, col="red")

# Deteccion de observaciones influyentes
# Residuales estudentizados (detecta observaciones atipicas)
student_r <- rstudent(m6)
which(abs(student_r) > 3)

#
library(car)
outlierTest(m6) # Test de deteccion de outliers
#

# analisis de multicolinealidad
# Calcula VIFs (factor de inflacion de la varianza)
vif(m11)
# valores entre 5 y 10 indican posibles problemas
# valores mayores o iguales a 10 se consideran problematicos

m6a <- lm(log(AGB_kg) ~ log(DAP_cm) + log(Altura_m) + log(WDtrunk_gcm3),
          data = bd[-c(27,129,160),])
summary(m6a)
# plot(m6a)
shapiro.test(resid(m6a))

