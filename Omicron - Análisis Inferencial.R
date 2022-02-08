# -------------------------------------------------------------------------------
# ANALISIS ESTADISTICO VARIABLE OMICRON EN ECUADOR
#
# Autor: Edith Perez
# 7-Feb-2022
# -------------------------------------------------------------------------------
# Modificado: Maria Belen Benalcazar Tovar 
# 8-Feb-2022
# Estructuración de gráficos

setwd("C:/Users/User/Desktop/UNIR/AnalisisInterpretacionDatos/Actividad 2/ARIMA_R")

#Se instala los paquetes necesarios
# install.packages("tseries")
# install.packages("astsa")
# install.packages("forecast")
# install.packages("foreign")
# install.packages("quantmod")
# install.packages("lubridate")
# install.packages("readxl")

#Se activa las funciones de los paquetes anteriores
library("tseries")
library("astsa")
library("forecast")
library("foreign")
library("quantmod")
library("lubridate")
library("ggplot2")
library("readxl")

#Muestra
#Casos totales confirmados en el periodo 14 de noviembre 2021 hasta 14 de enero de 2022
d <- read.csv("https://raw.githubusercontent.com/mabebt2b/OMICRON/main/datos_inferencial.csv")

#Se convierte a serie temporal
d$Fecha <- as.Date(d$Fecha)
d$Casos <- ts(d$Casos)
#Gráfico inicial de la evolución del número de casos
par(mar = c(3, 4, 2.5, 1) + 0.5)  # Márgenes para ejes
plot(d$Fecha,d$Casos,type="l",lwd=2,col="red",axes = TRUE,  xlab = "", ylab = "",cex.axis=0.7,las=2,xaxt="n", font=7)
axis.Date(1, at = seq(as.Date("2021-11-12"), as.Date("2022-01-20"),by = "3.5 days"),las = 1,font=7,cex.axis=0.7,mgp=c(3,0.5,0))
mtext("EVOLUCIÓN DE NÚMERO DE CASOS COVID-19 EN ECUADOR",side=3, line=.8, cex=1, font=2)
mtext("Núm. Casos",side=2, line=3.2, cex=0.8, font=2)
mtext("Fecha",side=1, line=1.4, cex=0.8, font=2)

#Modelo

# 1.) Estacionariedad
#Se logra estacionaria con una diferencia
adf.test(d$Casos,alternative = "stationary")
Casos_dif <- diff(d$Casos)
adf.test(Casos_dif,alternative = "stationary")
Casos_dif2 <- diff(Casos_dif)
adf.test(Casos_dif2,alternative = "stationary")

# Se grafica la serie estacionaria
par(mar = c(3, 4, 2.5, 1) + 0.5)  # Márgenes para ejes
plot(Casos_dif2,type="o",lwd=2,col=c(rgb(0.0,0.7,0.8)), axes = TRUE,  xlab = "", ylab = "",xaxt="n",yaxt="n")
mtext("Número de casos - Serie estacionaria a través de diferencias",side=3, line=.8, cex=1, font=2)
axis(1, at = pretty(range(62,10)), las=0,font=7,cex.axis=0.8,mgp=c(3,0.3,0))
axis(2, at = pretty(range(Casos_dif2,2000)), hadj=1.25,las=1,font=7,cex.axis=0.8,mgp=c(3,0.3,0))
mtext("Casos_dif2",side=2, line=3, cex=0.8, font=2)
grid(nx = NULL, ny = NULL, lty = 3, col = "gray", lwd = 1)


# 2.) ParÃ¡metros modelo ARIMA (p,d,q)
par(mfrow=c(2,1),mar = c(0, 2, 2, 1) + 0.5)
#Funcion de autocorrelacion: nÃºmero de medias mÃ³viles
acf(Casos_dif2,main="",ylab="",col="red",xlab="",font=7,cex.axis=0.7,las=2,xaxt="n")
mtext("Serie estacionaria - Autocorrelación (q)",side=3, line=.6, cex=0.9, font=2)
#Funcion de autocorrelacion parcial: nÃºmero de autoregresivos
pacf(Casos_dif2,main="",ylab="",col="red",xlab="",font=7,cex.axis=0.7,las=2,xaxt="n")
mtext("Serie estacionaria - Autocorrelación parcial (p)",side=3, line=.6, cex=0.9, font=2)


# 3.) Modelo ARIMA (1,2,3)
d.arima <- arima(d$Casos,order = c(1,2,3))
d.arima

# 4.) EvaluaciÃ³n del modelo
Box.test(residuals(d.arima),type = "Ljung-Box")
tsdiag(d.arima)
plot(d.arima)
# error <- residuals(modelo)
# plot(error)

# 5.) Pronostico
d.forecast <- forecast::forecast(d.arima,h=12)

par(mar = c(3, 4, 2, 1) + 0.5)  # Márgenes para ejes
plot(d.forecast,col="red",main="",axes = TRUE,  xlab = "", ylab = "", las=2,xaxt="n",yaxt="n",font.axis=7)
mtext("Pronóstico de casos COVID-19 - 12 días",side=3, line=.8, cex=1, font=2)
mtext("Núm. Casos",side=2, line=3.2, cex=0.8, font=2)
mtext("Número de días",side=1, line=1.4, cex=0.8, font=2)
axis(1, at = pretty(range(74,10)), las=0,font=7,cex.axis=0.8,mgp=c(3,0.3,0))
axis(2, at = pretty(range(650000:950000)),las=1,font=7,cex.axis=0.8,mgp=c(3,0.3,0),hadj=1.2)
grid(nx = NULL, ny = NULL, lty = 3, col = "gray", lwd = 1)
