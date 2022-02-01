# -------------------------------------------------------------------------------
# ANALISIS ESTADISTICO VARIABLE OMICRON EN ECUADOR
#
# Autor: Maria Belen Benalcazar Tovar
# 29-Ene-2022
# -------------------------------------------------------------------------------
# Modificado: Patricia Tigrero. 
# 30-Ene-2022
# Estructuración de gráficos, medidas tendencia central
# -------------------------------------------------------------------------------
# Modificado: Belén Benalcázar
# 31-Ene-2022
# Estructura de gráfico - función par()


# Se cargan los datasets limpiados, preparados y publicados en GitHub
path_ds<-"https://raw.githubusercontent.com/mabebt2b/OMICRON/main/covidecu.csv"
ds_casos<-read.csv(path_ds)

#Transformacion de la columna fecha sobre la misma columna
ds$fecha <- as.Date(ds$fecha)
summary(ds_casos)

#Asignación de variables para la creación de gráficos,
t<-as.Date(ds_casos[,"fecha"])
x<-ds_casos[,"muertes"]
y<-ds_casos[,"positivas_total"]

#Reiniciar el área del gráfico
dev.off()

# GRAFICO DE CASOS Y MUERTES (OPCION 1, GRAFICAS SUPERPUESTAS CASOS POSITIVOS Y MUERTES)
par(mar = c(3, 4, 2.5, 4) + 0.5)  # Márgenes para ejes
# Se grafica numero de casos (positivas total) en función del tiempo con color rojo (554) ,ylim=range(c(y,x))
plot(t, y, type = "h", col=rgb(red = 0.4, green = 0.4, blue = 0.4, alpha = 0.5), lwd=2 ,axes = TRUE,  xlab = "", ylab = "",cex.axis=0.7,las=2,xaxt="n", font=7)
par(new=TRUE) #opción para graficar sobre el gráfico anterior
#se grafica las muertes 
plot(t,x,type="l", col=colors()[554],axes = FALSE, lwd=2,xaxt="none",xlab="", ylab="", main="",font=7,cex.axis=0.7,ylim=range(c(50000,x)))
# Se imprime el eje X
axis.Date(1, at = seq(as.Date("2021-11-12"), as.Date("2022-01-13"),by = "3.5 days"),las = 1,font=7,cex.axis=0.7,mgp=c(3,0.5,0))
axis(4, at = pretty(range(x)), las=2,font=7,cex.axis=0.7)
mtext("EVOLUCIÓN DE NÚMERO DE CASOS COVID-19 EN ECUADOR",side=3, line=.8, cex=1, font=2)
mtext("Núm. Casos",side=2, line=3.2, cex=0.8, font=2)
mtext("Núm. Muertes", side=4, line=2.6, cex=0.8,font=2)
# líneas verticales
abline(v=seq(as.Date("2021-11-15"), as.Date("2022-01-15"),by = "weeks"), lty=3, col="gray")
# línea de división para identificar con facilidad los casos antes y después de identificación de variante omicron
abline(v=t[30], lty=5, col="black",lwd=2)
legend("topleft", legend=c("Casos Positivos","Muertes"),col=c(rgb(0.6,0.6,0.6),colors()[554]), lty=1:1, lwd=3:3,cex=0.8,bty="n")
legend("left",  legend=c("Pre Omicron"),col=c(rgb(0.6,0.6,0.6)), lty=0, lwd=5:5,cex=0.8,bty="n")
legend("right",  legend=c("Post Omicron   "),col=c(rgb(0.6,0.6,0.6)), lty=0, lwd=5:5,cex=0.8,bty="n")


#Reiniciar el área del gráfico
dev.off()

# GRAFICO DE CASOS Y MUERTES (OPCION 2, GRAFICAS SEPARADAS CASOS POSITIVOS Y MUERTES)
#Histograma
par(mfrow=c(2,1),mar = c(2, 4, 1.5, 1) + 0.25)   # Márgenes para ejes
# Se grafica numero de casos (positivas total) en función del tiempo con color rojo (554) ,ylim=range(c(y,x))
plot(t, y, type = "h", col=rgb(red = 0.0, green = 0.4, blue = 0.7, alpha = 0.5), lwd=2 ,axes = TRUE,  xlab = "", ylab = "",cex.axis=0.7,las=2,xaxt="n", font=7)
# línea de división para identificar con facilidad los casos antes y después de identificación de variante omicron
abline(v=t[30], lty=5, col="blue",lwd=2)
# Se imprime el eje X
axis.Date(1, at = seq(as.Date("2021-11-12"), as.Date("2022-01-13"),by = "3.5 days"),las = 1,font=7,cex.axis=0.7,mgp=c(3,0.5,0))
# líneas verticales
abline(v=seq(as.Date("2021-11-15"), as.Date("2022-01-15"),by = "weeks"), lty=3, col="gray")
# Se imprimen titulo y leyendas
mtext("Evolución del número de casos de COVID-19",side=3, line=.5, cex=0.8, font=2)
mtext("Núm. Casos",side=2, line=3.2, cex=0.8, font=2)
legend("topleft", legend=c("Casos Positivos"),col=c(rgb(0.0,0.7,0.8)), lty=1:1, lwd=3:3,cex=0.8,bty="n")
legend("left",  legend=c("Pre Omicron"),col=c(rgb(0.6,0.6,0.6)), lty=0, lwd=5:5,cex=0.8,bty="n")
legend("right", legend=c("Post Omicron   "),col=c(rgb(0.6,0.6,0.6)), lty=0, lwd=5:5,cex=0.8,bty="n")

#se grafica las muertes 
plot(t,x,type="l", col=colors()[554],axes = TRUE, lwd=2,xlab="", ylab="", main="",font=7,cex.axis=0.7,las=2,xaxt="n")
# líneas verticales
abline(v=seq(as.Date("2021-11-15"), as.Date("2022-01-15"),by = "weeks"), lty=3, col="gray")
# Se imprime el eje X
axis.Date(1, at = seq(as.Date("2021-11-12"), as.Date("2022-01-13"),by = "3.5 days"),las = 1,font=7,cex.axis=0.7,mgp=c(3,0.5,0))
# línea de división para identificar con facilidad los casos antes y después de identificación de variante omicron
abline(v=t[30], lty=5, col="blue",lwd=2)
# Se imprimen titulo y leyendas
mtext("Evolución del número de muertes por COVID-19",side=3, line=.5, cex=0.8, font=2)
mtext("Núm. Muertes",side=2, line=3.2, cex=0.8, font=2)
legend("topleft", legend=c("Muertes"),col=c(colors()[554]), lty=1:1, lwd=3:3,cex=0.8,bty="n")
legend("left",  legend=c("Pre Omicron"),col=c(rgb(0.6,0.6,0.6)), lty=0, lwd=5:5,cex=0.8,bty="n")
legend("bottomright",  legend=c("Post Omicron   "),col=c(rgb(0.6,0.6,0.6)), lty=0, lwd=5:5,cex=0.8,bty="n")

#Estadistica descriptiva Summary para determinar  valores maximos de las variables

#Primer periodo Pre Omicron
summary(ds_casos[ds_casos$fecha<'2021-12-14',"positivas_total"])
#Segundo periodo Post Omicron
summary(ds_casos[ds_casos$fecha>='2021-12-14',"positivas_total"])

#Primer periodo Pre Omicron
summary(ds_casos[ds_casos$fecha<'2021-12-14',"muertes"])
#Segundo periodo Post Omicron
summary(ds_casos[ds_casos$fecha>='2021-12-14',"muertes"])

#Primer periodo Pre Omicron
summary(ds_casos[ds_casos$fecha<'2021-12-14',"casos_nuevos"])
#Segundo periodo Post Omicron
summary(ds_casos[ds_casos$fecha>='2021-12-14',"casos_nuevos"])

#Primer periodo Pre Omicron
summary(ds_casos[ds_casos$fecha<'2021-12-14',"muertes_nuevas"])
#Segundo periodo Post Omicron
summary(ds_casos[ds_casos$fecha>='2021-12-14',"muertes_nuevas"])

# Estadística descriptiva de nuevos casos en dos etapas. Media determina los valores de la media por cada variable
#Primer periodo Pre Omicron
mean(ds_casos[ds_casos$fecha<'2021-12-14',"casos_nuevos"])
#Segundo periodo Post Omicron
mean(ds_casos[ds_casos$fecha>='2021-12-14',"casos_nuevos"])

# Estadística descriptiva de muertes nuevas en dos etapas
#Primer periodo Pre Omicron
mean(ds_casos[ds_casos$fecha<'2021-12-14',"muertes_nuevas"])
#Segundo periodo Post Omicron
mean(ds_casos[ds_casos$fecha>='2021-12-14',"muertes_nuevas"])

#Calcular el indice de variacion sobre las medias de Casos_nuevos Indice_variacion=valorfinal/valor_inicial

Variacion_casos_nuevos=((mean(ds_casos[ds_casos$fecha>='2021-12-14',"casos_nuevos"])-mean(ds_casos[ds_casos$fecha<'2021-12-14',"casos_nuevos"]))/mean(ds_casos[ds_casos$fecha<'2021-12-14',"casos_nuevos"]))*100
Variacion_casos_nuevos

Variacion_muertes=((mean(ds_casos[ds_casos$fecha>='2021-12-14',"muertes_nuevas"])-mean(ds_casos[ds_casos$fecha<'2021-12-14',"muertes_nuevas"]))/mean(ds_casos[ds_casos$fecha<'2021-12-14',"muertes_nuevas"]))*100
Variacion_muertes

