# -------------------------------------------------------------------------------
# ANALISIS ESTADISTICO VARIABLE OMICRON EN ECUADOR
#
# Autores
# 
# 20-Ene-2022
# -------------------------------------------------------------------------------

# Se cargan los datasets limpiados, preparados y publicados en GitHub
path_vacunas<-"https://raw.githubusercontent.com/mabebt2b/COVID_ECU/main/vacunas_ecu.csv"
path_ds<-"https://raw.githubusercontent.com/mabebt2b/OMICRON/main/covidecu.csv"
ds_casos<-read.csv(path_ds)
ds$fecha <- as.Date(ds$fecha)

t<-as.Date(ds_casos[,"fecha"])
x<-ds_casos[,"muertes"]
y<-ds_casos[,"positivas_total"]

# Regresion lineal para los dos grupos
# modelo_g1<-lm(y[0:30]~t[0:30])
# modelo_g2<-lm(y[30:61]~t[30:61])

# GRAFICO DE CASOS Y MUERTES
par(mar = c(3, 4, 2.5, 4) + 0.5)  # Márgenes para ejes
# Se grafican todos los casos en función del tiempo con color rojo (554)
plot(t,y,type="l", col=colors()[554], lwd=2,xaxt="none",xlab="", ylab="", main="",las = 1,font=7,cex.axis=0.7)
# Se añade una línea para el primer perído antes de la detección de Omicron en anaranjado
lines(t[0:30],y[0:30],type="l", col=colors()[149], lwd=2,xaxt="none", xlab="", ylab="", main="",las = 1,font=7)
par(new=TRUE) # para graficara sobre el mismo gráfico
# Se gráfica el número de muertes
plot(t, x, type = "h", col=rgb(red = 0.4, green = 0.4, blue = 0.4, alpha = 0.5), lwd=2 ,axes = FALSE, bty = "n", xlab = "", ylab = "")
# Se imprimen los ejes
axis.Date(1, at = seq(as.Date("2021-11-15"), as.Date("2022-01-13"),by = "3.5 days"),las = 1,font=7,cex.axis=0.7,mgp=c(3,0.5,0))
axis(4, at = pretty(range(x)), las=2,font=7,cex.axis=0.7)
mtext("EVOLUCIÓN DE NÚMERO DE CASOS COVID-19 EN ECUADOR",side=3, line=.8, cex=1, font=2)
mtext("Núm. Casos",side=2, line=3.2, cex=0.8, font=2)
mtext("Núm. Muertes", side=4, line=2.6, cex=0.8,font=2)
# líneas verticales
abline(v=seq(as.Date("2021-11-15"), as.Date("2022-01-15"),by = "weeks"), lty=3, col="gray")
# línes de división de los dos grupos poblacionales
abline(v=t[30], lty=5, col="black",lwd=2)
legend("topleft", legend=c("Casos Pre Omicron", "Casos Post Omicron", "Muertes"),col=c(colors()[149], colors()[554],rgb(0.4,0.4,0.4)), lty=1:1, lwd=3:3,cex=0.8,bty="n")

# EstadÍstica descriptiva de nuevos casos en dos etapas
summary(ds_casos[ds_casos$fecha<'2021-12-14',"casos_nuevos"])
summary(ds_casos[ds_casos$fecha>='2021-12-14',"casos_nuevos"])
mean(ds_casos[ds_casos$fecha<'2021-12-14',"casos_nuevos"])
mean(ds_casos[ds_casos$fecha>='2021-12-14',"casos_nuevos"])

# EstadÍstica descriptiva de muertes nuevas en dos etapas
summary(ds_casos[ds_casos$fecha<'2021-12-14',"muertes_nuevas"])
summary(ds_casos[ds_casos$fecha>='2021-12-14',"muertes_nuevas"])
mean(ds_casos[ds_casos$fecha<'2021-12-14',"muertes_nuevas"])
mean(ds_casos[ds_casos$fecha>='2021-12-14',"muertes_nuevas"])

# gráfica de dispersión casos vs. muertes
par(mar = c(5, 5, 4, 2))
plot(y,x, col=colors()[584], lwd=3,xaxt="none", xlab="", ylab="", main="",las = 1,font=7,cex=0.3,cex.axis=0.8)
axis(1, at = pretty(range(y)), las=0,font=7,cex.axis=0.8,mgp=c(3,0.3,0))
mtext("CASOS VS. MUERTES",side=3, line=.8, cex=1.1, font=2)
mtext("Núm. Muertes",side=2, line=3, cex=0.8, font=2)
mtext("Núm. Casos", side=1, line=1.1, cex=0.8,font=2)
abline(h=seq(from=300,to=550,by=50),v=seq(from=0,to=12), lty=3, col="gray")

# RegresiÃ³n LogarÃ­tmica
modelo<-lm(y~log(x))
summary(modelo)
