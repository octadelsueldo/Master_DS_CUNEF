#------------------------------------------------------------
# MDS - Fundamentos
# Paquete quantmod
#------------------------------------------------------------

rm(list=ls())
library(quantmod) #trae mucha informacion de series financieras y ademas calculo los modelos simples y comlejos de finanzas, indicadores. Tmb sirve para trading

#-------------------------------------------------------
# Serie y periodo
#-------------------------------------------------------
getSymbols("AMZN", from="2019-01-01", to="2020-09-16")
barChart(AMZN,theme = "white.mono")

#no es tan flexible. Por ejemplo para pintar por encima
AMZN #variables disponibles
#-------------------------------------------------------
# Rendimientos
#-------------------------------------------------------
AMZN$AMZN.Adjusted  #es un precio corregido
RendAMZN <- diff(log(AMZN$AMZN.Adjusted))[-1]  #rentabilidad en valores continuos
tail(RendAMZN)  #datos mas recientes
plot(RendAMZN, main="AMZN(Rend.Diarios)")   #plot de la serie de rendimientos

#-------------------------------------------------------
# Medidas Estad?sticas
#-------------------------------------------------------
mean(RendAMZN)
summary(RendAMZN)
library(moments)
c(skewness(RendAMZN),kurtosis(RendAMZN)) #la curtosis alta significa

#-------------------------------------------------------
# Histograma y kernel
#-------------------------------------------------------
hist(RendAMZN)
hist(RendAMZN,breaks = seq(-0.1,0.08,0.01), col = "lightblue", probability = T)
lines(density(RendAMZN))
box(lwd=1)  
