#-------------------------------------------------------
# MDS - Fundamantos
# Distribuciones multivariantes
# Practica 6
# Calculo de probabilidades en Normal Trivariada
#-------------------------------------------------------#-----------------------------------------------------------------

#-----------------------------------------------------------------
# Paquete mvtnorm
#-----------------------------------------------------------------

rm(list=ls())
install.packages("mvtnorm")
library(mvtnorm)

#-----------------------------------------------------------------
# Media y Covar
#-----------------------------------------------------------------

media <- c(0, 0, 0)
media
covar <- matrix(c(1, 3/5, 1/3, 3/5, 1, 11/15, 1/3, 11/15, 1),3,3)
covar

#-----------------------------------------------------------------
# Probabilidad exacta 
#-----------------------------------------------------------------

n<-3
pmvnorm(media, covar, lower=rep(-Inf, n), upper=c(1,4,2)) #lower y upper son los extremos superior e inferior que se quiere calcular

#en inferior si no hay se pone menos infinito y el upper se pone esos puntos xq es lo q manda el enunciado
#-----------------------------------------------------------------
# Simulacion de la muestra
#-----------------------------------------------------------------

nmuestrasim<-10000
set.seed(1111)

muestrasim <- rmvnorm(nmuestrasim, media, covar)
muestrasim
boxplot(muestrasim, col = "lightgray")

# Tener en cuenta que es un vector logico y la respuesta es con si

m<-sum(muestrasim[,1]<1 & muestrasim[,2]<4 & muestrasim[,3]<2) #la primer columna debe ser menor que 1 la siguiente menor q 4 y la ultima menor a 2
m
Prob <- m/nmuestrasim
Prob
