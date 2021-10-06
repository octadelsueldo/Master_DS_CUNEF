#-----------------------------------------------------
# MDS - Fundamentos 
# Distribuciones Multivariantes
# Practica 5
#-----------------------------------------------------

rm(list=ls())

#-----------------------------------------------------------------
# Vector de medias y Matriz de Covarianzas
#-----------------------------------------------------------------
# Vector de medias; Un vector es siempre un vector columna (aqui 2x1)

media <- c(1, 0.5)   
media

# Matriz de Covarianzas

S <- matrix(c(0.55^2, 0.3, 0.3, 0.7^2),2,2)   #debe ser simetrica
S

#-----------------------------------------------------------------
# Calculo de la Media y varianza de Y
#-----------------------------------------------------------------
# Vector de pesos
a <- c(0.7, 0.3)

# Media y Varianza
media.Y <- t(a) %*% media    
media.Y
varianza.Y <- t(a) %*% S %*% a
varianza.Y

#-----------------------------------------------------------------
# Calculo de la Probabilidad Pr(Y<1.8) de forma exacta
#-----------------------------------------------------------------

pnorm(1.8, media.Y, sqrt(varianza.Y))  


#-----------------------------------------------------------------
# Simulacion de una normal bivariada
#-----------------------------------------------------------------
library(MASS)  #paquete MASS trabaja con la dist normal en dos dimensiones
set.seed(2121)
bivnormal <- mvrnorm(10000, media, S)  #10000 muestras con ese vector de medias y esa matriz de covarianza
bivnormal
plot(bivnormal)

#-----------------------------------------------------------------
# Calcular Pr(0.7X1+0.3X2<1.8) por Simulacon
#-----------------------------------------------------------------


m<-sum(0.7*bivnormal[,1]+0.3*bivnormal[,2]<1.8)   
m
m/10000


#Prob (x1 <1.2, x2<0.7)

m2 <- sum(bivnormal[,1]<1.2&bivnormal[,2]<0.7)
m2
m2/10000

