#----------------------------------------------------------------------
# MDS - Simulacion
# Poisson
# Practica 4
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Muestra aleatoria de n=500 de Poisson con lambda=75
#----------------------------------------------------------------------
rm(list=ls())
set.seed(11)
muestra<-rpois(500,75)
muestra

#----------------------------------------------------------------------
# Características de la muestra
#----------------------------------------------------------------------
mean(muestra)
sd(muestra)
summary(muestra)

#----------------------------------------------------------------------
# Histograma de la muestra
#----------------------------------------------------------------------
hist(muestra)
hist(muestra,breaks=50:110,xlab="x",ylab="frecuencia",main="Histograma")

#----------------------------------------------------------------------
# Histograma de la muestra con estimación de la densidad
#----------------------------------------------------------------------
hist(muestra,breaks=50:110,xlab="número de contactos",ylab="frecuencia relativa",
     main="Histograma y kernel",prob=T)
lines(density(muestra))

# Incluyendo menos intervalos
hist(muestra,xlab="número de contactos",ylab="frecuencia relativa",
     main="Histograma y kernel",prob=T)
lines(density(muestra))

#----------------------------------------------------------------------
# Número esperado con más de 90 contactos
#----------------------------------------------------------------------
# Usando la muestra

muestra90<-sum(muestra>90)
muestra90


# Usando la probabilidad
round(500*(1-ppois(90,75)),1)

#----------------------------------------------------------------------
# Gráfico conjunto de la  muestra de contactos y la teórica de Poisson. 
#----------------------------------------------------------------------
hist(muestra,breaks=50:110,xlab="x",ylab="frecuencia",main="Histograma",
     probability = "T")
lines(0:110,dpois(0:110,75))


