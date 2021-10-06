#-----------------------------------------------------------------------
# MDS - Contrastes de bondad de sjuste
# Practica 2
# Test de la chi-cuadrado de Pearson
#-----------------------------------------------------------------------

# Introducirmos los datos (frecuencias)

rm(list=ls())
frecuencias<-c(32,37,20,7,4)
ndatos<-sum(frecuencias)

#-----------------------------------------------------------------------
# Barplot de los Datos
#-----------------------------------------------------------------------
x<-c(0:4)
barplot(frecuencias,names.arg=0:4,xlab="Número de cancelaciones")

#-----------------------------------------------------------------------
# Test de la chi-cuadrado
#-----------------------------------------------------------------------

# Calculo de las probabilidades teóricas
lambda <- 1.1
prob<-c(dpois(0:3,lambda),1-ppois(3,lambda))
prob
sum(prob)


# Calculo del contraste

chisq.test(frecuencias, p=prob)
chisq.test(frecuencias, p=prob)$expected
