#-----------------------------------------------------------------------
# MDS - Contrastes de bondad de ajuste
# Practica 6
# Kolmogorov-Smirnov distribucion exponencial
#-----------------------------------------------------------------------

datos <- c(0.4,  3.8,  3.9,  2.2,  4.9, 12.6,  7.9, 15.3, 11.5,  6.3)
mean(datos)

# Es necesario especificar la distribucion con el valor de sus parametros

ks.test(datos,"pexp", rate=1/5) #distribucion como p exp que es funcion exponencial y parametro rate (la inversda de la media)

#la hipotesis nula dice que los datos siguen una distribucion exponencial y la alternativa de que no siguen la dist exponencial
#no rechazamos la hipotesis de que los datos siguen una exponencial

# Si hacemos

ks.test(datos,"pexp") # Rechaza H0! Xq el r entiende que el pexp es lo mas basico y cree que la media es 1 entonces sale rechazado
#se deben poner los parametros

# Otro ejemplo simulado

x<-rexp(100,1/5)  #genera 100 muestras de la distrib exponencial con media 5
ks.test(x,"pexp", rate=1/5)
hist(x)

