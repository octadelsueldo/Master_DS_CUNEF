#-----------------------------------------------------
# MDS - Fundamentos de probabilidad
# Distribucion exponencial
# Practica 11
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Distribucion exponencial
# La funcion de densidad es f(x)=(1/m)exp(-x/m)
# donde m es la media
# rate = 1/m
#----------------------------------------------------------------------


#----------------------------------------------------------------------
# Graficos de la funcion de densidad y de distrbucion
# Instruccion para 2X1 graficos conjuntos
#----------------------------------------------------------------------
par(mfrow=c(1,2))
x<-seq(0,10,0.01)
plot(x,dexp(x,rate=1/3),type="l",xlab="x", ylab="Funcion de densidad")   #lo definimos en terminos de X - la inversa de la media. La funcion es siempre decreciente
plot(x,pexp(x,rate=1/3),type="l",xlab="x", ylab="Funcion de distribucion")
par(mfrow=c(1,1))

#----------------------------------------------------------------------
# Probabilidad que el tiempo de servicio sea inferior
# a 2 minutos Pr(X<2)
# Aqui m=3
#----------------------------------------------------------------------
pexp(2, rate=1/3) #es una prob simulada entonces se usa la probabilidad p exp


#----------------------------------------------------------------------
# Muestra aleatoria de n=100
#----------------------------------------------------------------------
set.seed(2020)  #esto permite reproducir los resultados
nsample<-100
muestra<-rexp(100, rate=1/3) 
mean(muestra)   #siempre que aumentemeos el tamano muestra la media sera mas precisa
sd(muestra)   #aqui coinciden

var(muestra)
min(muestra)
max(muestra)
summary(muestra)

hist(muestra,xlab="tiempo",ylab="frecuencia relativa",main="Histograma y kernel",prob=T)   #ponemos prob igual a true xq despues quiero agregar el kernel
lines(density(muestra))   #vemos una estimacion de la densidad pero de manera continua (definida para cualquier valor) dependiente del ancho de banda

#----------------------------------------------------------------------
# Histograma de la muestra y funcion de densidad teorica
#----------------------------------------------------------------------
hist(muestra,xlab="tiempo",ylab="frecuencia relativa",main="Histograma y Densidad",prob=T)
x<-seq(0,20,0.1)
# Estimamos m con la media de la muestra, ya que E(X)=m
lines(x,dexp(x,1/mean(muestra)))
