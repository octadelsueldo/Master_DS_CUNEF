#----------------------------------------------------------------------
# Master DSF - Fundamentos
# Distribucion Normal
# Practica 10
#----------------------------------------------------------------------
rm(list=ls())

#---------------#Prueba

#simulas valores N(0,1) media = 0 dt=1

#para simular se usa el comando rnorm

nsim <- 1000
set.seed(22)   #hacemos esto para que las muestras siempre sean iguales
y <- exp(x)
x <- rnorm(n = nsim, 0, 1)  #media 0 y dt=1  #dt es la desviacion tipica
x
y <- exp(x)
hist(x)
hist(y)
#----------------------------------------------------------------------
# Parametros distribucion normal 
#----------------------------------------------------------------------
media<-0.1
dt<-0.2

#----------------------------------------------------------------------
# Pr(R<0)    #siempre la p de probabilida y la normal es decir, pnorm(0, media, dt)
#----------------------------------------------------------------------
pnorm(0,media,dt)

#----------------------------------------------------------------------
# Pr(R>0.15) = 1-P(R<=0.15)
#----------------------------------------------------------------------
1-pnorm(0.15,media,dt)

#----------------------------------------------------------------------
# Pr(R>0.15) 0.4012937 (exacta) por Simulacion  
#----------------------------------------------------------------------
nmuestrasim<-1000000
set.seed(2018)   
muestrasim <- rnorm(nmuestrasim, media, dt)
hist(muestrasim)
Pr <- sum(muestrasim>0.15) / nmuestrasim
Pr    #FORMA OPTIMIZADA PARA HACERLO

#otra forma menos eficiente
Prob <- length(muestrasim[muestrasim>0.15])/nmuestrasim   
Prob

#Hallar P(0.14 <R<0.18)

pnorm(0.18, media, dt) - pnorm(0.14, media, dt)
#----------------------------------------------------------------------
# Graficos funcion de densidad y de distribucion 
#----------------------------------------------------------------------
par(mfrow=c(1,2))  #dividimos la pantalla
x<-seq(media-4*dt,media+4*dt,0.001)  #elegimos el +-4 para poder llegar con el tamano del grafico
plot(x,dnorm(x,media,dt),type="l",xlab="Rendimiento", ylab="densidad",
     main="Densidad del Rendimiento")
plot(x,pnorm(x,media,dt),type="l",xlab="Rendimiento", ylab="prob. acumulada",
     main="Distribucion del Rendimiento")
par(mfrow=c(1,1))


#----------------------------------------------------------------------
# Deciles del Rendimiento
#----------------------------------------------------------------------
d<-(1:9)/10 
qnorm(d,media,dt)
cbind(d,qnorm(d,media,dt))



