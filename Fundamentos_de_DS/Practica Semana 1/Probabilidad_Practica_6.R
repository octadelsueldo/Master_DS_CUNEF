#-----------------------------------------------------
# MDS - Fundamentos de probabilidad
# Distribucion binomial
# Practica 6
#----------------------------------------------------------------------
x<-0:25
nensayos<-25
probexito<-0.15
dbinom(x,nensayos,probexito)    #fundion de densidad de la dist binomial con parametros X, nensayos y probexito
tabla<-cbind(x,dbinom(x,nensayos,probexito))
tabla

#----------------------------------------------------------------------
# Media y desviacion tipica
#----------------------------------------------------------------------
mediab<-nensayos*probexito
mediab
dtbinomial<-(nensayos*probexito*(1-probexito))^0.5
dtbinomial

#----------------------------------------------------------------------
# Graficos funcion de densidad y de distribucion
#----------------------------------------------------------------------

par(mfrow=c(1, 2))
plot(x,dbinom(x,nensayos,probexito),type="h",xlab="Numero de ventas",  #type es importante para elegir el grafico correcto.
     ylab="Probabilidades",main="Prob. numero de ventas")
plot(x,pbinom(x,nensayos,probexito),type="s",xlab="Numero de ventas",ylab="Probabilidades",
     main="Numero de ventas, prob. acumuladas")
par(mfrow=c(1, 1))

#----------------------------------------------------------------------
# Tabla funcion de distribucion 
#----------------------------------------------------------------------
tabla<-cbind(x,pbinom(x,nensayos,probexito))
tabla    #tabla de probabilidades acumuladas

#----------------------------------------------------------------------
# Probabilidades
#----------------------------------------------------------------------
1-dbinom(0,5,probexito)  #probabilidad de que en 5 visita realice al menos una venta
1-pbinom(3,10,probexito) #probabilidad de que en 10 visita realice al menos cuatro ventas

