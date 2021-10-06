#Fundamentos de data science

#Ejercicio numero 7 de Probabilidad

n <- 0:10
npreguntas <- 10
Probabilidad <- 1/4
dbinom(n, npreguntas, Probabilidad)
tabla <- cbind(n, dbinom(n, npreguntas, Probabilidad))
tabla

#----------------------------------------------------------------------
# Graficos funcion de densidad y de distribucion
#----------------------------------------------------------------------

par(mfrow=c(1, 2))
plot(n,dbinom(n, npreguntas,Probabilidad),type="h",xlab="Numero de preguntas",  #type es importante para elegir el grafico correcto.
     ylab="Probabilidades",main="Prob. num de resp correct")
plot(n,pbinom(n, npreguntas,Probabilidad),type="s",xlab="Numero de preguntas",ylab="Probabilidades",
     main="Num de resp correct, prob. acumulada") #probabilidad acumulada
par(mfrow=c(1, 1))

#----------------------------------------------------------------------
# Tabla funcion de distribucion 
#----------------------------------------------------------------------
tabla<-cbind(n,pbinom(n, npreguntas, Probabilidad))
tabla #tabla de probabilidad acumulada

#----------------------------------------------------------------------
# Probabilidades
#----------------------------------------------------------------------
1-pbinom(5,npreguntas,Probabilidad)  #probabilidad que acierte 5 o mas preguntas
1-dbinom(6,npreguntas,Probabilidad)  #probabilidad que acierte 5 preguntas o menos.




