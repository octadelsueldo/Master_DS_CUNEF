#----------------------------------------------------------------------
# Probabilidades
#----------------------------------------------------------------------
1-ppois(4,4)  # Probabilidad  de  que  en  una  jornada  cometa  al  menos  un  fallo.
dpois(4,4)  # Obtener  las  probabilidades  de  que  en  una  jornada  cometa  x  =  0, 1, 2, 3, 4  y  fallos.
1-dpois(6,4) # Probabilidad  de  que  en  una  jornada  cometa  x  fallos  o  menos,  para   x  =  0, 1, 2, 3, 4  y 5.


# Tabla funcion de distribucion 
#----------------------------------------------------------------------
n <- 0:12
tabla<-cbind(n,ppois(n,4))
tabla #tabla de probabilidad acumulada
