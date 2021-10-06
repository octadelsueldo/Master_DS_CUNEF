#-----------------------------------------------------------------------
# MDS - Metodos de Estimacion y Contraste
# Practica 11
# Intervalos de confianza para un proporciones
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Intervalo de confianza para una proporcion
#-----------------------------------------------------------------------

prop.test(270,600)  
prop.test(270,600,conf.level=0.95)  #aqui prop.test contrasta que la hipotesis nula tiene un p igual a 0.5 y la hipotesis alternativa que es distinto de 0.5
#aqui ya vemos que hay que rechazar la hipotesis nula xq es distinto de 0.5
prop.test(270,600,conf.level=0.99)


#-----------------------------------------------------------------------
# Contraste de hipotesis H0: p=0.5 frente a distinto
#-----------------------------------------------------------------------
prop.test(270, 600, p = 0.5,
          alternative = c("two.sided"),
          conf.level = 0.95)

#-----------------------------------------------------------------------
# Intervalo de confianza para la proporcion duplicando el tamano muestral
#-----------------------------------------------------------------------
prop.test(2*270,2*600,conf.level=0.95)

#-----------------------------------------------------------------------
# Intervalo de confianza para la diferencia de proporciones
#-----------------------------------------------------------------------
x2<-c(270,94); n2<-c(600,220)  #aca debo agregar un vector de los que estan a favor y de los otros. Aqui se contrasta es la igualdad de proporciones
prop.test(x2,n2,conf.level=0.95) #contrasta que la proporcion de la primera muestra menos la segunda es igual a 0 y la hipotesis alternativa que la diferencia de proporciones es distinta de cero
#No se rechaza xq no es menor al 5%

#-----------------------------------------------------------------------
# Determinacion del tamano muestral
#-----------------------------------------------------------------------

nmuestra<-function(alfa,p0,Errormax){qnorm(1-alfa/2)^2*p0*(1-p0)/Errormax^2}  #alfa es el error de los intervalor, p0 es el error inicial (estimacion inicial de la proporcion) si no sabemos es 0.5, Errormax es el 6%.(maxima diferencia aceptada entre lo real y lo que vamos a obtener)
alfa<-0.05; p0<-1/2; Errormax<-0.06
nmuestra(alfa,p0,Errormax)
#habria qyue hacer 266 encuestras
#-----------------------------------------------------------------------
# Determinacion del tamano muestral para diferentes valores de alfa
#-----------------------------------------------------------------------
alfa<-1:5/100; p0<-1/2; Errormax<-0.06
nmuestra(alfa,p0,Errormax)
plot(alfa,nmuestra(alfa,p0,Errormax),type="b")


#-----------------------------------------------------------------------
# Intervalo para la diferencia
#-----------------------------------------------------------------------
prop.test(c(270,310),c(600,600),conf.level=0.95)
