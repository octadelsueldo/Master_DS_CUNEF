

#Fundamentos de data science

#Ejercicio 13 de Algebra

#Descomposicion en valores singulares

A <- matrix(c(1,2,-1,3,0,0,0,0,2,3,-2,2,1,1,1,1,6,7,9,11,3,1,1,2,-7,-8,-7,-9,2,2,4,1),8,4)
A
#------------------------------------------------------------
# Descomposicion SDV
#------------------------------------------------------------
s <- svd(A)  #comando de la descom en valores singulares
s

#------------------------------------------------------------
# Elementos de la descomposicion
#-----------------------------------------------------------
D <- diag(s$d) #matriz diagonal
D
s$u
s$v

#------------------------------------------------------------
# Comprobacion de la Descomposicion
#------------------------------------------------------------
# Se verifica: X = U D V'
X1 <- s$u %*% D %*% t(s$v)   
round(X1,2)


#  D = U' A V
t(s$u) %*% A %*% s$v 

# Veamos que es OK
round(D-t(s$u) %*% A %*% s$v ,2)





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


#----------------------------------------------------------------------
# MDS - Fundamentos 
# Metodos de estimacion y contraste de hipotesis
# Practica 3
#----------------------------------------------------------------------

rm(list=ls())
datos<-read.table("Cotizaciones2020.txt",header=T)
names(datos)
attach(datos)

# Variable dummy covid

p_break <- 164 # a apartir de la linea 164 colocarle el numero 1 al vector 
covid <- c(rep(0,p_break),rep(1,nrow(datos)-p_break)) #vector covid con unos y ceros


#----------------------------------------------------------------------
# Base de datos con los Precios
#----------------------------------------------------------------------

Precios<-cbind(IBEX, SAN.MC, BBVA.MC, REP.MC, ITX.MC, TL5.MC, covid) #con cbind unimos todas las columnas mas el covid para poder comparar en base a el pbreak


#----------------------------------------------------------------------
# Elegir una muestra de n=30 Precios sin repeticion
#----------------------------------------------------------------------

set.seed(2020) 

nrow(Precios) #precios son los individuos
nmuestra <- 30 #tamanio de la muestra 

# Primero elegimos los numeros de la muestra y luego los seleccionamos
indices <- sample(1:nrow(Precios), nmuestra, replace = FALSE) #muestra aleatoria de 30 individuos sin reemplazamiento

indices

Precios.muestra <- Precios[indices,] #crea el dataframe precios.muestra con todas las columnas y con 30 filas

# Comprobacion
Precios.muestra
nrow(Precios.muestra) #vemos que el dataframe tiene 30 filas

# Comparacion de medias poblacion y muestra
mean(IBEX) #media de Ibex
mean(Precios.muestra[,1]) #media del Ibex en la muestra

summary(Precios.muestra) #vemos los principales estadisticos, media, mediana, cuartiles, max y min.

#----------------------------------------------------------------------
# Igualdas de Medias REP y ITX antes y despues COVID en la muestra y en la poblacion
#----------------------------------------------------------------------

# Precios REP antes y despues en la Poblacion y en la Muestra

#t.test es siempre contraste de medias. Se refiere a la estimacion por punto y por intervalos de medias

t.test(REP.MC ~  covid)  
#el p-valor del resultado contrasta la media de dos poblaciones. Contrasta la hipotesis nula (la media antes del covid) contra la alternativa de despues
# al ser p menor a 5 % se rechaza la hipotesis nula

t.test(Precios.muestra[,4] ~ Precios.muestra[,7]) #la variable 4 es REP y la variable 7 es el covid. Aqui vemos que los valores de la muestra no son muy diferentes
#al ser p valor < a 5% se rechaza la hipotesis nula

# Precios ITX antes y despues

t.test(ITX.MC ~  covid) #se rechaza la hipotesis nula porque el p valor < 5% 

t.test(Precios.muestra[,5] ~ Precios.muestra[,7]) #se rechaza la hipotesis nula porque el p valor < 5% 

#aqui vemos que la diferencia es mayor

#----------------------------------------------------------------------
# Igualdas de Medias REP y ITX en la muestra y en la poblacion
#----------------------------------------------------------------------

t.test(Precios[,4], Precios[,5])   #compara muestras no apareadas
#se rechaza la hipotesis nula de que las medias entre REP y ITX son iguales porque el p valor < 5% 

longitud<-(-16.55260)-(-15.59078)
longitud

t.test(Precios.muestra[,4], Precios.muestra[,5])
#En base a las muestras se rechaza la hipotesis nula de que las medias entre REP y ITX son iguales porque el p valor < 5% 

# Muestras apareadas (mas preciso)

t.test(Precios[,4], Precios[,5], paired = TRUE) #importante poner paired igual a true

longitud2<-(-16.29236)-(-15.85102)
longitud2

t.test(Precios.muestra[,4], Precios.muestra[,5], paired = TRUE)

#en un intervalo de confianza si uno suma hipotesis adicionales, gana en precision

#----------------------------------------------------------------------
# Test de Igualdad de rendimientos
#----------------------------------------------------------------------

n <- dim(datos)[1]
n
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1 #rentabilidad del IBEX

RREP  <- REP.MC[2:n] / REP.MC[1:n-1] - 1 #rentabilidad REPSOL
RITX  <- ITX.MC[2:n] / ITX.MC[1:n-1] - 1 #rentabilidad ITX

covidr <- covid[2:n] #agregamos un vector de rendimiento en base al covid
covidr <-as.factor(covidr)


# Contrastes entre REP e ITX

# Contrates en le MUESTRA

var.test(RREP[indices], RITX[indices])  #contrasta la hipotesis de cociente de varianzas.La hipotesis nula dice q Varianza del rep sobre varianza del itx es igual a 1, la alternativa que es distinto de uno.
#p valor no es menor a 5% entonces no se rechaza la hipotesis nula. Es decir, las volatilidades de las muestras van a ser las mismas.

t.test(RREP[indices], RITX[indices], paired= TRUE, var.equal = TRUE) #esta se usa para mejorar los valores ya que son apareados

t.test(RREP[indices], RITX[indices], var.equal = TRUE) 
#los rendimientos son igual xq el pvalor no es menor al 5% entonces no se rechaza la hipotesis nula

# Contrates en la POBLACION

var.test(RREP, RITX) #se rechaza la hipotesis nula de igualdad de varianzas al ser p valor<5% por lo tanto las varianzas no son iguales

t.test(RREP, RITX, var.equal = TRUE) #no se rechaza la hipotesis nula al ser p valor > 5%

t.test(RREP, RITX, paired= TRUE, var.equal = TRUE)#con datos apareados se confirma que no se rechaza la hipotesis nula al ser p valor > 5%


# Contrastes entre REP antes y despues del COVID

t.test(RREP ~ covidr) #no se rechaza la hipotesis nula



#Ejercicio 4 de Finanzas 


#Los rendimientos de dos activos con riesgo presentan volatilidades del 11 y del 15 por ciento,
#con una correlacion de 0.48. Si los rendimientos esperados son del 4 y del 11 por ciento,respectivamente,
#encontrar la cartera de varianza mınima que garantiza un rendimientodel 7 por ciento.

desv1<-0.11 #volatilidad activo 1
desv2<-0.15 #volatilidad activo 2
corr <-0.48 #correlacion entre activo 1 y 2
rend1<-0.04 #rend esperado activo 1
rend2<-0.11 #rendimiento esperado activo 2
rendcar <- 0.07 #rendimiento que debe tener la cartera

covar12 <- corr * (desv1 * desv1) #covarianza de la cartera
covar12

#En base al rendimiento que esperamos calculamos los pesos optimos por activo

#rendcartera <- w1r*rend1 + w2r*rend2 A partir de esa funcion despejamos los w1r y w2r

w1r <- (rendcar - rend2) / (rend1 - rend2) #w1r deberia ser 0.57

w2r <- 1 - w1r #w2r tendria que ser 0.43

rendcartera <- w1r*rend1 + w2r*rend2 #Combrobamos que el rendimiento nos da 7%


#Probamos que proporciones tendriamos con la minima varianza

w1 <- (desv2^2-covar12) / ((desv1^2 - covar12) + (desv2^2 - covar12)) #proporcion optima w1 teniendo en cuenta las varianzas y covarianzas
w2 <- 1 - w1 #proporcion w2 por diferencia
varcartera0 <- (w1^2 * desv1^2) + (1+w1)^2 * (desv2^2) + (2*w1)*(1-w1)*covar12 #varianza de la cartera con estas proporciones
#vemos que varcartera0 es igual a 0.0757



rendcar0 <- w1*rend1 + w2*rend2
varcartera <- (w1r^2 * desv1^2) + (1+w1r)^2 * (desv2^2) + (2*w1r)*(1-w1r)*covar12 
#la varianza de la cartera con estas proporciones es 0.0623






