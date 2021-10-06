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

p_break <- 164
covid <- c(rep(0,p_break),rep(1,nrow(datos)-p_break))


#----------------------------------------------------------------------
# Base de datos con los Precios
#----------------------------------------------------------------------

Precios<-cbind(IBEX, SAN.MC, BBVA.MC, REP.MC, ITX.MC, TL5.MC, covid)


#----------------------------------------------------------------------
# Elegir una muestra de n=40 Precios sin repeticion
#----------------------------------------------------------------------

set.seed(2020)

nrow(Precios) #precios son los individuos
nmuestra <- 40

# Primero elegimos los numeros de la muestra y luego los seleccionamos
indices <- sample(1:nrow(Precios), nmuestra, replace = FALSE) #muestra de 40 individuos sin reemplazamiento

indices

Precios.muestra <- Precios[indices,]

# Comprobacion
Precios.muestra
nrow(Precios.muestra)

# Comparacion de medias poblacion y muestra
mean(IBEX)
mean(Precios.muestra[,1])

summary(Precios.muestra)

#----------------------------------------------------------------------
# Igualdas de Medias SANT y BBVA antes y despues COVID en la muestra y en la poblacion
#----------------------------------------------------------------------

# Precios SANT antes y despues en la Poblacion y en la Muestra

#t.test es siempre contraste de medias. Se refiere a la estimacion por punto y por intervalos de medias

t.test(SAN.MC ~  covid)  #para calcular intervalos de confianza se utiliza t.test Aca es el interv de confianza de san.mc para los precios de covid. Permite calcular el contraste de medias para eso niveles de covid
#el p-valor del resultado contrasta la media de dos poblaciones. Contrasta la hipotesis nula (la media antes del covid) contra la alternativa de despues
# al ser p menor a 5 % se rechaza la hipotesis nula

t.test(Precios.muestra[,2] ~ Precios.muestra[,7]) #la variable 2 es santander y la variable 7 es el covid. Aqui vemos que los valores de la muestra no son muy diferentes

# Precios BBVA antes y despues

t.test(BBVA.MC ~  covid)

t.test(Precios.muestra[,3] ~ Precios.muestra[,7])

#aqui vemos que la diferencia es mayor

#----------------------------------------------------------------------
# Igualdas de Medias SANT y BBVA en la muestra y en la poblacion
#----------------------------------------------------------------------

t.test(Precios[,2], Precios[,3])   #compara muestras no apareadas

longitud<-(-0.7935942)-(-1.0484395)
longitud

t.test(Precios.muestra[,2], Precios.muestra[,3])

# Muestras apareadas (mas preciso)

t.test(Precios[,2], Precios[,3], paired = TRUE) #importante poner paired igual a true

longitud2<-(-0.8992447)-(-0.9427890)
longitud2

t.test(Precios.muestra[,2], Precios.muestra[,3], paired = TRUE)

#en un intervalo de confianza si uno suma hipotesis adicionales, gana en precision

#----------------------------------------------------------------------
# Test de Igualdad de rendimientos
#----------------------------------------------------------------------

n <- dim(datos)[1]
n
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1

RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RBBVA  <- BBVA.MC[2:n] / BBVA.MC[1:n-1] - 1

covidr <- covid[2:n]
covidr <-as.factor(covidr)


# Contrastes entre SANTANDER y BBVA

# Contrates en le MUESTRA

var.test(RSAN[indices], RBBVA[indices])  #contrasta la hipotesis de cociente de varianzas.La hipotesis nula dice q Varianza del santander sobre varianza del bbva es igual a 1, la alternativa que es distinto de uno.
#p valor no es menor a 5% entonces no se rechaza la hipotesis nula. Es decir, las volatilidades de las muestras van a ser las mismas.

t.test(RSAN[indices], RBBVA[indices], paired= TRUE, var.equal = TRUE) #esta se usa para mejorar los valores ya que son apareados

t.test(RSAN[indices], RBBVA[indices], var.equal = TRUE) 
#los rendimientos son igual xq el pvalor no es menor al 5% entonces no se rechaza la hipotesis nula

# Contrates en la POBLACION

var.test(RSAN, RBBVA)

t.test(RSAN, RBBVA, var.equal = TRUE)

t.test(RSAN, RBBVA, paired= TRUE, var.equal = TRUE)


# Contrastes entre SANTANDER antes y despues del COVID

t.test(RSAN ~ covidr)
 

