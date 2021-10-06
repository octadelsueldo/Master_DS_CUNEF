#-------------------------------------------------------
# MDS - Fundamentos
# Distribuciones multivariantes
# Practica 3
#-------------------------------------------------------

#En una junta de accionistas, el 55 por ciento de los delegados llega en avion, el 25 porciento en tren
#y el resto en coche. Se selecciona una muestra de 8 delegados.

#(i)Probabilidad que 5 hayan llegado en avion, 2 en tren y 1 en coche
#(ii) Probabilidad de que todos hayan llegado en avion
#(iii)Probablidad que 4 lleguen en avion y 4 en tren
#(iv)  Simular lo que ocurre en 6 juntas de accionistas suponiendo que a cada junta acuden 25 delegados.


#(i) Probabilidades multinomiales

pa <- c(0.55, 0.25, 0.2)

# Prob(5,2,1)

dmultinom(x=c(5,2,1), prob =pa)         #son pequenas xq son probabilidades aisladas

#(ii) Prob(8,0,0)

dmultinom(x=c(8,0,0), prob = pa)

#(iii) Prob(4,4,0)

dmultinom(x=c(4,4,0), prob = pa)


# Simulacion de muestras multinomiales

#aqui hay dos restricciones

n_de_juntas <- 6 # numero de muestras multinomiales. Esto es el numero de vectores que se van a generar
tamano <- 25 # numero de delegados. Este es el total que se debe distribuir entre los 6

set.seed(2020) # para obtener el mismo resultado

junta <- rmultinom(n=n_de_juntas, size=tamano, prob=pa)
junta    #aqui aparecen las 6 muestras y la suma da 25

todas <- data.frame(junta)/tamano
todas   #aqui lo unimos para ver la representacion grafica

par(mfrow=c(2,3))  #dividimos la pantalla en un 2 x 3

for(i in 1:6) {
  barplot(todas[,i],ylim=c(0,1))   #hacemos graficos de barras
  box(lwd=1) 
}


#la principal aplicacion de la multinomial es cuando estamos trabajando con modelos y queremos revisar mas de dos posibilidades
