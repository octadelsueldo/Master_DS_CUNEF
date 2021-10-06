#Ejercicio 4.2 de Probabilidad

A <- 0.7
B <- 0.2
AB <- 0.1

# (i)comprobacion terminos independientes. #la interseccion entre A y B debe ser igual a la probabilidad A x la probabilidad B


interseccion <- A * B    #no son independientes xq no se cumple la condicion de probabilidades independientes ya que el producto A x B no es la interseccion

# (ii)  

AuB <- A + B - interseccion
Bc <- 1 - B
Ac <- 1 - A
AcondB <- interseccion/B
AcB<- B - interseccion
ABc <- A - interseccion
AcBc <- Ac*Bc
