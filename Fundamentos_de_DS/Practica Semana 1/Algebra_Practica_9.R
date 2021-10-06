#--------------------------------------------------
# Master DSF - Fundamentos Algebra
# Autovalores y autovectores
# Descomposici?n Espectral
#--------------------------------------------------

A <- matrix(c(8, -2, -3, -2, 11, 4, -3, 4, 5),3,3)
A

#--------------------------------------------------
# Descomposici?n Espectral
#--------------------------------------------------

evA<-eigen(A)   #descomposicion
evA
avalores<-evA$values  #values son los autovalores. El autovalor mayor es el autovalor que corresponde al primer componente principal. 
avalores
avectores<-evA$vectors  #autovectores
avectores

#--------------------------------------------------
# Comprobaci?n Descomposicion espectral
# Comprobaci?n Ortogonalidad
#--------------------------------------------------

D <- diag(avalores)   #primer se genera la matriz diagonal para comprobar
avectores %*% D %*% t(avectores) #con esto deberiamos obtener la matriz original para comprobar

evA$vectors %*% t(evA$vectors)
round(evA$vectors %*% t(evA$vectors),9) #aqui comprobamos que nos da la matriz identidad

#--------------------------------------------------
# Determinante de la Matriz
#--------------------------------------------------

det(A)  #determinante de la matriz A
prod(avalores)  #el producto de los autovalores coincide con el determinante

#--------------------------------------------------
# Traza de A
#--------------------------------------------------

sum(avalores)

#--------------------------------------------------
# Rango de la matriz
#--------------------------------------------------

sum(avalores != 0)   #sumamos los valores distintos de 0 y eso es el rango

#--------------------------------------------------
# Autovalores de A^(-1) =1/autovalores(A)
#--------------------------------------------------

AInv <- solve(A)
AInv
round(A%*%AInv,9)
eigen(AInv)
eigen(AInv)$values
1/avalores

#--------------------------------------------------
# Autovalores de A^(2) =autovalores(A^2)
#--------------------------------------------------

A2 <- A%*%A
eigen(A2)$values
eigen(A)$values^2





#-------------
# Ejercicio 3.8 de Calculo sobre matrices.
#descomposicion

#--------------------

#aca debemos definir r y le damos un valor
r <- 0.8
R <- matrix(c(1, r, r, 1), 2, 2)
R

#Descomposicion espectral

ER <- eigen(R)
ER
Autovalores <- ER$values
Autovalores
Autovectores <- ER$vectors   #ocurre el cambio de signo xq si multiplico por una constante lo autovalores tmb se mantienen

#primer autovalor y autovector

Autovalores[1]
Autovectores[,1]

# segundo autovalor y autovector

Autovalores[2]
Autovectores[,2]


