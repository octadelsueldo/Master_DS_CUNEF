#------------------------------------------------------------
# Master DSF-Fundamentos
# Descomposicion SVD
# Practica 12
#------------------------------------------------------------

A <- matrix(c(2,3,0,-2,7,-1,5,3,6,4,1,-2),4,3)
A

#------------------------------------------------------------
# Descomposicion SDV
#------------------------------------------------------------
s <- svd(A)  #comando de la descom en valores singulares
s

#------------------------------------------------------------
# Elementos de la descomposicion
#-----------------------------------------------------------
D <- diag(s$d) 
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


# Valores Singulares
vsA <- eigen(t(A) %*% A)  #si multiplico la transpuesta de a por a 
valSing <- vsA$values^0.5    #y le calculo la raiz cuadrada obtenemos la descomposicion en valores singulares
valSing
 
