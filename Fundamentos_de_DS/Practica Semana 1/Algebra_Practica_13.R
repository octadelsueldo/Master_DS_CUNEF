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
