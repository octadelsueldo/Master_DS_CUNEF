#------------------------------------------------------------
# Master DSF - Fundamentos Algebra
# Simulaci?n de n rendimientos normales correlacionados
# Pr?ctica 11
#------------------------------------------------------------

rm(list=ls())

s1 <- 0.2
s2 <- 0.3
s3 <- 0.25
r12 <- 0.9
r13 <- 0.75
r23 <- 0.5
s12 <- r12*s1*s2
s13 <- r13*s1*s3
s23 <- r23*s2*s3

V<- matrix(c(s1^2, s12, s23, s12, s2^2, s23, s13, s23, s3^2),3,3) #matriz de covarianzas. Recorda que es simetrica y tiene autovalores reales
V
V.chol <- chol(V)  #formula para sacar la matriz de choleski. Factoralizacion de la matriz V. 
V.chol

# Comprobaci?n 
V2 <- t(V.chol) %*% V.chol  
V2

nsimul <- 30   #generar 30 datos 

set.seed(14)   #seed sirve para simular datos. Si no se hace esto cada uno obtendra muestras dierentes. Es un modo de tener todos la misma muestra o simulacion
Z <- matrix(rnorm(nsimul*3),nsimul,3) #matriz sirve para simular los activos correlacionados.
Z

Z[,1]
Z[,2]
Z[,3]

#------------------------------------------------------------
# Soluci?n: Matriz de serie temporal de rendimientos correlacionados
#------------------------------------------------------------
X <- Z %*% V.chol

t<-1:nsimul

# Gr?fico temporal de los 3 activos DEPENDIENTES
plot(t, X[,1], type = "l", col="red")
lines(t,X[,2],col="green")
lines(t,X[,3],col="blue")

cov(X)   
V # no coincide exactamente porque se obtiene por simulacion


# Grafico temporal de los 3 activos INDEPENDIENTES
plot(t, Z[,1], type = "l", col="red")
lines(t,Z[,2],col="green")
lines(t,Z[,3],col="blue")

cor(Z)  
