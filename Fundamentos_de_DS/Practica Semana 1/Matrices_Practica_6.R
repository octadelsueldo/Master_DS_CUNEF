#------------------------------------------------------------
# Master DSF - Fundamentos Algebra
# An?lisis matricial de regresi?n
# Pr?ctica 6
#------------------------------------------------------------

rm(list=ls())

n <- 5
X <- matrix(c(10, 12, 12, 14, 10,
       4, 4, 5, 5, 3), nrow = 5, ncol = 2)
y <- c(122, 115, 128, 145, 108)
X
y

# (i) Estimador MCO de b   Metodo de MINIMOS CUADRADOS ORDINALES

b <- solve(t(X) %*% X) %*% t(X) %*% y       #(solve) es la inversa de la transpuesta de X por X... por la transpuesta de X por y
b

# Estimacion
yhat <- X %*% b
yhat

# Residuos o error. Es igual a y menos el y estimado
e <- (y - yhat)
e    #resultado del apartado (i)

# (ii) Equivalencia

P <- X %*% solve(t(X) %*% X) %*% t(X)    
P
yhat2 <- P %*% y
round(yhat-yhat2, 2) #redondea la diferencia entre los dos con dos decimales. Sirve para COMPARAR.

# Otra forma residuos

e2 <- (diag(n)-P) %*% y    #diag de n es la matriz diagonal de n.
round(e-e2,2)      #para comparar que da bien los errores o residuos por ambas formas.

# P e (I-P) sim?tricas e idempotentes

# Para P
round(P - t(P))   #Es simetrica porque la transpuesta de P es igual P
round(P %*% P-P,2)   #si la multiplicacion entre P por si misma es igual a la original P entonces es Idempotente. Al restar por si misma da 0, es decir, que esta bien.

# Para I-P
round((diag(n)-P) - t(diag(n)-P),2)  #es simetrica
round((diag(n)-P) %*% (diag(n)-P)-(diag(n)-P),2) #es idempotente


# (iii) Suma de cuadrados de los residuos
SCR1 <- t(e) %*% e
SCR1
SCR2 <- t(y) %*% (diag(n)-P) %*% y
SCR2

round(SCR1-SCR2,2)    #si se puede escribir con la misma funcion


# Anadir termino independiente

uno <- rep(1,n) #crear una matriz de 1 a n
uno
X1 <-cbind(uno,X) #union de matrices
X1
X <- X1

# (i) Estimador MCO de b   Metodo de MINIMOS CUADRADOS ORDINALES

b <- solve(t(X) %*% X) %*% t(X) %*% y       #(solve) es la inversa de la transpuesta de X por X... por la transpuesta de X por y
b

# Estimacion
yhat <- X %*% b
yhat

# Residuos o error. Es igual a y menos el y estimado
e <- (y - yhat)
e    #resultado del apartado (i)

# (ii) Equivalencia

P <- X %*% solve(t(X) %*% X) %*% t(X)    
P
yhat2 <- P %*% y
round(yhat-yhat2, 2) #redondea la diferencia entre los dos con dos decimales. Sirve para COMPARAR.

# Otra forma residuos

e2 <- (diag(n)-P) %*% y    #diag de n es la matriz diagonal de n.
round(e-e2,2)      #para comparar que da bien los errores o residuos por ambas formas.

# P e (I-P) sim?tricas e idempotentes

# Para P
round(P - t(P))   #Es simetrica porque la transpuesta de P es igual P
round(P %*% P-P,2)   #si la multiplicacion entre P por si misma es igual a la original P entonces es Idempotente. Al restar por si misma da 0, es decir, que esta bien.

# Para I-P
round((diag(n)-P) - t(diag(n)-P),2)  #es simetrica
round((diag(n)-P) %*% (diag(n)-P)-(diag(n)-P),2) #es idempotente


# (iii) Suma de cuadrados de los residuos
SCR1 <- t(e) %*% e
SCR1
SCR2 <- t(y) %*% (diag(n)-P) %*% y
SCR2

round(SCR1-SCR2,2)    #si se puede escribir con la misma funcion

