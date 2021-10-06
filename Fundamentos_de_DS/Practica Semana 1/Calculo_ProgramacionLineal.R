#-----------------------------------------------------
# MDS - Fundamentos Cálculo
# Ejemplo programacion lineal
# Práctica 8
#-----------------------------------------------------

library(lpSolve)

# Parametros del problema
coef <- c(2000, 3000)
A <- matrix(c(20, 25, 18, 10, 50, 24), ncol=2, nrow = 3)
A
b <- c(200, 500, 300)
dir <- rep('>=', 3)

# Solucion
solucion <- lp('min', coef, A, dir, b)

solucion$objval

solucion$solution

