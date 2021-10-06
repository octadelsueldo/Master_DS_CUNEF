#-----------------------------------------------------
# MDS - Fundamentos Calculo
# Ejemplo programacion lineal
# Practica 8
#-----------------------------------------------------
#Hallar el minimo de la funcion 2000x + 3000y sujeta a 20x + 10y ≥ 200, 25x + 50y ≥ 500
#y 18x + 24y ≥ 300 con x, y ≥ 0. Plantear el problema dual y obtener su solucion.

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

