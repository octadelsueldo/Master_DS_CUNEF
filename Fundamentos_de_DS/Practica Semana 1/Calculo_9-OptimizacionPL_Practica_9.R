#-----------------------------------------------------
# MDS - Fundamentos de calculo
# Programaci?n lineal finanzas
# Practica 9
#-----------------------------------------------------

library(lpSolve)

# Parametros del problema
coef <- c(12, 6, 2)/100    #rentabilidades de la funcion objetivo

# f= 0.12 * A + 0.06* M+0.02*B

A <- matrix(c(1,1,1, 
              0,1,0,
              0,0,1,
              0,0,1,
              5,-4,0), nrow = 5, ncol=3, byrow = T) #byrow igual T significa que se ordena por filas
A
b <- c(50,5,9,11,0)
dir <- c('=', '>=','>=','<=','<=') #las direcciones de las restricciones

#-----------------------------------------------------
# Solucion
#-----------------------------------------------------
solucion <- lp('max', coef, A, dir, b) #coef se refiere a la funcion objetivo
solucion
solucion$objval
solucion$solution

