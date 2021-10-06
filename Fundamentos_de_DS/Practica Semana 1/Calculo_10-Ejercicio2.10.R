#-----------------------------------------------------
# MDS - Fundamentos de calculo
# Programacion lineal finanzas
# Practica 10
#-----------------------------------------------------

library(lpSolve)

# Parametros del problema
coef <- c(30, 20, 15, 12, 10, 7)/100    #rentabilidades de la funcion objetivo

# f= 0.30 * A + 0.20*A + 0.15 *A + 0.12* M+0.10*M + 0.07 *B

A <- matrix(c(1,1,1,1,1,1, 
              1,1,1,0,0,0,
              1,1,1,0,0,0,
              0,0,0,1,1,0,
              0,0,0,1,1,0,
              0,0,0,0,0,1,
              -2,1,0,0,0,0,
              -3,0,1,0,0,0,
              0,0,0,-2,1,0), nrow = 9, ncol=6, byrow = T) #byrow igual T significa que se ordena por filas
A
b <- c(100, 50,75,20,30,5,0,0,0)
dir <- c('=', '>=','<=','>=','<=','<','=','=','=') #las direcciones de las restricciones

#-----------------------------------------------------
# Solucion
#-----------------------------------------------------
solucion <- lp ('max', coef, A, dir, b) #coef se refiere a la funcion objetivo
solucion
solucion$objval
solucion$solution

