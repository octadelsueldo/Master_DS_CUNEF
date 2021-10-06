#----------------------------------------------------------------------
# MDS - Simulacion Monte Carlo
# Uso de sample
# Practica 1
#----------------------------------------------------------------------

rm(list=ls())
set.seed(2020)

x  <- c(-2,-1,0,1,2)
probabilidad <- c(0.15, 0.25, 0.3, 0.2, 0.1)
sum(probabilidad)

nmuestra <- 10000
muestrax <- sample(x, nmuestra, replace = T, prob = probabilidad) #el replace lo pongo True para elegir muestras distintas

# es importante poner prob proabilidad xq sino los elegira con la misma probabilidad
# Caracteristicas

table(muestrax)
sum(table(muestrax))
table(muestrax)/sum(table(muestrax))
barplot(table(muestrax))

mean(muestrax)
sd(muestrax)
