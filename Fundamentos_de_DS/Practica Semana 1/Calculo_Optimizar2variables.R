#-----------------------------------------------------
# MDS - Fundamentos de c?lculo
# Optimizar funci?n de dos variables
#-----------------------------------------------------

rm(list=ls())

# Representaci?n de la funci?n en tres dimensiones 

f <- function(x1,y1) (x1^2+y1-11)^2+(x1+y1^2-7)^2
x<- seq(-5,5,0.2)
y<- seq(-5,5,0.2)
z<- outer(x,y,f) #la funcion outer es como una hoja de calculo. Nos define para un valor de una fila y una columna los datos. Se pueden poner otras operaciones dentro de outer.
z
persp(x,y,z, phi = -20, col="yellow", shade=0.60, ticktype = "detailed") # ok dibuja una funcion de tres variables

# Optimizamos la funci?n: los argumentos se introducen como un vector

f1 <- function(x) (x[1]^2+x[2]-11)^2+(x[1]+x[2]^2-7)^2 #definimos los argumentos de la funcion como vectores

optim(c(-3,3),f1)   #value indica el valor de la funcion. 
#para optimizar una funcion y no se puede resumir a un problema de programacion lineal. Por ejemplo para maxima verosimilitud es bueno el comando optim
optim(c(-3,3),f1)$par

optim(c(-3,-3),f1)

optim(c(3,3),f1)

optim(c(3,-3),f1) # min

