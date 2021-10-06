#-------------------------------------------------------
# MDS - Fundamantos
# Distribuciones multivariantes
# Práctica 1
#-------------------------------------------------------

rm(list=ls())

# Vamos a obtener el valor de k

no.densidad <- function (x,y){
  (1/(x+y+1))
}
x <- 0:4
y <- 0:4 

outer(x,y,FUN=no.densidad)
k1<-sum(outer(x,y,FUN=no.densidad))
k1

# Tabular las probabilidades y representarlas en forma de tabla de doble entrada
# Para que sumen 1 las dividimos por el total k1

densidad <- function (x,y){
  (1/(x+y+1))/k1
}
sum(outer(x,y,FUN=densidad))

# Definimos la función de densidad conjunta y obtenemos las leyes marginales

dens.f <- outer(x,y,FUN=densidad)
dens.f
sum(dens.f)
colnames(dens.f) <- y
row.names(dens.f) <- x

# Sumamos por filas y por columnas para obtener las marginales

mg.x <- apply(dens.f, 1, sum) # Suma dens.f por filas al escribir "1"
mg.x
sum(mg.x)
mg.y <- apply(dens.f, 2, sum) # Suma dens.f por columnas al escribir "2"
mg.y
sum(mg.y)


# Esperanzas y varianzas de X e Y:

media.x <- sum(mg.x*x)
media.x
media.y <- sum(mg.y*y)
media.y

var.x <- sum(mg.x*x^2)-media.x^2
var.y <- sum(mg.y*y^2)-media.y^2

# Covarianza y correlacion
# Covarianza: cov(X,Y)=E(XY) - E(X)*E(Y)
# Calcula primero E(XY)=suma(xi*yi*P(X=xi,y=yi)

mat.cov <- outer(x, y, FUN=function(x,y) x*y*densidad(x,y))  
covar <- sum(mat.cov) - media.x * media.y
covar
correl <- covar/(var.x*var.y)^0.5
correl

