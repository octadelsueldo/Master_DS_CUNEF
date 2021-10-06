#-----------------------------------------------------
# MDS - Fundamentos de Cálculo
# Optimizar una función de verosimilitud exponencial 
# mediante optimize
#-----------------------------------------------------

rm(list=ls())

# f(x,theta)=theta*exp(-theta*x) es la función de densidad
# Función de verosimilitud cambiada de signo

LL <- function(theta) {
  f = theta*exp(-theta*x)
  LL = -sum(log(f))
}

# Datos: Los obtenemos mediante simulación. Se puede cambiar x por cualquier vector
set.seed(2020)
x <- rexp(10000, rate=5) # es un vector de datos simulados 
mean(x); sd(x)
hist(x)

# Maximizamos el log de la verosimilitud LL, o lo que es lo mismo minimizamos -LL

opf <- optimize(f=LL, interval = c(0,100), tol=0.001)
opf
opf$minimum

# Representación histograma y función ajustada
f <- function(x,theta) { theta*exp(-theta*x) }
hist(x,main="Histograma y Densidad exponencial",prob=T)
x1<-seq(0,1,0.1)
lines(x1,f(x1,opf$minimum), col="blue")
box(lwd=1) 


# Comprobamos el resultado mediante MASS, paquete que permite estimar algunas 
# distribuciones importantes

library("MASS")
fitdistr(x, "exponential")

