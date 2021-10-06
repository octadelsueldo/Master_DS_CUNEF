#------------------------------------------------------------
# MDS - Fundamentos de Calculo
# Representar funciones mediante plot lines y curve
# Practicas 1 y 2
#------------------------------------------------------------

#------------------------------------------------------------
# Practica 1 Fundamentos Calculo
#------------------------------------------------------------

#Representar las siguientes funciones mediante plot, lines y curve:
# (i) f (x) = e−(x−a) /   (1+e ^ −(x−a) )^2  para a = 0 y a continuacion a ∈ {−2,−1,0,1,2}

# (ii) f (x) = 1 / 1+(x−a) ^ 2

# (iii) f3(x) = sin(3cos(x)e^−x2/10), en el intervalo (−8,5) y a continuacion en el intervalo (−9, 9)

rm(list=ls())


# Funcion f1
f <- function(x,a) exp(-(x-a))/(1+exp(-(x-a)))^2   #funcion de densidad de la logistica. Funcion de distribucion. Se parece mucho a una normal. El parametro a va desplazando la funcion a la derecha o a ka izquierda
x<-seq(-10,10,0.01) #elegir una secuencia correcta es clave

a<-0 # Comenzamos con un grafico particular 
plot(x,f(x,a),type="l",ylab="Funcion de densidad logistica") #calcula la funcion para a = 0 
for (a in c(-2,-1,0,1,2)){ 
  y<-f(x,a)
  lines(x,y)   #esto sirve para hacer graficos encima
}

# Funcion f2
f2 <- function(x,a) 1/(1+(x-a)^2)
x<-seq(-10,10,0.01)

a<-0 # Comenzamos con un grafico particular 
plot(x,f2(x,a),type="l",ylab="Funcion de densidad logistica")
for (a in c(-2,-1,0,1,2)){ 
  y<-f2(x,a)
  lines(x,y)
}

# Funcion f3
f3 <- function(x) sin(3*cos(x)*exp(-x^2/10))
curve (f3, -8, -5, n=1000)   #desde -8 hasta -5 con 1000 puntos
curve(f3, -9, 9, n = 2000)
  
  
#------------------------------------------------------------
# Practica 2 Fundamentos Calculo
#------------------------------------------------------------
#Hallar el minimo de la funcion y = − exp(−(x − 5)^2) mediante optimize.

rm(list=ls())

f<- function(x) -exp(-(x-5)^2)

plot(f,-10,10)

df <- function(x)  2*(x-5)*f(x)


fmin<-optimize(f,c(-10,10),tol=0.001) #tol es la tolerancia
fmin #es el minimo

fmin2<-optim(1,f) #aca debemos darle un valor inicial
fmin2 #enviara un valor par que es el minimo. Lo hace en 18 passos en este caso. $convergence nos dice alcanzo uno en caso de que de 
#ese valor.
fmin3<-optim(1,f,df) #si uno utiliza la derivada tmb llega al resultado. 
fmin3


