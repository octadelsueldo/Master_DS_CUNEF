#-------------------------------------------------------
# MDS - Fundamentos
# Distribuciones multivariantes
# Practica 4  Distribucion normal bidimensional
#-------------------------------------------------------

rm(list=ls())
library(plot3D)
library(scatterplot3d)

#----------------------------------------------
# Normal Bidimensional
# Definicion de la funcion de densidad conjunta
# Especificar: medias, varianzas y corr o cov
#----------------------------------------------

m1<-2  #media1
m2<-2.5  #media2
s1<-0.55 #desv1
s2<-0.45  #desv 2
r<-0.71  #correlacion
x<-seq(m1-3*s1,m1+3*s1,0.1)   
y<-seq(m2-3*s2,m2+3*s2,0.1)
Q<-function(x,y){((x-m1)/s1)^2+((y-m2)/s2)^2-2*r*(x-m1)*(y-m2)/(s1*s2)}  #forma cuadratica de la funcion. Esto son las elipses
f<-function(x,y){(1/(2*pi*s1*s2*sqrt(1-r^2)))*exp(-Q(x,y)/(2*(1-r^2)))}  #funcion de densidad


#----------------------------------------------
# Representacion grafica
# Con Outer Perpective y con ContourPlot
#----------------------------------------------
z<-outer(x,y,f)    #el outer es como una hoja de calculo. Es muy bueno para evaluar una funsion.
z
persp(x,y,z,col="lightgreen")   #toma la media y la varianza de cada una de las variables a evaluar y su correlacion y con eso se crea el grafico 
contour(x,y,z)    #contornos. Son las curvas de nivel. Hacer la funcion de densidad constante y 

# Contornos colores
filled.contour(x,y,z,col=rainbow(256),nlevels=(256))

#----------------------------------------------
# Con Scatter Plot 

#Es otra forma de hacer el grafico pero con puntos (si unirles)
#----------------------------------------------

Datosx<-seq(m1-3*s1,m1+3*s1,0.1)
Datosy<-seq(m2-3*s2,m2+3*s2,0.1)
Datos<-expand.grid(Datosx,Datosy)
x<-Datos[,1]
y<-Datos[,2]
z<-f(x,y)
scatterplot3d(x, y, z, highlight.3d=TRUE,col.axis="blue",col.grid="lightblue")


              