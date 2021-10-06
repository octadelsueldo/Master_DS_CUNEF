#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# kNN
# Practica 3
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Paquetes y Datos
#----------------------------------------------------------------------
rm(list=ls())

# Paquete con los Datos
library(ISLR)
# Datos sillas infantiles
data("Carseats")
attach(Carseats)
names(Carseats)
datos <- Carseats
summary(datos)
str(datos)
nrow(datos)

#----------------------------------------------------------------------
# Variable dependiente e independientes
# Muestra 70-30
#----------------------------------------------------------------------

VentasA <- ifelse(Sales>8,1,0) #ventas grandes por arriba de 8 y las otras 
VentasA <- as.factor(VentasA)
datos <- cbind(datos,VentasA)
datos <- as.data.frame(datos)
datos<-datos[,-1]
head(datos)


library(class)
# Muestra de entrenamiento
set.seed(2021)
# Indices 280 train y 120 test
train<-sample(seq(length(datos$VentasA)),length(datos$VentasA)*0.70,replace=FALSE)

x<-datos[,c(1,2,3,4,5,7,8)] #estamos eligiendo el indice de las variables
y<-datos[,11] #variable dependiente o a predecir

# Ajuste con muestra de entrenamiento
knn1 <- knn(train=x[train,], test= x[-train,], cl=y[train], k=20)

t1<-table(knn1,y[-train]) #hacemos la tabla para ver como nos dan los resultados
t1
sum(t1)
sum(diag(t1))/sum(t1) # 0.6083 porcentaje de acierto
mean(knn1==y[-train]) 


#----------------------------------------------------------------------
# Validacion cruzada con la muestra de entrenamiento 
#----------------------------------------------------------------------
knn.cross <- knn.cv(train=x[train,],y[train],k=20,prob=TRUE)
t2<-table(knn.cross,y[train])
t2
sum(diag(t2))/sum(t2) # 0.639


#----------------------------------------------------------------------
# Con toda la muestra
#----------------------------------------------------------------------
knn.cross2 <- knn.cv(x,y,k=20,prob=TRUE)
t3<-table(knn.cross2,y)
t3
sum(diag(t3))/sum(t3) # 0.665

#----------------------------------------------------------------------
# Optimizacion de k
#----------------------------------------------------------------------
k <- 1:100
resultado <- data.frame(k, Precision = 0)
for(n in k){
  datos_output_kNN <- knn(train = x[train,], 
                          test = x[-train,],
                          cl = y[train], 
                          k = n)
  
  resultado$Precision[n] <- mean(datos_output_kNN == y[-train])
}

library(ggplot2)
library(dplyr)
resultado %>% 
  ggplot() +
  aes(k, Precision) +
  geom_line()

# Que k optimo utilizamos?
which.max(resultado$Precision) #42 k es el optimo. Utilizar siempre el mas pequeno.
which.max(resultado[,2]) #otra forma.

          