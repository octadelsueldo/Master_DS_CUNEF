rm(list=ls())
library(e1071)
library(class)
# Selecci?n de variables
library(MASS)
data("Aids2")
str(Aids2)

library(class)
# Muestra de entrenamiento
set.seed(2021)
# Indices 280 train y 120 test
train<-sample(seq(length(Aids2$state)),length(Aids2$state)*0.70,replace=FALSE)

x<-Aids2[,c(3,7)] #estamos eligiendo el indice de las variables
y<-Aids2[,1] #variable dependiente o a predecir

# Ajuste con muestra de entrenamiento
knn1 <- knn(train=x[train,], test= x[-train,], cl=y[train], k=3)

t1<-table(knn1,y[-train]) #hacemos la tabla para ver como nos dan los resultados
t1
sum(t1)
sum(diag(t1))/sum(t1) # 0.5193 porcentaje de acierto
mean(knn1==y[-train]) 

# Con k=4
# Ajuste con muestra de entrenamiento
knn1 <- knn(train=x[train,], test= x[-train,], cl=y[train], k=4)

t1<-table(knn1,y[-train]) #hacemos la tabla para ver como nos dan los resultados
t1
sum(t1)
sum(diag(t1))/sum(t1) # 0.5545 porcentaje de acierto
mean(knn1==y[-train]) 

#----------------------------------------------------------------------
# Validacion cruzada con la muestra de entrenamiento 
#----------------------------------------------------------------------
knn.cross <- knn.cv(train=x[train,],y[train],k=4,prob=TRUE)
t2<-table(knn.cross,y[train])
t2
sum(diag(t2))/sum(t2) # 0.5412


#----------------------------------------------------------------------
# Con toda la muestra
#----------------------------------------------------------------------
knn.cross2 <- knn.cv(x,y,k=4,prob=TRUE)
t3<-table(knn.cross2,y)
t3
sum(diag(t3))/sum(t3) # 0.5318

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
which.max(resultado$Precision) #17 k es el optimo. Utilizar siempre el mas pequeno.
which.max(resultado[,2]) #otra forma.

