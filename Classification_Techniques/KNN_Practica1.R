#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Metodo kNN
# Practica 1

#----------------------------------------------------------------------
rm(list=ls())
datos <- read.csv("weight-height.csv")
names(datos)
head(datos)
str(datos)
attach(datos)

datos %>% distinct(datos$Gender)
#----------------------------------------------------------------------
# Analisis exploratorio datos
#----------------------------------------------------------------------
plot(Height,Weight, col=datos$Gender, pch=6)

par(mfrow=c(1,2))
boxplot(Height~Gender, col="orange") #diagramas de caja de altura y peso segun genero
boxplot(Weight~Gender, col="orange")
par(mfrow=c(1,1))

# Normalidad de la altura y el peso ??
qqnorm(Height) #calculamos el qq plot para ver si la variable se comporta como normal
#vaemos que tenemos el problema de los outliers que no estan como el patron pero realmente lo son.
#en las colas se ve clarito los outliers.
qqline(Height)

# Normalidad de la altura y el peso ??
set.seed(2020) #debemos elegir una muestra de 5000 datos para poder realizar el test. Es el limite del test
Height2 <- sample(5000, Height, replace = FALSE) 
qqnorm(Height2) #vemos el qq plot de la muestra
qqline(Height2)
shapiro.test(Height2) 
#al ser el pvalue menor a 5% rechazamos la hipotesis nula. Es decir se rechaza la hipotesis nula de que los datos siguen una distribucion normal

#----------------------------------------------------------------------
# kNN con muestra de entrenamiento del 70 por ciento
#----------------------------------------------------------------------

library(class)
# Muestra de entrenamiento
set.seed(2021)
# Indices 7000 train y 3000 test
train<-sample(seq(length(datos$Gender)),length(datos$Gender)*0.70,replace=FALSE) 
#elegimos muestra de entrenamiento. Tambien podemos utilizar el paquete caret.

x<-datos[,c(2,3)] #variables independ
y<-datos[,1] #variable depen

# Ajuste con muestra de entrenamiento
knn1 <- knn(train=x[train,], test= x[-train,], cl=y[train], k=20) 

t1<-table(knn1,y[-train])
t1
sum(t1)
sum(diag(t1))/sum(t1)
mean(knn1==y[-train]) #porcentaje de bien clasificados 0.91133

# Que pasa con k=21, el metodo mejora??

#----------------------------------------------------------------------
# Validacion cruzada con la muestra de entrenamiento 
#----------------------------------------------------------------------
knn.cross <- knn.cv(train=x[train,],y[train],k=20,prob=TRUE)
#al modelo lo ajustamos con la muestra de entrenamiento y lo validamos con el  test

knn.cross
t2<-table(knn.cross,y[train])
t2
sum(diag(t2))/sum(t2) # 0.918


#----------------------------------------------------------------------
# Con toda la muestra
#----------------------------------------------------------------------
knn.cross2 <- knn.cv(x,y,k=20,prob=TRUE)
t3<-table(knn.cross2,y)
t3
sum(diag(t3))/sum(t3) # 0.9168

#----------------------------------------------------------------------
# Optimizacion de k 
#----------------------------------------------------------------------
#bucle para optimizar el k (hasta 200 k ponemos)

k <- 1:200
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

## Donde se alcanza el optimo?

#con la funcion wichmax calculamos el k optimo

which.max(resultado$Precision) #el maximo esta en 69 k

#si tenemos dos maximos debemos elegir el menor xq exige menos tiempo de procesamiento al algoritmo



