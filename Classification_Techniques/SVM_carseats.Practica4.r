#----------------------------------------------------------------------
# Paquetes y Datos
#----------------------------------------------------------------------
rm(list=ls())

library(e1071)
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
attach(datos)
#----------------------------------------------------------------------
# Se estima un modelo svm radial con todos los datos: kernel radial
#----------------------------------------------------------------------
svm1 <- svm(VentasA~CompPrice + Income + Advertising + Population + Price + Age + Education, data=datos, kernel="radial") #default sobre el resto
print(svm1) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm1) #clases, niveles, etc
t1<-table(VentasA,svm1$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.84 es el acierto
plot(svm1, data=datos)

#----------------------------------------------------------------------
# Seleccion muestra entrenamiento
#----------------------------------------------------------------------
set.seed(2021)
train<-sample(seq(length(VentasA)), length(VentasA)*0.70,replace=F)
# 7000 train; 3000 test


#----------------------------------------------------------------------
# Se estima un modelo svm radial para la muestra de entrenamiento
#----------------------------------------------------------------------
svmfit2 <- svm(VentasA ~CompPrice + Income + Advertising + Population + Price + Age + Education, data=datos, kernel="radial", subset=train)
print(svmfit2)
summary(svmfit2)

# Predicci?n para la muestra test
svm.pred2 <- predict(svmfit2, datos[-train,]) #predecimos en base al train

t2<-table(svm.pred2, datos$VentasA[-train])
t2
sum(diag(t2))/sum(t2) # 0.825 acierto

plot(svmfit2, data=datos)

#----------------------------------------------------------------------
# Mejorando mediante tune.svm sobre el total de datos
#----------------------------------------------------------------------
#tuneamos los datos con algunas modificaciones en el setup. Variamos 10 elevado a la -3 a 0 que es -3-2-4 en valores. Coste seran 3 valores. Maneras de ahorrarnos la idea de poner un vector
tuned<-tune.svm(VentasA~CompPrice + Income + Advertising + Population + Price + Age + Education, 
                data=datos, 
                kernel="radial", 
                gamma=10^(-3:0), 
                cost=10^(0:2))

# tarda un momento
summary(tuned)

# best parameters:
# gamma 0.001
# cost 100

svmbest <- svm(VentasA~CompPrice + Income + Advertising + Population + Price + Age + Education, data=datos, kernel="radial", gamma=0.001, cost=100)
print(svmbest)
summary(svmbest)
t3<-table(VentasA,svmbest$fitted)
t3
sum(diag(t3))/sum(t3) # 0.8075
plot(svmbest, data=datos)
