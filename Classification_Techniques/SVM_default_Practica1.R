#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Maquinas de soporte vectorial - SVM
# Practica 1
#----------------------------------------------------------------------

rm(list=ls())
library(e1071)

# Selecci?n de variables
library(ISLR)
str(Default)
attach(Default)
names(Default)
datos <- data.frame(Default[-2]) #quitamos la variable categorica
str(datos)
nrow(datos)

#----------------------------------------------------------------------
# Se estima un modelo svm radial con todos los datos: kernel radial
#----------------------------------------------------------------------
svm1 <- svm(default~., data=datos, kernel="radial") #default sobre el resto
print(svm1) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm1) #clases, niveles, etc
t1<-table(default,svm1$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.9725 es el acierto
plot(svm1, data=datos)

###### EJERCICIO DEL EXAMEN
svm1 <- svm(default~balance + income, data=datos, kernel="sigmoid") #default sobre el resto
print(svm1) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm1) #clases, niveles, etc
t1<-table(default,svm1$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.9725 es el acierto
plot(svm1, data=datos)

#----------------------------------------------------------------------
# Selecci?n muestra entrenamiento
#----------------------------------------------------------------------
set.seed(2021)
train<-sample(seq(length(default)), length(default)*0.70,replace=F)
# 7000 train; 3000 test


#----------------------------------------------------------------------
# Se estima un modelo svm radial para la muestra de entrenamiento
#----------------------------------------------------------------------
svmfit2 <- svm(datos$default~., data=datos, kernel="radial", subset=train)
print(svmfit2)
summary(svmfit2)

# Predicci?n para la muestra test
svm.pred2 <- predict(svmfit2, datos[-train,]) #predecimos en base al train

t2<-table(svm.pred2, datos$default[-train])
t2
sum(diag(t2))/sum(t2) # 0.973 acierto

plot(svmfit2, data=datos)

#----------------------------------------------------------------------
# Mejorando mediante tune.svm sobre el total de datos
#----------------------------------------------------------------------
#tuneamos los datos con algunas modificaciones en el setup. Variamos 10 elevado a la -3 a 0 que es -3-2-4 en valores. Coste seran 3 valores. Maneras de ahorrarnos la idea de poner un vector
tuned<-tune.svm(default~., 
                data=datos, 
                kernel="radial", 
                gamma=10^(-3:0), 
                cost=10^(0:2))

# tarda un momento
summary(tuned)

# best parameters:
# gamma 1
# cost 10

svmbest <- svm(default~., data=datos, kernel="radial", gamma=1, cost=10)
print(svmbest)
summary(svmbest)
t3<-table(default,svmbest$fitted)
t3
sum(diag(t3))/sum(t3) # 0.9729
plot(svmbest, data=datos)
