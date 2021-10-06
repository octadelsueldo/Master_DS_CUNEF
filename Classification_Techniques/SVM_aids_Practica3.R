#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Maquinas de soporte vectorial - SVM
# Practica 3
#----------------------------------------------------------------------

rm(list=ls())
library(e1071)

# Selección de variables
library(MASS)
data("Aids2")
str(Aids2)
datos <- data.frame(Aids2[,-4])
str(datos)

#----------------------------------------------------------------------
# Se estima un modelo svm lineal con todos los datos
#----------------------------------------------------------------------
svm1 <- svm(status~., data=datos, kernel="linear", scale=FALSE)
print(svm1)
summary(svm1)


t1<-table(datos$status,svm1$fitted)
t1
sum(diag(t1))/sum(t1) # 0.785

#----------------------------------------------------------------------
# Se estima un modelo SVM lineal para la muestra de entrenamiento
#----------------------------------------------------------------------

# Selección muestra entrenamiento
set.seed(2021)
train<-sample(seq(length(Aids2$status)),length(Aids2$status)*0.70,replace=F)

svmfit <- svm(datos$status~.,data=datos,kernel="linear",scale=FALSE,subset=train)
print(svmfit)

# Predicción para la muestra test
svm.pred <- predict(svmfit,datos[-train,])

# Matriz de confusion
t2<-table(svm.pred,datos$status[-train])
t2
sum(diag(t2))/sum(t2) # 0.784
# de forma equivalente
with(datos[-train,],table(svm.pred,status))

#----------------------------------------------------------------------
# Modelo SVM kernel radial para la muestra de entrenamiento y se predice la muestra de test
#----------------------------------------------------------------------
svmfit2<-svm(datos$status~.,data=datos,kernel="radial",scale=FALSE,subset=train,probability=TRUE)
print(svmfit2)
print(svmfit2)

svm.pred2<-predict(svmfit2,datos[-train,],probability=TRUE)

t3<-table(svm.pred2,datos$status[-train])
t3
sum(diag(t3))/sum(t3) # 0.735
with(datos[-train,],table(svm.pred,status))




