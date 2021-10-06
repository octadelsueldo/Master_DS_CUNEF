#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Maquinas de soporte vectorial - SVM
# Practica 2
#----------------------------------------------------------------------

library(e1071)
library(mlbench)
data(Glass)
attach(Glass)

# Nos aseguramos que la variable y=Type es un factor
str(Glass)

summary(Glass)

barplot(table(Type)) #tiene 7 categorias la variable dependiente
pairs(Glass[,-10])

#----------------------------------------------------------
# Muestra de entrenamiento y de control 66/33
#----------------------------------------------------------
# Dividimos los datos en train y test
nrow(Glass) # Total datos

index <- sample(1:nrow(Glass), trunc(length(1:nrow(Glass))*(2/3)))
trainset <- Glass[index,]
testset <- Glass[-index,] 

# Comprobaci?n 
nrow(trainset)
nrow(testset)

#----------------------------------------------------------
# svm 
#----------------------------------------------------------
svm.model0 <- svm(Type ~ ., data = trainset) #elegimos el tipo en funcion de las variables de entrenaminto
svm.pred0 <- predict(svm.model0, testset[,-10])

# matriz de confusion
t0<-table(pred = svm.pred0, true = testset[,10])
t0
sum(t0)
sum(diag(t0))/sum(t0) # 0.6944

#----------------------------------------------------------
# svm con cost =100 y gamma=1
#----------------------------------------------------------
svm.model1 <- svm(Type ~ ., data = trainset, cost = 10, gamma = 1) #tuneamos los parametros con un cost 10 y gamma 1
svm.pred1 <- predict(svm.model1, testset[,-10])

# matriz de confusion
t1<-table(pred = svm.pred1, true = testset[,10])
t1
sum(t1)
sum(diag(t1))/sum(t1) 

#----------------------------------------------------------------------
# Mejorando mediante tune.svm sobre el total de datos
#----------------------------------------------------------------------
tuned<-tune.svm(Type ~ ., 
                data=trainset, 
                kernel="radial", 
                gamma=10^(-2:0), 
                cost=10^(0:2))

summary(tuned)

# best parameters:
# gamma 0.1
# cost 100

svm.best <- svm(Type ~ ., data = trainset, cost = 10, gamma = 0.1)
pre.svm.best <- predict(svm.best, testset[,-10])

# matriz de confusion
t2<-table(pred = pre.svm.best, true = testset[,10])
t2
sum(t2)
sum(diag(t2))/sum(t2) # 0.6111

# Ampliar cost y gamma


