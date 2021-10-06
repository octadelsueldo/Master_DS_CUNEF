# Lectura de datos
#----------------------------------------------------------------------
rm(list=ls())
library(ISLR)
str(Default)
attach(Default)
names(Default)
datos<-Default

#----------------------------------------------------------------------
# Descriptiva
#----------------------------------------------------------------------
summary(Default)

par(mfrow=c(2,2))
boxplot(balance~default, col="orange")
boxplot(balance~student, col="orange")
boxplot(income~default, col="orange")
boxplot(income~student, col="orange")
par(mfrow=c(1,1))

hist(balance)
hist(income)

#----------------------------------------------------------------------
# Crecimiento del arbol
#----------------------------------------------------------------------
arbolrpart <- rpart(default ~ ., method = "class", data =datos )

#----------------------------------------------------------------------
# Estad?sticos y gr?fico
#----------------------------------------------------------------------

print(arbolrpart)                         

# extra=2: muestra probabilidad de observaciones por clase
rpart.plot(arbolrpart,extra=2)  

# Estad?sticas de resultados
printcp(arbolrpart) #aqui ocurre que el error relativo llega un mommento que en 0.62 se para entonces deberiamos cortar aqui o en el anterior

# Evoluci?n del error a medida que se incrementan los nodos
plotcp(arbolrpart)              

#----------------------------------------------------------------------
# Podado del arbol
#----------------------------------------------------------------------
# Forma autom?tica
parbolrpart<- prune(arbolrpart, cp= arbolrpart$cptable[which.min(arbolrpart$cptable[,"xerror"]),"CP"])
# Forma manual
#parbolrpart<- prune(arbolrpart, cp= 0.045732)
printcp(parbolrpart)

#----------------------------------------------------------------------
# Grafico del arbol podado
#----------------------------------------------------------------------
rpart.plot(parbolrpart,extra=4) 
print(parbolrpart)

printcp(parbolrpart)
plotcp(parbolrpart)

#----------------------------------------------------------------------
# Predicci?n 
# Validamos la capacidad de predicci?n del ?rbol 
#----------------------------------------------------------------------

predrpart <- predict(parbolrpart, newdata = datos, type = "class")

# Matriz de confusi?n
t1<-table(predrpart, default)
t1

# Porcentaje de aciertos
sum(diag(t1))/sum(t1)


#----------------------------------------------------------------------
# Capacidad de prediccion del arbol inical
#----------------------------------------------------------------------

pre2 <- predict(arbolrpart, newdata = datos, type = "class")

# Matriz de confusi?n
t2<-table(pre2, default)
t2

# Porcentaje de aciertos
sum(diag(t2))/sum(t2) #hubo una mejora en las predicciones

