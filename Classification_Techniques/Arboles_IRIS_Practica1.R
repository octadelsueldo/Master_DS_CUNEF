#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Arboles de Decision
# Practica 1
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Cargar el paquete de clasificacion rpart
rm(list=ls())
library(rpart)    
library(rpart.plot)
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Datos iris 
#----------------------------------------------------------------------
data(iris)
datos <- iris
names(datos)

par(mfrow=c(2,2))
boxplot(Petal.Length ~ Species, data=datos, col="orange")
boxplot(Petal.Width ~ Species, data=datos, col="orange")
boxplot(Sepal.Length ~ Species, data=datos, col="orange")
boxplot(Sepal.Width ~ Species, data=datos, col="orange")
par(mfrow=c(1,1))

#-----------------------------------------------------
# Crecimiento del arbol con rpart
#-----------------------------------------------------
modelo <- rpart(Species ~ ., method = "class", data =datos)

# Informacion y grafico
print(modelo)                         
# La opcion extra=4: es la probabilidad de datos por clase
rpart.plot(modelo,extra=4)  

#-----------------------------------------------------
# Estadisticos de resultados y 
# Errores del arbol en funcion del numero de nodos
#-----------------------------------------------------
printcp(modelo)             
plotcp(modelo)            


#-----------------------------------------------------
# Prediccion y validacion 
#-----------------------------------------------------
prediccion <- predict(modelo, newdata = datos, type = "class")

# Matriz de confusion
t1<-table(prediccion, datos$Species)
t1

# Porcentaje de aciertos
sum(diag(t1))/sum(t1)

