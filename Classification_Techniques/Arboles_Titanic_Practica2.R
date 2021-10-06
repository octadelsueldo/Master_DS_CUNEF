#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Arboles de Decision Datos Titanic
# Practica 2
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Carga el paquete de clasificacion rpart
rm(list=ls())
library(rpart)    
library(rpart.plot)
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Datos
#----------------------------------------------------------------------
data("ptitanic")
str(ptitanic)
summary(ptitanic)

#-----------------------------------------------------
# Crecimiento del arbol con rpart
#-----------------------------------------------------
arbol <- rpart(survived ~ ., data = ptitanic)
print(arbol)

# Grafico errores. La opcion extra=4: es la probabilidad de datos por clase
rpart.plot(arbol,extra=4) 

#-----------------------------------------------------
# Estadisticos de resultados y 
# Errores del arbol en funcion del numero de nodos
#-----------------------------------------------------
printcp(arbol)             
plotcp(arbol)  


#-----------------------------------------------------
# Reducimos la complejidad del árbol
#-----------------------------------------------------
arbol1 <- rpart(survived ~ ., data = ptitanic, cp = 0.02)
print(arbol1)
rpart.plot(arbol1, extra=4)

printcp(arbol1)             
plotcp(arbol1)  


#-----------------------------------------------------
# Prediccion y validacion 
#-----------------------------------------------------
prediccion <- predict(arbol1, newdata = ptitanic, type = "class")

# Matriz de confusion
t1<-table(prediccion, ptitanic$survived)
t1

# Porcentaje de aciertos
sum(diag(t1))/sum(t1)


#-----------------------------------------------------
# Comparamos con el arbol original
#-----------------------------------------------------
prediccion0 <- predict(arbol, newdata = ptitanic, type = "class")

# Matriz de confusion
t2<-table(prediccion0, ptitanic$survived)
t2

# Porcentaje de aciertos
sum(diag(t2))/sum(t2)


