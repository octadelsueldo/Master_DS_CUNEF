#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Arboles de Clasificacion
# Practica 8
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Paquetes y Datos
#----------------------------------------------------------------------
rm(list=ls())
library(rpart)      
library(rpart.plot) 

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
# Variable dependiente
#----------------------------------------------------------------------

VentasA <- ifelse(Sales>8,1,0) 
VentasA <- as.factor(VentasA)
datos <- cbind(datos,VentasA)
datos <- as.data.frame(datos)
datos<-datos[,-1]
head(datos)

#----------------------------------------------------------------------
# Crecimiento del arbol
#----------------------------------------------------------------------
arbolrpart <- rpart(VentasA ~ ., method = "class", data =datos )

#----------------------------------------------------------------------
# Estad?sticos y gr?fico
#----------------------------------------------------------------------

print(arbolrpart)                         

# extra=4: muestra probabilidad de observaciones por clase
rpart.plot(arbolrpart,extra=4)  

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
parbolrpart<- prune(arbolrpart, cp= 0.045732)
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
t1<-table(predrpart, VentasA)
t1

# Porcentaje de aciertos
sum(diag(t1))/sum(t1)


#----------------------------------------------------------------------
# Capacidad de prediccion del arbol inical
#----------------------------------------------------------------------

pre2 <- predict(arbolrpart, newdata = datos, type = "class")

# Matriz de confusi?n
t2<-table(pre2, VentasA)
t2

# Porcentaje de aciertos
sum(diag(t2))/sum(t2) #hubo una mejora en las predicciones

