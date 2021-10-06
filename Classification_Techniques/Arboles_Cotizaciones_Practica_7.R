#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Arboles de clasificacion
# Práctica 7
#----------------------------------------------------------------------

rm(list=ls())
datos<-read.table("Cotizaciones2020.txt",header=T)
names(datos)
attach(datos)
head(datos)

#----------------------------------------------------------------------
# Calculo de los Rendimientos
#----------------------------------------------------------------------

n <- dim(datos)[1]
n
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1
RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RBBVA  <- BBVA.MC[2:n] / BBVA.MC[1:n-1] - 1
RREP  <- REP.MC[2:n] / REP.MC[1:n-1] - 1
RITX  <- ITX.MC[2:n] / ITX.MC[1:n-1] - 1
RTL5 <- TL5.MC[2:n] / TL5.MC[1:n-1] - 1

#----------------------------------------------------------------------
# Variable RSAN en dos categorias
#----------------------------------------------------------------------
summary(RSAN)

sum(RSAN>0)

RSANBIN <- ifelse(RSAN>0,2,1)

table(RSANBIN)

RSANBIN<-as.factor(RSANBIN)
levels(RSANBIN) <- c("Negativo", "Positivo")
table(RSANBIN)

datos1<-cbind(RSANBIN, RSAN, RIBEX, RBBVA, RREP, RITX, RTL5)
datos1<-as.data.frame(datos1)
summary(datos1)

#----------------------------------------------------------------------
# Paquetes
#----------------------------------------------------------------------
library(rpart)      
library(rpart.plot) 

#----------------------------------------------------------------------
# Crecimiento del arbol
#----------------------------------------------------------------------
arbolrpart <- rpart(RSANBIN ~ RIBEX+RBBVA+RREP+RITX+RTL5, method = "class", data =datos1 )

#----------------------------------------------------------------------
# Estadisticos y grafico
#----------------------------------------------------------------------

print(arbolrpart)                         

# extra=4: muestra probabilidad de observaciones por clase
rpart.plot(arbolrpart,extra=4)  

# Estadisticas de resultados
printcp(arbolrpart)
# Evolucion del error a medida que se incrementan los nodos
plotcp(arbolrpart)              

#----------------------------------------------------------------------
# Predicción 
# Validamos la capacidad de predicción del árbol 
#----------------------------------------------------------------------

predrpart <- predict(arbolrpart, newdata = datos1, type = "class")

# Matriz de confusión
t1<-table(predrpart, RSANBIN)
t1

# Porcentaje de aciertos
sum(diag(t1))/sum(t1)

