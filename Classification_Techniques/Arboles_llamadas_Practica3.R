#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Arboles de Decision
# Practica 3
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Paquetes y Datos
#----------------------------------------------------------------------
rm(list = ls())
library(rpart)      # la mejor para arboles
library(rpart.plot) # para hacer plots de arboles

# Paquete con los Datos
library(modeldata)
# Datos del operador de telefonia
data("mlc_churn")
attach(mlc_churn)
names(mlc_churn)
churn <- mlc_churn
summary(churn)
str(churn)


#----------------------------------------------------------------------
# Seleccion de variables 
#----------------------------------------------------------------------
churn        <- churn[,c(4,7,8,16,19,17,20)]  #trabajaremos con esta variables
names(churn) <- c("Dispone_plan_internac", "Min_dia", "Llam_dia", 
                  "Min_internac", "Reclamaciones", "Llam_internac", 
                  "Cancelacion")

boxplot(Min_dia ~ Cancelacion, col="orange" , data=churn) #un poco de EDA para conocer los datos. Minutos diarios de los que cancelan
boxplot(Min_internac ~ Cancelacion, col="orange" , data=churn) #minutos internacionales que cancelan y no

table(churn$Cancelacion) #los que no cancelan y los que si cancelan

#----------------------------------------------------------------------
# Crecimiento del arbol
#----------------------------------------------------------------------
arbolrpart <- rpart(Cancelacion ~ ., method = "class", data =churn ) #cancelacion frente al resto e variables con el metodo class 

#----------------------------------------------------------------------
# Estadisticos y grafico
#----------------------------------------------------------------------

print(arbolrpart)      #nos imprime las reglas de decision. son importantes cuando queremos vender el producto, saber los porcentajes, etc.                   

# extra=4: muestra probabilidad de observaciones por clase
rpart.plot(arbolrpart,extra=4)  #extra 4 significa que busca la probabilidad de las observaciones por clase

# Estadisticas de resultados
printcp(arbolrpart) #nos muestra lo mismo en tablas
# Evolucion del error a medida que se incrementan los nodos
plotcp(arbolrpart) #plot del error para ver si merece la pena un corte. Se ve claramente que en 7 podemos podar el arbol. 
# Lo mantenemos en el nivel 4. Debemos fijarnos en el error relativo si aumenta o disminuye

#----------------------------------------------------------------------
# Podado del arbol
#----------------------------------------------------------------------
# Forma automatica
parbolrpart<- prune(arbolrpart, cp= arbolrpart$cptable[which.min(arbolrpart$cptable[,"xerror"]),"CP"])
# Forma manual
parbolrpart<- prune(arbolrpart, cp= 0.015559) #lo cortamos en un valor de complejidad de 0.015559

#----------------------------------------------------------------------
# Grafico del arbol podado
#----------------------------------------------------------------------
rpart.plot(parbolrpart,extra=4) 
print(parbolrpart)

printcp(parbolrpart) #factor de complejidad es el primer dato que nos sale en la tabla
plotcp(parbolrpart)    

#----------------------------------------------------------------------
# Predicci0n 
# Validamos la capacidad de prediccion del arbol 
#----------------------------------------------------------------------

predrpart <- predict(parbolrpart, newdata = churn, type = "class")

# Matriz de confusi0n
t1 <- table(predrpart, churn$Cancelacion)
t1

# Porcentaje de aciertos
sum(diag(t1))/sum(t1)

#----------------------------------------------------------------------
# Modelo Naive Bayes
#----------------------------------------------------------------------
modelo1 <- naiveBayes(formula=Cancelacion ~ ., data = churn) #modelo
modelo1
prediccion1 <- predict(modelo1, newdata=churn, type = "raw")#para predecir en base a naiveBaye. Raw nos saca las probab si ponemos class nos saca la prediccion del genero
head(prediccion1)

prediccion11 <- predict(modelo1, newdata=churn, type = "class")

matrizconfusion <- table(churn$Cancelacion, prediccion11)
matrizconfusion

# Porcentaje de aciertos
sum(diag(matrizconfusion))/sum(matrizconfusion) #87.36%

