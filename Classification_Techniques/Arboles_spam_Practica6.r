# Arboles de decisión y método Naive Bayes

## Practicos - Semana 3 
### Ejercicio 6

# La  base  de  datos `spam7` del  paquete `DAAG` contiene  informacion  sobre  un  total  de  4601 emails, de los que 1813 se identifican como spam
# 
# - crl.tot, longitud total de las palabras que estan en mayusculas;
# 
# - dollar, frecuencia del sımbolo $, en terminos del porcentaje respecto de caracteres;
# 
# - bang, frecuencia del simbolo !, en terminos del porcentaje respecto de caracteres;
# 
# - money frecuencia de la palabra money, en terminos del porcentaje respecto de caracteres;
# 
# - n000, frecuencia de la cadena 000, en terminos del porcentaje respecto de caracteres;
# 
# - make, frecuencia de la palabra make, en terminos del porcentaje respecto de caracteres.
# 
# Se pide,
# 
# - Predecir la variable yesno en terminos de las variables anteriores. Representar el arbol obtenido
# 
# - Encontrar un tamaño de  arbol  optimo mediante la libreria `rpart` haciendo uso del parametro `cp`
# 
# - Obtener el porcentaje de aciertos del arbol final.
# 
# - Además, predecir la variable mediante el método Naive Bayes y obtener la matriz de confusión y el porcentaje de aciertos. Comentar los resultados.



library(DAAG)
library(e1071)
library(dplyr)
library(corrplot)
library(rpart)      
library(rpart.plot) 
data("spam7")
data <- spam7
names(data)
head(data)
str(data)



# Correlations
corrplot(cor(data %>% 
               select_at(vars(-yesno)), 
             use = "complete.obs"), 
         method = "circle",type = "upper")



#* Primer punto



# Crecimiento del arbol con rpart
modelo <- rpart(yesno ~ ., method = "class", data =data)

# Informacion y grafico
print(modelo)                         
# La opcion extra=2: es la probabilidad de datos por clase
rpart.plot(modelo,extra=2)  


#* Segundo punto



# Estad?sticas de resultados
printcp(modelo)

# Evolucion del error a medida que se incrementan los nodos
plotcp(modelo) 

# Podado del arbol

# Forma automatica
parbolrpart<- prune(modelo, cp= modelo$cptable[which.min(modelo$cptable[,"xerror"]),"CP"])

# Grafico del arbol podado
rpart.plot(parbolrpart,extra=2) 
print(parbolrpart)




#* Punto 3. Porcentaje de acierto del arbol final

# Predicci?n 
# Validamos la capacidad de prediccion del arbol 

predrpart <- predict(parbolrpart, newdata = data, type = "class")

# Matriz de confusion
t1<-table(predrpart, data$yesno)
t1

# Porcentaje de aciertos
sum(diag(t1))/sum(t1) #Accuracy del 86%




#Predecir la variable mediante el método Naive Bayes y obtener la matriz de confusión y el porcentaje de aciertos. 


# Modelo Naive Bayes
modelo1 <- naiveBayes(yesno ~ ., data = data)

# Prediccion Naive Bayes
prediccion <- predict(modelo1, newdata = data, type = "class")

# Matriz de Confusion y Probabilidad de Acierto
matrizconfusion <- table(prediccion, data$yesno)
matrizconfusion

# Porcentaje de aciertos
sum(diag(matrizconfusion))/sum(matrizconfusion) #accuracy del 75%

