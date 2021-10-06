#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Naive Bayes - Datos Iris
# Practica 1 
#----------------------------------------------------------------------

rm(list=ls())
library(e1071) #nombre del despacho
data(iris)

#----------------------------------------------------------------------
# Modelo Naive Bayes
#----------------------------------------------------------------------
modelo1 <- naiveBayes(Species ~ ., data = iris) #ponemos la variable que se puede predecir. 
#La variable que se quiere predecir debe ser categorica. Las otras continuas o discretas. En el caso discreto lo hace por la regla de la place (se suma uno en el numerado y denominador).
#para las continuas utiliza la distribucion normal.

modelo1 #aparecen las 4 variables. La primera es la media de las longifutd dpor por especie la probabilidad condicional de la longitud por la especie.

# Comprobacion medias por grupo
aggregate(iris$Sepal.Length, by=list(iris$Species), FUN=mean)
aggregate(iris$Sepal.Width, by=list(iris$Species), FUN=mean)
aggregate(iris$Petal.Length, by=list(iris$Species), FUN=mean)
aggregate(iris$Petal.Width, by=list(iris$Species), FUN=mean)

#----------------------------------------------------------------------
# Prediccion Naive Bayes
#----------------------------------------------------------------------
prediccion <- predict(modelo1, newdata = iris, type = "class") #en terminos de las clases

# En terminos de probabilidades
prediccion1 <- predict(modelo1, newdata = iris, type = "raw") #calcula en terminos de las probabilidades de bayes
head(prediccion1)

#----------------------------------------------------------------------
# Matriz de Confusion y Probabilidad de Acierto
#----------------------------------------------------------------------
matrizconfusion <- table(iris$Species, prediccion)
matrizconfusion

# Porcentaje de aciertos
sum(diag(matrizconfusion))/sum(matrizconfusion)

