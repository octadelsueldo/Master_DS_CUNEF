#----------------------------------------------------------------------
# MDS - Tecnicas de Clasificacion 
# Contrastar Normalidad multivariante
#----------------------------------------------------------------------

rm(list=ls())
library(MVN)
data(iris)
attach(iris)
# Una variable categorica Species y 4 variables continuas
names(iris) 

#----------------------------------------------------
# Normalidad Univariada
#----------------------------------------------------

qqnorm(Sepal.Width) #estamos trabajando con colas pesadas
qqline(Sepal.Width)
shapiro.test(Sepal.Width) #no rechazamos que esa variable sigue una distirbucion normal

#----------------------------------------------------
# Normalidad multivariante
#----------------------------------------------------
# Tres Test Diferentes:
# - Test de Mardia: Se basa en la asimetria y curtosis multivariantes
# - Test de Henze-Zirkler: Se basa en la funcion caracteristica. Contrasta la funcion caracterica de los datos sobre el de la poblacion
# - Test de Royston: Similar a SW pero multivariante 
#----------------------------------------------------

# a) Se elige subconjunto setosa de Iris data
setosa <- iris[1:50, 1:4] #elefimos las 50 primeras que son las setodas y las variables de 1 a 4

# b) TEST  de Mardia - Henze-Zirkler - Royston
result <- mvn(data = setosa, mvnTest = "mardia")
result$multivariateNormality
#el test primero contrasta la curtosis y luego la asimetria. Como dice yes sigue una distribucion normal



# TEST Henze-Zirkler
result <- mvn(data = setosa, mvnTest = "hz")
result$multivariateNormality
#es mas exigente que el test anteior, hay mas probabilidades de que se rechace a que no

#  TEST Royston
result <- mvn(data = setosa, mvnTest = "royston")
result$multivariateNormality

# TEST Doornik-Hansen
result <- mvn(data = setosa, mvnTest = "dh")
result$multivariateNormality

# c) Chi-square Q-Q plot

mplot <- mvn(data = setosa, mvnTest = "royston", multivariatePlot = "qq")
#este qq plot se basa en el test de royston. Es un qqplot multivariado. Sirve para medir la distancia entre dos vectores de datos que estan correlacionados

# d) Plots univariantes

# Crea QQ plots univariantes 
result <- mvn(data = setosa, mvnTest = "royston", univariatePlot = "qqplot")

# Crea histogramas univariantes 
result <- mvn(data = setosa, mvnTest = "royston", univariatePlot = "histogram")

# Tests S-W univariantes 
result <- mvn(data = setosa, mvnTest = "royston", univariateTest = "SW", desc = TRUE)
result$univariateNormality

# e) Contrastes MVN con dos variables, sin Petal.Width 

setosa2 <- iris[1:50, 1:2]

result2 <- mvn(data = setosa2, mvnTest = c("mardia"))
result2$multivariateNormality
result2 <- mvn(data = setosa2, mvnTest = c("hz"))
result2$multivariateNormality
result2 <- mvn(data = setosa2, mvnTest = c("royston"))
result2$multivariateNormality
result2 <- mvn(data = setosa2, mvnTest = c("dh"))
result2$multivariateNormality
result2 <- mvn(data = setosa2, mvnTest = c("energy"))
result2$multivariateNormality


# f) Graficos Perspective and contour plots

par(mfrow=c(1,1))

# Perspective
result <- mvn(setosa2, mvnTest = "hz", multivariatePlot = "persp")

# contour plot
result <- mvn(setosa2, mvnTest = "hz", multivariatePlot = "contour")

# g) Multivariate outliers

versicolor <- iris[51:100, 1:3]

# Mahalanobis distance

par(mfrow=c(1,2))
result <- mvn(data = versicolor, mvnTest = "hz", multivariateOutlierMethod = "quan")

# Adjusted Mahalanobis distance

result <- mvn(data = versicolor, mvnTest = "hz", multivariateOutlierMethod = "adj")
par(mfrow=c(1,1))


# h) Analisis MVN con especies

result <- mvn(data = iris, subset = "Species", mvnTest = "hz")
result$multivariateNormality

