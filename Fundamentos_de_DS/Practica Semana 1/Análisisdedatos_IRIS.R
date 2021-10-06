#-----------------------------------------------------
# MDS - Fundamentos 
# Analisis de Datos IRIS: Seleccion de casos y variables
#-------------------------------------------------------

rm(list = ls())

data(iris)
head(iris)
class(iris)
str(iris)
variable.names(iris)
levels(iris$Species)
help(iris)

summary(iris)
boxplot(iris$Sepal.Length, col="lightgray")

boxplot(iris, col = "lightgray")

#-------------------------------------------------------
# Prescindir de especies y boxplot sin especies
#-------------------------------------------------------

irissin <- iris[,-5]
irissin

boxplot(irissin, col = "lightgray")

#-------------------------------------------------------
# Seleccionar las 10 primeras observaciones
#-------------------------------------------------------

vb <- iris[1:10,]
vb
boxplot(vb)

#-------------------------------------------------------
# Seleccionar variables "Sepal.Length" "Sepal.Width"
#-------------------------------------------------------

sepalos <- c("Sepal.Length" , "Sepal.Width")
sepalos

iris_sepalos <- iris[sepalos]
boxplot(iris_sepalos)

iris_sepalos <- iris[c("Sepal.Length" , "Sepal.Width")]
boxplot(iris_sepalos)

#-------------------------------------------------------
#Seleccionar una sola variable
#-------------------------------------------------------

vc <- iris["Sepal.Length"]
vc
boxplot(vc)

#-------------------------------------------------------
# Seleccionar los datos correspondientes a las especies "setosa", "virginica". 
# A continuaci?n, seleccionar los casos 1 a 3 y 98 a100
#-------------------------------------------------------

iris_less <- subset(iris, Species %in% c("setosa", "virginica"))
iris_less
boxplot(iris_less)

#-------------------------------------------------------
# A continuci?n, seleccionar los casos 1 a 11 y 57 a 100
#-------------------------------------------------------

iris_2 <- iris_less[c(1:11, 57:100),]
iris_2

#-----------------------------------------------------
# Gr?ficos IRIS Bidimensionales y tridimensionales
#-----------------------------------------------------

data(iris)
iris
variable.names(iris)

library(plot3D)

x <-  iris$Sepal.Length
y <-  iris$Petal.Length
z <-  iris$Sepal.Width


#----------------------------------------------------------------------
# Gr?ficos bidimensionales
#----------------------------------------------------------------------
par(mfrow=c(1, 1))

plot(x,z, xlab="Longitud S?palo", ylab = "Anchura S?palo", pch=16)
plot(x,y, xlab="Longitud S?palo", ylab = "Longitud P?talo", pch=16)

par(mfrow=c(1, 2))
plot(x,z, xlab="Longitud S?palo", ylab = "Anchura S?palo", pch=16)
plot(x,y, xlab="Longitud S?palo", ylab = "Longitud P?talo", pch=16)
par(mfrow=c(1, 1))

#----------------------------------------------------------------------
# Gr?ficos bidimensionales por Especies
#----------------------------------------------------------------------

plot(x,z, xlab="Longitud S?palo", ylab = "Anchura S?palo", col=iris$Species, pch=16)
plot(x,y, xlab="Longitud S?palo", ylab = "Longitud P?talo", col=iris$Species, pch=16)

par(mfrow=c(1, 2))
plot(x,z, xlab="Longitud S?palo", ylab = "Anchura S?palo", col=iris$Species, pch=16)
plot(x,y, xlab="Longitud S?palo", ylab = "Longitud P?talo", col=iris$Species, pch=16)
par(mfrow=c(1, 1))

#-----------------------------------------------------
# Diagrama coordenadas paralelas
#-----------------------------------------------------

library(MASS)
par(mfrow=c(1, 1))

parcoord(iris[,1:4], col=iris$Species, var.label = T)

#-----------------------------------------------------
# Gr?ficos trimensional b?sicos
#-----------------------------------------------------

scatter3D(x, y, z, clab = c("Sepal", "Width (cm)"))


scatter3D(x, y, z, phi = 2, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")

scatter3D(x, y, z, phi = 10, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")

data(iris)
head(iris)


attach(iris)
boxplot(Sepal.Length ~ Species, ylab="Sepal.Length", main = "Sepal.Length por Specie")


boxplot(Sepal.Width ~ Species, ylab="Sepal.Width", main = "Sepal.Width por Specie")


boxplot(Petal.Length ~ Species, ylab="Petal.Length", main = "Petal.Length por Specie")

boxplot(Petal.Width ~ Species, ylab="Petal.Width", main = "Petal.Width por Specie")


library(ggplot2)
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + geom_point()


ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) + geom_point()


ggplot(iris, aes(x = Petal.Width, y = Sepal.Length, color = Species)) + geom_point()


ggplot(iris, aes(x = Petal.Length, y = Sepal.Width, color = Species)) + geom_point()


cr <- cor(iris[1:4])
library(corrplot)
corrplot(cr,method="number")

corrplot(cr)

iris_setosa <- subset(iris, Species %in% c("setosa"))
iris_setosa <- iris_setosa[,-5]
iris_setosa
boxplot(iris_setosa)


library(ggplot2)
library(gridExtra)

# Sepal Length & Width
p1 <- ggplot(iris, aes(x=Species, y=Sepal.Length), aes(fill = factor(Species))) +
  ggtitle("Sepal Length") + geom_boxplot(aes(fill = factor(Species))) +  
  guides(fill=guide_legend(title="Species")) + geom_jitter()
p2 <- ggplot(iris, aes(x=Species, y=Sepal.Width), aes(fill = factor(Species))) +
  ggtitle("Sepal Width") + geom_boxplot(aes(fill = factor(Species))) +  
  guides(fill=guide_legend(title="Species")) + geom_jitter()

grid.arrange(p1, p2, ncol=2, nrow=1)


# Pedal Length & Width
p3 <- ggplot(iris, aes(x=Species, y=Petal.Length), aes(fill = factor(Species))) +
  ggtitle("Petal Length")+  geom_boxplot(aes(fill = factor(Species))) + 
  guides(fill=guide_legend(title="Species")) + geom_jitter()
p4 <- ggplot(iris, aes(x=Species, y=Petal.Width), aes(fill = factor(Species))) +
  ggtitle("Pedal Width") + geom_boxplot(aes(fill = factor(Species))) +    
  guides(fill=guide_legend(title="Species")) + geom_jitter()

grid.arrange(p3, p4, ncol=2, nrow=1)

