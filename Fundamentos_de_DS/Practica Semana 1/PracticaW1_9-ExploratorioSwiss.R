#-----------------------------------------------------------------
# MDS - Fundamentos
# Fichero Swiss
# Analisis exploratorio multivariante. Visualizacion multivariante
#-----------------------------------------------------------------
#Para la base de datos Swiss realizar un analisis exploratorio multivariante completo, con los siguientes elementos:
#(i) Realizar un analisis exploratorio univariante, incluyendo histogramas y estimadores nucleo de las variables marginales
#(ii) Obtener medidas exploratorias multivariantes
#(iii) Visualizacion de los datos, mediante diversos metodos

rm(list=ls())

data(swiss)
names(swiss)
help(swiss)
swiss

X <- as.data.frame(swiss)

#------------------------------------------------------------------------
# Numero de provincias nX y numero de variables pX
#------------------------------------------------------------------------

dim(X)

nX <- nrow(X)
nX
pX <- ncol(X)
pX

#------------------------------------------------------------------------
# Visualizacion datos
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Diagramas de caja de variables
#------------------------------------------------------------------------

hist(X[,1])

# Veremos dos procedemientos: mediante un bucle y mediante un vector
# sapply aplica una funci?n a una lista y la hace vector

par(mfrow=c(2,3)) #divide la ventana en 2 por 3

for (j in 1:6) {
  boxplot(X[,j],main=colnames(X)[j],xlab="",col="orange")
}

sapply(seq(1,6),function(j)boxplot(X[,j],main=colnames(X)[j],xlab="",col="orange")) #la ventaja de de los vectores es que ganan mucho tiempo computacional respecto a los buqles

#------------------------------------------------------------------------
# Histogramas con  optimo binwidth con m?todo de Sturges 
#------------------------------------------------------------------------

par(mfrow=c(2,3))

for (j in 1:6) {
  hist(X[,j],main=colnames(X)[j],xlab="",col="orange",breaks = "Sturges")
}

#------------------------------------------------------------------------
# Estimaci?n Kernel para las variables con el optimal bandwidth con kernel Gaussiano
#------------------------------------------------------------------------

par(mfrow=c(2,3))

for (j in 1:6) {
  plot(density(X[,j],kernel="gaussian"),main=colnames(X)[j],xlab="",col="orange",lwd=2)
}

#-----------------------------------------------------------------
# Estimaci?n Kernel con nucleos Gaussiano y Epanechnikov
#-----------------------------------------------------------------

par(mfrow=c(2,3))
for (j in 1:6) {
  plot(density(X[,j],kernel="gaussian"),main=colnames(X)[j],xlab="",col="orange",lwd=1)
  lines(density(X[,j],kernel="epanechnikov"),main=colnames(X)[j],xlab="",col="blue",lwd=1)
  }

#-----------------------------------------------------------------
# Scatterplot Fertility,Agriculture
#-----------------------------------------------------------------

attach(X)

par(mfrow=c(1,1))

plot(Fertility,Agriculture)


plot(Fertility,Agriculture,pch=19,col="blue",xlab="Fertility",ylab="Agriculture")

#-----------------------------------------------------------------
# 3D-Scatterplots  Fertility, Agriculture, Education
#-----------------------------------------------------------------

library(scatterplot3d)
scatterplot3d(Fertility,Agriculture,Education,pch=19,color="blue") 
scatterplot3d(Fertility,Agriculture,Education,pch=19,color="blue",type="h")


library(rgl)
open3d() # Abre ventana en 3 dimensiones
plot3d(Fertility,Agriculture,Education,size=5) # Se giran las variables


#-----------------------------------------------------------------
# Scatterplot para todas las variables de Swiss
#-----------------------------------------------------------------

pairs(X,pch=19,col="orange") #se usan mucho para ver las dependencias que hay

#-----------------------------------------------------------------
# Coordenadas paralelas para las variables de Swiss
#-----------------------------------------------------------------

library(MASS)    #sirve para interpretar clase o grupos de individuos. Para cada individuo hay un punto para cada variable. Por ej hay mucha gente religiosa o ninguna
parcoord(X,col="orange",var.label = TRUE)

#-----------------------------------------------------------------
# Medidas multivariantes
#-----------------------------------------------------------------

# Vector de medias

muX <- colMeans(X)
muX


#-----------------------------------------------------------------
# Matriz e covarianzas y autovalores
#-----------------------------------------------------------------

SX <- cov(X)
SX

# Autovalores y autovectores de S
eigen(SX) 
eigen(SX)$values
sum(eigen(SX)$values) # Traza de S
det(SX)

RX <- cor(X)
RX

# Autovalores y autovectores de R
eigen(RX) 
eigen(RX)$value
sum(eigen(RX)$values) # Traza de S
det(RX) 

# Vector de medias de un subconjunto

muX1 <- colMeans(X[,1:3])
muX1


#-----------------------------------------------------------------
# Diagrama de Caras de Chernoff y de Estrellas
#-----------------------------------------------------------------

library(TeachingDemos)  #el paquete teaching demos relaciona un numero a una cara. Por ejemplos datos de los jugadores con sus nombres
faces2(swiss)
faces2(swiss[1:12,])

stars(swiss)
stars(swiss[1:12,]) #cogen las variables y forman una estrella donde la punta es el dato normalizado


#-----------------------------------------------------------------
# Coplot
#-----------------------------------------------------------------

coplot(swiss$Fertility ~ swiss$Agriculture |swiss$Education) #dos variables para los datos de una tercera variable quitando datos de cada nivel

#-----------------------------------------------------------------
# Estandarizacion multivatiante de Swiss
#-----------------------------------------------------------------

pairs(X,pch=19,col="orange")

# Estandarizaci?n univariante

sX <- scale(X)

pairs(sX,pch=19,col="orange")

# Estandarizacion multivariante
# Lo veremos mas adelante

iS.X <- solve(SX) # Inversa de IS

e <- eigen(iS.X)
V <- e$vectors
B <- V %*% diag(sqrt(e$values)) %*% t(V) 

Xtil <- scale(X,scale = FALSE)

S_X <- Xtil %*% B

colMeans(S_X)
cov(S_X)
round(colMeans(S_X),2) # comprobar
round(cov(S_X),2) # comprobar

pairs(S_X,pch=19,col="red")
