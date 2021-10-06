#-----------------------------------------------------
# Master MDSF
# Metodos de Clasificacion
# Analisis Discriminante - Practica 6 - Prestamos
#-----------------------------------------------------
rm(list=ls())

#-----------------------------------------------------
# Lectura de datos
#-----------------------------------------------------
datos<-read.table("prestamos.txt",header=T)
attach(datos)

categ <- factor(categ, labels = c("Cumplidor", "Moroso", "Fallido"))
casado <- factor(casado, labels = c("Otros", "Casado"))
proviv <- factor(proviv, labels = c("Otros", "Propietario"))
salfijo <- factor(salfijo, labels = c("Otros", "Fijo"))

categ <- as.factor(categ)
casado <- as.factor(casado)
proviv <- as.factor(proviv)
salfijo <- as.factor(salfijo)
datos<-as.data.frame(datos)

par(mfrow=c(2,2))
plot(ingresos,patrneto, xlab="ingresos", ylab = "patrneto", col=categ, pch=16)
plot(ingresos,patrneto, xlab="ingresos", ylab = "patrneto", col=casado, pch=16)
plot(ingresos,patrneto, xlab="ingresos", ylab = "patrneto", col=salfijo, pch=16)
plot(ingresos,patrneto, xlab="ingresos", ylab = "patrneto", col=proviv, pch=16)
par(mfrow=c(1,1))

#-----------------------------------------------------
# Box-Plot
#-----------------------------------------------------
par(mfrow=c(2,2))
boxplot(ingresos~categ,col="orange")
boxplot(patrneto~categ,col="orange")
boxplot(ingresos~casado,col="orange")
boxplot(patrneto~casado,col="orange")
par(mfrow=c(2,2))
boxplot(ingresos~proviv,col="orange")
boxplot(patrneto~proviv,col="orange")
boxplot(ingresos~salfijo,col="orange")
boxplot(patrneto~salfijo,col="orange")
par(mfrow=c(1,1))

#-----------------------------------------------------
# Comprobar que el nivel de referencia de la dependiente 
# es el valor 1, a efectos de interpretacion
#-----------------------------------------------------
categ <- relevel(categ, ref = "Cumplidor")
categ
table(categ)

#-----------------------------------------------------
# LDA
#-----------------------------------------------------
library(MASS)

# LDA analisis discriminante lineal
# Elegimos a  priori el tama?o de cada grupo
ldamod <- lda(categ ~ ingresos+patrneto , data = datos)
ldamod

#primero aparecen las proba a priori por grupo, luego las medidas de las dos variables dentro de cada grupo (centroides),
#luego las funciones lineales de fisher y luego 

# Probabilidades posteriores
predict(ldamod)$posterior 
#obtenemos las probailidades posteriores de cada elemento. tenemos que tener 3 estimaciones de la probabilidad y la asignaremos al que tenga la maxima
#para el primer individuo por ej lo asignariamos al primer grupo

# Prediccion respuesta
ldaResult <- predict(ldamod, newdata = datos) 
#hacemos la prediccion de toda la muestra y con esa tabla de resultados creamos la matriz de confusion

# Matriz de confusion
tldamod<-table(ldaResult$class, datos$categ) #los datos los dividimos en categorias 
tldamod
# Precision
sum(diag(tldamod))/sum(tldamod) # acierto (68 %)

library(klaR)
partimat(datos[,-c(1,4,5,6)],categ,data=datos,method="lda",main="Partition Plots") #elegimos primero las variables que hemos utilizado en el analisis, luego la variable categoria y luego la particion por el metodo lineal

#los puntos rojos serian los centroides 

# QDA analisis discriminante cuadratico

# Elegimos a  priori el tama?o de cada grupo

qdamod <- qda(categ ~ ingresos+patrneto , data = datos)  #aqui hacemos lo miso solo que elegimos qda pero el resto de comandos coinciden
qdamod
# Prediccion respuesta
qdaResult <- predict(qdamod, newdata = datos) 
# Matriz de confusion
tqdamod<-table(qdaResult$class, datos$categ) 
tqdamod
sum(diag(tqdamod))/sum(tqdamod) # acierto (56 %)

library(klaR)
partimat(datos[,-c(1,4,5,6)],categ,data=datos,method="qda",main="Partition Plots") 

#-----------------------------------------------------
# Hipotesis de LDA
#-----------------------------------------------------

#-----------------------------------------------------
# (a) Contraste de igualdad del vector de medias                  
#-----------------------------------------------------

#tenemos que corrobar que las medias de los centroides seran iguales
library(MASS)
require(rrcov)
Wilks.test(datos[,2:3],grouping=datos[,1], method="c") #test de wilks sigue para comparar. La variable 1 es la cateogria

#en este caso la conclusion se puede rechazar la hipotesis nula. Es decir sus medias son distintas porq su p valor es menor al 5%

#-----------------------------------------------------
# (b) Contraste de igualdad de matrices de covarianzas                  
#-----------------------------------------------------
#install.packages("biotools")
library(biotools)
boxM(data=datos[,c(2,3)], grouping=datos[,1]) #la hipotesis nula es que las matrices de covarianzas de esas dos variables por grupos son las mismas.

#no se rechaza la hipotesis nula. Es decir puede que las matrices de covarianzas de esas dos variables por grupos pueden ser las mismas
# las matrices de covarianzas de esos tres grupos no son distintas. Es decir, no se puede usar la QDA. Si fueran distintas se pueden elegir el LDA y el QDA
#-----------------------------------------------------
# (c) Contraste de normalidad multivariante	                          
#-----------------------------------------------------
library(MVN)

# Por grupos
grupo1 <- subset(datos, categ  %in% c("1")); grupo1 #elegimos el subconjunto del primer, segundo y tercer grupo
grupo2 <- subset(datos, categ  %in% c("2")); grupo2
grupo3 <- subset(datos, categ  %in% c("3")); grupo3
#
mvn(data =grupo1[,c(2,3)],mvnTest="mardia")$multivariateNormality #comparamos la hipotesis de la normalidad con mardia para las dos variables que utilizamos y con multivariadnormal le decimos que nos incluya la ifnormacion que queremos
mvn(data =grupo2[,c(2,3)],mvnTest="mardia")$multivariateNormality
mvn(data =grupo3[,c(2,3)],mvnTest="mardia")$multivariateNormality
#
mvn(data =grupo1[,c(2,3)],mvnTest="hz")$multivariateNormality
mvn(data =grupo2[,c(2,3)],mvnTest="hz")$multivariateNormality
mvn(data =grupo3[,c(2,3)],mvnTest="hz")$multivariateNormality
#
mvn(data =grupo1[,c(2,3)],mvnTest="royston")$multivariateNormality
mvn(data =grupo2[,c(2,3)],mvnTest="royston")$multivariateNormality
mvn(data =grupo3[,c(2,3)],mvnTest="royston")$multivariateNormality

# como todos los test dicen que etsa ok hay unanimidad de que es normal

# Veamos globalmente 
mvn(data =datos[,c(2,3)],mvnTest="mardia")$multivariateNormality
mvn(data =datos[,c(2,3)],mvnTest="hz")$multivariateNormality
mvn(data =datos[,c(2,3)],mvnTest="royston")$multivariateNormality

#--------------------------------------------------
# (d) Normalidad variables por categorias graficamente
#--------------------------------------------------
par(mfrow=c(2,3))
for(j in c(2,3)){
  j0 <- names(datos)[j]
  x0<-seq(min(datos[,j]), max(datos[,j]), le=50)
  for(i in 1:3){
    i0 <- levels(categ)[i]
    x <- datos[categ == i0, j0]
    qqnorm(x, main=paste("Categoria", i0, j0), pch=19, col=i+1)
    qqline(x)
  }
}
par(mfrow=c(1,1))
#--------------------------------------------------

#--------------------------------------------------
# (d) p-valores variables por niveles categ por S-W
#--------------------------------------------------

# Por variable-niveles *****
for (j in c(2,3)) {
  for (i in c(1,2,3)) {
    print(shapiro.test(datos[datos$categ==i,j])$p.value)
  }
}
# Fin
