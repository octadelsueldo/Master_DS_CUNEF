#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Datos Empresas 
# RL LDA QDA
#----------------------------------------------------------------------
rm(list=ls())
datos<-read.table("Empresas1.txt",header=T)
names(datos)
attach(datos)
head(datos)
summary(datos)
nrow(datos)
ncol(datos)
str(datos)
#-----------------------------
# Analisis exploratorio
#-----------------------------
par(mfrow=c(3,2))
sapply(seq(2,7),function(j)hist(datos[,j],main=colnames(datos)[j],xlab="",col="blue"))
par(mfrow=c(1,1))

# Variables asimetricas
par(mfrow=c(3,2))
hist(Ventas,col="orange")
hist(log(Ventas),col="orange")
hist(Emple,col="orange")
hist(log(Emple),col="orange")
hist(BEN,col="orange")
hist(log(BEN),col="orange")
par(mfrow=c(1,1))

#-----------------------------
# Nuevas variables
#-----------------------------
summary(datos)
y1 <- ifelse(ROE>10.87,1,0) # Dependiente

lVentas <- log(Ventas)
lEmple <- log(Emple)

datos1 <- cbind(y1,lVentas, lEmple, datos)
datos2 <- na.omit(datos1)
summary(datos2)

#-----------------------------------------
# Diversos modelos
#-----------------------------------------
model1<- glm(y1 ~ AExp+Ventas+Emple+BEN+RSolv, data=datos2, family = binomial(link = logit))
summary(model1)

#-----------------------------------------
model2<- glm(y1 ~ AExp+lVentas+lEmple+BEN+RSolv, data=datos2, family = binomial(link = logit))
summary(model2)

#-----------------------------------------
model3<- glm(y1 ~ AExp+lEmple+BEN+RSolv, data=datos2, family = binomial(link = logit))
summary(model3)

# Por pasos
library(MASS)
modelo4 <- stepAIC(model2)
summary(modelo4)

#-----------------------------------------------------
# LDA
#-----------------------------------------------------
library(MASS)

# LDA analisis discriminante lineal
# Elegimos a  priori el tamanho de cada grupo
ldamod <- lda(y1 ~ lVentas + lEmple + BEN + RSolv , data = datos2)
ldamod

# Prediccion respuesta
ldaResult <- predict(ldamod, newdata = datos2) 
# Matriz de confusion
tldamod<-table(ldaResult$class, datos2$y1)
tldamod
# Precision
sum(diag(tldamod))/sum(tldamod) # acierto (71 por ciento)

library(klaR)
datos2$y1<-as.factor(datos2$y1)
partimat(datos2[,c(2,3,7,10)],datos2$y1,data=datos2,method="lda",main="Partition Plots") 

#----------------------------------------------------------------------
# Contraste de Homogeneidad de varianza
#----------------------------------------------------------------------
library(biotools)
boxM(data=datos2[,c(2,3,7,10)], grouping=datos2[,1])

# QDA analisis discriminante cuadratico
# Elegimos a  priori el tamanho de cada grupo
qdamod <- qda(y1 ~ lVentas + lEmple + BEN + RSolv , data = datos2) 
qdamod
# Prediccion respuesta
qdaResult <- predict(qdamod, newdata = datos2) 
# Matriz de confusion
tqdamod<-table(qdaResult$class, datos2$y1) 
tqdamod
sum(diag(tqdamod))/sum(tqdamod) # acierto (71 por ciento)

library(klaR)
datos2$y1<-as.factor(datos2$y1)
partimat(datos2[,c(2,3,7,10)],datos2$y1,data=datos2,method="qda",main="Partition Plots") 

