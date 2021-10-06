#----------------------------------------------------------------------
# Master MDSF
# Metodos de Clasificacion
# An?lisis Discriminante - Practica 5
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Lectura de datos
#----------------------------------------------------------------------
rm(list=ls())
library(ISLR)
str(Default)
attach(Default)
names(Default)
datos<-Default

#----------------------------------------------------------------------
# Descriptiva
#----------------------------------------------------------------------
summary(Default)

par(mfrow=c(2,2))
boxplot(balance~default, col="orange")
boxplot(balance~student, col="orange")
boxplot(income~default, col="orange")
boxplot(income~student, col="orange")
par(mfrow=c(1,1))

hist(balance)
hist(income)

#-----------------------------------------------------
# LDA
#-----------------------------------------------------
library(MASS)

# LDA analisis discriminante lineal
# Elegimos a  priori el tama?o de cada grupo
ldamod <- lda(default ~ balance+income , data = datos)
ldamod

# Predicciones Bayesianas
ldamod$prior
predict(ldamod)$posterior

# Prediccion respuesta
ldaResult <- predict(ldamod, newdata = datos) 
# Matriz de confusion
tldamod<-table(ldaResult$class, datos$default) 
tldamod
# Precision
sum(diag(tldamod))/sum(tldamod) # acierto (97 %)

library(klaR)
partimat(datos[,c(3,4)],default,data=datos,method="lda",main="Partition Plots")

# QDA analisis discriminante cuadratico
# Elegimos a  priori el tama?o de cada grupo
qdamod <- qda(default ~ balance+income , data = datos) 
qdamod

# Predicciones Bayesianas
qdamod$prior
predict(qdamod)$posterior

# Prediccion respuesta
qdaResult <- predict(qdamod, newdata = datos) 
# Matriz de confusion
tqdamod<-table(qdaResult$class, datos$default) 
tqdamod
sum(diag(tqdamod))/sum(tqdamod) # acierto (97 %)

library(klaR)
partimat(datos[,c(3,4)],default,data=datos,method="qda",main="Partition Plots") 

#----------------------------------------------------------------------
# Contraste de Homogeneidad de varianza
#----------------------------------------------------------------------
library(biotools)
boxM(data=datos[,c(3,4)], grouping=datos[,1])


#----------------------------------------------------------------------
# Ajuste del Modelo de regresi?n log?stica
# Predicci?n
#----------------------------------------------------------------------

# Incluimos la variable student
model <- glm(default ~ student+balance+income, data=Default, 
             family = binomial(link = logit))
summary(model)

#Bondad de ajuste LR test
model_0 <- glm(default ~ 1, data=Default, family = binomial(link = logit))
anova(model_0,model,test="Chisq")

# Pseudo R2
library("pscl")
pR2(model)

# McFadden = 0.4619194

# Calculo directo McFadden
1-model$deviance/model_0$deviance

# Matriz de confusi?n

fit.pred <- ifelse(model$fitted.values>0.5,1,0)
tabla<-table(fit.pred,Default$default)
tabla
(tabla[1,1]+tabla[2,2])/sum(tabla)

