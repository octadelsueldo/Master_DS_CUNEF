#----------------------------------------------------------------------
# Master MDS
# Metodos de Clasificacion
# Regresi?n logistica - Practica Regresion por pasos
# Practica 10
#----------------------------------------------------------------------

rm(list=ls())
# Datos
library(AER) #paquete que tiene el modelo clasificacion sobre todo para clasificar viviendas y valorarlas en base al precio
data(HousePrices)
help(HousePrices)
names(HousePrices)
attach(HousePrices)

summary(HousePrices)

#----------------------------------------------------------------------
# Ajuste RL
#----------------------------------------------------------------------

fit1 <- glm(aircon~.,family="binomial",data=HousePrices) #aircon es una variable binaria entnoces podemos hacer regresion logistica
summary(fit1)


#----------------------------------------------------------------------
# Regresion por pasos
#----------------------------------------------------------------------
library(MASS)
fit2 <- stepAIC(fit1) #con el stepAIC definimos la seleccion del modelo
summary(fit2) 

#Bondad de ajuste LR test
model_0 <- glm(aircon ~ 1, family="binomial",data=HousePrices)
anova(model_0,fit2,test="Chisq")

# Pseudo R2 de McFadden
library("pscl")
pR2(fit2)  # McFadden = 0.5457844 

# Calculo directo McFadden
1-fit2$deviance/model_0$deviance

#----------------------------------------------------------------------
# Matriz de confusi?n
#----------------------------------------------------------------------

fit.pred <- ifelse(fit2$fitted.values>0.5,1,0)
tmodel<-table(fit.pred,aircon)
tmodel
(tmodel[1,1]+tmodel[2,2])/sum(tmodel)



