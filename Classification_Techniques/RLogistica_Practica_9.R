#Practica 9 de regresion logistica del master en data science

library(AER)
library(dplyr)
data("CreditCard")
head(CreditCard)

# 1) Seleccionar los individuos de > 18 

creditmayores <- CreditCard %>% filter(age >= 18.)

# 2) Regresion logistica

regr.log <- glm(card ~ ., data = creditmayores, family = binomial(link = logit))
summary(regr.log)

# 3) Regresion logistica por pasos para seleccion del mejor modelo

library(MASS)
regresionxpasos <- stepAIC(regr.log) #con el stepAIC definimos la seleccion del modelo
summary(regresionxpasos)

#segun el AIC el modelo simplificado que mejor predice la variable dependiente es
#Step:  AIC=128.02
#card ~ reports + expenditure + dependents + active

#De estas variables, la que mayor significancia tiene es active

