#----------------------------------------------------------------------
# MDS - Tecnicas de Clasificacion 
# R Logistica Practica 3 Datos Alcohol
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Datos
#----------------------------------------------------------------------

rm(list=ls())

library(wooldridge)
data(alcohol)
head(alcohol)
attach(alcohol)

summary(cbind(alcohol$abuse, alcohol$employ, alcohol$age, alcohol$educ, alcohol$married , 
              alcohol$white))

#----------------------------------------------------------------------
# Variable Dependiente BINARIA
#----------------------------------------------------------------------
table(alcohol$employ)

#----------------------------------------------------------------------
# En el caso de Variable Dependiente BINARIA regresion lineal no va bien
#----------------------------------------------------------------------
reg.sim <- lm(employ ~ abuse + age +  educ  + married + white, data=alcohol)
summary(reg.sim)

# Se define este objeto para predecir
pred.ind <- data.frame(abuse=0, age=30, educ=18, married=1, white=1)

predict(reg.sim, newdata=pred.ind)

#----------------------------------------------------------------------
# Regresi?n LOGISTICA
#----------------------------------------------------------------------

reg.logs <- glm(employ ~ abuse + age +  educ  + married + white, data=alcohol, 
                family=binomial(logit))
summary(reg.logs)

#----------------------------------------------------------------------
# Prediccion Regresi?n LOGISTICA
#----------------------------------------------------------------------

prerl1 <- predict(reg.logs, newdata=pred.ind, type="response")
prerl1

# Otra forma
prerl2 <- predict(reg.logs, newdata=pred.ind)
prerl2
exp(prerl2)/(exp(prerl2)+1)


#----------------------------------------------------------------------
# Efectos
#----------------------------------------------------------------------
library(mfx)
logitmfx(employ ~ abuse + age +  educ  + married + white, data=alcohol)

