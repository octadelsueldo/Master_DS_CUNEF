#----------------------------------------------------------------------
# Master MDS
# Metodos de Clasificacion
# Regresi?n logistica - Practica 2
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Lectura de datos
#----------------------------------------------------------------------
library(ISLR)
str(Default)
attach(Default)
names(Default)

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

#----------------------------------------------------------------------
# Modelos de RL Simple
# a) x =balance
# b) x =student (cualitativa)
#----------------------------------------------------------------------

# a) x Cuantitativa
model_a <- glm(default ~ balance, data=Default, family = binomial(link = logit))
summary(model_a) 

#Bondad de ajuste LR test
model_0 <- glm(default ~ 1, data=Default, family = binomial(link = logit))
anova(model_0,model_a,test="Chisq")
# Odds
exp(coef(model_a))
# Intervalos de confianza
confint(model_a)
# Predicciones para x=1000 y x=2000
predict(model_a, newdata=data.frame(balance=c(1000,2000)))
predict(model_a, newdata=data.frame(balance=c(1000,2000)),type="response")

# b) x cualitativa
model_b <- glm(default ~ student, data=Default, family = binomial(link = logit))
summary(model_b)
#Bondad de ajuste LR test
model_0 <- glm(default ~ 1, data=Default, family = binomial(link = logit))
anova(model_0,model_b,test="Chisq")
# Odds
exp(coef(model_b))
# Intervalos de confianza
confint(model_b)
# Predicciones 

predict(model_b, newdata=data.frame(student=factor(c("No","Yes"))))

predict(model_b, newdata=data.frame(student=factor(c("No","Yes"))), type = "response")


#----------------------------------------------------------------------
# Ajuste del Modelo de regresion logistica
# Predicci?n
#----------------------------------------------------------------------
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

#----------------------------------------------------------------------
# Matriz de confusi?n
#----------------------------------------------------------------------

fit.pred <- ifelse(model$fitted.values>0.5,1,0)
tabla<-table(fit.pred,Default$default)
tabla
(tabla[1,1]+tabla[2,2])/sum(tabla)

#----------------------------------------------------------------------
# ROC 
#----------------------------------------------------------------------
library(Deducer)

model2 <- glm(default ~ student+balance+income, data=Default, family = binomial(link = logit))

model2 <- glm(formula=default ~ student+balance+income,data=Default, family = binomial(link = logit), na.action=na.omit)
rocplot(model2)


###### Ejercicio 15 del examen
modelo_examen <- glm(default ~ student + balance, data=Default, family = binomial(link = probit))
summary(modelo_examen)
# el error estandar de balance es 0.0001138