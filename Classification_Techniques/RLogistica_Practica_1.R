#----------------------------------------------------------------------
# Master MDS
# Metodos de Clasificacion
# Regresion logistica - Practica 1
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Lectura de datos
#----------------------------------------------------------------------
rm(list=ls())
datos <- read.table("LogisticRegression1.txt",header = T)
names(datos)
attach(datos)
datos


#----------------------------------------------------------------------
# Descriptiva
#----------------------------------------------------------------------
boxplot(Ingresos~Compra,data=datos, main="Ingresos compra/no compra",
        xlab="compra 0/1", ylab="Ingresos",col="grey")
summary(Ingresos)

#----------------------------------------------------------------------
# Ajuste del Modelo de regresion logistica
# Bondad de ajuste
#----------------------------------------------------------------------
model <- glm(Compra ~ Ingresos, data=datos, family = binomial(link = logit)) #si queremos cambiar al modelo normal cambiamos por probit en el link
summary(model)

#La función glm() de R nos permite ajustar modelos lineales de muchos tipos, incluyendo los que
#ajustamos con lm(), modelos de Poisson y los logit
#El segundo, family=binomial(), especifica la función de probabilidad que utilizaremos.
#Para modelos logit es una función binomial. Dentro de los paréntesis se puede especificar la 
#función de enlace. Para la familia de distribuciones binomial glm() por defecto usa una función
#logit. Si nos interesa una función probit deberíamos especificar link=probit.

model$coefficients
model$deviance #menos dos veces el logaritmo de la verosimilitud
model$null.deviance #modelo que ajusta unicamente con el termino independiente

summary(model$fitted.values) 

model$fitted.values #valores ajustados

#Bondad de ajuste LR test: contraste GLOBAL. 
#ajustamos un modelo donde el model tiene unicamente el termino independiente. Lo que se hace es poner menos 1 al final.
model_0 <- glm(Compra ~ 1, data=datos, family = binomial(link = logit))

#lo que hacemos es un cociente de verosimilitudes
anova(model_0,model,test="Chisq") #comando que nos permite calcular la sumad de cuadrados. Al poner chisq ya entiende que estamos haciendo
#un cociente de verosimilitudes.
#se diferencian en un grado de libertad. El p valor del chiq es en el que nos debemos fijar para rechazar la hipotesis nula.
#LA hipotesis nula H0 es que el cociente de los ingresos es cero contra la hipotesis de que no lo es

# Pseudo R2 de McFadden
library("pscl") #paquete que permite calcularlo
pR2(model)  # McFadden = 0.5457844 

# Calculo directo McFadden
1-model$deviance/model_0$deviance #1 menos el cociente de las deviance

# Otra paquete
library(epiDisplay)

logistic.display(model) 
exp(cbind(coef(model), confint.default(model)))  #ORratio


#----------------------------------------------------------------------
# Dibujo en ggplot2
#----------------------------------------------------------------------
library("ggplot2")
ggplot(datos, aes(x=Ingresos, y=Compra)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)
#ingresos frente a la compra
#----------------------------------------------------------------------
# Matriz de confusion
#----------------------------------------------------------------------

fit.pred <- ifelse(model$fitted.values>0.5,1,0) #si el valor ajustado >0.5 entonces la clasificacion sera 1
tmodel<-table(fit.pred,datos$Compra) #los predichos frente a los observados
tmodel
(tmodel[1,1]+tmodel[2,2])/sum(tmodel) #medida de calidad del modelo

#----------------------------------------------------------------------
# ROC  #forma visual de ver la capacidad de discriminacion del modelo
#----------------------------------------------------------------------
library(Deducer)
model2 <- glm(formula=Compra ~ Ingresos, data=datos, family = binomial(), na.action=na.omit)
rocplot(model2) #aca nos deberia aparecer la figura de antes

predict1<-predict(model, type="response") #el comando predict sirve para predecir. Si no indicamos el predic nos da el valor del logit. Debemos poner el response osea la respuesta.
predict1

#----------------------------------------------------------------------
# Otra forma ROC
#----------------------------------------------------------------------
model <- glm(Compra ~ Ingresos, data=datos, family = binomial)
summary(model)

# Elementos necesarios para ROC
prob=predict(model,type=c("response"))
datos$prob=prob

library(pROC)
g <- roc(Compra ~ prob, data = datos)
plot(g) #esto es lo del ROC

#----------------------------------------------------------------------
# Predecir Odds Ratio Ingreso=56.2 
# Odds ratio
#----------------------------------------------------------------------

# Predicci?n

x0<-data.frame(Ingresos=56.2) #debemos hacer un dataframe con los datos que queremos predecir
predict(model, newdata=x0, type="response") #en predict debemos anadir la funcion newdata en este caso x0
# al ser 0.9 estaria en la clase 1

# Odds ratio

exp(model$coefficients) #la exponencial del cociente

Odds<-function(x)
{
exp(model$coefficients[1]+model$coefficients[2]*x)  #funcion exponencial de esos cociente por la x
}
Odds(56.2) #hay 9 mas probabilidad de que compre si fuere 1 a que no

Odds(56.2)/(1+Odds(56.2)) 
1/(1+exp(-(model$coefficients[1]+model$coefficients[2]*56.2))) #esto es otra forma de verlo



