# Ejercicio numero 13


Sensibilidad <- 10/(10+15) # TP/TP+FN
Sensibilidad
#### PARA MI LA RESPUESTA CORRECTA HUBIESE SIDO 0.4

Especifidad <- 8/10
Especifidad

Accucary <- 18/25
Accucary


# Ejercicio numero 14

prob <- 1/(1 + exp(-0.01 + 0.5*0.24 + 0.17*1.25 - 2.1*0.73))

Odds <- prob / (1 - prob)

# Ejercicio numero 15

dnorm(3.5, 3, 1)

dnorm(3.5, 2, 2)

dnorm(3.5, 4, 0.5)

dnorm(3.5, 3.5, 1)

# Ejercicio numero 16 


str(Default)
attach(Default)
names(Default)
modelo_examen <- glm(default ~ student + balance, data=Default, family = binomial(link = probit))
summary(modelo_examen)

predict(modelo_examen, newdata=data.frame(balance=2137, student=factor("No")))

# Ejercicio numero 17

library(ISLR)
data("Wage")
str(Wage)

head(Wage)

# Modelo Probit
model_a <- glm(health ~ age + race + education + logwage, data=Wage, family = binomial(link = probit))
summary(model_a) 

# Matriz de confusi?n
#----------------------------------------------------------------------

fit.pred <- ifelse(model_a$fitted.values>0.5,1,0)
tabla<-table(fit.pred,Wage$health)
tabla
(tabla[1,1]+tabla[2,2])/sum(tabla) # Accuracy 0.71

## QDA
library(MASS)
qdamod <- qda(health ~ age + race + education + logwage, data=Wage)  #aqui hacemos lo miso solo que elegimos qda pero el resto de comandos coinciden
qdamod
# Prediccion respuesta
qdaResult <- predict(qdamod, newdata=Wage) 
# Matriz de confusion
tqdamod<-table(qdaResult$class, Wage$health) 
tqdamod
sum(diag(tqdamod))/sum(tqdamod) # Accuracy (0.6983)

## Naive Bayes

# Modelo Naive Bayes
#----------------------------------------------------------------------
library(e1071)
modelo1 <- naiveBayes(formula=health ~ age + race + education + logwage, data=Wage) #modelo
modelo1

prediccion11 <- predict(modelo1, newdata=Wage, type = "class")

matrizconfusion <- table(Wage$health, prediccion11)
matrizconfusion
sum(diag(matrizconfusion))/sum(matrizconfusion) # Accuracy 0.723

## Modelo SVM radial

svm1 <- svm(health ~ age + race + education + logwage, data=Wage, kernel="radial") #default sobre el resto
print(svm1) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm1) #clases, niveles, etc
t1<-table(Wage$health,svm1$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.717 es el acierto

## Modelo SVM sigmoid

svm2 <- svm(health ~ age + race + education + logwage, data=Wage, kernel="sigmoid") #default sobre el resto
print(svm2) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm2) #clases, niveles, etc
t1<-table(Wage$health,svm2$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.6473 es el acierto



# Ejercicio 18
library(ISLR)
str(Default)
attach(Default)
names(Default)
datos <- data.frame(Default[-2]) #quitamos la variable categorica

# Kernel Sigmoid
svm1 <- svm(default~balance + income, data=datos, kernel="sigmoid") #default sobre el resto
print(svm1) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm1) #clases, niveles, etc
t1<-table(default,svm1$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.9419 es el acierto

# Kernel Radial
svm2 <- svm(default~balance + income, data=datos, kernel="radial") #default sobre el resto
print(svm2) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm2) #clases, niveles, etc
t1<-table(default,svm2$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.9725 es el acierto

# Kernel Lineal
svm3 <- svm(default~balance + income, data=datos, kernel="linear") #default sobre el resto
print(svm3) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm3) #clases, niveles, etc
t1<-table(default,svm3$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.9667 es el acierto

# Kernel Polinomial
svm4 <- svm(default~balance + income, data=datos, kernel="polynomial") #default sobre el resto
print(svm4) #nos da parametros de clasificacion, tiene que ver con variables categoricoas, Number of support vector son los puntos que me definen la frontera
summary(svm4) #clases, niveles, etc
t1<-table(default,svm4$fitted) #creamos la tabla de default con el ajustado
t1
sum(diag(t1))/sum(t1) # 0.9718 es el acierto



### Ejercicio 19

library(MASS)
data("cats")
str(cats)

ldamod <- lda(Sex ~ Bwt + Hwt, data = cats)
ldamod

predict(ldamod, newdata=data.frame(Bwt=2.8, Hwt=15.1))


### Ejercicio 20

# Obtener una suma de 7 entre los dos dados
siete <- 7/12
siete
oddsratio <- siete/(1 - siete)
oddsratio 
