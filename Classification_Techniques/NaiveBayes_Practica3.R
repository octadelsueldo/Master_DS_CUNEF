#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Naive Bayes - Datos altura genero
# Practica 3
#----------------------------------------------------------------------

rm(list=ls())
library(e1071)
library(dslabs)

data(heights)
datos<-heights
attach(datos)
names(datos)

#naive bayes dice que la variable continua sigue una distr normal 
#el problema aqui es qe pasa si elegimos otra dist q no sea la normal
#----------------------------------------------------------------------
# Modelo Naive Bayes
#----------------------------------------------------------------------
modelo1 <- naiveBayes(formula= sex ~ height, data = datos) #modelo
modelo1
prediccion1 <- predict(modelo1, newdata=datos, type = "raw")#para predecir en base a naiveBaye. Raw nos saca las probab si ponemos class nos saca la prediccion del genero
head(prediccion1)

prediccion11 <- predict(modelo1, newdata=datos, type = "class")

matrizconfusion <- table(datos$sex, prediccion11)
matrizconfusion

# Porcentaje de aciertos
sum(diag(matrizconfusion))/sum(matrizconfusion) #80%

#----------------------------------------------------------------------
# Modelo Naive Bayes Modelo distribucion normal
#----------------------------------------------------------------------

m<-aggregate(height, by=list(sex), FUN=mean)
m
dt<-aggregate(height, by=list(sex), FUN=sd)
dt

probbayes <- function(x){
  PM <- sum(sex=="Female")/length(sex) #proporcion de males
  PF <- 1-PM
  GM <- PM * dnorm(x,m[1,2],dt[1,2])  #dist de densisdad normal m, es la altura
  GF <- PF * dnorm(x,m[2,2],dt[2,2]) #lo mismo
  PM <- GM/(GM+GF)
  PF <- GF/(GM+GF)
  return(list(PM=PM,PF=PF))
}

prob1<-probbayes(height)
prob1<-as.data.frame(prob1)
head(prob1)

# Porcedimientos coincidentes
head(cbind(prob1, prediccion1))

#----------------------------------------------------------------------
# Analisis exploratorio
#----------------------------------------------------------------------
summary(datos)
length(sex)
boxplot(height~sex, col="blue")

heightF <- height[sex=="Female"]
heightM <- height[sex=="Male"]

par(mfrow=c(1,2))
qqnorm(heightF); qqline(heightF)
qqnorm(heightM); qqline(heightM)
par(mfrow=c(1,1))

shapiro.test(heightF)
shapiro.test(heightF)


