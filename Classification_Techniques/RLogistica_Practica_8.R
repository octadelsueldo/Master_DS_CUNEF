#----------------------------------------------------------------------
# MDS - Fundamentos 
# Tecnicas de Clasificacion
# Comparación logit-probit
#----------------------------------------------------------------------

#----------------------------------------------------------------------
# Comenzamos con una simulación del modelo Logit
#----------------------------------------------------------------------

rm(list=ls())

set.seed(2020)
x1 <- rnorm(5000, 1, 2)

y1<- rbinom(5000, size=1, prob=plogis(x1))
datos1<-cbind(x1,y1)

datos1<-data.frame(datos1)

m1<-glm(y1~x1)
summary(m1)

# Dibujo en ggplot2
library("ggplot2")
ggplot(datos1, aes(x=x1, y=y1)) + geom_point() + 
  stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

# Matriz de confusión
fit.pred <- ifelse(m1$fitted.values>0.5,1,0)
table(fit.pred,datos1$y1)
table(fit.pred,y1)
(1122+2855)/5000

#----------------------------------------------------------------------
# Comparamos modelos logit y probit
#----------------------------------------------------------------------

x <- rnorm(1000) 
y <- rbinom(n=1000, size=1, prob=pnorm(x)) 
glm(y~x, family=binomial(link="logit")) 

set.seed(2020) 
probMenor <- vector(length=100) 
devprobit <- vector(length=100)
devlogit <- vector(length=100)

# Genera un modelo probit en cada paso (100 modelos) con 1.000 datos
for(i in 1:100){ 
  x <- rnorm(1000) 
  y <- rbinom(n=1000, size=1, prob=pnorm(x)) 
  logitModel <- glm(y~x, family=binomial(link="logit")) 
  probitModel <- glm(y~x, family=binomial(link="probit"))
  probMenor[i] <- deviance(probitModel)<deviance(logitModel) 
  devprobit[i] <- deviance(probitModel)
  devlogit[i] <- deviance(logitModel)
}

# Porcentaje de casos en que deviance Probit 
# es menor a Deviance Logit (Probit mejor modelo)
sum(probMenor)/100

# Gráfico de ambas deviances
comparaPL <- cbind(devprobit, devlogit, probMenor)
comparaPLdf <- as.data.frame(comparaPL)
ggplot(comparaPLdf, aes(devprobit, devlogit)) + geom_point()+ geom_smooth()

