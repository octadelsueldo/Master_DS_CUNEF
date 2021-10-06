#----------------------------------------------------------------------
# Master MDS
# Metodos de Clasificacion
# Regresi?n Logit Multinomial - Practica Morosos
#----------------------------------------------------------------------

rm(list=ls())
library(foreign)
library(nnet) # multinomial
library(stargazer) #sirve para formatos



#----------------------------------------------------------------------
# Lectura de datos
#----------------------------------------------------------------------
datos<-read.table("prestamos.txt",header=T)
datos
attach(datos)
names(datos)
cor(ingresos,patrneto) #correlacion
cor.test(ingresos,patrneto)
#----------------------------------------------------------------------
# Incluimos los labels
#----------------------------------------------------------------------
categ <- factor(categ, labels = c("Cumplidor", "Moroso", "Fallido"))
casado <- factor(casado, labels = c("Otros", "Casado"))
proviv <- factor(proviv, labels = c("Otros", "Propietario"))
salfijo <- factor(salfijo, labels = c("Otros", "Fijo"))

table(categ)
table(casado)
table(proviv)
table(salfijo)

par(mfrow=c(1,1))
plot(ingresos,patrneto, xlab="ingresos", ylab = "patrneto", col=categ, pch=16)
plot(ingresos,patrneto, xlab="ingresos", ylab = "patrneto", col=casado, pch=16)
plot(ingresos,patrneto, xlab="ingresos", ylab = "patrneto", col=salfijo, pch=16)
plot(ingresos,patrneto, xlab="ingresos", ylab = "patrneto", col=proviv, pch=16)

#----------------------------------------------------------------------
# Box-Plot
#----------------------------------------------------------------------
par(mfrow=c(1,2))
boxplot(ingresos~categ,col="orange")
boxplot(patrneto~categ,col="orange")

boxplot(ingresos~casado,col="orange")
boxplot(patrneto~casado,col="orange")

boxplot(ingresos~proviv,col="orange")
boxplot(patrneto~proviv,col="orange")

boxplot(ingresos~salfijo,col="orange")
boxplot(patrneto~salfijo,col="orange")
par(mfrow=c(1,1))

#----------------------------------------------------------------------
# Comprobar que el nivel de referencia de la dependiente 
# es el valor 1, a efectos de interpretacion
#----------------------------------------------------------------------
categ <- relevel(categ, ref = "Cumplidor")
categ
table(categ)

#----------------------------------------------------------------------
# Resultados basicos
#----------------------------------------------------------------------
multi1 <- multinom(categ ~ ingresos + patrneto + proviv + casado + salfijo, data=datos)
summary(multi1) #aparecen dos modelos xq tiene 3 niveles

multi1$fitted.values

exp(coef(multi1))

stargazer(multi1, type="text")

#----------------------------------------------------------------------
# Seleccionamos variables
#----------------------------------------------------------------------
multi2 <- multinom(categ ~  ingresos + casado + salfijo, data=datos)

summary(multi2)

multi2$fitted.values

stargazer(multi2, type="text")

#----------------------------------------------------------------------
# Riesgos relativos
#----------------------------------------------------------------------
multi2.rrr <- exp(coef(multi2))
multi2.rrr

stargazer(multi2, type="text", coef = list(multi2.rrr), p.auto=FALSE)

probability.table <- fitted(multi2, "class")
probability.table


# Predicci?n
datos$precticed <- predict(multi2, "class", newdata = datos)
datos$precticed

#----------------------------------------------------------------------
# Matriz de confusion
#----------------------------------------------------------------------
ctabla <- table(categ, datos$precticed)
ctabla

# Precision tabla
round((sum(diag(ctabla))/sum(ctabla))*100,2)

