#----------------------------------------------------------------------
# MDS - Tecnicas de Clasificacion 
# Modelos de AD para Datos del MC. Modelos LDA - QDA
# Práctica 8
#----------------------------------------------------------------------

rm(list=ls())
datos<-read.table("Cotizaciones2020.txt",header=T)
names(datos)
attach(datos)
head(datos)

#----------------------------------------------------------------------
# Calculo de los Rendimientos
#----------------------------------------------------------------------

n <- dim(datos)[1]
n
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1
RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RBBVA  <- BBVA.MC[2:n] / BBVA.MC[1:n-1] - 1
RREP  <- REP.MC[2:n] / REP.MC[1:n-1] - 1
RITX  <- ITX.MC[2:n] / ITX.MC[1:n-1] - 1
RTL5 <- TL5.MC[2:n] / TL5.MC[1:n-1] - 1

#----------------------------------------------------------------------
# Variable RSAN en cuatro categorias
#----------------------------------------------------------------------
summary(RSAN)

RSANBIN <- cut(RSAN, breaks=c(-Inf, -0.015468,-0.003004, 0.011675, Inf), 
                 labels=c("lo", "med", "high", "vhigh"))

table(RSANBIN)

datos1<-cbind(RSANBIN, RSAN, RIBEX, RBBVA, RREP, RITX, RTL5)
datos1<-as.data.frame(datos1)
summary(datos1)

#----------------------------------------------------------------------
# Analisis discriminante lineal
#----------------------------------------------------------------------
library(MASS)
sant.lda <- lda(RSANBIN ~ RIBEX+RBBVA, data=datos1)
sant.lda

plot(sant.lda, pch=16)

#Probabilidades a priori
sant.lda$prior
#Probabilidades a posteriori
predict(sant.lda)$posterior

# Matriz de confusion
predicted.lda <- predict(sant.lda, data = datos1)
tabla1<-table(RSANBIN, predicted.lda$class, dnn = c("Grupo real","Grupo Pronosticado"))
tabla1

# Precision del modelo
sum(diag(tabla1))/sum(tabla1) # Precision de 0.66

# Graficos de Particion 
library(klaR)
partimat(datos1[,c(3,4)],RSANBIN,data=datos1,method="lda",main="Partition Plots") 

#----------------------------------------------------------------------
# Analisis discriminante cuadratico
#----------------------------------------------------------------------
library(MASS)
sant.qda <- qda(RSANBIN ~ RIBEX+RBBVA, data=datos1)
sant.qda

# Comprobar la hipotesis para el QDA
library(biotools)
boxM(datos1[,c(3,4)],RSANBIN)

#Probabilidades a priori
sant.qda$prior
#Probabilidades a posteriori
predict(sant.qda)$posterior

# Matriz de confusion
predicted.qda <- predict(sant.qda, data = datos1)
tabla2<-table(RSANBIN, predicted.qda$class, dnn = c("Grupo real","Grupo Pronosticado"))
tabla2

# Precision del modelo
sum(diag(tabla2))/sum(tabla2) # 0.67


# Graficos de Particion 
library(klaR)
partimat(datos1[,c(3,4)],RSANBIN,data=datos1,method="qda",main="Partition Plots")
