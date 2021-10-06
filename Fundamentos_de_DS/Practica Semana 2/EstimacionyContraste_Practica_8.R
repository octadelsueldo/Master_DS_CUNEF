#-----------------------------------------------------
# MDS - Fundamentos 
# Estimación de máxima verosimlitud mediante MASS
# Practica 8
# Comparación de tres modelos
#-----------------------------------------------------

rm(list=ls())

data(swiss)
names(swiss)
attach(swiss)


hist(Agriculture)
x <- Agriculture

library("MASS")

ajuste1 <- fitdistr(x, "normal")
ajuste1
ajuste1$loglik

ajuste2 <- fitdistr(x, "logistic")
ajuste2
ajuste2$loglik

ajuste3 <- fitdistr(x, "cauchy")
ajuste3
ajuste3$loglik

ajuste4 <- fitdistr(x, "exponential")
ajuste4
ajuste4$loglik


# Seleccionamos la distribución normal max(loglik)

# Representación histograma y función ajustada

hist(x,main="Histograma",prob=T,xlim = c(0,100))
x1<-seq(0,100,0.1)
lines(x1,dnorm(x1,ajuste1$estimate[1],ajuste1$estimate[2]), col="blue")
box(lwd=1)
