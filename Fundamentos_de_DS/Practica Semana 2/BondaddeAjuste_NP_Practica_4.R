#-----------------------------------------------------------------------
# MDS - Contrastes de bondad de sjuste
# Practica 4
# Hipotesis de Normalidad : Fichero Swiss
#-----------------------------------------------------------------------

rm(list=ls())

data(swiss)
names(swiss)
help(swiss)
attach(swiss)

#-----------------------------------------------------------------------
# Variables education y agriculture
#-----------------------------------------------------------------------

qqnorm(Education) #qq significa quantile quantile. Comparar los quantiles teoricos con los quantiles muestrales. Si  la distr es normal los quant teoricos de la distribucion deben coincidir con los quant muestrales
qqline(Education) #si todos los datos fueran normales todos deberian estar sobre la linea recta. La hipotesis nula aqui es que los datos son normales
shapiro.test(Education)
#se rechaza la hipotesis nula
qqnorm(Agriculture)
qqline(Agriculture)
shapiro.test(Agriculture)
#aqui no se rechaza porq la p es 0.193
#-----------------------------------------------------------------------
# Todas las variables
#-----------------------------------------------------------------------

X <- as.data.frame(swiss)
X

par(mfrow=c(2,3))
for (j in 1:6) {
  qqnorm(X[,j],main=colnames(X)[j],xlab="",col="blue",lwd=1)
  qqline(X[,j],main=colnames(X)[j],xlab="",col="red",lwd=1)
}

# Contraste Shapiro Wilks
sapply(seq(1,6),function(j) shapiro.test(X[,j])) #con shapiro y sapply reviso si todas las variables se comportan como normal

       


