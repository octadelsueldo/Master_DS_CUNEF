#-----------------------------------------------------
# MDS - Fundamentos de Calculo
# Optimizacion mediante NLS
# Ajuste de Curvas de Lorenz
#-----------------------------------------------------

# Dominian republic 2015
x <- c(0, 0.20, 0.40, 0.60, 0.80, 1)   #tienen que ser datos acumulados de la renta
y <- c(0, 0.05, 0.14, 0.28, 0.49, 1)   #porcentaje de renta que se lleva cada grupo acumulado
plot(x,y)


# Curva de Lorenz 1            #se usa mucho para rentas. Por ekemplo el 20 porciento de la poblacion se lleva el 35 porciento de la renta. Cuando menor es el valor de a es que hay menor desigualdad
ml1 <- nls(y ~ (1-(1-x)^a)^(1/a), start = list(a=1)) #modelo 1. nls es el comando y luego se escribe la curva de lorenz. start es un valor inicial con el que se puede empezar
ml1

#la suma de los cuadrados de los residuos nos sirve para comparar modelos. Se busca el que tenga el minimo que tenga la suma de los cuadrados.
lines(x, fitted(ml1), lty = 1, col = "green", lwd = 2)  #para dibujar se utiliza el fitted del modelo
abline(0,1)


# Curva de Lorenz 2
ml2 <- nls(y ~ 1-(1-x^a)^(1/a), start = list(a=1))
ml2

lines(x, fitted(ml2), lty = 1, col = "red", lwd = 2)
abline(0,1)


# Curva de Lorenz 3
m3 <- nls(y ~ x^a, start = list(a=1))
m3

lines(x, fitted(m3), lty = 1, col = "green", lwd = 2)
abline(0,1)

#-----------------------------------------------------
# Resto de Datos
#-----------------------------------------------------

# Brazil  2015
x <- c(0, 0.20, 0.40, 0.60, 0.80, 1)
y <- c(0, 0.03, 0.11, 0.24, 0.44, 1)
plot(x,y)

# Nicaragua  2015
x <- c(0, 0.20, 0.40, 0.60, 0.80, 1)
y <- c(0, 0.05, 0.14, 0.28, 0.48, 1)
plot(x,y)


# Peru 2015
x <- c(0, 0.20, 0.40, 0.60, 0.80, 1)
y <- c(0, 0.04, 0.14, 0.29, 0.50, 1)
plot(x,y)

