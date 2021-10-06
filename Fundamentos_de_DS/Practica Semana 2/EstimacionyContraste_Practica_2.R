#----------------------------------------------------------------------
# MDS - Métodos de Estimacion y Contraste
# Elección de muestras
# Practica 2
# Comparación de medias
#----------------------------------------------------------------------

rm(list=ls())
datos<-read.table("Cotizaciones2020.txt",header=T)
names(datos)
attach(datos)

nrow(datos)
ncol(datos)

#----------------------------------------------------------------------
# Cálculo de los Rendimientos
#----------------------------------------------------------------------

n <- dim(datos)[1]
n
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1
RIBEX2  <- log(IBEX[2:n] / IBEX[1:n-1])

RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RSAN2  <- log(SAN.MC[2:n] / SAN.MC[1:n-1])

RTL5 <- TL5.MC[2:n] / TL5.MC[1:n-1] - 1
RTL52  <- log(TL5.MC[2:n] / TL5.MC[1:n-1])

#----------------------------------------------------------------------
# Tiempo continuo y discreto
# H0: mu_D = mu_C; H1 distinto
#----------------------------------------------------------------------

# IBEX

plot(RIBEX, RIBEX2)

var.test(RIBEX,RIBEX2) # Igualdad de volatilidades

t.test(RIBEX, RIBEX2) # Igualdad de medias

t.test(RIBEX, RIBEX2, var.equal = TRUE) # Igualdad de medias con var. igules


# Santander

plot(RSAN, RSAN2)

var.test(RSAN,RSAN2) # Igualdad de volatilidades

t.test(RSAN, RSAN2) # Igualdad de medias

t.test(RSAN, RSAN2, var.equal = TRUE) # Igualdad de medias con var. igules


# TL5

plot(RTL5, RTL52)

var.test(RTL5,RTL52) # Igualdad de volatilidades

t.test(RTL5, RTL52) # Igualdad de medias

t.test(RTL5, RTL52, var.equal = TRUE) # Igualdad de medias con var. igules

