#----------------------------------------------------------------------
# MDS - Tecnicas de Clasificacion 
# Analisis Discriminante
# Practica 1
#----------------------------------------------------------------------
#aqui la distribucion de los riesgos por empresa podemos verlas como multinomiales donde las proporciones son las probabilidad de la 
#multinomial entre una empresa u otra. 
#Aqui se debe asignar el x a la empresa que tiene menor verosimilitud


rm(list=ls())

# Probabilidades de cada Grupo
A <- c(0.25, 0.6, 0.15) #proporciones
B <- c(0.25, 0.7, 0.05) #proporciones

# Datos: frecuencias de cada clase
x <- c(8, 15,1) #datos

# Logaritmo de las verosimilitudes 
LL1 <- t(log(A)) %*% x #produc de los logaritmos de los x en una empresa u otra
LL1
LL2 <- t(log(B)) %*% x
LL2

# Clasificacion: Primera forma
ifelse(LL1-LL2>0,"Empresa 1","Empresa 2") #si esto es mayor q uno nos quedamos con la empresa uno sino con la dos

# Clasificacion: Segunda forma (equivalente)
z<-t(log(A/B)) %*% x 
ifelse(z>0,"Empresa 1","Empresa 2") #la alternativa es utilizando logaritmos haciendo que la diferencia sea positiva
