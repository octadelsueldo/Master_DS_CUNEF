#----------------------------------------------------------------------
# Master MDS - Fundamentos de Finanzas
# Calculo VAN, TIR Payback
# Practica 1
#----------------------------------------------------------------------

rm(list=ls())

install.packages("FinancialMath")
library(FinancialMath)

#----------------------------------------------------------------------
# Eleccion de proyectos segun VAN
#----------------------------------------------------------------------

VAN.Proyecto1 <- NPV(-1100, c(1350, 15, 15, 20, 25), c(1:5), 0.10 , plot = TRUE) 
VAN.Proyecto1
VAN.Proyecto2 <- NPV(-1100, c(-10, 0, 10, 20, 2100), c(1:5), 0.10 , plot = TRUE )
VAN.Proyecto2

#----------------------------------------------------------------------
# Eleccion de proyectos segun TIR
#----------------------------------------------------------------------

TIR.Proyecto1 <- IRR(-1100, c(1350, 15, 15, 20, 25), c(1:5) , plot = TRUE)
TIR.Proyecto1
TIR.Proyecto2 <- IRR(-1100, c(-10, 0, 10, 20, 2100), c(1:5) , plot = TRUE)
TIR.Proyecto2

#----------------------------------------------------------------------
# Eleccion de proyectos Payback
#----------------------------------------------------------------------

# Payback Proyecto1
cf0 <- -1100
cf <- c(1350, 15, 15, 20, 25)
t <- 5

# Calculo del Payback 

temp <- 0

for (i in 1:5){
  temp[i]=sum(cf[1:i])
}
for (i in 1:5){
  if ((temp[i]+cf0) > 0){
    payback.Proyecto <- i
    break
  }
  print(payback.Proyecto)
}
print(payback.Proyecto)


# Payback Proyecto2

cf0 <- -1100
cf <- c(-10, 0, 10, 20, 2100)
t <- 5

# Calculo del Payback 

temp <- 0

for (i in 1:5){
  temp[i]=sum(cf[1:i])
}
for (i in 1:5){
  if ((temp[i]+cf0) > 0){
    payback.Proyecto <- i
    break
  }
  print(payback.Proyecto)
}
print(payback.Proyecto)

