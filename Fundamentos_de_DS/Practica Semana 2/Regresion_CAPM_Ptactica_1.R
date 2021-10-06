#----------------------------------------------------------------------
# Master DSF
# Pr?ctica Regresi?n CAPM
# Pr?ctica 1
#----------------------------------------------------------------------

rm(list=ls())
datos<-read.table("Cotizaciones2020.txt",header=T)
names(datos)
attach(datos)
head(datos)
tail(datos)

#----------------------------------------------------------------------
# Rendimientos
#----------------------------------------------------------------------

n <- dim(datos)[1]
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1
RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RBBVA  <- BBVA.MC[2:n] / BBVA.MC[1:n-1] - 1
RREP  <- REP.MC[2:n] / REP.MC[1:n-1] - 1
RITX  <- ITX.MC[2:n] / ITX.MC[1:n-1] - 1
RTL5 <- TL5.MC[2:n] / TL5.MC[1:n-1] - 1


#----------------------------------------------------------------------
# Modelos CAPM
#----------------------------------------------------------------------

plot(RIBEX, RSAN,main = "Modelo R(SAN)=a+bR(IBEX)", col = "red")
modelo1 <- lm(RSAN ~ RIBEX)
summary(modelo1)
abline(modelo1)

modelo2 <- lm(RBBVA ~ RIBEX)
summary(modelo2)
abline(modelo2)

modelo3 <- lm(RREP ~ RIBEX)
summary(modelo3)
abline(modelo3)


modelo4 <- lm(RITX ~ RIBEX)
summary(modelo4)
abline(modelo4)

modelo5 <- lm(RTL5 ~ RIBEX)
summary(modelo5)
abline(modelo5)

