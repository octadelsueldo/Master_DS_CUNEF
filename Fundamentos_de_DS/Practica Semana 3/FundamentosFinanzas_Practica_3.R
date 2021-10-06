#----------------------------------------------------------------------
# Master MDS - Fundamentos de Finanzas
# Practica modelos CAPM y diversificacion de cartera 
# Practica 3
#----------------------------------------------------------------------

rm(list=ls())
datos<-read.table("Cotizaciones2020.txt",header=T)
names(datos)
attach(datos)
head(datos)
tail(datos)

#----------------------------------------------------------------------
# Calculo de los Rendimientos
#----------------------------------------------------------------------

n <- dim(datos)[1]
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1
RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RBBVA  <- BBVA.MC[2:n] / BBVA.MC[1:n-1] - 1
RREP  <- REP.MC[2:n] / REP.MC[1:n-1] - 1
RITX  <- ITX.MC[2:n] / ITX.MC[1:n-1] - 1
RTL5 <- TL5.MC[2:n] / TL5.MC[1:n-1] - 1

#----------------------------------------------------------------------
# Propiedades Estadisticas
#----------------------------------------------------------------------
Rendimientos<-cbind(RIBEX, RSAN, RBBVA, RREP, RITX,RTL5)
summary(Rendimientos)
pairs(Rendimientos)

#----------------------------------------------------------------------
# Modelo CAPM: RSAN=a+b*RIBEX
#----------------------------------------------------------------------
plot(RIBEX,RSAN)
cor(RIBEX,RSAN)
CAPM_S <- lm(RSAN ~ RIBEX) 
summary(CAPM_S)
abline(CAPM_S)
# Rendimientos medio, dt y varianza
mean(RSAN)
sd(RSAN)
var(RSAN)

#----------------------------------------------------------------------
# Modelo CAPM RSAN=a+b*RIBEX
# Riesgos ESPECIFICO y SISTEMATICO
#----------------------------------------------------------------------
# Riesgo ESPECIFICO:
summary(CAPM_S)$sigma*100
# Riesgo SISTEMATICO: b*sd(RIBEX)
CAPM_S$coefficients[[2]]*sd(RIBEX)*100


#----------------------------------------------------------------------
# Modelo CAPM: RTL5=a+b*R(IBEX)
# Riesgos ESPECIFICO y SISTEMATICO
#----------------------------------------------------------------------
plot(RIBEX,RTL5)
CAPM_I <- lm(RTL5 ~ RIBEX) 
summary(CAPM_I)
abline(CAPM_I)
# Rendimientos medio, dt y varianza
mean(RTL5)
sd(RTL5)
var(RTL5)

#----------------------------------------------------------------------
# Modelo CAPM: RTL5=a+b*R(IBEX)
# Riesgos ESPECIFICO y SISTEMATICO
#----------------------------------------------------------------------
# Riesgo ESPECIFICO 
summary(CAPM_I)$sigma*100
# Riesgo SISTEMATICO: b*var(RIBEX)
CAPM_I$coefficients[[2]]*sd(RIBEX)*100

#----------------------------------------------------------------------
# Construccion de las Carteras
#----------------------------------------------------------------------
w <-0.6
RCartera <- w*RSAN+(1-w)*RTL5
CAPM_cartera <- lm(RCartera ~ RIBEX) 
summary(CAPM_cartera)

# Riesgo ESPECIFICO 
summary(CAPM_cartera)$sigma*100
# Riesgo SISTEMATICO: b*var(RIBEX)
CAPM_cartera$coefficients[[2]]*sd(RIBEX)*100

plot(RIBEX,RCartera)
abline(CAPM_cartera)


















