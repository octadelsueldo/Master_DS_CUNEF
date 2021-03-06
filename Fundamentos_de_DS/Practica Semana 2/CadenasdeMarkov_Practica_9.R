#----------------------------------------------------------------------
# MDS - Cadenas de Markov
# Practica 9
#----------------------------------------------------------------------

library(markovchain)

#----------------------------------------------------------------------
# Definicion de la cadena de Markov por medio de la matriz de transici�n
#----------------------------------------------------------------------

P <- matrix(c(0.37,0.23,0.22,
             0.18,0.45,0.3,
             0.45,0.32,0.48),nrow = 3) 
P

CMT <- new("markovchain",transitionMatrix=P,states=c("Movistar","Orange","Vodafone"),
          name="Telefon�a") 
CMT

#----------------------------------------------------------------------
# Diagrama matriz de transici�n
#----------------------------------------------------------------------

plot(CMT)

#----------------------------------------------------------------------
# Clasificaci�n de estados
#----------------------------------------------------------------------

summary(CMT)

#----------------------------------------------------------------------
# Cuota de mercado desp�es de uno, dos y tres a�os
#----------------------------------------------------------------------

# Las potencias de la matriz proporcionan la situacion de la cadena al cabo de 
# n periodos

X0 <- c(23/(30+25+23),30/(30+25+23),25/(30+25+23)) # La distribucion de X en t = 0
X0
X1 <- X0*(CMT^1)
X1

CMT^2
X2 <- X0*(CMT^2)
X2

CMT^5
X5 <- X0*(CMT^5)
X5

#----------------------------------------------------------------------
# Distribuci�n estacionaria
#----------------------------------------------------------------------

DistEst <- steadyStates(CMT)
DistEst
