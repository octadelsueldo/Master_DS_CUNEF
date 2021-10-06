#----------------------------------------------------------------------
# MDS - Fundamentos probabilidad
# Practica 4
# Teorema de Bayes Clasificacion
#----------------------------------------------------------------------

# probabilidad a priori
pCi <- c(15, 15, 21, 27, 22)/100    #porcentajes de fabicacion
sum(pCi)


# verosimilitudes 
pD_Ci <- c(1, 0.5, 1.5, 0.7, 1.2)/100         #porcentajes de defectuosos


# Prob. defectuosa total

pD <- pCi %*% pD_Ci                #probabilidad de defectuosos


# Probabilidades posteriores Bayes

pCi_D <- (pCi * pD_Ci) /as.numeric(pD)
round(pCi_D*100,2)
sum(pCi_D)

# Identifico la linea de produccion mas probable
which.max(pCi_D)              #esto calcula el indice y me dice el maximo que debo elegir 
