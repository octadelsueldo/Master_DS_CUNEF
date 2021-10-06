#Ejercicio 4 de Probabilidad


# Probabilidad a priori
porcentajes <- c(15, 15, 21, 27, 22)/100
sum(porcentajes)


# verosimilitudes 
porcentaje_noapta <- c(1, 0.5, 1.5, 0.7, 1.2)/100


# Prob. defectuosa total

probabilidad_posterior <- porcentajes %*% porcen_noapta


# Probabilidades posteriores Bayes

porcen_noapta <- (porcentajes * porcen_noapta) /as.numeric(pD)
round(porcen_noapta*100,2)
sum(porcen_noapta)

# Identifico la linea de produccion mas probable
which.max(porcen_noapta)