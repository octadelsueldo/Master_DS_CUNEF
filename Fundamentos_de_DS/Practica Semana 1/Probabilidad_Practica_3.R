#-------------------------------------------------------
# MDS - Fundamentos de probabilidad
# Momentos de una variable discreta
# Practica 3
#-------------------------------------------------------

# Distribucion de la variable

x <- c(-2,-1,0,1,2) # seq(-2,2,1)
p <- c(0.15, 0.25, 0.3, 0.2, 0.1)

sum(p) # distribucion genuina

# Media y varianza
mx <- sum(x*p)
mx
varx <- sum((x-mx)^2*p)
varx
sum(x^2*p)-mx^2 # Otra forma

# Coeficientes de asimetria y curtosis
g1 <- sum((x-mx)^3*p)/varx^(3/2)
g2 <- sum((x-mx)^4*p)/varx^(2)-3

c(mx,sqrt(varx),g1,g2)
