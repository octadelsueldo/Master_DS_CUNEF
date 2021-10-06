#-------------------------------------------------------
# MDS - Fundamentos de Algebra
# Calculo matricial con carteras
#-------------------------------------------------------

# Rendimientos medios y volatilidades
s1 <- 0.12
s2 <- 0.18
r12 <- 0.45

m1 <- 0.05
m2 <-0.08

# matriz de covarianzas
V <- matrix(c(s1^2, r12*s1*s2, r12*s1*s2, s2^2),2,2)
V

# rendientos medios
m <- c(m1, m2)

# Peso cartera
w <- c(2/5,3/5)

# Rendimiento medio cartera
ER <- t(w) %*% m
ER

# Varianza de la cartera
VR <- t(w) %*% V %*% w
VR
sqrt(VR)*100
