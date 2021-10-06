#-----------------------------------------------------------------------
# MDS - Contrastes de bondad de ajuste u no parametricos
# Practica 9
# Hipótesis Aleatoriedad :  Contraste de Wald-Wolfowitz
#----------------------------------------------------------------------

rm(list=ls())

data(swiss)
names(swiss)
help(swiss)
attach(swiss)

# Contraste de Wald-Wolfowitz

library(randtests)

runs.test(Education)
runs.test(Education)$p.value


X <- as.data.frame(swiss)
X

# Contraste Wald-Wolfowitz
sapply(seq(1,6),function(j) runs.test(X[,j]))

