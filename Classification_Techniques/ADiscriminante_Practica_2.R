#practica 2 analisis discriminante

library(dplyr)
library(ggplot2)
# 1)
set.seed(2020)

n <- 1000

x1 <- rnorm(n, 5, 1)
x2 <- rnorm(n, 7, 1)


hist(x1, xlim=c(1,11), col = "red", main = "muestras normales")
hist(x2, add = T, col=rgb(0, 1, 0, 0.5)) #los colores
abline(v=6, lwd = 4) #la linea
box(lwd = 2) #para encerrarlo en una caja

mm <- (mean(x1) - mean(x2))/ 2
mm
# 2)
# Errores

e1 <- sum(ifelse(x1 > mm, 1, 0)) / length(x1); e1*100
e2 <- sum(ifelse(x2 > mm, 1, 0)) / length(x2); e2*100


# Funcion para clasificar

clasificar <- function(z) {
  set.seed(2020)
  ifelse (z > (mean(x1) - mean(x2))/ 2, "Grupo 2", "Grupo 1")
}
clasificar(5.1)
clasificar(8.2)






