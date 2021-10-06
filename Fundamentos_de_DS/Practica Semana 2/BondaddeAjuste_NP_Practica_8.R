#----------------------------------------------------------------------
# MDS - Contraste de Homogeneidad en Tablas de contingencia
# Practica 8
#----------------------------------------------------------------------

fumar <- matrix(c(14, 17, 18, 16, 88, 83, 82, 80),4,2)
fumar
colnames(fumar) <- c("Fumadores", "No Fumadores")
rownames(fumar) <- c("F1", "F2", "F3", "F4")
fumar

#--------------------------------------------------
# Contraste chi cuadrado
#--------------------------------------------------

chisq.test(fumar)

chisq.test(fumar)$expected

chisq.test(fumar)$statistic


library(MASS)
fisher.test(fumar)

