#-----------------------------------------------------------------------
# MDS - Contrastes de bondad de ajuste
# Practica 1
# Test de la chi-cuadrado de Pearson
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Probabilidades teoricas y frecuencias
#-----------------------------------------------------------------------
rm(list=ls())

pteorico <- c(0.45, 0.35, 0.15, 0.05)

frecuencia <- c(49, 27, 14, 10)

barplot(pteorico,names.arg =c("bajo", "medio", "alto", "muy alto")) #diagrama de barras. Los argumentos se pueden hacer con el names.arg

t2 <- rbind(pteorico*100, frecuencia)
barplot(t2,names.arg =c("bajo", "medio", "alto", "muy alto"))

#-----------------------------------------------------------------------
# Test chi-cuadrado
#-----------------------------------------------------------------------

testchi2 <- chisq.test(frecuencia , p = pteorico)  #frecuencia es ka frecuencia de los datos

testchi2

#como no es menor al 5%, no se rechaza la hipotesis nula

testchi2$expected  #estos son los valores esperados suponiendo que la hipotesis es cierta

chisq.test(frecuencia , p = pteorico, simulate.p.value = TRUE) #una variante
