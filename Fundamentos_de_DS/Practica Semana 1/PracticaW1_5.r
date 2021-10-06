#Haciendo el ejercicio 5 de Analisis Exploratorio de datos

#Haciendo uso de paquete simFrame, la base de datos eusilcP y la variable: eqIncome:
 # (i) Representar graficamente la variable anterior. Obtener medidas de desigualdad basadas en cocientes de cuantiles.
#(ii) Haciendo uso del paquete ineq, calcular el  ́ındice de Gini y los  ́ındices de desigualdad de Atkinson y entrop ́ıa generalizada segun valores del parametro.
#(iii) Considerar una nueva variable prescindiendo de valores por encima de 100.000. Calcular  ındice de Gini.



#install.packages("simFrame")
library(simFrame)
data(eusilcP)
help("eusilcP")

eusilcP

hist(eusilcP$eqIncome, xlim = c(0,100000))

library(ineq)

#------------------------------------------------
# (i)Medidas basadas en cuantiles
#------------------------------------------------
eqIncome <- eusilcP$eqIncome
m1eqIncome <- quantile(eqIncome,0.9)/quantile(eqIncome,0.1)
m2eqIncome <- quantile(eqIncome,0.9)/quantile(eqIncome,0.5)
m3eqIncome <- quantile(eqIncome,0.5)/quantile(eqIncome,0.1)


meqIncome<-c(m1eqIncome,m2eqIncome,m3eqIncome)
meqIncome

#(ii)

ineq(eqIncome)

ineq(eqIncome, type = c("Gini", "RS", "Atkinson", "Theil", "Kolm", "var",
                 "square.var", "entropy"), na.rm = TRUE)

ineq(eqIncome, parameter=0.5, type="Atkinson")
ineq(eqIncome, parameter=2.5, type="Atkinson")


ineq(eqIncome, parameter=1, type="entropy")
ineq(eqIncome, parameter=2, type="entropy")
