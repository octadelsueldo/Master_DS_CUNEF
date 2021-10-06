#-----------------------------------------------------------------------
# MDS - Contrastes de bondad del ajuste
# Practica 7
# Tablas de contingencia 
#-----------------------------------------------------------------------

rm(list=ls())

library(ISLR)
data("Wage")
help(Wage)
variable.names(Wage)
str(Wage)
attach(Wage)
head(Wage)

#-----------------------------------------------------
# Tabular una variable categorica 
#-----------------------------------------------------

t0<-table(race)   
t0
sum(t0)
t0/sum(t0)
barplot(table(race))

#-----------------------------------------------------
# Tabla de contingencia. Dos variables categoricas
# race y education
# Contraste de independencia
#-----------------------------------------------------

t1<-table(race,education)  #table tiene que reconocer que son variables categoricas
t1
sum(t1)

margin.table(t1, 1) # Obtiene el total Filas. Pone uno para refererise a las filas
margin.table(t1, 2) # Columnas. Pone 2 para referirse a las columnas
addmargins(t1, c(1, 2)) # Totales Filas y Columnas

prop.table(t1,1) 
prop.table(t1,2) 

chisq.test(t1) # Se rechaza la hipotesis de independencia

#se rechaza la hip nula entonces hay una fuerte dependencia entre la variable raza y la educacion

chisq.test(t1)$expected  #esta tendria que salir en caso de que fueran independientes

t1-chisq.test(t1)$expected  #aca muestra donde hay mas diferencia
max(t1-chisq.test(t1)$expected)  #aca sirve para reconocer donde hay mas discrepancia en el caso de independencia
which.max(t1-chisq.test(t1)$expected)

#-----------------------------------------------------
# Tabla de contingencia health education
#-----------------------------------------------------

t2<-table(health,education)
t2

chisq.test(t2) # Se rechaza la hipotesis de independencia

t2-chisq.test(t2)$expected
max(t2-chisq.test(t2)$expected)



#-----------------------------------------------------
# Tabla de contingencia race education
#-----------------------------------------------------


t3<-table(race,education)
t3

chisq.test(t3) # Se rechaza la hipotesis de independencia

t3-chisq.test(t3)$expected
max(t3-chisq.test(t3)$expected)

