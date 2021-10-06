#-----------------------------------------------------
# MDS - Fundamentos 
# ENCUESTA Wage: Analisis grafico y numerico
#-----------------------------------------------------
#La base de datos Wage (se encuentra en el paquete ISLR) ofrece informacion sobre un grupo de 3000 trabajadores hombres en una determinada region
#(i) Representar la variable wage segun los niveles de educacion, raza, salud y estado civil mediante box plot. Obtener el salario medio segun los niveles de las variables categoricas.
#(ii) Estudiar de forma aislada alguna variable categorica. Construir una tabla de contingencia de dos variables categoricas
#(iii) Representar el salario wage en funcion de la edad, y a continuacion obtener el mismo grafico segun los niveles de la raza, salud y estado civil.


#DESARROLLO
#(i)

rm(list=ls())

library(ISLR)
data("Wage")
help(Wage)
variable.names(Wage)
str(Wage)
attach(Wage)
head(Wage)

summary(Wage)
mean(wage)
sd(wage)

#-----------------------------------------------------
# Boxplot wage por niveles de un factor
#-----------------------------------------------------
boxplot(wage)
par(mfrow=c(2, 2)) 
boxplot(wage ~ education, col=4) #bloxplot segun variables categorias
boxplot(wage ~ race, col=3)
boxplot(wage ~ health,col=5)
boxplot(wage ~ maritl,col=2)
par(mfrow=c(1,1))


#-----------------------------------------------------
# Media de la variable wage por niveles de un factor
#-----------------------------------------------------

mean(wage)
by(wage, education, mean) #IMPORTANTE Calcula la media de wage para cada uno de los niveles de educacion
wage_education <- by(wage, education,summary) #calcula el summary de wage para cada uno de los niveles de educacion
wage_education
by(wage, maritl,mean) #calcula la media de wage para cada uno de los estados civiles posibles

# De entre los que mas ganan, ?A que grupo pertenecen?

wage_maritl <- by(wage, maritl,summary) #interpretacion por cuartiles
wage_maritl


wage_maritl <- by(wage, maritl,fivenum)
wage_maritl

#-----------------------------------------------------
# (ii)Variables categoricas y tablas de contingencia
#-----------------------------------------------------

# Una sola variable categorica
table(race) #tabula la variable
barplot(table(race))

# Dos variables categoricas
t1<-table(race,education) #cruza dos variables
t1
t2<-table(health,education)
t2
t3<-table(race,education)
t3

t3-chisq.test(t3)$expected
max(t3-chisq.test(t3)$expected)
which.max(t3-chisq.test(t3)$expected)

#---------------------------------------------
#(iii) Graficos de una variable segun niveles de la segunda
#---------------------------------------------
library(lattice)

histogram(wage, data = Wage) #se usa histogram distinto a hist que usaba anteriormente. REVISAR PAQUETE. Los histograma sirven solo para variables cuantitativas y el barplot para categoricas

histogram( ~ wage | education, data = Wage) #calcula el histograma de wage para cada uno de los niveles de educacion

par(mfrow=c(1, 1))

# Graficos bidimensionales

plot(age,wage)  #plot de wage segun la edad

plot(age,wage, col = 2, pch = 20)


#---------------------------------------------
# Graficos de dos variables segun niveles de una tercera
#---------------------------------------------

# Dos variables
xyplot(wage ~ age, data = Wage, type=c("p","smooth"), col=3,  pch = 20)

# Dos variables segun niveles de una tercera, incluyendo tendencia
xyplot(wage ~ age | health, data = Wage, type=c("p","smooth"), col=3,  pch = 20)  #plot de wage segun la edad dependiendo de la saluda de los individuos

xyplot(wage ~ age | race, data = Wage, type=c("p","smooth"), col=3,  pch = 20)

xyplot(wage ~ age | maritl, data = Wage, type=c("p","smooth"), col=3,  pch = 20)



#---------------------------------------------
# Otra forma de seleccionar variables y promediar
#---------------------------------------------

#install.packages("dplyr")                       
library("dplyr")

Wage %>%                                        # Especifica data frame
  group_by(education) %>%                       # Especifica grupo 
  summarise_at(vars(wage),                      # Especifica columna
               list(name = mean))   


Wage %>%                                        # Especifica data frame
  group_by(race) %>%                            # Especifica grupo
  summarise_at(vars(wage),                      # Especifica columna
               list(name = mean)) 


#---------------------------------------------

