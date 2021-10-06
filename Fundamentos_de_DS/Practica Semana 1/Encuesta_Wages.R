#-----------------------------------------------------
# MDS - Fundamentos 
# ENCUESTA Wage: An?lisis gr?fico y num?rico
#-----------------------------------------------------

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
by(wage, education, mean) #IMPORTANTE
wage_education <- by(wage, education,summary)
wage_education
by(wage, maritl,mean)

# De entre los que m?s ganan, ?A que grupo pertenecen?

wage_maritl <- by(wage, maritl,summary) #interpretacion por cuartiles
wage_maritl


wage_maritl <- by(wage, maritl,fivenum)
wage_maritl

#-----------------------------------------------------
# Variables categ?ricas y tablas de contingencia
#-----------------------------------------------------

# Una sola variable categ?rica
table(race) #tabula la variable
barplot(table(race))

# Dos variables categ?ricas
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
# Gr?ficos de una variable seg?n niveles de la segunda
#---------------------------------------------
library(lattice)

histogram(wage, data = Wage) #se usa histogram distinto a hist que usaba anteriormente. REVISAR PAQUETE. Los histograma sirven solo para variables cuantitativas y el barplot para categoricas

histogram( ~ wage | education, data = Wage)

par(mfrow=c(1, 1))

# Gr?ficos bidimensionales

plot(age,wage)

plot(age,wage, col = 2, pch = 20)


#---------------------------------------------
# Gr?ficos de dos variables seg?n niveles de una tercera
#---------------------------------------------

# Dos variables
xyplot(wage ~ age, data = Wage, type=c("p","smooth"), col=3,  pch = 20)

# Dos variables seg?n niveles de una tercera, incluyendo tendencia
xyplot(wage ~ age | health, data = Wage, type=c("p","smooth"), col=3,  pch = 20)

xyplot(wage ~ age | race, data = Wage, type=c("p","smooth"), col=3,  pch = 20)

xyplot(wage ~ age | maritl, data = Wage, type=c("p","smooth"), col=3,  pch = 20)



#---------------------------------------------
# Otra forma de seleccionar variables y promediar
#---------------------------------------------

install.packages("dplyr")                       
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

