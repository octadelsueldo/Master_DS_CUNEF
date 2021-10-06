#install.packages("NHANES")
library(NHANES)
data(NHANES)
head(NHANES)
names(NHANES)
nrow(NHANES)

library(lattice)
histogram( ~ Length + Weight | Gender, data = NHANES)


# Dos variables segun niveles de una tercera, incluyendo tendencia
xyplot(Age ~ Length + Weight| Gender, data = NHANES, type=c("p","smooth"), col=3,  pch = 20)  #plot de wage segun la edad dependiendo de la saluda de los individuos
xyplot(Age ~ Length + Weight| Race3, data = NHANES, type=c("p","smooth"), col=3,  pch = 20) 


library("dplyr")

NHANES %>%                                        # Especifica data frame
  group_by(Race3) %>%                       # Especifica grupo 
  summarise_at(vars(c(Weight, Length, Age)),                      # Especifica columna
               list(name = mean))

