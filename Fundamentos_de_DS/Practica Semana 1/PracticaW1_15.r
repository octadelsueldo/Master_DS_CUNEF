data("mtcars")
names(mtcars)
head(mtcars)

library(dplyr)
data <- mtcars %>% select(c(1,3,4,5,6,7))

covarianza <- cov(data)
correlacion <- cor(data)

library(Hmisc)
#rh <- rcorr(as.matrix(data,type="pearson") #correlacion de los rendimientos tal cual pearson. 
#rh
#rh$r
#rh$P


#-----------------------------------------------
# Representaci?n gr?fica de la matriz de correlaciones
#-----------------------------------------------

#install.packages("corrplot")
library(corrplot)
corrplot(correlacion, type = "upper", order="hclust", tl.col="black", tl.srt=45) #si no hay correlacion es azul claro y si es neativa es rojo. 

# Autovalores y autovectores de la matriz de covarianzas
eigen(covarianza) 
eigen(covarianza)$values
sum(eigen(covarianza)$values) # Traza de S
det(covarianza)

# Autovalores y autovectores de la matriz de covarianzas
eigen(correlacion) 
eigen(correlacion)$values
sum(eigen(correlacion)$values) # Traza de S
det(correlacion)

'''
Significancia del coeficiente de correlación (r)
Se trata de probar la hipótesis de que r≠0, es decir, buscamos rechazar la H0:r=0
.
Tenemos dos maneras de retar la H0
: i) usando z−scores
; ii) mediante estadístico t
'''


#>>> Cálculo automático con la función de R cor.test()
(cor.test(data,data))
# Method 1: Formula notation
##  Use if x and y are in a dataframe
attach(data)
cor.test(formula = ~ mpg + disp + hp + drat + wt + qsec,
         data = data)
