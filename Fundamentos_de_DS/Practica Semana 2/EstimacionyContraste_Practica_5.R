#--------------------------------------------------------------
# MDS - Fundamentos 
# Práctica t-test para muestras segújn variable categórica
# Practica 5
#--------------------------------------------------------------

data("mtcars")
names(mtcars)
help(mtcars)
mtcars
attach(mtcars)

#--------------------------------------------------------------
# Contraste gráfico y formal de normalidad de mpg según trasmisión
#--------------------------------------------------------------

# Contraste gráfico y formal de normalidad de mpg

qqnorm(mpg)
qqline(mpg)
shapiro.test(mpg)

# Contraste gráfico y formal de normalidad de mpg según trasmisión

boxplot(mpg ~ am) # boxplot de mpg segun trasmision

# Contraste gráficos de normalidad

par(mfrow=c(1,2))
qqnorm(mpg[am==0]); qqline(mpg[am==0])
qqnorm(mpg[am==1]); qqline(mpg[am==1])
par(mfrow=c(1,1))

# Contraste formales de normalidad

shapiro.test(mpg[am==0])
shapiro.test(mpg[am==1])


#--------------------------------------------------------------
# Intervalo de confianza y contraste para diferencia de medias de mpg según trasmisión
#--------------------------------------------------------------

t.test(mpg ~ am)

#--------------------------------------------------------------
# Intervalo de confianza y contraste para cociente de varianzas
#--------------------------------------------------------------

var.test(mpg ~ am)

#--------------------------------------------------------------
# Intervalo de confianza para diferencia de medias con igualdad de varianzas
# Se trata de intervalo más preciso
#--------------------------------------------------------------

t.test(mpg ~ am,var.equal=T,conf.level=0.95) 
