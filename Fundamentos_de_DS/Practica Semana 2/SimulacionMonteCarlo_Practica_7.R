#----------------------------------------------------------------------
# MDS - Simulacion
# Practica 7
# Simulación de la distribución de la suma de 3 variables LogNormales
#----------------------------------------------------------------------

rm(list=ls())

n<- 1000
set.seed(2020)
sumalog <- numeric(n)
for(i in 1:n){
  sumalog[i] <- 
    rlnorm(1, meanlog = 0, sdlog = 1)+
    rlnorm(1, meanlog = 0, sdlog = 2)+
    rlnorm(1, meanlog = 0, sdlog = 3)
}

# Características de la distribución de la Suma
sumalog
summary(sumalog)

qqnorm(sumalog,col="blue",lwd=1)
qqline(sumalog)

# QQ plot del logaritmo de la distribución de la Suma
qqnorm(log(sumalog),col="blue",lwd=1)
qqline(log(sumalog))


# Histograma y kernel de la distribución de la Suma
hist(sumalog,probability = T)
lines(density(sumalog)) # no funciona

# Características del logaritmo de la distribución de la Suma
hist(log(sumalog),probability = TRUE)
lines(density(log(sumalog))) 
box(col="black")

mean(sumalog)
t.test(sumalog)$conf.int
sd(sumalog)
shapiro.test(sumalog)
shapiro.test(log(sumalog))


