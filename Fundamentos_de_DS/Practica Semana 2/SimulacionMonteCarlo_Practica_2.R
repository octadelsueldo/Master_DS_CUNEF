#----------------------------------------------------------------------
# MDS - Simulacion Monte Carlo
# Simulación normales
# Practica 2
#----------------------------------------------------------------------

rm(list=ls())
media<- 3
dt   <- 0.15

#-------------------------------------------------------------  
# P(X > 3.1)
#-------------------------------------------------------------  

# Exacta
1-pnorm(3.1,media,dt) # 0.2524925

# Simulada
nmuestrasim<-10000
set.seed(111)
muestrasim <- rnorm(nmuestrasim, media, dt)
muestrasim; boxplot(muestrasim)

# Simulacion
m1<-sum(muestrasim>3.1)
Prob1 <- m1/nmuestrasim
Prob1

#-------------------------------------------------------------  
# P(X <= 2.8)
#-------------------------------------------------------------  

# Exacta
pnorm(2.8,media,dt) # 0.09121122

# Simulada

m2<-sum(muestrasim<=2.8)
Prob2 <- m2/nmuestrasim
Prob2


#-------------------------------------------------------------  
# P(2.9< X <= 3.25)
#-------------------------------------------------------------  

# Exacta
pnorm(3.25,media,dt)-pnorm(2.9,media,dt) # 0.6997171

# Simulada

m3<-sum(muestrasim<=3.25 & muestrasim>2.9 )
nmuestrasim<-10000
Prob3 <- m3/nmuestrasim
Prob3
