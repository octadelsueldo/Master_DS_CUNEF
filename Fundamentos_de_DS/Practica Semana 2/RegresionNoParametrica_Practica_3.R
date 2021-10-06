#----------------------------------------------------------------------
# Master DSF
# Práctica Regresión No Paramétrica
# Práctica 1
#----------------------------------------------------------------------

rm(list=ls())

data(cars)
attach(cars)
help(cars)

# Variables

plot(speed, dist)

# Regresión no paramétrica kernel normal y varios bandwidth

for (b in c(2,3,5)) {
  lines(ksmooth(speed, dist, "normal", bandwidth = b),col=b)  
}

# Regresión no paramétrica kernel rectangular y varios bandwidth

plot(speed, dist)
for (b in c(2,3,5)) {
  lines(ksmooth(speed, dist, "box", bandwidth = b),col=b)  
}




      