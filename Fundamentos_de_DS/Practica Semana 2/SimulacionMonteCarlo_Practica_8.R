#----------------------------------------------------------------------
# MDS - Simulacion Montse Carlo
# Simular una densidad normal bivariada desde X e Y|X
# Practica 8
#----------------------------------------------------------------------

# Funcion rbvn

rbvn<-function (n, rho) 
{
  x <- rnorm(n, 0, 1)
  y <- rnorm(n, rho * x, sqrt(1 - rho^2))
  cbind(x, y)
}

bvn<-rbvn(10000,0.98)

# plot(bvn)

par(mfrow=c(3,2))
plot(bvn,col=1:100)
plot(bvn,type="l")
plot(ts(bvn[,1]))
plot(ts(bvn[,2]))
hist(bvn[,1],40) # MARGINAL 1
hist(bvn[,2],40) # MARGINAL 2
par(mfrow=c(1,1))

