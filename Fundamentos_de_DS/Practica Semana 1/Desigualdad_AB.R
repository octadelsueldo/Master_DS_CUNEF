#------------------------------------------------
# MDS - Fundamentos 
# Medidas de desigualdad
#------------------------------------------------

install.packages("ineq")
library(ineq)

# Datos
A <- c(13, 25, 15, 7, 12, 38, 42, 53, 7)
B <-  c(4, 23, 36, 18, 39, 20, 9, 45, 12)

ineq(A)

ineq(A, type = c("Gini", "RS", "Atkinson", "Theil", "Kolm", "var",
                                   "square.var", "entropy"), na.rm = TRUE)

ineq(A, parameter=0.5, type="Atkinson")
ineq(A, parameter=2.5, type="Atkinson")


ineq(A, parameter=1, type="entropy")
ineq(A, parameter=2, type="entropy")

#------------------------------------------------
# Medidas basadas en cuantiles
#------------------------------------------------

m1A <- quantile(A,0.9)/quantile(A,0.1)
m2A <- quantile(A,0.9)/quantile(A,0.5)
m3A <- quantile(A,0.5)/quantile(A,0.1)

m1B <- quantile(B,0.9)/quantile(B,0.1)
m2B <- quantile(B,0.9)/quantile(B,0.5)
m3B <- quantile(B,0.5)/quantile(B,0.1)

mA<-c(m1A,m2A,m3A)
mA
mB<-c(m1B,m2B,m3B)
mB

cat <- c("x(90)/x(0.1)","x(90)/x(0.5)","x(50)/x(0.1)")

juntar <- rbind(mA, mB)
barplot(juntar, names.arg = cat, col = c(2,3), beside=T,
        main = "Desigualdad entre A y B basada en cuantiles")
box(col="black")

#------------------------------------------------
# Curvas de Lorenz e Indices de Gini
#------------------------------------------------

CLorenzA <- Lc(A)
CLorenzA
CLorenzB <- Lc(B)
CLorenzB

plot(Lc(A))
lines(Lc(B), col="green")


