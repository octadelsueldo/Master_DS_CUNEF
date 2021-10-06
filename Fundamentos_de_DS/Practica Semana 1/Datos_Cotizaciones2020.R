#----------------------------------------------------------------------
# MDS - Fundamentos 
# An?lisis exploratorio multivariante
# Datos MC 
#----------------------------------------------------------------------

rm(list=ls())
datos<-read.table("Cotizaciones2020.txt",header=T)
names(datos)
attach(datos)
head(datos)
tail(datos)
datos


nrow(datos)
ncol(datos)

# Variable dummy covid

p_break <- 164   #punto de corte
covid <- c(rep(0,p_break),rep(1,nrow(datos)-p_break))
covid
#----------------------------------------------------------------------
# C?lculo de los Rendimientos
#----------------------------------------------------------------------

n <- dim(datos)[1]
n
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1
RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RBBVA  <- BBVA.MC[2:n] / BBVA.MC[1:n-1] - 1
RREP  <- REP.MC[2:n] / REP.MC[1:n-1] - 1
RITX  <- ITX.MC[2:n] / ITX.MC[1:n-1] - 1
RTL5 <- TL5.MC[2:n] / TL5.MC[1:n-1] - 1

covidr <- covid[2:n]
covidr <-as.factor(covidr)

#----------------------------------------------------------------------
# Tiempo continuo y discreto
#----------------------------------------------------------------------
RSAN2  <- log(SAN.MC[2:n] / SAN.MC[1:n-1])

plot(RSAN, RSAN2)

# Contraste formal : lo estudiaremos m?s adelante
t.test(RSAN, RSAN2)

#----------------------------------------------------------------------
# Gr?ficos b?sicos
#----------------------------------------------------------------------
plot(SAN.MC,type = "l")
plot(RSAN,type = "l")


#----------------------------------------------------------------------
# Propiedades Estad?sticas
#----------------------------------------------------------------------
Rendimientos<-cbind(RIBEX, RSAN, RBBVA, RREP, RITX,RTL5)
Rendimientos <- as.data.frame(Rendimientos)
summary(Rendimientos)


#----------------------------------------------------------------------
# Box-Plot
#----------------------------------------------------------------------

boxplot(RIBEX ~ covidr, col="orange")
boxplot(RSAN ~ covidr, col="orange")

# Mediante bucle

par(mfrow=c(2,3))

# Rendimientos
for (j in 1:6) {
  boxplot(Rendimientos[,j],main=colnames(Rendimientos)[j],xlab="",col="orange")
}

# Rendimientos antes y despues covid
for (j in 1:6) {
  boxplot(Rendimientos[,j]~covidr,main=colnames(Rendimientos)[j],ylab="",xlab="",col="orange")
}


# Como vector

sapply(seq(1,6),function(j) boxplot(Rendimientos[,j],main=colnames(Rendimientos)[j], xlab="",ylab="",col="orange"))

sapply(seq(1,6),function(j) boxplot(Rendimientos[,j]~covidr,main=colnames(Rendimientos)[j], xlab="",ylab="",col="orange"))


par(mfrow=c(1,1))

# Histogramas y kernel (selecci?n) - Creamos una matriz 2x2 de gr?ficos
par(mfrow=c(2,2))
hist(RIBEX,prob=T,main="Rendimientos IBEX")
lines(density(RIBEX))
hist(RSAN,prob=T,main="Rendimientos SANTANDER")
lines(density(RSAN))
hist(RREP,prob=T,main="Rendimientos REPSOL")
lines(density(RREP))
hist(RITX,prob=T,main="Rendimientos DIA")
lines(density(RITX))

par(mfrow=c(1,1))

#----------------------------------------------------------------------
# Parejas de Series
#----------------------------------------------------------------------
plot(RIBEX,RSAN)

pairs(Rendimientos, panel = panel.smooth, main = "Rendimientos", col="orange") #smooth calcula la linea de tendencia

#----------------------------------------------------------------------
# Matrices de covarianzas y correlaciones
#----------------------------------------------------------------------
cov(Rendimientos)
cor(Rendimientos)

install.packages("Hmisc")
library(Hmisc)
rh <- rcorr(as.matrix(Rendimientos),type="pearson") #correlacion de los rendimientos tal cual pearson. 
rh
rh$r
rh$P


#-----------------------------------------------
# Representaci?n gr?fica de la matriz de correlaciones
#-----------------------------------------------

install.packages("corrplot")
library(corrplot)
corrplot(rh$r, type = "upper", order="hclust", tl.col="black", tl.srt=45) #si no hay correlacion es azul claro y si es neativa es rojo. 

# No significativos
corrplot(rh$r, type = "upper", order="hclust", p.mat=rh$P, sig.level=0.01, insig = "blank")


#-----------------------------------------------------------------
# Diagrama de Estrellas
#-----------------------------------------------------------------

library(TeachingDemos)
stars(Rendimientos[seq(144,207,1),])


#----------------------------------------------------------------------
# Hip?tesis de Normalidad : Lo veremos m?s adelante 
#----------------------------------------------------------------------
shapiro.test(RIBEX)
shapiro.test(RSAN)

sapply(seq(1,6),function(j) shapiro.test(Rendimientos[,j]))




