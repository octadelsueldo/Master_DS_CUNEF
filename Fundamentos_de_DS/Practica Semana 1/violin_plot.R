#-----------------------------------------------------
# MDS - Fundamentos
# Violin plots
#-----------------------------------------------------

rm(list=ls())
install.packages("vioplot")

library(vioplot)
data("iris")
head(iris)
attach(iris)
names(iris)

# Comparación de los dos gráficos

par(mfrow=c(2,1))
boxplot(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,col="orange")
vioplot(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width,col="orange")
par(mfrow=c(1,1))

# Una variable respecto un factor

vioplot(Petal.Length ~ Species,main="Iris petal length",
        names=c("setosa","versicolor","virginica"),
        xlab="Species", ylab="petal Length in centimetres",col="green")

# Otra forma

x <- iris[,"Petal.Length"]
x

vioplot(x ~ iris$Species,main="Iris petal length",
        names=c("setosa","versicolor","virginica"),
        xlab="Species", ylab="petal Length in centimetres",col="green")


# box plot y violin-plot en variables bimodales

mu<-2
si<-0.6

bimodal<-c(rnorm(1000,-mu,si),rnorm(1000,mu,si))
hist(bimodal)

uniforme<-runif(2000,-4,4); hist(uniforme)

par(mfrow=c(2,1))
normal<-rnorm(2000,0,3)
vioplot(bimodal,uniforme,normal,col="blue")
boxplot(bimodal,uniforme,normal)
par(mfrow=c(1,1))

