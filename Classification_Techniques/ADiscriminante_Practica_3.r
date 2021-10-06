library("rattle.data")
library(leaps)
library(ggplot2)
library(reshape2)
library(MASS)
library(ggcorrplot)
library(plotmo)
data(wine)

dim(wine)

sapply(wine, class)

head(wine)

summary(wine)

# Correlations
corrplot(cor(wine %>% 
               select_at(vars(-Type)), 
             use = "complete.obs"), 
         method = "circle",type = "upper")


# Other Correlations

chart.Correlation(wine %>% 
                    select_at(vars(-Type)),
                  histogram=TRUE, pch=19)

attach(wine)
# Box-Plot
#-----------------------------------------------------
par(mfrow=c(2,2))
boxplot(Alcohol~Alcalinity,col="orange")
boxplot(Phenols~Alcalinity,col="orange")
boxplot(Alcohol~Magnesium,col="orange")
boxplot(Phenols~Magnesium,col="orange")
par(mfrow=c(1,1))

#-----------------------------------------------------
# LDA
#-----------------------------------------------------
library(MASS)

# LDA analisis discriminante lineal
# Elegimos a  priori el tama?o de cada grupo
ldamod <- lda(Type ~ ., data = wine)
ldamod

#primero aparecen las proba a priori por grupo, luego las medidas de las dos variables dentro de cada grupo (centroides),
#luego las funciones lineales de fisher y luego 

# Probabilidades posteriores
predict(ldamod)$posterior 
#obtenemos las probailidades posteriores de cada elemento. tenemos que tener 3 estimaciones de la probabilidad y la asignaremos al que tenga la maxima
#para el primer individuo por ej lo asignariamos al primer grupo

# Prediccion respuesta
ldaResult <- predict(ldamod, newdata = wine) 
#hacemos la prediccion de toda la muestra y con esa tabla de resultados creamos la matriz de confusion

# Matriz de confusion
tldamod<-table(ldaResult$class, wine$Type) #los datos los dividimos en categorias 
tldamod
# Precision
sum(diag(tldamod))/sum(tldamod) # acierto (100 %)


#-----------------------------------------------------
# Hipotesis de LDA
#-----------------------------------------------------

#-----------------------------------------------------
# (a) Contraste de igualdad del vector de medias                  
#-----------------------------------------------------

#tenemos que corrobar que las medias de los centroides seran iguales
library(MASS)
require(rrcov)
Wilks.test(wine[,2:14],grouping=wine[,1], method="c") #test de wilks sigue para comparar. La variable 1 es Type

#en este caso la conclusion se puede rechazar la hipotesis nula. Es decir sus medias son distintas porq su p valor es menor al 5%

#-----------------------------------------------------
# (b) Contraste de igualdad de matrices de covarianzas                  
#-----------------------------------------------------
#install.packages("biotools")
library(biotools)
boxM(data=wine[,2:14], grouping=wine[,1]) #la hipotesis nula es que las matrices de covarianzas de esas dos variables por grupos son las mismas.

#no se rechaza la hipotesis nula. Es decir puede que las matrices de covarianzas de esas dos variables por grupos pueden ser las mismas
# las matrices de covarianzas de esos tres grupos no son distintas. Es decir, no se puede usar la QDA. Si fueran distintas se pueden elegir el LDA y el QDA
#-----------------------------------------------------
# (c) Contraste de normalidad multivariante	                          
#-----------------------------------------------------
library(MVN)

# Por grupos
grupo1 <- subset(wine, categ  %in% c("1")); grupo1 #elegimos el subconjunto del primer, segundo y tercer grupo
grupo2 <- subset(wine, categ  %in% c("2")); grupo2
grupo3 <- subset(wine, categ  %in% c("3")); grupo3
#
mvn(data =grupo1[,2:14],mvnTest="mardia")$multivariateNormality #comparamos la hipotesis de la normalidad con mardia para las dos variables que utilizamos y con multivariadnormal le decimos que nos incluya la ifnormacion que queremos
mvn(data =grupo2[,2:14],mvnTest="mardia")$multivariateNormality
mvn(data =grupo3[,2:14],mvnTest="mardia")$multivariateNormality
#
mvn(data =grupo1[,2:14],mvnTest="hz")$multivariateNormality
mvn(data =grupo2[,2:14],mvnTest="hz")$multivariateNormality
mvn(data =grupo3[,2:14],mvnTest="hz")$multivariateNormality
#
mvn(data =grupo1[,2:14],mvnTest="royston")$multivariateNormality
mvn(data =grupo2[,2:14],mvnTest="royston")$multivariateNormality
mvn(data =grupo3[,2:14],mvnTest="royston")$multivariateNormality

# como todos los test dicen que etsa ok hay unanimidad de que es normal

# Veamos globalmente 
mvn(data =wine[,2:14],mvnTest="mardia")$multivariateNormality
mvn(data =wine[,2:14],mvnTest="hz")$multivariateNormality
mvn(data =wine[,2:14],mvnTest="royston")$multivariateNormality

#--------------------------------------------------
# (d) Normalidad variables por categorias graficamente
#--------------------------------------------------

par(mar=c(1,1,1,1))
for(j in c(2:14)){
  j0 <- names(wine)[j]
  x0<-seq(min(wine[,j]), max(wine[,j]), le=50)
  for(i in 1:3){
    i0 <- levels(Type)[i]
    x <- wine[Type == i0, j0]
    qqnorm(x, main=paste("Categoria", i0, j0), pch=19, col=i+1)
    qqline(x)
  }
}
par(mfrow=c(1,1))
#--------------------------------------------------

#--------------------------------------------------
# (d) p-valores variables por niveles categ por S-W
#--------------------------------------------------

# Por variable-niveles *****
for (j in c(2,3)) {
  for (i in c(1,2,3)) {
    print(shapiro.test(wine[wine$Type==i,j])$p.value)
  }
}


# QDA analisis discriminante cuadratico
#----------------------------------------------------------------------
# Contraste de Homogeneidad de varianza
#----------------------------------------------------------------------
library(biotools)
boxM(data=wine[,c(2:14)], grouping=wine[,1])


# Elegimos a  priori el tama?o de cada grupo

qdamod <- qda(Type ~ ., data = wine)  #aqui hacemos lo miso solo que elegimos qda pero el resto de comandos coinciden
qdamod
# Prediccion respuesta
qdaResult <- predict(qdamod, newdata = wine) 
# Matriz de confusion
tqdamod<-table(qdaResult$class, wine$Type) 
tqdamod
sum(diag(tqdamod))/sum(tqdamod) # acierto (99.4 %)


# Logistico
reg_log <- glm(Type ~ ., data=wine, family = binomial(link = logit))
summary(reg_log)

# Por pasos
library(MASS)
modelo2 <- stepAIC(reg_log)
summary(modelo2)

# Prediccion respuesta
log_pred <- predict(reg_log, newdata = wine) 

# Matriz de confusion
#regmod<-table(log_pred$class, wine$Type) 
#regmod
#sum(diag(regmod))/sum(regmod) # acierto (99.4 %)

