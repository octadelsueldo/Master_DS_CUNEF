library("DAAG")
data(spam7)
head(spam7)
names(spam7)
nrow(spam7)
dim(spam7)

### 1.  Realizar un analisis descriptivo de las variables explicativas




library(tidyverse)
library(skimr)
library(corrplot)
library(PerformanceAnalytics)
skim(spam7)



# Correlations
corrplot(cor(spam7 %>% 
               select_at(vars(-yesno)), 
             use = "complete.obs"), 
         method = "circle",type = "upper")


# Other Correlations

chart.Correlation(spam7 %>% 
                    select_at(vars(-yesno)),
                  histogram=TRUE, pch=19)




### 2. Comparar los modelos LR, LDA y QDA mediante la matriz de confusion. Realizar los graficos de particion

#### Modelos Regresion Logistica


library(magrittr)
str(spam7) #me fijo como quedan las variables para chequear

head(spam7$yesno)

model1 <- glm(yesno ~ ., data=spam7, family = binomial(link = logit))
summary(model1)

# Segun la regresion logistica las variables significas son crl.tot, dollar, bang, money y n000. Por lo tanto deja afuera el make.


#### LDA


library(MASS)

# LDA analisis discriminante lineal
# Elegimos a  priori el tamanho de cada grupo
ldamod <- lda(yesno ~ ., data = spam7)
ldamod

# Prediccion respuesta
ldaResult <- predict(ldamod, newdata = spam7) 
# Matriz de confusion
tldamod<-table(ldaResult$class, spam7$yesno)
tldamod
# Precision
sum(diag(tldamod))/sum(tldamod) # acierto  76%




# Partition Plots

library(klaR)
spam7$yesno <- as.factor(spam7$yesno)
partimat(spam7[,c(1,2,3,4,5,6)], spam7$yesno, data = spam7, method = "lda", main = "Partition Plots") 



#### Contraste de Homogeneidad de varianza




library(biotools)
boxM(data=spam7[,c(1,2,3,4,5,6)], grouping=spam7[,7])


#### QDA analisis discriminante cuadratico


# Elegimos a  priori el tamanho de cada grupo
qdamod <- qda(yesno ~ ., data = spam7) 
qdamod
# Prediccion respuesta
qdaResult <- predict(qdamod, newdata = spam7) 
# Matriz de confusion
tqdamod<-table(qdaResult$class, spam7$yesno) 
tqdamod
sum(diag(tqdamod))/sum(tqdamod) # acierto (76 %)



# 
# Partition Plots


library(klaR)
spam7$yesno <-as.factor(spam7$yesno)
partimat(spam7[,c(1,2,3,4,5,6)], spam7$yesno, data=spam7, method="qda",main="Partition Plots")



dbinom(8, 12, 0.5)
dbinom(8, 10, 0.5)
dbinom(8, 25, 0.33)


# Modelo Naive Bayes
#----------------------------------------------------------------------
modelo1 <- naiveBayes(formula= yesno ~ ., data = spam7) #modelo
modelo1
prediccion1 <- predict(modelo1, newdata=spam7, type = "raw")#para predecir en base a naiveBaye. Raw nos saca las probab si ponemos class nos saca la prediccion del genero
head(prediccion1)

prediccion11 <- predict(modelo1, newdata=spam7, type = "class")

matrizconfusion <- table(spam7$yesno, prediccion11)
matrizconfusion

# Porcentaje de aciertos
sum(diag(matrizconfusion))/sum(matrizconfusion) #80%
