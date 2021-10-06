
rm(list=ls())
datos<-read.table("Cotizaciones2020.txt",header=T)
names(datos)
attach(datos)
head(datos)




# Calculo de los Rendimientos

n <- dim(datos)[1]
n
RIBEX <- IBEX[2:n]/IBEX[1:n-1] - 1
RSAN  <- SAN.MC[2:n] / SAN.MC[1:n-1] - 1
RBBVA  <- BBVA.MC[2:n] / BBVA.MC[1:n-1] - 1
RREP  <- REP.MC[2:n] / REP.MC[1:n-1] - 1
RITX  <- ITX.MC[2:n] / ITX.MC[1:n-1] - 1
RTL5 <- TL5.MC[2:n] / TL5.MC[1:n-1] - 1

# Variable RSAN en 2 categorias

summary(RSAN)

RSANBIN <- cut(RSAN, breaks=c(-Inf, 0, Inf), 
               labels=c("low", "high"))

RSANBIN <- ifelse(RSANBIN == "low", 0, 1)

table(RSANBIN)

datos1<-cbind(RSANBIN, RSAN, RIBEX, RBBVA, RREP, RITX, RTL5)
datos1<-as.data.frame(datos1)
summary(datos1)


model <- glm(RSANBIN ~ RIBEX+ RBBVA + RREP + RITX + RTL5, data=datos1, family = binomial(link = logit)) 
summary(model)


#2. Bondad de ajuste LR test: contraste GLOBAL


#ajustamos un modelo donde el model tiene unicamente el termino independiente. Lo que se hace es poner menos 1 al final.
model_0 <- glm(RSANBIN ~ 1, data=datos1, family = binomial(link = logit))
anova(model_0, model,test="Chisq")

model <- glm(RSANBIN ~ RIBEX+ RBBVA + RREP + RITX + RTL5, data=datos1, family = binomial(link = probit)) 
summary(model)
