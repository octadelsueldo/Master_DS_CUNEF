#Analisis Exploratorio de datos

#Ejercicio 1.2

rm(list = ls())   #borra la informacion almacenada en el global environment
data(swiss)  #traigo los datos de la tabla swiss almacenada en memoria
names(swiss)  #traigo los nombres de las variables del data frame swiss
help(swiss)
swiss

X <- as.data.frame(swiss)
attach(X)

#definimos nuestra variable a analizar
education <- X$Education

#----------------------------------------------
# Medidas de posicion													
#----------------------------------------------
mean(education) # Media aritmetica
median(education) # Mediana

# Calculo manual de la mediana (aprendemos: length, sort, seleccionar datos 
# de un vector, eliminar datos de un vector)
length(education) 
central <- length(education) / 2 
central
# Porque tenemos un numero par de observaciones
educationord <- sort(education)  #ordena de las variables observadas
(educationord[central] + educationord[central + 1]) / 2 #hay un numero impar de observaciones por eso se hace esto

# Efecto de las observaciones extremas sobre media y mediana
mean(education)
valormax <- which.max(education)  #saca los datos extremos que pueden cambiar los valores de la media.
education2 <- education[-valormax]   #selecciona la observacion correspondiente al indice max
mean(education2)
median(education2)

# Cuantiles
quantile(education, 0.5)  #primera forma de calcular el cuantil. Esta es la mediana. Aqui directamente vamos cambiando el porcentaje para calcular el cuantil que quiero
0.5 * (length(education) - 1) + 1   #en esta linea y en la de abajo es una segunda opcion de calcular el cuantil
(educationord[23.5] + educationord[24.5]) / 2  #23.5 es la mitad de observaciones y hay que sumarle uno xq es impar

quantile(education, 0.1)
0.1 * (length(education) - 1) + 1
0.9 * educationord[23.5] + 0.1 * educationord[24.5]

# Cuartiles
quantile(education, c(0.25, 0.5, 0.75))

#es como R lo hace

# Deciles
quantile(education, seq(0.1, 0.9, 0.1))

# deja el 10% de los datos a la izquierda
# Percentiles
quantile(education, seq(0.01, 0.99, 0.01))

#----------------------------------------------
# Medidas de dispersion												    
#----------------------------------------------
var(education) # cuasivarianza. Es un termino anglosajon pero nosotros debemos considerarla como la varianza
sd(education) # cuasidesviacion tipica

n <- length(education)
var.e <- var(education) * (n - 1) / n # Varianza de Education. Se usa la CUASIVARIANZA menos n-1 sobre n
var.e^0.5 # desviacion t?pica

#----------------------------------------------
# Para comparar variables con distinta escala empleamos 
# El coeficiente de variacion (adimensional)
#----------------------------------------------

sd(education) / abs(mean(education))    

#el coefi de variacion es una medida que nos dice si la media es representativa. Si el coefi de variacion es < 0,8 la media es significativa. 


# Las medidas anteriores se ven muy afectadas por valores extremos
# Una medida de dispersion menos sensible a las observaciones extremas
# es el recorrido intercuartilico
# "names = F" se usa para que no muestre el porcentaje del cuantil
# calculado. Si no se especifica lo calcula igualmente y lo unico que
# teneis que hacer es ignorar esa informacion (compara RIr con RIe)

RIe <- quantile(education, 0.75, names=F) - quantile(education, 0.25, names=F) #cogemos los valores dentro de los cuartiles. Resta entre tercer y el primer cuartil

RIe


# Viene determinado por la escala de la variable
# Para comparar variables con distinta escala empleamos el 
# recorrido semi-intercuartilico (adimensional)


RSIe <- RIe/(quantile(education, 0.75) + quantile(education, 0.25)) 
RSIe       #el recorrido semiintercuartilico es la varianza

#----------------------------------------------
# Resumen de los principales estadisticos descriptivos
  #comando MAGICO que nos incluye el minimo maximo la media y los cuartiles
summary(education)
# medida propuesta por Tukey
fivenum(education) # medida propuesta por Tukey. La unica diferencia entre summary y tukey es que este ultimo no te dice la media
#----------------------------------------------

#----------------------------------------------
# Representacion grafica												
#----------------------------------------------
boxplot(education)   #parecida a summary pero graficamente. La ralla gorda es la medianaa. Abajo el primer cuartil y earriba el tercer cuartil. Los bigotes son los patrones de los datos. Por arriba del grafico estan los outliyer. Sirve para medir posicion, dispercion y outlayer.
boxplot(log(education)) #lo que es enorme lo pasa a otra escala. Elimina los valores que distorsionan

hist(education, freq = F) #se pone freq F para que te muestre la grafica con densidades y no frecuencias
hist(education, ylim = c(0, 30), xlab = "Education", 
     ylab = "Frecuencia", main = "Histograma de Educacion",
     border = "lightskyblue2", col = "navy", freq = T)
box(lwd = 2)

# Vamos a ver como variar las particiones. Para comparar graficos!!!
par(mfrow = c(1, 3)) # Abrimos una ventana para tres graficos en una misma fila. Se puede poner la cantidad que queramos
hist(education, breaks = seq(0, 70, 10)) #que los breaks sean desde 0 a 100 de 20 en 20
hist(education)
hist(education, breaks = 5) #breaks de 10 en 10. Preguntarle
par(mfrow = c(1, 1)) #elige solo un grafico

# Para dibujar la estimacion kernel:

#con plot sale solo el kernel
plot(density(education), main = "Estimacion kernel de la funcion de densidad", 
     ylab = "frecuency", 
     xlab = "education", col="Blue") #otra forma de evaluar datos cuantitativos. Evaluan los promedios de los datos y va cogiendo las diferencias. Para cualquier valor de X. Pasa los datos a valores continuos. 

# Suele presentarse junto con el histograma
hist(education, freq = F,ylim = c(0, 0.080))
lines(density(education), col = "Blue")   #lines nos une el la linea del kernel con el histograma

# Otra forma
hist(education, probability = "T", col="yellow", ylim = c(0, 0.080))  #colocar freq F y probability T es exactamente lo mismo
lines(density(education), col = "Blue")
box(lwd = 1)

# Variamos el tipo de ventana: triangular, gaussiana, rectangular
hist(education, freq = F,ylim = c(0, 0.080))
lines(density(education, kernel = "triangular"), col = "green") 
lines(density(education, kernel = "rectangular", bw=3), col = "Red") #bw es el ancho de bando. 
lines(density(education, kernel = "gaussian"), col = "Blue")
box(lwd = 2)

# Anadimos una leyenda al grafico
# La leyenda hay que hacerla a mano con el comando legend
# La mejor estrategia para aprender el significado de cada argumento es jugar con la funci?n
# Los dos primeros son la posici?n de la leyenda, el siguiente los titulos,
# lty = estilo de linea, pch = estilo de punto (ambos deben coincidir con el del grafico
# o la leyenda no ser? informativa)
# bg = color del fondo de la leyenda.

legend(76000, 6*10^-5, c("Ventana triangular", "Ventana rectangular", "Ventana gausiana"), 
       col = c("Green", "Red", "Blue"), lty = c(1, 1, 1), bg = "gray90")

# Variamos el ancho de banda: 
hist(education, freq = F,ylim = c(0, 0.08))
lines(density(education, bw = 5), col = "green")
lines(density(education), col = "Red")
lines(density(education, bw = 3), col = "Blue")
box(lwd = 2)
legend("topright", c("Ancho de banda = 8000", "Ancho de banda optimo", "Ancho de banda = 1000"), 
       col = c("Green", "Red", "Blue"), lty = c(3, 4, 6), bg = "gray90")

#----------------------------------------------
# Medidas de forma:													   
#----------------------------------------------
# Necesitamos cargar la libreria "moments" 
install.packages("moments")
library(moments)
skewness(education) # Asimetria
kurtosis(education)-3 # Curtosis. Los rendimientos tienen curtosis muy altas. Concentracion de una variable al centro de la distribucion

#la kurtosis tiene como valor de referencia 3. Si es >3 es mas empinada. Si es = 3 es parecida a la distribucion normal y <3 es mas aplanada.
#----------------------------------

#Ejercicio 1.12 Analisis exploratorio de una serie de datos de cotizaciones 

library(quantmod) #trae mucha informacion de series financieras y ademas calculo los modelos simples y comlejos de finanzas, indicadores. Tmb sirve para trading

#-------------------------------------------------------
# Serie y periodo
#-------------------------------------------------------
getSymbols("AAPL", from="2019-01-01", to="2020-09-16")
barChart(AAPL,theme = "black")

#no es tan flexible. Por ejemplo para pintar por encima
AAPL #variables disponibles
#-------------------------------------------------------
# Rendimientos
#-------------------------------------------------------
AAPL$AAPL.Adjusted  #es un precio corregido
RendAAPL <- diff(log(AAPL$AAPL.Adjusted))[-1]  #rentabilidad en valores continuos
tail(RendAAPL)  #datos mas recientes
plot(RendAAPL, main="AAPL(Rend.Diarios)")   #plot de la serie de rendimientos

#-------------------------------------------------------
# Medidas Estad?sticas
#-------------------------------------------------------
mean(RendAAPL)
summary(RendAAPL)
library(moments)
c(skewness(RendAAPL),kurtosis(RendAAPL)) #la curtosis alta significa

#-------------------------------------------------------
# Histograma y kernel
#-------------------------------------------------------
hist(RendAAPL)
hist(RendAAPL,breaks = seq(-0.1,0.08,0.01), col = "lightblue", probability = T)
lines(density(RendAAPL))
box(lwd=1)  


#----------------------------------------------------------
#Ejercicio 2.10 Fundamentos de Calculo
#-----------------------------------------------------

library(lpSolve)

# Parametros del problema
coef <- c(30, 20, 15, 12, 10, 7)/100    #rentabilidades de la funcion objetivo

# f= 0.30 * A + 0.20*A + 0.15 *A + 0.12* M+0.10*M + 0.07 *B

A <- matrix(c(1,1,1,1,1,1, 
              1,1,1,0,0,0,
              1,1,1,0,0,0,
              0,0,0,1,1,0,
              0,0,0,1,1,0,
              0,0,0,0,0,1,
              -2,1,0,0,0,0,
              -3,0,1,0,0,0,
              0,0,0,-2,1,0), nrow = 9, ncol=6, byrow = T) #byrow igual T significa que se ordena por filas
A
b <- c(100, 50,75,20,30,5,0,0,0)
dir <- c('=', '>=','<=','>=','<=','<','=','=','=') #las direcciones de las restricciones

#-----------------------------------------------------
# Solucion
#-----------------------------------------------------
solucion <- lp ('max', coef, A, dir, b) #coef se refiere a la funcion objetivo
solucion   #la rentabilidad optima
solucion$objval
solucion$solution

#------------------------------------------
#Fundamentos de algebra - Practica 3.6

#y es el precio en miles de euros;x1 metros cuadrados y x2 numero de habitaciones

rm(list=ls())

n <- 5
X <- matrix(c(10, 12, 12, 14, 10,
              4, 4, 5, 5, 3), nrow = 5, ncol = 2)
y <- c(122, 115, 128, 145, 108)
X
y

# (i) Estimador MCO de b   Metodo de MINIMOS CUADRADOS ORDINALES

b <- solve(t(X) %*% X) %*% t(X) %*% y       #(solve) es la inversa de la transpuesta de X por X... por la transpuesta de X por y
b

# Estimacion
yhat <- X %*% b
yhat

# Residuos o error. Es igual a y menos el y estimado
e <- (y - yhat)
e    #resultado del apartado (i)

# (ii) Equivalencia

P <- X %*% solve(t(X) %*% X) %*% t(X)    
P
yhat2 <- P %*% y
round(yhat-yhat2, 2) #redondea la diferencia entre los dos con dos decimales. Sirve para COMPARAR.

# Otra forma residuos

e2 <- (diag(n)-P) %*% y    #diag de n es la matriz diagonal de n.
round(e-e2,2)      #para comparar que da bien los errores o residuos por ambas formas.

# P e (I-P) sim?tricas e idempotentes

# Para P
round(P - t(P))   #Es simetrica porque la transpuesta de P es igual P
round(P %*% P-P,2)   #si la multiplicacion entre P por si misma es igual a la original P entonces es Idempotente. Al restar por si misma da 0, es decir, que esta bien.

# Para I-P
round((diag(n)-P) - t(diag(n)-P),2)  #es simetrica
round((diag(n)-P) %*% (diag(n)-P)-(diag(n)-P),2) #es idempotente


# (iii) Suma de cuadrados de los residuos
SCR1 <- t(e) %*% e
SCR1
SCR2 <- t(y) %*% (diag(n)-P) %*% y
SCR2

round(SCR1-SCR2,2)    #si se puede escribir con la misma funcion


# Anadir termino independiente

uno <- rep(1,n) #crear una matriz de 1 a n
uno
X1 <-cbind(uno,X) #union de matrices. 
X1
X <- X1    #al hacer esto no es necesario reemplazar X1 en cada uno de los apartados del ejercicios

# (i) Estimador MCO de b   Metodo de MINIMOS CUADRADOS ORDINALES

b <- solve(t(X) %*% X) %*% t(X) %*% y       #(solve) es la inversa de la transpuesta de X por X... por la transpuesta de X por y
b

# Estimacion
yhat <- X %*% b
yhat

# Residuos o error. Es igual a y menos el y estimado
e <- (y - yhat)
e    #resultado del apartado (i)

# (ii) Equivalencia

P <- X %*% solve(t(X) %*% X) %*% t(X)    
P
yhat2 <- P %*% y
round(yhat-yhat2, 2) #redondea la diferencia entre los dos con dos decimales. Sirve para COMPARAR.

# Otra forma residuos

e2 <- (diag(n)-P) %*% y    #diag de n es la matriz diagonal de n.
round(e-e2,2)      #para comparar que da bien los errores o residuos por ambas formas.

# P e (I-P) sim?tricas e idempotentes

# Para P
round(P - t(P))   #Es simetrica porque la transpuesta de P es igual P
round(P %*% P-P,2)   #si la multiplicacion entre P por si misma es igual a la original P entonces es Idempotente. Al restar por si misma da 0, es decir, que esta bien.

# Para I-P
round((diag(n)-P) - t(diag(n)-P),2)  #es simetrica
round((diag(n)-P) %*% (diag(n)-P)-(diag(n)-P),2) #es idempotente


# (iii) Suma de cuadrados de los residuos
SCR1 <- t(e) %*% e
SCR1
SCR2 <- t(y) %*% (diag(n)-P) %*% y
SCR2

round(SCR1-SCR2,2)    #si se puede escribir con la misma funcion





