#----------------------------------------------
# MDS - Fundamentos 
# Analisis Exploratorio de datos univariados
#----------------------------------------------

#----------------------------------------------
# Datos y variables											
#----------------------------------------------

rm(list=ls())

datos <- read.table("DatosD1_1.txt", header = T) # Cargamos los datos


names(datos) # Para ver que variables contiene el archivo
head(datos)
attach(datos) #nos trae a la memoria del ordenador

# Definimos nuestras variables de interes (creamos dos objetos)

renta <- datos$PIBpc
educacion <- datos$educ

#----------------------------------------------
# Medidas de posicion													
#----------------------------------------------
mean(renta) # Media aritmetica
median(renta) # Mediana

# Calculo manual de la mediana (aprendemos: length, sort, seleccionar datos 
# de un vector, eliminar datos de un vector)
length(renta) 
central <- length(renta) / 2 
central
# Porque tenemos un numero par de observaciones
rentaord <- sort(renta)
(rentaord[central] + rentaord[central + 1]) / 2 

# Efecto de las observaciones extremas sobre media y mediana
mean(renta)
quitar <- which.max(renta)  #saca los datos extremos que pueden cambiar los valores de la media. Depende de los extremos
renta2 <- renta[-quitar]   #slecciona la observacion correspondiente al indice max
mean(renta2)
median(renta2)

# Cuantiles
quantile(renta, 0.5)  #primera forma de calcular el cuantil
0.5 * (length(renta) - 1) + 1   #en esta linea y en la de abajo es una segunda opcion de calcular el cuantil
(rentaord[66] + rentaord[67]) / 2

quantile(renta, 0.1)
0.1 * (length(renta) - 1) + 1
0.9 * rentaord[14] + 0.1 * rentaord[15]

# Cuartiles
quantile(renta, c(0.25, 0.5, 0.75))

#es como R lo hace

# Deciles
quantile(renta, seq(0.1, 0.9, 0.1))

# deja el 10% de los datos a la izquierda
# Percentiles
quantile(renta, seq(0.01, 0.99, 0.01))

#----------------------------------------------
# Medidas de dispersion												    
#----------------------------------------------
var(renta) # cuasivarianza
sd(renta) # cuasidesviacion tipica

var(educacion)
sd(educacion)

n <- length(renta)
var.e <- var(educacion) * (n - 1) / n # Varianza
var.e^0.5 # desviacion tipica

#----------------------------------------------
# Para comparar variables con distinta escala empleamos 
# el coeficiente de variacion (adimensional)
#----------------------------------------------
sd(renta) / abs(mean(renta))
sd(educacion) / abs(mean(educacion))

# Las medidas anteriores se ven muy afectadas por valores extremos
# Una medida de dispersion menos sensible a las observaciones extremas
# es el recorrido intercuartilico
# "names = F" se usa para que no muestre el porcentaje del cuantil
# calculado. Si no se especifica lo calcula igualmente y lo unico que
# teneis que hacer es ignorar esa informacion (compara RIr con RIe)

RIr <- quantile(renta, 0.75, names=F) - quantile(renta, 0.25, names=F)
RIe <- quantile(educacion, 0.75) - quantile(educacion, 0.25)
RIr;RIe


# Viene determinado por la escala de la variable
# Para comparar variables con distinta escala empleamos el 
# recorrido semi-intercuartolico (adimensional)

RSIr <- RIr/(quantile(renta, 0.75) + quantile(renta, 0.25))
RSIe <- RIe/(quantile(educacion, 0.75) + quantile(educacion, 0.25))
RSIr;RSIe

#----------------------------------------------
# Resumen de los principales estadisticos descriptivos
summary(renta)   #comando MAGICO que no incluye el minimo maximo la media y los cuartiles
summary(educacion)
fivenum(renta) # medida propuesta por Tukey
fivenum(educacion) # medida propuesta por Tukey
#----------------------------------------------

#----------------------------------------------
# Representacion grafica												
#----------------------------------------------
boxplot(renta)   #parecida a summary pero graficamente. La ralla gorda es la medianaa. Abajo el primer cuartil y earriba el tercer cuartil. Los bigotes son los patrones de los datos. Por arriba del grafico estan los outliyer. Sirve para medir posicion, dispercion y outlayer.
boxplot(log(renta)) #lo que es enorme lo pasa a otra escala. Elimina los valores que distorsionan

hist(renta, freq = F)
hist(renta, ylim = c(0, 100), xlab = "PIB per capita", 
     ylab = "Frecuencia", main = "Histograma del PIB per capita",
  border = "lightskyblue2", col = "navy", freq = T)
box(lwd = 2)

# Vamos a ver como variar las particiones
par(mfrow = c(1, 3)) # Abrimos una ventana para tres graficos en una misma fila
hist(renta, breaks = seq(0, 120000, 20000))
hist(renta)
hist(renta, breaks = 50)
par(mfrow = c(1, 1)) 

# Para dibujar la estimacion kernel:
plot(density(renta), main = "Estimaci?n kernel de la funcion de densidad", 
     ylab = "densidad", 
  xlab = "PIB per capita", col="Blue") #otra forma de evaluar datos cuantitativos. Evaluan los promedios de los datos y va cogiendo las diferencias. Para cualquier valor de X. Pasa los datos a valores continuos. 

# Suele presentarse junto con el histograma
hist(renta, freq = F,ylim = c(0, 0.00006))
lines(density(renta), col = "Blue")   #lines nos figura encima del histograma

# Otra forma
hist(renta, probability = "T", col="orange")
lines(density(renta), col = "Blue")
box(lwd = 1)

# Variamos el tipo de ventana: triangular, gaussiana, rectangular
hist(renta, freq = F,ylim = c(0, 0.00006))
lines(density(renta, kernel = "triangular"), col = "green") 
lines(density(renta, kernel = "rectangular", bw=5000), col = "Red")
lines(density(renta, kernel = "gaussian"), col = "Blue")
box(lwd = 2)

# Anadimos una leyenda al grafico
# La leyenda hay que hacerla a mano con el comando legend
# La mejor estrategia para aprender el significado de cada argumento es jugar con la funcion
# Los dos primeros son la posicion de la leyenda, el siguiente los titulos,
# lty = estilo de linea, pch = estilo de punto (ambos deben coincidir con el del grafico
# o la leyenda no ser? informativa)
# bg = color del fondo de la leyenda.

legend(76000, 6*10^-5, c("Ventana triangular", "Ventana rectangular", "Ventana gausiana"), 
  col = c("Green", "Red", "Blue"), lty = c(1, 1, 1), bg = "gray90")

# Variamos el ancho de banda: 
hist(renta, freq = F,ylim = c(0, 0.00009))
lines(density(renta, bw = 8000), col = "green")
lines(density(renta), col = "Red")
lines(density(renta, bw = 1000), col = "Blue")
box(lwd = 2)
legend("topright", c("Ancho de banda = 8000", "Ancho de banda ?ptimo", "Ancho de banda = 1000"), 
  col = c("Green", "Red", "Blue"), lty = c(1, 1, 1), bg = "gray90")

#----------------------------------------------
# Medidas de forma:													   
#----------------------------------------------
# Necesitamos cargar la libreria "moments" 
#install.packages("moments")
library(moments)
skewness(renta) # Asimetria
kurtosis(renta)-3 # Curtosis. Los rendimientos tienen curtosis muy altas. 

# Boxplot con dos variables
boxplot(renta, educacion)
boxplot(log(renta), educacion)

#----------------------------------------------
# Datos bivariantes													
#----------------------------------------------
# Covarianza
cov(renta, educacion) #me dara un solo valor
# Coeficiente de correlacion
cor(renta, educacion) #me dara un solo valor
# Grafico de dispersion
plot(educacion, renta, pch=20)
plot(educacion, log(renta), pch=20) #se pone log a renta porq renta toma valores grandes



