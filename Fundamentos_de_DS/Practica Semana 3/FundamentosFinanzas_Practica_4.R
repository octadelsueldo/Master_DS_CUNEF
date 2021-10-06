#Ejercicio 4 de Finanzas 


#Los rendimientos de dos activos con riesgo presentan volatilidades del 11 y del 15 por ciento,
#con una correlacion de 0.48. Si los rendimientos esperados son del 4 y del 11 por ciento,respectivamente,
#encontrar la cartera de varianza mınima que garantiza un rendimientodel 7 por ciento.

desv1 <- 0.11 #volatilidad activo 1
desv2 <- 0.15 #volatilidad activo 2
corr <- 0.48 #correlacion entre activo 1 y 2
rend1 <- 0.04 #rend esperado activo 1
rend2 <- 0.11 #rendimiento esperado activo 2
rendcar <- 0.07 #rendimiento que debe tener la cartera

covar12 <- corr * (desv1 * desv1) #covarianza de la cartera
covar12

#En base al rendimiento que esperamos calculamos los pesos optimos por activo

#rendcartera <- w1r*rend1 + w2r*rend2 A partir de esa funcion despejamos los w1r y w2r

w1r <- (rendcar - rend2) / (rend1 - rend2) #w1r deberia ser 0.57

w2r <- 1 - w1r #w2r tendria que ser 0.43

rendcartera <- w1r*rend1 + w2r*rend2 #Combrobamos que el rendimiento nos da 7%


#Probamos que proporciones tendriamos con la minima varianza

w1 <- (desv2^2 - covar12) / ((desv1^2 - covar12) + (desv2^2 - covar12)) #proporcion optima w1 teniendo en cuenta las varianzas y covarianzas
w2 <- 1 - w1 #proporcion w2 por diferencia
varcartera0 <- (w1^2 * desv1^2) + (1 + w1)^2 * (desv2^2) + (2*w1)*(1 - w1)*covar12 #varianza de la cartera con estas proporciones
#vemos que varcartera0 es igual a 0.0757



rendcar0 <- w1*rend1 + w2*rend2
varcartera <- (w1r^2 * desv1^2) + (1 + w1r)^2 * (desv2^2) + (2*w1r)*(1 - w1r)*covar12 
#la varianza de la cartera con estas proporciones es 0.0623






