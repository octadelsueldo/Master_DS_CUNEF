#-------------------------------------------
# MDS - Fundamentos
# Ejercicio Votantes
#-------------------------------------------

# Votantes
votos <- rep(c("PP","PSOE","CD","UD"),c(60,50,40,10))
votos <- data.frame(votos)
head(votos)

#Informacion
class(votos)
ncol(votos)
nvotos<-nrow(votos)
nvotos

# Tablas basicas
table(votos) # tabla para variables categoricas
table(votos)/nvotos *100
round(table(votos)/nvotos *100,1)

# Graficos
par(mfrow = c(1, 2))
pie(table(votos))
barplot(table(votos),col = "orange", ylim = c(0,62))
box(lwd=1)  
par(mfrow = c(1, 1))

# Tabla ordenada
sort(table(votos)/nvotos * 100, decreasing = T)
round(sort(table(votos)/nvotos * 100, decreasing = T),1)
