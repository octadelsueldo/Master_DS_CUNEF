## Practica 7 Tecnicas de clasificacion

## Se lanza un dado y se consideran los sucesos:
#(a) Obtener un as; 
az <- 1/6
az
oddsratioas <- (1/6)/(5/6)
oddsratioas
logitaz <- log(oddsratioas)
logitaz
##(b) Obtener un numero par; 
numpar <- 1/2
numpar
oddsratiopar <- (1/2)/(1/2)
oddsratiopar
logitpar <- log(oddsratiopar)
logitpar

##(c) Obtener un numero del 1 al 4. 
onetofour <- 4/6
onetofour
oddsratioonetofour <- (4/6)/(2/6)
oddsratioonetofour #existe dos veces mas probabilidad de que salga un numero del 1 al 4 a que no salga
logitaz <- log(oddsratioonetofour)
logitaz



#Para cada uno de los tres sucesos calcular la probabilidad, el odds ratio y el logit


