#----------------------------------------------------------------------
# MDSF - Tecnicas de Clasificacion 
# Naive Bayes - Datos Iris
# Practica 2 
#----------------------------------------------------------------------

Datos_Historicos <- read.csv("Datos_Historicos.csv")
Datos_Historicos
Nuevos_Clientes <- read.csv("Nuevos_Clientes.csv") 
Nuevos_Clientes

summary(Datos_Historicos)

#----------------------------------------------------------------------
# Modelo Naive Bayes
#----------------------------------------------------------------------
library(e1071)

Probabilidades <- naiveBayes(Compra ~., data=Datos_Historicos[-1]) 
Probabilidades

Prediccion <- predict(Probabilidades , Nuevos_Clientes[,-6],type = "raw") 
Prediccion

# --------------------------------------------------------------------------------
# A?ade columna de Prediccion a tabla Nuevos_Clientes
Nuevos_Clientes$Prediccion_de_Compra <- Prediccion   

# ---------------------------------------------------------------------------------
# Guarda los Nuevos_Clientes con la predicci?n:
write.csv(Nuevos_Clientes,"Predict_NuevosClientes.csv")

Nuevos_Clientes
