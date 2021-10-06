#----------------------------------------------------------------------
# MDS - Metodos de Estimacion y Contraste
# Eleccion de muestras
# Practica 1
#----------------------------------------------------------------------

# Ejemplo sample

# Poblacion
x <- 1:100  #la poblacion son 100 personas

# Tamano muestral
nmuestra <-30

# Eleccion de las muestras
set.seed(2021) # Para reproducir resultados

# Muestra sin reemplazamiento
m_sinrem <- sample(x, nmuestra, replace = FALSE)   #sample permite simular distribuciones discretas
m_sinrem
table(m_sinrem)  #la tabla nos muestra la frecuencia

# Muestra con reemplazamiento
m_conrem <- sample(x, nmuestra, replace = TRUE)  #con reemplazamiento cuando quiere o tiene q repetir algunos valores 
m_conrem
table(m_conrem)
