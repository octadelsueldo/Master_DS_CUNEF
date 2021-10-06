# Un data scientist ha obtenido la matriz 10 × 10 deﬁnida como A = {i2 + j2 + 1}, donde 
#i, j  =  0, 1, . . . , 9

# --- Declarar primero las columnas
x = 1:10; i = seq(0,9, by = 1); j= seq(0,9,1); const = rep(1, 10) ; x; i; j; const 
#[1] 1 2 3
#[1] 0 1 2 3 4 5 6 7 8 9
#[1] 0 0 0 0 0 0 0 0 0
C = matrix(c(i^2,j^2, const), nrow = length(x)); C # ncol no es necesario declararlo # [,1] [,2] [,3]


det(C)
