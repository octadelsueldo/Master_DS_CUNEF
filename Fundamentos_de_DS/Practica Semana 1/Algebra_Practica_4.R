A <- matrix(c(1,5,-2,1,2,-1,3,6,-3),3,3)
A

library(Biodem)
AA<-mtx.exp(A,3)
AA


A[, 3] = A[, 1] + A[,2]
A
