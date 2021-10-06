#A partir del fichero survey de la librarıa MASS se pide:
#(i) Identificar variables cuantitativas y categoricas (factores)
#(ii) Representar dos variables cuantitativas mediante plot(x,y) y a continuacion identificar los niveles de una variable categorica
#(iii) Representar variables cuantitativas mediante hist(x) y mediante boxplot
#(iv) Representar variables cuantitativas segun niveles de alǵun factor

library(MASS)
??MASS
data(survey)
names(survey)  #traigo los nombres de las variables del data frame swiss
help(survey)
survey

##(i) Identificar variables cuantitativas y categoricas (factores)

sapply(survey, class)


#(ii) Representar dos variables cuantitativas mediante plot(x,y) y a continuacion identificar los niveles de una variable categorica
span_writing <- survey$Wr.Hnd
span_nonwriting <- survey$NW.Hnd
plot(span_writing,span_nonwriting, main = "Span Wh and NonWh")


#(iii) Representar variables cuantitativas mediante hist(x) y mediante boxplot
par(mfrow = c(1,2))
hist(span_writing)
hist(span_nonwriting)
par(mfrow = c(1,1))
boxplot(span_writing,span_nonwriting)

#(iv) Representar variables cuantitativas segun niveles de algun factor
sex <- survey$Sex
plot(span_writing ~ sex)
plot(span_nonwriting ~ sex)

