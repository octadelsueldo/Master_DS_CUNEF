f <-  function(x1,y1)  ((1-x1)**2)+100*(y1-x**2)**2

x<- seq(-3,3,0.2)
y<- seq(-3,3,0.2)
z<- outer(x,y,f) #la funcion outer es como una hoja de calculo. Nos define para un valor de una fila y una columna los datos.
#Se pueden poner otras operaciones dentro de outer.
z

persp(x,y,z, phi = -20, col="yellow", shade=0.60, ticktype = "detailed") # ok dibuja una funcion de tres variables

f1 <-  function(x)  ((1-x[1])**2)+100*(x[2]-x[1]**2)**2

optim(c(-5,5),f1) 
optim(c(-5,5),f1)$par
optim(c(-5,5),f1)$value

#otra forma de verlo
image(x,y,z)
'''
This command allows us to detect minima and maxima by showing us the height of the function at different points: lighter colors
(yellow/white) indicate high regions, while darker red indicates that the function decreases.
Here, we notice that the angle at the upright boundary of the picture is almost “white”, while the remaining three are more “red”.
However, this is not enough to conclude that these points are optimizers, since the function can grow or decrease to infinity.
Instead, we can observe on the picture some “bright red” parts that meet forming a cross. This allows us to identify three saddle points.
'''

'''
Now we must obtain the partial derivatives in order to draw the zero level sets that will show us precisely, through their intersections, where the stationary points are located.
The partial derivative with respect to 𝒙 in R is:
'''
fx <- function(x,y,h=0.001) (f(x+h,y)-f(x,y))/h

'''
This describes the incremental ratio. We shock 𝒙 by adding to it an arbitrarily small value, 𝒉(= 𝟎. 𝟎𝟎𝟏). In this way, we compute the rate of change (of the function).
The same applies to 𝒚, as follows:
'''

fy <- function(x,y,h=0.001) (f(x,y+h)-f(x,y))/h

'''
We now re-use the other function to compute the 𝒛 values corresponding to the partial derivatives.
Thus 𝒛𝒇𝒙 and 𝒛𝒇𝒚 are matrices, 400 rows and 400 columns, with values of 𝒇𝒙 and 𝒇𝒚 respectively.
'''

zfx <- outer(x,y,fx)
zfy <- outer(x,y,fy)

'''
Now we are perfectly able, using the “contour” function. This command draws lines that show the level sets of the function you insert
as an input. If we use it with respect to both partial derivatives at the zero level, it becomes possible to see the stationary points,
which are:  * Saddle points where the lines cross;
            * Maxima and minima where we observe small circles.
'''

contour(x,y,zfx,level=0) 
contour(x,y,zfy,level=0, add=T, col="red")

'''
When giving the command we write:
- the two variables, 𝒙 and 𝒚;
- the function for which we want to see the level sets.
- level =0 because we are not interested just in the others.
To the command that draws the zero level sets of zfy, we add:
- add=T to impose this graph over the previous one;
- col=”red” to change the colors of the line, to distinguish
properly the contours for 𝒇𝒙 and 𝒇𝒚.
Afterwards, it is straightforward to observe the 4 stationary points at the four crossing points. We can read the coordinates 
of the points directly on the axis of the pictures.
From the image picture we have found just three saddle points, and no maxima or minima, while here we see the zero level sets 
crossing in 4 points. This means that we still do not know the nature of one stationary point. To understand what kind of optimizer it is,
we must zoom on its area, changing the proportions of the axis.
We do this through “seq” (choosing a shorter interval for x and y), and defining 𝒛 correspondingly.
'''


x <- seq(-2,2,len=400) 
y <- seq(-2,2,len=400) 
z<- outer(x,y,f)


#Looking at the picture of both colors and contours, we will identify the “misterious” point.

image(x,y,z)
contour(x,y,z,add=T)
