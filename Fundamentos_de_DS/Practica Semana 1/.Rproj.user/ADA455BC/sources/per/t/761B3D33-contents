# Estamos  trabajando  con  la  funci´on 
#f (x, y)  =  −2(x2  + y2 ) + (x4  + y4 ) − 11 
# Probar  que  tiene  9  puntos  cr´ıticos,  de  los  cu´ales  4  son  m´ınimos,  1  m´aximo  y  el  resto  puntos de  silla.

f <-  function(x1,y1) -2*(((x1**2)+(y1**2)))+((x1**4)+(y1**4))-11

x<- seq(-5,5,0.2)
y<- seq(-5,5,0.2)
z<- outer(x,y,f) #la funcion outer es como una hoja de calculo. Nos define para un valor de una fila y una columna los datos.
#Se pueden poner otras operaciones dentro de outer.
z

persp(x,y,z, phi = -20, col="yellow", shade=0.60, ticktype = "detailed") # ok dibuja una funcion de tres variables

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

# Finally, we can spot a circle in a very bright area. This is precisely where our maximum lies.  

### Algebraic solution:
'''
It is possible to find the exact coordinates of the stationary points of a function, using the command “optim”.
In principle, the “optim” command does not read multivariate functions. However, it is possible to transform our bivariate function 
into a univariate one, by changing the names of the two variables x and y, into 𝒙[𝟏] and 𝒙[𝟐].
The new function therefore is:
'''

fbb<-function(x) f(x[1],x[2])

'''
The logic behind this transformation, is that [ ] means extraction. So, 𝒙[𝟏] and 𝒙[𝟐] are viewed by R as elements belonging 
to the same vector 𝒙.
Here, we simply plug the values 𝒙[𝟏] and 𝒙[𝟐] into the function 𝒇, so that 𝒙[𝟏]= 𝒙, and 𝒙[𝟐] = 𝒚.
Next, we use “optim” to find the coordinates of our maximizer. Remember is that R finds one stationary point at a time. Moreover,
by default R just gives the coordinates of the minima. Two parameters are needed: an approximate guess of where the critical point might be
and the name of the function.
Since we have a maximizer, the best way to overcome the “minimizer-default” of R is to reverse the function down, so that the original 
minimizer becomes now a maximizer. This is done by changing the sign of the value corresponding to the parameter “fnscale”.

'''

optim(c(-1,1),fbb,control=list(fnscale=-1))

'''
Here, the vector is our guess of where we expect to find the point (just look at the previous graph). “𝒈𝒃” is the function we need 
to maximize, and the last part of the command is to the “maximizing trick”. Here is what we get:
'''
optim(c(-1,1),fbb,control=list(fnscale=-1))$par #(the coordinates of the point)

optim(c(-1,1),fbb,control=list(fnscale=-1))$value #(the corresponding z value)

'''
To find the coordinates of the saddle points algebraically, we should solve a system where we set both partial derivatives equal to zero.
This is not possible in R. We minimize the sum of the square partial derivatives with “optim”. This sum is always non-negative. 
The points where it is equal to zero are exactly where both (second order, considering the very initial objective function) partial 
derivatives are equal to zero, then the system is solved.
Lets define the partial derivatives plugging 𝒙[𝟏] and 𝒙[𝟐] instead of 𝒙 and 𝒚.
'''

fxb <- function(x) fx(x[1],x[2])
fyb <- function(x) fy(x[1],x[2])

'''

Call sumssq the function of the vector 𝒙 made of the sum of the squares of the two partial derivatives.
'''

sumssq <- function(x) fxb(x)**2+fyb(x)**2

'''
Now we can find the coordinates of the saddle points through “optim”. We do exactly the same thing for all three points, 
just changing the guess of their locations, according to the graphs
'''

optim(c(-2,2),sumssq)$par ##(this is almost 0,0)

optim(c(-1,1),sumssq)$par

optim(c(-0.5,0.5),sumssq)$par

'''
Now please recall what is written at about half of the second page. We should make sure that the function increases or decreases
to infinity at the angles of the pictures, in ensure that we have found ALL the stationary points of the function, and that none 
is hidden beyond the boarder of our graphs.
We maximize the “white angle” and minimize the other three (to understand what angle we are working on, just have a look at the 
vector c that indicates approximately where we place our initial guess):
'''

optim(c(-2,2),fbb) #The values are: 5.457516e+53 and 5.399318e+53 => 𝒙 → +∞ and y-> +∞

optim(c(0.4,-0.4),fbb) #The values are 4.804367e+54 and -4.727385e+54 => x-> +∞ and y-> -∞.

optim(c(-2,2),fbb,control=list(fnscale=-1)) # Thevaluesare-4.727385e+54and 4.804367e+54=>x->-∞andy->+∞.

optim(c(-0.4,-0.4),fbb) # The values are -1.185271e+54 and -6.367610e+54 =>x-> -∞ and y-> -∞.


# These results imply that there are no points of our interest outside our pictures.


f1 <-  function(x) -2*(((x[1]**2)+(x[2]**2)))+((x[1]**4)+(x[2]**4))-11
optim(c(-5,5),f1) 
optim(c(-5,5),f1)$par
optim(c(-5,5),f1)$value
