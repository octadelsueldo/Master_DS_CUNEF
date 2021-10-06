SELECT carrier AS airline, [name] AS FullName 
FROM airlines ALN



-- Recover all data from the first ten airports
SELECT TOP 10 * FROM flights

-- Recover the tail number, the manufacturer, the model and the number of seats of the first hundred planes using synonyms
SELECT TOP 10 tailnum AS TailNumber, manufacturer AS Manufacturer, model AS Modelo, seats AS Asientos
FROM planes

-- Ejemplo de Order by
SELECT carrier AS AirLine, name AS FullName FROM airlines ALN
ORDER BY FullName DESC

-- Ejemplo de operaciones aritmeticas
SELECT TOP 10 *, dep_time - sched_dep_time AS Difer_dep_time FROM flights

-- Ejemplo de NewId para sacar muestra aleatoria
SELECT top 10 * FROM flights ORDER BY NEWID()

-- ############## SOME BASIC FUNCTIONS ###############
--con el numero -1 redondea a decimales y con -2 redondea a centenas y luego a millares
SELECT ROUND(10000.0000 / 3.0000 , -1) AS RESULTADO  

--returns the square root of a positive real number
SELECT SQRT(100) AS RESULTADO 

--2 elevado a la 3
SELECT POWER(2,3) AS RESULTADO  

--2 elevado a la 0.5 nos da 1 por redondeo
SELECT POWER(2,0.5) AS RESULTADO 
 
-- cuenta la cantidad de observaciones de la columna T
SELECT COUNT(*) FROM T 

-- esto es 150*151 /2 (150 son las observaciones de t)
SELECT SUM(t) FROM T

--substring(expression ,start , length)
SELECT SUBSTRING('abcdef',3,2) AS RESULT 

 --elimina los espacios en blanco
SELECT TRIM('      TEXT     ') AS RESULT

--largo del string
SELECT LEN('abcdef') AS RESULT 

--reemplaza los espacios por guiones
-- REPLACE ( string_expression , string_pattern , string_replacement )
SELECT REPLACE('      TEXT     ','  ','-') AS RESULT 

SELECT ABS(-5) AS RESULT

-- UPPER – LOWER returns uppercase – lowercase characters
SELECT UPPER('abcdef') AS RESULT 
SELECT LOWER('TEXT') AS RESULT
-- #######FUNCIONES DE FECHA ###########

-- nos dice la hora en este momento
SELECT GETDATE() 

-- SQL toma la fecha en str si esta bien escrita
SELECT DATEADD(MM,5,'2020-10-05') 
-- MM significa mes, es decir en esta funcion nos da la fecha de hoy en cinco meses
-- WK se usa para semanas

-- esto nos dice la fecha final del mes actual
SELECT EOMONTH(GETDATE(),0) 

-- el ultimo dia del mes de hace cuatro meses
SELECT EOMONTH(GETDATE(),-4) 

-- esta la fecha final de ejercicio del mes del año pasado
SELECT EOMONTH(GETDATE(), -MONTH(GETDATE()))

-- Declaro con @a un objeto de tipo float, le pongo un valor y luego hago una operacion
DECLARE @a FLOAT
SET @a = 2
SELECT POWER(@a, 2) 

-- Dime la fecha de hoy
SELECT GETDATE() 

-- Intervalo de dias entre la fecha y el trimestre 
SELECT DATEDIFF(QQ, 0, '2020-10-05 21:01:09.663')

--Calcula el ultimo dia del trimestre anterior
SELECT DATEADD(QUARTER,DATEDIFF(QUARTER,0,'2020-10-05 21:01:09.663'),-1) 

-- how to get the week number of today’s week?
SELECT DATEDIFF(WK,'2021-01-01',GETDATE())


-- ####### AGGREGATE FUNCTIONS ########
--Aggregate functions helps to calculate results on grouped data
--These functions only return aggregated values
--Used to summarize data in a plain SELECT without ordinary columns

--CONCEPT

--COUNT: returns the frequency number of cases 
--SUM: returns the sum of the specified expression
--MIN : returns the minimum value of the specified expression
--MAX : returns the maximum value of the specified expression
--AVG : returns the average of the specified expression
--VARP: returns the variance 
--STDEVP: returns the standard deviation
--VAR: returns the unbiased estimator of the variance [ n/(n-1) * VARP ]
--STDEV: returns the unbiased estimator of the standard deviation

-- minimo, promedio y maximo del retraso en arrivo
SELECT 
MIN(arr_delay) AS MINDELAY, --minimo
AVG(arr_delay) AS AVGDELAY, --media
MAX(arr_delay) AS MAXDELAY  --maximo
FROM flights FL --hora y media adelantado y 25 horas de retraso. El promedio 7 min

SELECT COUNT(*) FROM flights

-- promedio de asientos
SELECT AVG(seats)
FROM planes

-- Please determine the quality of the average delay by calculating the relative standard deviation (desviacion tipica / media) 
-- Coeficiente de variacion
SELECT STDEVP(arr_delay) / AVG(arr_delay) AS CV--coeficiente de variacion para calcular la calidad del retraso. La desviacion tipica es 8 veces superior a la media es decir una dispersion muy grande
FROM flights FL
WHERE dest = 'MIA' --calcula el retraso minimo, max, desv, coefi de variacion con destino miami. Aqui la media mejora pero la incertidumbre respecto al retraso real a aumentado muchisimo.

--la media sin la varianza no tiene ningun sentido


-- ######## WHERE CLAUSE ###########
--SQL filter clause
--allows to filter rows using multiple conditions

-- cuentame las filas que tengan una altitud mayor o igual a 3000
SELECT COUNT(*) FROM airports WHERE alt >= 3000

-- muestrame las filas que tengan una altitud mayor o igual a 3000
SELECT * FROM airports WHERE alt >= 3000

-- cuentame las filas que tengan una altitud menor a 3000
SELECT COUNT(*) FROM airports WHERE alt < 3000

-- retrieve all airports from Chicago
-- cuando usamos %nombre% significa que me va a filtrar todo aquello que contenga chicago independientemente de lo q tenga por delante o
-- por detras de la palabra chicago
SELECT faa 
FROM airports
WHERE [name] LIKE '%CHICAGO%' 

--cuando hacemos busqueda de texto lo comun es usar LIKE y en numeros se usa el simbolo igual. Like usa mas recursos del ordenador
--Like es una busqueda aproximada


--retrieve all planes made by AIRBUS
SELECT COUNT(*) FROM planes
WHERE manufacturer = 'AIRBUS'

-- Cambiar por el LIKE y %Airbus% nos muestra mas datos porq muestras otros modelos de Airbus y asi suman 200 aviones mas
SELECT COUNT(*) FROM planes 
WHERE manufacturer LIKE '%AIRBUS%' 

--retrieve all planes with a number of seats between 200 and 250
SELECT COUNT(*) FROM planes 
WHERE seats BETWEEN 200 AND 250

--retrieve all planes made by AIRBUS with 150 seats or less 
SELECT COUNT(*) FROM planes 
WHERE manufacturer = 'AIRBUS' AND seats <= 150

SELECT COUNT(*) FROM planes 
WHERE manufacturer LIKE '%AIRBUS%' AND seats <= 150

--vuelos con retraso de dos horas
SELECT COUNT(*) FROM flights 
WHERE dep_delay > 120 

-- retrieve all flights to CHIGAGO airports with a departure delay longer than two hours
-- vuelos con 2 horas de retraso y como destino el aeropuerto de chicago. faa es el codigo unico del aeropuerto
SELECT COUNT(*) FROM flights 
WHERE dep_delay > 120 AND dest IN(SELECT faa FROM airports WHERE [name] LIKE '%CHICAGO%') 

-- IS NOT NUL debe aparecer xq si hubiera algun vuelo con retraso no informado tmb apareceria
SELECT* FROM flights
WHERE dep_delay > 200 AND dep_delay IS NOT NULL AND dest IN(SELECT faa FROM airports WHERE [name] LIKE'%CHICAGO%') 



SELECT * FROM flights WHERE dest IN
(
    SELECT faa FROM airports WHERE [name] LIKE '%CHICAGO%'
)
AND dep_delay > 120 AND dep_delay IS NOT NULL



SELECT * FROM flights WHERE dest IN
(
    SELECT faa FROM airports WHERE [name] LIKE '%CHICAGO%'
)
AND ISNULL(dep_delay,0) > 120 --AND dep_delay IS NOT NULL  

--Con ISNULL si encuentra un nulo en dep delay le da valor 0


-- ############ SELECT DISTINCT CLAUSE ############
--recovers all unique combinations of field values

--de planes busca los pocos fabricantes distintos que hay. No muestra todos solo los distintos. Ordenados por fabricantes en orden alfabetico
SELECT DISTINCT manufacturer 
FROM planes ORDER BY manufacturer 


--para cada fabricante sus modelos distintos
SELECT DISTINCT manufacturer, model 
FROM planes ORDER BY manufacturer  


SELECT DISTINCT manufacturer, COUNT(*) AS AVIONES
FROM planes
GROUP BY manufacturer  --agrupa por fabricante
ORDER BY manufacturer  --ordena por fabricante


SELECT DISTINCT model FROM planes 
WHERE manufacturer LIKE '%AIRBUS%' AND seats < 150

-- ############ FUNCION CASE #########
-- PARA CONCATENAR CONDICIONES
SELECT TOP 5 *, 
CASE dest 
WHEN 'MIA'THEN 'HOLIDAYS'
WHEN 'MCO'THEN 'HOLIDAYS' 
ELSE 'WORK'END 
FROM flights

SELECT TOP 5 *,
ALTITUD = CASE  
WHEN alt > 2000 THEN 'alto' 
WHEN alt BETWEEN 1000 AND 1999 THEN 'mediano'
WHEN alt < 999 THEN 'bajo' END
FROM airports

SELECT TOP 5 
CASE dest 
WHEN 'MIA'THEN 'HOLIDAYS'
WHEN 'MCO'THEN 'HOLIDAYS' 
ELSE 'WORK'END AS TIPO, * --ESTO sirve para tener en la primera columna la clasificacion del viaje
FROM flights

SELECT TOP 5 
TIPO = CASE dest --Otra forma de poner la columna TIPO
WHEN 'MIA'THEN 'HOLIDAYS'
WHEN 'MCO'THEN 'HOLIDAYS' 
ELSE 'WORK'END, * -- Esto sirve para tener en la primera oclumna la clasificacion del viaje
FROM flights

SELECT*,
CASE 
    WHEN dep_delay< 0  THEN 'IN ADVANCE' 
    WHEN dep_delay > 0 THEN 'DELAYED' 
    ELSE'ON TIME' END AS Status
FROM flights


--  ############ JOIN CLAUSE #########
--syntax 
--SELECT... FROM table1 xxxxx 
--JOIN table2 ON table2.column2 =table1.column1


--xxxxx: INNER / LEFT / RIGHT / FULL ...


--recover the flights to ORLANDO (MCO) including the number of seats of the plane
SELECT FL.*, PL.seats FROM flights FL 
INNER JOIN planes PL ON PL.tailnum=FL.tailnum 
WHERE FL.dest='MCO'


SELECT TOP 5 FL.*,PL.seats --asi recupera todos los campos de flights y ademas la columna seats de plane
FROM flights FL
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
WHERE FL.dest='MCO'--con el Where sacamos todos los vuelos de orlandi

--ponemos el seat al comienzo
SELECT TOP 5 PL.seats, FL.* --asi recupera todos los campos de flights y ademas la columna seats de plane
FROM flights FL
INNER JOIN planes PL ON PL.tailnum=FL.tailnum 
WHERE FL.dest='MCO'--con el Where sacamos todos los vuelos de orlandi

SELECT TOP 3 *
FROM flights FL
INNER JOIN airlines AR ON AR.carrier=FL.carrier

SELECT TOP 10 FL.*,RP.* --aca tenemos los datos de vuelos a Orlando y los de rp (airports)
FROM flights FL
INNER JOIN airports RP ON RP.faa=FL.dest 
WHERE rp.name LIKE '%ORLANDO%'


SELECT COUNT(*) AS FILAS_INNER --Cuentalo y ponle de nombre Fillas_INNer
FROM flights FL 
INNER JOIN airports RP ON RP.faa=FL.dest --INNER JOIN de flights con airports

SELECT 'LEFT' AS TIPO, COUNT(*) AS FILAS
FROM flights FL
LEFT JOIN airports RP ON RP.faa=fl.dest

SELECT 'RIGHT' AS TIPO, COUNT(*) AS FILAS
FROM flights FL
RIGHT JOIN airports RP ON RP.faa=fl.dest

SELECT 'FULL' AS TIPO, COUNT(*) AS FILAS --es el conjunto de INNER, RIGHT y LEFT si quitamos los duplicados
FROM flights FL
FULL OUTER JOIN airports RP ON RP.faa=fl.dest --se puede usar FULL JOIN o FULL OUTER JOIN da IGUAL


SELECT *
FROM flights FL
FULL OUTER JOIN airports RP ON RP.faa=fl.dest
WHERE FL.dest IS NULL AND RP.FAA IS NULL --muestra solo los vuelos con aeropuertos nulos


--recover all the flights and the name of the destination airport
SELECT COUNT(*)AS CUENTA --FL.*,RP.name
FROM flights FL 
INNER JOIN airports RP ON RP.faa=FL.dest


--Recover all flights to a destination over 3,000 ft high
SELECT *
FROM flights FL
LEFT JOIN airports RP ON RP.faa=FL.dest ----EL LEFT JOIN es mejor xq quiero sacar TODOS los vuelos. En cambio con Inner me hubiese quitado los registros que no tienen aeropuertos o aerolineas
WHERE RP.alt > 3000

--Recover all flights carried by Alaska Airlines
SELECT *
FROM flights FL
LEFT JOIN airlines RL ON RL.carrier=FL.carrier --EL LEFT JOIN es mejor xq quiero sacar TODOS los vuelos. En cambio con Inner me hubiese quitado los registros que no tienen aeropuertos o aerolineas
WHERE RL.name  LIKE '%ALASKA AIRLINES%'

--Recover all flights to a destination over 3,000 ft high carried by airlines which name stars by 'DELTA'
SELECT *
FROM flights FL
LEFT JOIN airports RP ON RP.faa=FL.dest
LEFT JOIN airlines RL ON RL.carrier=FL.carrier
WHERE RL.name  LIKE '%DELTA%' AND RP.alt > 3000


-- ############## GROUP CLAUSES ######################

-- MUCHAS VECES NECESITAMOS INFORMACION DE ALTO NIVEL
-- LOS DATOS RESUMIDOS MUCHAS VECES SON SUFICIENTES PARA DAR INFORMACION Y SON MAS FACIL DE TRATAR QUE LOS DATOS DETALLADOS
-- UTILIZAMOS GROUP BY PARA RESUMIR DATOS
-- GROUP BY AYUDA A REDUCIR LA NECESIDAD DE OTROS PROGRAMAS

-- SQL ES MAS FELXIBLE QUE R PARA AGRUPAR ESTE TIPO DE INFORMACION
-- SQL TRABAJA EN DISCO Y NO EN MEMORIA. DIFERENTE A R QUE TRABAJA DIRECTO EN MEMORIA Y CUANDO SE ACABA ESTAMOS FRITOS

-- The list of columns in the SELECT ... must not include columns which are not included in the GROUP BY list: only columns used in the GROUP BY
-- and aggregate functions may be used

-- In the ORDER BY clause the same principle is in force 

-- EL JOIN FROM Y WHERE SE HACEN ANTES DEL GROUP BY OSEA QUE SE HACE LA OPERACION Y LUEGO SE HACE EL GROUP BY


--PARA CADA AEROLINEA CALCULAR LA CAPACIDAD EN PLAZAS QUE TIENE CADA UNA PARA LLEVAR PERSONAS A ORLANDO
SELECT RL.carrier, SUM(PL.seats) AS Capacity 
FROM flights FL  
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
INNER JOIN airlines RL ON RL.carrier=FL.carrier 
WHERE FL.dest='MCO'
GROUP BY RL.carrier

--resultado: delta aerlines tralsado de NY a MCO 655566


--Si quisiera el nombre en vez del codigo
SELECT RL.name, SUM(PL.seats) AS Capacity  --colocamos name
FROM flights FL  
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
INNER JOIN airlines RL ON RL.carrier=FL.carrier 
WHERE FL.dest='MCO'
GROUP BY RL.name --colocamos name



SELECT RL.carrier, RL.name, SUM(PL.seats) AS Capacity  --colocamos name
FROM flights FL  
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
INNER JOIN airlines RL ON RL.carrier=FL.carrier 
WHERE FL.dest='MCO'
GROUP BY RL.name, RL.carrier --colocamos name

--para cada aerolinea que estuvo moviendo gente cual es el nombre del fabricante
SELECT RL.carrier, RL.name, PL.manufacturer, SUM(PL.seats) AS Capacity  --colocamos name
FROM flights FL  
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
INNER JOIN airlines RL ON RL.carrier=FL.carrier 
WHERE FL.dest='MCO'
GROUP BY RL.name, RL.carrier, PL.manufacturer --colocamos name
ORDER BY RL.name, PL.manufacturer
--con esto vemos que airbus tiene muy pocas plazas en el mercado americano.
-- Entonces pueden hacer una campana para potenciar su posicion en el mercado



-- recover the number of flights to a destination over 3,000 ft high grouped by destination 
SELECT  FL.dest AS Destino, COUNT(*) AS Number_flights
FROM flights FL
INNER JOIN airports RP ON RP.faa=FL.dest
WHERE RP.alt > 3000
GROUP BY FL.dest

-- recover the average number of seats for each destination for flights carried
-- by Alaska Airlines sorted by decreasing value of the average
SELECT AVG(PL.seats) AS Promedio_asientos, FL.dest FROM flights FL
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
INNER JOIN airlines RL ON RL.carrier=FL.carrier
WHERE RL.name LIKE '%ALASKA AIRLINES%'
GROUP BY FL.dest
ORDER BY  AVG(PL.seats) DESC 



-- ############### HAVING CLAUSE ######################

-- clausula condicional que sirve para filtrar los resultado de group by.
-- funciona una vez que haya terminado la agrupacion. Es posterior al group by
-- Se aplica el filtro en el WHERE o en el HAVING
-- Cuando se puede elegir poner el filtro. Ponerlo en el WHERE que es mas eficiente que ponerlo en el having

-- En SQL server, la clausula que mas restringa ponerla PRIMERO

--VER ARCHIVO ASEGURADOS.SQL con la practica


-- ##### EJEMPLO RANDOM CREADOS POR MI ######

-- dime aquellos asegurados que utilizaron un acto medico en el ultimo mes

SELECT TOP 5 CTNOMBRE, MONTH(SNFACTO) AS [Mes] --sumame el importe por nombre y por año de la prestacion
FROM ASEGURADOS AG 
INNER JOIN PRESTACIONES PR ON PR.SNIDASEGURADO=AG.CTIDASEG --traeme las coincidencias entre la tabla asegurado y prestaciones
WHERE MONTH(SNFACTO) = MONTH(GETDATE())
GROUP BY CTNOMBRE, MONTH(SNFACTO) --pero agrupado por nombre y por año de la prestacion
ORDER BY CTNOMBRE, MONTH(SNFACTO) -- ordenado por nombre del asegurado y anio de la prest

-- dime lo mismo mas el costo del acto medico

SELECT TOP 5 CTNOMBRE, MONTH(SNFACTO) AS [Mes], SUM(SNIMPORTE) AS COSTE --sumame el importe por nombre y por año de la prestacion
FROM ASEGURADOS AG 
INNER JOIN PRESTACIONES PR ON PR.SNIDASEGURADO=AG.CTIDASEG --traeme las coincidencias entre la tabla asegurado y prestaciones
WHERE MONTH(SNFACTO) = MONTH(GETDATE())
GROUP BY CTNOMBRE, MONTH(SNFACTO) --pero agrupado por nombre y por año de la prestacion
ORDER BY CTNOMBRE, MONTH(SNFACTO) -- ordenado por nombre del asegurado y anio de la prest


-- dime lo mismo pero solo aquellos que gastaron mas de 200 euros
SELECT TOP 5 CTNOMBRE, MONTH(SNFACTO) AS [Mes], SUM(SNIMPORTE) AS COSTE --sumame el importe por nombre y por año de la prestacion
FROM ASEGURADOS AG 
INNER JOIN PRESTACIONES PR ON PR.SNIDASEGURADO=AG.CTIDASEG --traeme las coincidencias entre la tabla asegurado y prestaciones
WHERE MONTH(SNFACTO) = MONTH(GETDATE())
GROUP BY CTNOMBRE, MONTH(SNFACTO) --pero agrupado por nombre y por año de la prestacion
HAVING SUM(SNIMPORTE) > 200
ORDER BY CTNOMBRE, MONTH(SNFACTO) -- ordenado por nombre del asegurado y anio de la prest


-- EXAMEN BBDD RELACIONALES SQL: 6 DE FEBRERO DE 2019
--PREGUNTA 1: Número y coste total de las prestaciones que han sido grabadas 6 meses después de la ocurrencia del acto

SELECT COUNT (*) AS [No de prestaciones], CONVERT(MONEY, SUM(SNIMPORTE))
AS [Suma de importes], DATEDIFF(MONTH, SNFACTO, SNFGRAB) AS [Diferencia en
meses]
-- Se realiza el conteo, la suma de los importes y se crea una nueva columna que simboliza la diferencia de meses entre la fecha del acto y la fecha en la que se grabó el acto.
FROM PRESTACIONES
WHERE DATEDIFF(MONTH, SNFACTO, SNFGRAB) >= 6
-- Se filtra por aquellos que tengan una diferencia menor/igual que 6 meses GROUP BY DATEDIFF(MONTH, SNFACTO, SNFGRAB)
-- Se agrupa por esa columna de diferencia
ORDER BY DATEDIFF(MONTH, SNFACTO, SNFGRAB) ASC
-- Y se ordena por esa columna creada de forma ascendente (se cree mejor este tipo de ordenación)




-- PREGUNTA 2: Creación vista AsegEdad.

CREATE VIEW AsegEdad AS 
-- Se crea la vista con el correspondiente nombre
SELECT CTIDASEG AS [ID Asegurado], CTDELEGACION AS [Sucursal Asegurado], CTPRIMA AS [Prima anual], CTFNAC AS [Fecha de nacimiento], CTCATEGORIA AS [Categoría Seguro], SNIDPREST [ID prestación], SNESPECIALIDAD AS [ID Especialidad], SNFACTO [Fecha del acto], SNFGRAB AS [Fecha Grabación Acto], SNIMPORTE AS [Coste Acto], DATEDIFF(MONTH, CTFNAC, SNFACTO) AS [Edad en meses]
-- Para una mejor comprensión se han renombrado las variables a elegir (todas las de las tablas ASEGURADOS y PRESTACIONES). Cabe destacar que la columna SNIDASEGURADO no se ha seleccionado para evitar duplicidades
FROM ASEGURADOS
INNER JOIN PRESTACIONES ON SNIDASEGURADO=CTIDASEG
-- Se realiza un inner join (todas las que estén en ambas tablas) y se une por la columna que ambas tablas tienen en común correspondiente al ID del asegurado.


-- PREGUNTA 3: Coste, conteo y promedio agrupados por meses.

SELECT ROUND(SUM([Coste Acto]),2) AS [Suma de los actos], COUNT(*) AS [No
Prestaciones], CONVERT(MONEY, SUM([Coste Acto])/COUNT(*)) AS [Promedio],
[Edad en Meses]
-- Se seleccionan todas las variables que se van usar, convirtiendo a dinero para evitar excesivos decimales. Se usan los nombres introducidos en la vista
FROM AsegEdad
GROUP BY [Edad en Meses]
ORDER BY [Edad en Meses] ASC
-- Se agrupa y se ordena por la edad en meses para una mejor visualización

-- PREGUNTA 4: Suma término amortizativo por sucursal en dicho año.

SELECT CONVERT(MONEY, SUM(CUOTA)) AS [Suma de las cuotas], SUCURSAL
FROM PRESTAMO
-- Se seleccionan las variables cuota (para su suma) y la sucursal
WHERE INICIO BETWEEN CONVERT(date,'01/07/2017', 103) AND CONVERT(date,'30/06/2018',103)
-- Se filtra por la fecha de inicio si está entre ese rango temporal GROUP BY SUCURSAL
ORDER BY [Suma de las cuotas] DESC
-- Se agrupa por sucursal y se ordena (para verlo mejor) por la suma de las cuotas

-- PREGUNTA 5: Capital pendiente

CREATE FUNCTION dbo.CapitalPendiente
(@CUOTA money, @DURACION int, @INICIO date)
-- Se definen las variables Cuota (dinero), Duración (en años) e Inicio (fecha)
RETURNS money
-- Se determina que la salida debe ser de tipo money.
AS
BEGIN
DECLARE @Capital money
-- Se declara la variable Capital, que será el resultado que devuelva la función.
IF CONVERT(INT, LEFT(@INICIO, 4)) + @DURACION < 2018
-- Se seleccionan los cuatro primeros caracteres de la fecha de inicio (correspondientes al año) y se le suma la duración. Si ese número es menor de 2018 el préstamo habrá sido amortizado ya en su totalidad y el capital pendiente será 0.
BEGIN
SELECT @Capital = 0 END
IF CONVERT(INT, LEFT(@INICIO, 4)) + @DURACION > 2018 BEGIN
SELECT @Capital = @CUOTA*(1 - POWER(1+0.03000000, - (CONVERT(INT, LEFT(@INICIO,4)) + @DURACION - 2018)))/0.03000000
-- Si el préstamo estuviese vivo todavía, se halla el capital pendiente con
la fórmula superior. Se sigue el mismo razonamiento que se uso en el IF, se suma el año de inicio y la duración y se le resta 2018. Así se consiguen los años que faltan por amortizar.
    END
RETURN @Capital
-- Se devuelve el capital pendiente
END
-- Cada vez que se empieza/acaba una parte se pone el BEGIN y END para señalizarlo
SELECT dbo.CapitalPendiente(CUOTA, DURACION, INICIO) AS [Capital Pendiente] FROM PRESTAMO
-- Se prueba la fórmula creada con los datos de las tablas que se tiene.



-- PRACTICAS DE FECHAS

SELECT DATEDIFF(day, CTFALTA, CTFBAJA) AS 'Duration'  FROM ASEGURADOS  

SELECT * FROM planes
SELECT manufacturer, [2017], [2018], [2019], [2020] FROM
(SELECT PL.manufacturer, FL.tailnum, FL.[year] FROM flights FL
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
WHERE PL.manufacturer LIKE '%Airbus%') DATOS
PIVOT 
(
    COUNT(tailnum)
    FOR [year] IN ([2017], [2018], [2019], [2020])
) AS PIVOT_TABLE