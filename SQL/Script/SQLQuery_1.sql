-- Comentarios 

/* Comentarios

Varias lineas 
*/

--recupera las tres primeras filas con select top 3
SELECT TOP 3 carrier AS Airline, [name] AS FullName 
FROM airlines ALN --Tabla ALN
ORDER BY [name] DESC  --se debe hacer poniendo el nombre original y no el renombrado
-- Se puede poner el sinonimo pero es una BUENA PRACTICA no utilizar los sinonimos fuera de lo que seria el SELECT FROM

SELECT TOP 10 
faa AS AirportCode, [name], lat AS Latitude, lon AS Longitute, alt AS Altitude, tz AS TimeZone, dst AS DaySavingTime, tzone AS UsoHorario
FROM airports


--Recover the tail number, the manufacturer, the model and the number of seats of the first hundred planes using synonyms
SELECT TOP 100 tailnum AS TAILNUM, manufacturer AS FABRICANTE, model AS MODEL, seats AS ACIENTO --recover only a limited number of rows
FROM planes 


SELECT TOP 10 --recover only a limited number of rows
tailnum AS tailn, year AS Year, model, engines, manufacturer 
FROM planes
ORDER BY year DESC


SELECT 3 + 5 AS Resultado  -- Crea la tabla con resultado igual a 8


SELECT TOP 10 *, dep_time - sched_dep_time FROM flights -- hay que pasar el tiempo a HORAS 

SELECT NEWID() --numero aleatorio que no se repite jamas

SELECT TOP 10 seats 
FROM PLANES
ORDER BY NEWID() -- permite extraer de forma aleatorio simple una muestra de 5 o 10 elementos que no se repiten nunca

--NEWID junto con TOP sirve para sacar muestras

SELECT ROUND(10000.0000 / 3.0000 , -1) AS RESULTADO  --con el numero -1 redondea a decimales y con -2 redondea a centenas y luego a millares

SELECT SQRT(100) AS RESULTADO --returns the square root of a positive real number

SELECT POWER(2,3) AS RESULTADO  --2 elevado a la 3

SELECT POWER(2,0.5) AS RESULTADO --2 elevado a la 0.5 nos da 1 por redondeo
 


SELECT COUNT(*) FROM T -- cuenta la cantidad de observaciones de la columna T

SELECT SUM(t) FROM T -- esto es 150*151 /2 (150 son las observaciones de t)

SELECT GETDATE() --nos dice la hora en este momento

SELECT DATEADD(MM,5,'2020-10-05') --SQL toma la fecha en str si esta bien escrita

--MM significa mes, es decir en esta funcion nos da la fecha de hoy en cinco meses
-- WK se usa para semanas

SELECT EOMONTH(GETDATE(),0) --esto nos dice la fecha final del mes actual

SELECT EOMONTH(GETDATE(),-4) --el ultimo dia del mes de hace cuatro meses

SELECT EOMONTH(GETDATE(), -MONTH(GETDATE())) --esta la fecha final de ejercicio del mes del ano pasado



DECLARE @a FLOAT
SET @a = 2
SELECT POWER(@a, 2) 

SELECT GETDATE() 

SELECT DATEDIFF(QQ, 0, '2020-10-05 21:01:09.663')
SELECT DATEADD(QUARTER,DATEDIFF(QUARTER,0,'2020-10-05 21:01:09.663'),-1) --Calcula el ultimo dia del trimestre anterior

SELECT SUBSTRING('abcdef',3,2) AS RESULT --substring(expression ,start , length)

SELECT TRIM('      TEXT     ') AS RESULT --elimina los espacios en blanco

SELECT LEN('abcdef') AS RESULT --largo del string

SELECT REPLACE('      TEXT     ','  ','-') AS RESULT --reemplaza los espacios por guiones
-- REPLACE ( string_expression , string_pattern , string_replacement )

SELECT ABS(-5) AS RESULT





--Aggregate functions
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


SELECT 
MIN(arr_delay) AS MINDELAY, --minimo
AVG(arr_delay) AS AVGDELAY, --media
MAX(arr_delay) AS MAXDELAY  --maximo
FROM flights FL --hora y media adelantado y 25 horas de retraso. El promedio 7 min

SELECT COUNT(*) FROM flights

SELECT AVG(seats)
FROM planes

-- Please determine the quality of the average delay by calculating the relative standard deviation (desviacion tipica / media) Coeficiente de variacion
SELECT STDEVP(arr_delay) / AVG(arr_delay) AS CV--coeficiente de variacion para calcular la calidad del retraso. La desviacion tipica es 8 veces superior a la media es decir una dispersion muy grande
FROM flights FL
WHERE dest = 'MIA' --calcula el retraso minimo, max, desv, coefi de variacion con destino miami. Aqui la media mejora pero la incertidumbre respecto al retraso real a aumentado muchisimo.

--la media sin la varianza no tiene ningun sentido



--WHERE CLAUSE
--SQL filter clause
--allows to filter rows using multiple conditions

SELECT COUNT(*) FROM airports WHERE alt >= 3000
SELECT * FROM airports WHERE alt >= 3000


SELECT COUNT(*) FROM airports WHERE alt < 3000

--retrieve all airports from Chicago
SELECT faa FROM airports WHERE [name] LIKE '%CHICAGO%' --cuando usamos %nombre% significa que me va a filtrar todo aquello que contenga chicago independientemen lo q tenga por delante o por detras de la palabra chicago

--cuando hacemos busqueda de texto lo comun es usar LIKE y en numeros se usa el simbolo igual. Like usa mas recursos del ordenador

--Like es una busqueda aproximada


--retrieve all planes made by AIRBUS
SELECT COUNT(*) FROM planes WHERE manufacturer = 'AIRBUS'
SELECT COUNT(*) FROM planes WHERE manufacturer LIKE '%AIRBUS%' --Cambiar por el LIKE y %Airbus% nos muestra mas datos porq muestras otros modelos de Airbus y asi suman 200 aviones mas

--retrieve all planes with a number of seats between 200 and 250
SELECT COUNT(*) FROM planes WHERE seats BETWEEN 200 AND 250

--retrieve all planes made by AIRBUS with 150 seats or less 

SELECT COUNT(*) FROM planes WHERE manufacturer = 'AIRBUS' AND seats <= 150

SELECT COUNT(*) FROM planes WHERE manufacturer LIKE '%AIRBUS%' AND seats <= 150

SELECT COUNT(*) FROM flights WHERE dep_delay > 120 --vuelos con retraso de dos horas

----retrieve all flights to CHIGAGO airports with a departure delay longer than two hours
SELECT COUNT(*) FROM flights 
WHERE dep_delay > 120 AND dest IN(SELECT faa FROM airports WHERE [name] LIKE '%CHICAGO%') --vuelos con 2 horas de retraso y como destino el aeropuerto de chicago. faa es el codigo unico del aeropuerto


SELECT* FROM flights
WHERE dep_delay > 200 AND dep_delay IS NOT NULL  AND dest IN(SELECT faa FROM airports WHERE [name] LIKE'%CHICAGO%') --IS NOT NUL debe aparecer xq si hubiera algun vuelo con retraso no informado tmb apareceria



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



-- SELECT DISTINCT CLAUSE
--recovers all unique combinations of field values

SELECT DISTINCT manufacturer 
FROM planes ORDER BY manufacturer --de planes busca los pocos fabricantes distintos que hay. No muestra todos solo los distintos. Ordenados por fabricantes en orden alfabetico



SELECT DISTINCT manufacturer, model 
FROM planes ORDER BY manufacturer  --para cada fabricante sus modelos distintos


SELECT DISTINCT manufacturer, COUNT(*) AS AVIONES
FROM planes
GROUP BY manufacturer  --agrupa por fabricante
ORDER BY manufacturer  --ordena por fabricante


SELECT DISTINCT model FROM planes 
WHERE manufacturer LIKE '%AIRBUS%' AND seats < 150


--FUNCION CASE-- PARA CONCATENAR CONDICIONES
SELECT TOP 5 *, 
CASE dest 
WHEN 'MIA'THEN 'HOLIDAYS'
WHEN 'MCO'THEN 'HOLIDAYS' 
ELSE 'WORK'END 
FROM flights

SELECT TOP 5 *,
ALTITUD = CASE  
WHEN alt > 2000 THEN 'alto' 
WHEN alt between 1000 and 1999 THEN 'mediano'
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
ELSE 'WORK'END, * --ESTO sirve para tener en la primera oclumna la clasificacion del viaje
FROM flights

SELECT*,
CASE 
    WHEN dep_delay< 0  THEN 'IN ADVANCE' 
    WHEN dep_delay > 0 THEN 'DELAYED' 
    ELSE'ON TIME' END AS Status
FROM flights

--JOIN CLAUSE
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
SELECT TOP 5 PL.seats , FL.* --asi recupera todos los campos de flights y ademas la columna seats de plane
FROM flights FL
INNER JOIN planes PL ON PL.tailnum=FL.tailnum WHERE FL.dest='MCO'--con el Where sacamos todos los vuelos de orlandi

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
FULL JOIN airports RP ON RP.faa=fl.dest
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


-- GROUP CLAUSES

--MUCHAS VECES NECESITAMO INFORMACION DE ALTO NIVEL
--LOS DATOS RESUMIDOS MUCHAS VECES SON SUFICIENTES PARA DAR INFORMACION Y SON MAS FACIL DE TRATAR QUE LOS DATOS DETALLADOS
-- UTILIZAMOS GROUP BY PARA RESUMIR DATOS
-- GROUP BY AYUDA A REDUCIR LA NECESIDAD DE OTROS PROGRAMAS

--SQL ES MAS FELXIBLE QUE R PARA AGRUPAR ESTE TIPO DE INFORMACION
--SQL TRABAJ EN DISCO Y NO EN MEMORIA. DIFERENTE A R QUE TRABAJA DIRECTO EN MEMORIA Y CUANDO SE ACABA ESTAMOS FRITOS

--The list of columns in the SELECT ... must not include columns which are not included in the GROUP BY list: only columns used in the GROUP BY and aggregate functions may be used

--In the ORDER BY clause the same principle is in force 

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
--con esto vemos que airbus tiene muy pocas plazas en el mercado americano. Entonces pueden hacer una campana para potencial su posicion en el mercado



-- recover the number of flights to a destination over 3,000 ft high
-- grouped by destination 

SELECT  FL.dest AS Destino, COUNT(*) AS Number_flights
FROM flights FL
INNER JOIN airports RP ON RP.faa=FL.dest
WHERE RP.alt > 3000
GROUP BY FL.dest

--recover the average number of seats for each destination for flights carried
-- by Alaska Airlines sorted by decreasing value of the average

SELECT AVG(PL.seats) AS Promedio_asientos, FL.dest FROM flights FL
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
INNER JOIN airlines RL ON RL.carrier=FL.carrier
WHERE RL.name LIKE '%ALASKA AIRLINES%'
GROUP BY FL.dest
ORDER BY  AVG(PL.seats) DESC 






-- HAVING CLAUSE

-- clausula condicional que sirve para filtrar los resultado de group by.
-- funciona una vez que haya terminado la agrupacion. Es posterior al group by
-- Se aplica el filtro en el WHERE o en el HAVING
-- Cuando se puede elegir poner el filtro. Ponerlo en el WHERE que es mas eficiente que ponerlo en el having

-- En SQL server, la clausula que mas restringa ponerla PRIMERO

--VER ARCHIVO ASEGURADOS.SQL con la practica













