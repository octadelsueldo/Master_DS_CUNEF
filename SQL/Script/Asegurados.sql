USE CUNEFDS

--Cuenta la cantidad de filas en la columna PRESTACIONES
SELECT COUNT(*) 
FROM PRESTACIONES

--Cuenta la cantidad de filas en la columna ASEGURADOS
SELECT COUNT(*) 
FROM ASEGURADOS

SELECT TOP 100 * FROM PRESTACIONES
SELECT TOP 100 * FROM ASEGURADOS
SELECT TOP 100 * FROM ACTMED

--Traeme para cada acto medico, los tres primeros nombres de la especialidad correspondiente
SELECT TOP 3 *
FROM ACTMED
INNER JOIN ESPECIALIDAD ON ESPECIALIDAD.ESPCODIGO=ACTMED.ACCODESPEC--para cada acto medico el nombre de la especialidad correspondiente


-- Otra forma de escribir lo anterior con un select dentro del select. Siempre que hay un select dentro del select debo ponerle
-- un nombre a ese select interno.
SELECT * FROM
(SELECT * FROM ACTMED 
INNER JOIN ESPECIALIDAD ON ESPCODIGO = ACCODESPEC) NUEVO -- esto es lo mismo que si lo que esta entre parentesis es una lista

-- ######################  HAVING CLAUSE  #############################


-- TRABAJO
-- Quiero saber cual ha sido el coste gastado por asegurado 
-- y año de las prestaciones?

SELECT TOP 5 CTNOMBRE, YEAR(SNFACTO) AS [Año], SUM(SNIMPORTE) AS COSTE --sumame el importe por nombre y por año de la prestacion
FROM ASEGURADOS AG 
INNER JOIN PRESTACIONES PR ON PR.SNIDASEGURADO=AG.CTIDASEG --traeme las coincidencias entre la tabla asegurado y prestaciones
GROUP BY CTNOMBRE, YEAR(SNFACTO) --pero agrupado por nombre y por año de la prestacion
ORDER BY CTNOMBRE, YEAR(SNFACTO) -- ordenado por nombre del asegurado y anio de la prest


--Traeme lo anterior pero SOLAMENTE los que tienen un importe por arriba de 2000
SELECT CTNOMBRE, YEAR(SNFACTO) AS [Año], SUM(SNIMPORTE) AS COSTE --sumame el importe por nombre y por año de la prestacion
FROM ASEGURADOS AG 
INNER JOIN PRESTACIONES PR ON PR.SNIDASEGURADO=AG.CTIDASEG
GROUP BY CTNOMBRE, YEAR(SNFACTO) --agrupado por nombre y por año de la prestacion
HAVING SUM(SNIMPORTE) > 2000 
ORDER BY CTNOMBRE, YEAR(SNFACTO)

--EJERCICIOS DE REPASO HAVING CLAUSE

--recover the airports with a total number of flights above 1000 served by Delta 
--airlines(explore both two solutions and evaluate performance)
SELECT RP.name, COUNT(FL.tailnum) AS NUM_VUELOS FROM flights FL
INNER JOIN airports RP ON RP.faa=FL.dest
INNER JOIN airlines RL ON RL.carrier=FL.carrier
WHERE RL.name LIKE '%DELTA%'
GROUP BY RP.name
HAVING COUNT(FL.tailnum) > 1000

--recover the airports receiving less than 5 Delta flights each and every day
SELECT RP.name, RL.name, COUNT(FL.tailnum) AS NUM_VUELOS FROM flights FL
INNER JOIN airports RP ON RP.faa=FL.dest
INNER JOIN airlines RL ON RL.carrier=FL.carrier
WHERE RL.name LIKE '%DELTA%'
GROUP BY RP.name, RL.name
HAVING COUNT(FL.tailnum) <= 5


-- ############ EJERCICIOS PARA ENTREGAR #############

-- EN LA PARTE DE SEGUROS TENEMOS PRESTACIONES Y PERSONAS. HAY QUE DETERMINAR 
-- CUANTAS PERSONAS NO TIENEN PRESTACIONES Y CUANTAS PRESTACIONES
-- NO TIENEN ASEGURADO.
USE CUNEFDS

--Asegurados sin prestaciones
SELECT AG.CTIDASEG, PR.SNIDPREST FROM PRESTACIONES PR
FULL OUTER JOIN ASEGURADOS AG ON AG.CTIDASEG=PR.SNIDASEGURADO
WHERE SNIDPREST IS NULL --AND CTIDASEG IS NOT NULL
GROUP BY AG.CTIDASEG, PR.SNIDPREST --4813 asegurados sin prestaciones

--Prestaciones sin asegurado
SELECT AG.CTIDASEG, PR.SNIDPREST FROM PRESTACIONES PR
FULL OUTER JOIN ASEGURADOS AG ON AG.CTIDASEG=PR.SNIDASEGURADO
WHERE CTIDASEG IS NULL --AND SNIDPREST IS NOT NULL
GROUP BY AG.CTIDASEG, PR.SNIDPREST --7495  prestaciones sin asegurado

--Prestaciones sin asegurado y asegurados sin prestaciones
SELECT AG.CTIDASEG, PR.SNIDPREST FROM PRESTACIONES PR
FULL OUTER JOIN ASEGURADOS AG ON AG.CTIDASEG=PR.SNIDASEGURADO
WHERE SNIDPREST IS NULL AND CTIDASEG IS NULL
GROUP BY AG.CTIDASEG, PR.SNIDPREST --0

--CALIDAD DE LOS DATOS BUSCAR CUANTAS PRESTACIONES NO TIENEN UNA ESPECIALIDAD
-- MEDICA VALIDA O UN ACTO MEDICO VALIDO

SELECT PR.SNIDPREST, PR.SNESPECIALIDAD FROM PRESTACIONES PR
LEFT JOIN ACTMED AC ON AC.ACCODACTO=PR.SNACTO
WHERE PR.SNESPECIALIDAD IS NULL 
GROUP BY PR.SNIDPREST, PR.SNESPECIALIDAD


-- ADEMAS, QUE ACTOS MEDICOS NO TIENEN PRESTACIONES. ESTOS SON ACTOS MEDICOS
-- QUE ESTAN CUBIERTOS POR LA COMPAÑIA DE SEGURO QUE DA IGUAL SI LOS OFRECEN
-- PORQ NO SE ESTAN UTILIZANDO

SELECT AC.ACNOMBRE, PR.SNIDPREST,PR.SNIMPORTE FROM PRESTACIONES PR
RIGHT JOIN ACTMED AC ON AC.ACCODACTO=PR.SNACTO
WHERE PR.SNIDPREST IS NULL
GROUP BY AC.ACNOMBRE, PR.SNIDPREST, PR.SNIMPORTE


-- ########################  AGREGATE FUNCTIONS ########################
--EJERCICIO 1
--We wonder if the departure delay is somehow related to the number of seats of the plane; 
--please prepare the data to determine if the delay is somehow related to the number of seats of the plane
-- (for example, this data could be used in R)
SELECT seats, MIN(dep_delay) AS minimo, AVG(dep_delay) as media, MAX(dep_delay) AS maximo 
FROM flights FL
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
WHERE seats IS NOT NULL
GROUP BY seats
ORDER BY seats

--EJERCICIO 2
--Same case but giving three ranges for the number of seats: 1-50    51-150    151-
SELECT 
Grupo = CASE 
    WHEN seats between 1 and 50 THEN 'S1' --Creamos el case agrupado por grupos con nombres S1, S2 y S3
    WHEN seats between 51 and 150 THEN 'S2'
    WHEN seats >= 151 THEN 'S3'
END,
 MIN(dep_delay) AS minimo, AVG(dep_delay) as media, MAX(dep_delay) AS maximo
FROM flights FL
INNER JOIN planes PL ON PL.tailnum=FL.tailnum
WHERE seats IS NOT NULL
GROUP BY  CASE  --Colocamos en el group by el case que esta en el select. El group by no entiende por Grupo, entiendo por la funcion que esta dentro
    WHEN seats between 1 and 50 THEN 'S1'
    WHEN seats between 51 and 150 THEN 'S2'
    WHEN seats >= 151 THEN 'S3'
END
ORDER BY Grupo --ordena los datos por grupo


-- ################### PIVOT ######################

-- Base para el analisis bidimensional. Por ej las aerolineas en filas y los aeropuertos en columnas

-- convierte algunos valores de una columna en diferentes columnas

-- Trabaja con valores agrupados. Parecido al group by solo que los resultados salen por filas y columnas. En cambio en group by salen solo por filas

-- Trabaja igual que las tablas dinamicas en excel. 

-- Funciones agregado en PIVOT: count, sum, min, max, avg, etc

-- PIVOT is a compound clause: you need to specify the original column to aggregate as well as the values of an original column
-- to build the result’s columns:

-- SELECT * FROM
-- (select all data you will need command) DATA
-- PIVOT (
-- aggregate function(column to aggregate)
-- FOR column IN (list of [values] for new columns separated by commas)
-- ) AS PIVOT_TABLE

-- EJEMPLO 1
-- Fabricar una tabla para cada aeropuerto de NY en columnas. Cuantos vuelos han salido para cada una de las aerolineas?


--Aqui averiguo cuantos aeropuertos tiene nueva york
USE CUNEFDS
SELECT DISTINCT origin FROM flights --vuelos de nueva york


SELECT carrier, EWR, LGA, JFK FROM --carrier primera columna, EWR segunda, etc
(SELECT carrier, Fl.tailnum, FL.origin FROM flights FL --INNER JOIN airlines RL ON RL.carrier=FL.carrier --INNER JOIN airports RP ON RP.faa=FL.dest
) DATOS  --recupera carrier xq va a parecer en filas, tailnum xq aparece en columnas
PIVOT ( 
    COUNT(tailnum) --va a hacer la cuenta de tailnum. Debo poner el nombre de un campo, cualquiera que sea 
FOR origin IN ([EWR], [JFK], [LGA])
)AS PIVOT_TABLE

--EJEMPLO 2 de NY con funciones agregadas

SELECT carrier,
SUM(CASE origin WHEN 'EWR' THEN 1 ELSE 0 END) AS EWR,
SUM(CASE origin WHEN 'JFK' THEN 1 ELSE 0 END) AS JFK,
SUM(CASE origin WHEN 'LGA' THEN 1 ELSE 0 END) AS LGA
FROM flights
GROUP BY carrier 

--EJEMPLO 3
--Crear una tabla que muestre el importe por la especialidad tratadas por numero de delegacion (of comercial que presta el servicio)
USE CUNEFDSM
SELECT DISTINCT SNDELEGACION FROM PRESTACIONES --con esto saco el numero de delegaciones


SELECT ESPNOMBRE, [1],[10],[12],[14],[2],[3],[4],[5],[99]  FROM
(SELECT ESPNOMBRE, SNDELEGACION, SNIMPORTE FROM PRESTACIONES
INNER JOIN ESPECIALIDAD ON ESPCODIGO=SNESPECIALIDAD) DATOS --DATOS va siempre para nombrar la tabla cuando se ponga un select dentro del select 
PIVOT (
    SUM(SNIMPORTE)
    FOR SNDELEGACION IN ([1],[10],[12],[14],[2],[3],[4],[5],[99])
) AS TABDIN


--EJERCICIOS
--EJERCICIO (1)
--calculate the average departure delay for each airline and airport of origin
-- (airlines in rows - airports in columns - average in cells)
USE CUNEFDS

SELECT DISTINCT origin FROM flights --muestra los aeropuerto de origen de la tabla airline

SELECT [name],[EWR],[JFK],[LGA] FROM 
(SELECT RL.name, origin, dep_delay FROM flights FL
INNER JOIN airlines RL ON RL.carrier=FL.carrier
INNER JOIN airports RP ON RP.faa=FL.dest) DATOS 
PIVOT 
(
    AVG(dep_delay)
    FOR origin IN([EWR],[JFK],[LGA])
) AS PIVOT_TABLE

--EJERCICIO (2)
--calculate the average departure delay, only for positive delays, 
--for each airline and airport of origin (this corresponds to a conditional 
--average)(airlines in rows - airports in columns - average in cells) 

SELECT [name],[EWR],[JFK],[LGA] FROM 
(SELECT RL.name, origin, dep_delay FROM flights FL
INNER JOIN airlines RL ON RL.carrier=FL.carrier
INNER JOIN airports RP ON RP.faa=FL.dest
WHERE FL.dep_delay > 0) DATOS 
PIVOT 
(
    AVG(dep_delay)
    FOR origin IN([EWR],[JFK],[LGA])
) AS PIVOT_TABLE

-- EJERCICIO (3)
-- get the number of flights from each airport of origin for the top 5 airlines by total
-- number of flights and a column with the total of flights of the other airlines
USE CUNEFDS
SELECT DISTINCT [name] FROM airlines

-- LO SIGUIENTE NO FUNCIONA

SELECT TOP 5 RL.name, FL.origin, COUNT(FL.tailnum) AS NUM_FLIGHTS FROM flights FL
INNER JOIN airlines RL ON RL.carrier=FL.carrier
GROUP BY RL.name, FL.origin
HAVING COUNT(*) > 100
ORDER BY COUNT(*) ASC



SELECT TOP 5 RL.[name], [EWR],[JFK],[LGA] FROM
(SELECT RL.carrier, RL.name FL.origin FROM flights FL
INNER JOIN airlines RL ON RL.carrier=FL.carrier) DATOS 
PIVOT 
(
COUNT(carrier) FOR origin IN ([EWR],[JFK],[LGA]))
) AS PIVOT_TABLE
ORDER BY [EWR]+[JFK]+[LGA] DESC
--



SELECT TOP 5 [name], [EWR],[JFK],[LGA] FROM
(SELECT origin,RL.carrier, RL.[name] FROM flights FL
INNER JOIN airlines RL ON RL.carrier=FL.carrier) DATOS
PIVOT (
    COUNT(carrier) 
    FOR origin IN ([EWR],[JFK],[LGA])) AS PIVOT_TABLE
ORDER BY [EWR]+[JFK]+[LGA] DESC



SELECT TOP 5 [name], [EWR],[JFK],[LGA] FROM 
(
		SELECT origin,RL.carrier, RL.[name] FROM flights FL 
		INNER JOIN airlines RL ON RL.carrier=FL.carrier
) DATOS
PIVOT (
COUNT(carrier) FOR origin IN ([EWR],[JFK],[LGA]) 
) AS PIVOT_TABLE
ORDER BY [EWR]+[JFK]+[LGA] DESC