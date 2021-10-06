USE CUNEFDSM

--Guardar en cunefdsm una tabla que contenga todos los vuelos con origen
--JFK pero solamente, el numero de vuelo, hora, nombre aerolinea, numero de cola del avion, aeropuerto de destino y la distancia

SELECT f.flight, f.time_hour, a.name, f.tailnum, f.dest, f.distance 
--INTO flightsJFK_withCarrier_277 
FROM CUNEFDS.dbo.flights f --siempre que los datos vengan de otra tabla debemos poner el nombre de esa tabla.dbo
INNER JOIN CUNEFDS.dbo.airlines a ON f.carrier= a.carrier
WHERE f.origin= 'JFK'


SELECT TOP 10  * FROM flightsJFK_withCarrier_277 

DROP TABLE flightsJFK_withCarrier_277 --para borrar la tabla creada

DROP TABLE airport_277
--poner los ultimos 3 digitos de su movil


-- Pra insertar valores en una tabla se utiliza el comando INSERT INTO

--Primero para el ejemplo, duplicamos la tabla airports
SELECT * 
INTO airport_277 
FROM CUNEFDS.dbo.airports

--Agregamos datos de Madrid y barcelona a la tabla airport_277
INSERT INTO airport_277(faa, [name], lat, lon, alt, tz, dst, tzone)
VALUES
('MAD', 'Madrid Barajas Airport', 40.48, -3.56, 2001, 1, 'A', 'Europe/Paris'),
('BCN', 'Barcelona-El Prat Airport', 41.29, 2.08, 26, 1, 'A', 'Europe/Paris')

--Vemos como estan saliendo los resultados
SELECT * FROM airport_277


--Actualizamos datos del aeropuerto de Madrid con el comando UPDATE
UPDATE airport_277
SET [name] = 'Adolfo Suarez Madrid-Barajas Airport'
WHERE faa= 'MAD' --muy importante poner el where sino modificara todos las tablas
--El where sirve como condicion.


--Borramos los datos del aeropuerto de Barcelona. IMPORTANTE poner el where sino borrara todo
DELETE FROM airport_277
WHERE faa= 'BCN' --quitamos el aeropuerto de barcelona. 



--Let’s create a new table called aerolineas that contains a copy of the airlines table.
--PRIMERO BORRAMOS airlines_277 xq en el ejemplo de clase el profe lo hizo mal
--DROP TABLE airlines_277

--Ahora si creamos la tabla aerolineas
SELECT * 
INTO airlines_277
FROM CUNEFDS.dbo.airlines

--Vemos como quedo
SELECT * FROM airlines_277

--Le agregamos valores a la tabla
INSERT INTO airlines_277 (carrier,[name])
VALUES
('IB','Iberia, Compañia Aérea de Transporte'),
('SP', 'Spanair S.A.')

--Vemos los resultados de nuevo
SELECT * FROM airlines_277

--Borramos los datos de SP
DELETE FROM airlines_277 WHERE carrier='SP'


UPDATE airlines_277 
SET [name] = 'Iberia Líneas Aéreas de España S.A.' 
WHERE carrier = 'IB'

DROP TABLE airlines_277
DROP TABLE airport_277

