EXECUTE[sys].[sp_execute_external_script] 
@input_data_1 = N'SQL command to select all data you will need ', --va a ser un texto para el programa SI o si en comillas y con N adelante para indicarle q lo q esta dentro es una cadena unicod
@language = N'R', --lenguaje que vamos a utilizar R o Python
@script =N
'data is loaded into InputDataSet(dataframe) 
program processes data and delivers a dataframe 
results are returned in OutputDataSet(dataframe)' --script que nos diga que es lo que tenga que hacer. Script del lenguaje tecnico que vamos a utilizar. 1ra parte para recuperacion de los datos SQl. Siempre vienen en el comando InputDAtaSet. Segunda parte del script el programa que procesa los datos y genera un dataframe. Ultimo, los datos de salida se tienen que guardar en un OutDataSet
WITH RESULT SETS ((columns separated by commas)) --poner los nombres de las columnas y el tipo de las columnas


--Calculate the number of claims, the average cost, the standard deviation
-- of the cost and the relative standard deviation (s/m) of the private health 
--insurance claims.


--para cada especialidad quiero, el coste medio, desv, numero de siniestros
-- y la relative standar deviation

--comando SQL para InputDataSet
SELECT TOP 5 ESPNOMBRE, CTCATEGORIA, SNIMPORTE, CTFNAC, SNFACTO  --nombre de la esp, categoria prod, coste, fecha de nac y fecha acto medico(la cogemos xq tenemos varios actos de por medio)
FROM  PRESTACIONES 
LEFT JOIN ASEGURADOS ON CTIDASEG=SNIDASEGURADO
LEFT JOIN ESPECIALIDAD ON ESPCODIGO=SNESPECIALIDAD


library(dplyr) --cargo la libreria
STROS <-group_by(InputDataSet,ESPNOMBRE) --agrupamos por nombre y cargamos 
Resumen<-summarize(STROS,
        NumSin= n(),
        Media = mean(SNIMPORTE),
        DesvEst= sd(SNIMPORTE))
Resumen<-mutate(Resumen,CV= DesvEst/Media)
Resumen10 <-arrange(Resumen[1:10,],-CV) -- las primeras dias, de menor a mayor el CV
OutputDataSet<-as.data.frame(Resumen10) --guardalo en outputdataset



--HACMEOS LA EJECUCION
EXECUTE[sys].[sp_execute_external_script] 
@input_data_1 = N'
        SELECT ESPNOMBRE, CTCATEGORIA, SNIMPORTE, CTFNAC, SNFACTO  
        FROM  CUNEFDS.dbo.PRESTACIONES 
        LEFT JOIN CUNEFDS.dbo.ASEGURADOS ON CTIDASEG=SNIDASEGURADO
        LEFT JOIN CUNEFDS.dbo.ESPECIALIDAD ON ESPCODIGO=SNESPECIALIDAD
 ', 
@language = N'R', 
@script =N'
    library(dplyr) 
    STROS <-group_by(InputDataSet,ESPNOMBRE) 
    Resumen<-summarize(STROS,
            NumSin= n(),
            Media = mean(SNIMPORTE),
            DesvEst= sd(SNIMPORTE))
    Resumen<-mutate(Resumen,CV= DesvEst/Media)
    Resumen10 <-arrange(Resumen[1:10,],-CV) 
    OutputDataSet<-as.data.frame(Resumen10)

' 
WITH RESULT SETS (([especialidad] varchar(100), [numsin] int, [media] float, [DE] float, [CV] float)) 

USE CUNEFDSM


--EJEMPLO 2
--analizar cómo varía el coste medio de las prestaciones en el año 2019 
--en función de la edad (hipótesis de experto: el coste medio crece exponencialmente con la edad)

--PASO 1
--por un lado contar los asegurados agrupados por edad
--PASO 2
--por otro lado sumar el coste de las prestaciones por edad del asegurado
--PASO 3
--unir ambos cálculos en una consulta fácilmente consultable
--PASO 4
--calcular en R al ajuste exponencial de los datos


--COMANDO SQL
--contar los asegurados agrupados por edad
SELECT 2019-YEAR(CTFNAC) AS EDAD, COUNT(*) AS ASEGURADOS --lo primero es la edad y luego cuenta por edad del asegurado
FROM ASEGURADOS 
WHERE 2019-YEAR(CTFNAC)>=0 --pongo el filtro de que la edad es nula o positiva para que no salgan ningun que nazca en 2019
GROUP BY 2019-YEAR(CTFNAC) 

--sumar el coste de las prestaciones por edad del asegurado
SELECT 2019-YEAR(CTFNAC) AS EDAD, SUM(SNIMPORTE) AS COSTE --saco la edad y la suma de coste para la edad
FROM PRESTACIONES 
LEFT JOIN ASEGURADOS ON CTCONTRATO=SNCONTRATO
WHERE YEAR(SNFACTO)=2019 --fecha 2019
GROUP BY 2019-YEAR(CTFNAC)



--JUNTAMOS TODO PARA PASARLO LUEGO A R
SELECT AG.EDAD, ISNULL(SN.COSTE,0) AS COSTE, AG.ASEGURADOS,
 ISNULL(SN.COSTE,0)/AG.ASEGURADOS AS COSTEMEDIO --el coste medio
 FROM (
SELECT 2019-YEAR(CTFNAC) AS EDAD, COUNT(*) AS ASEGURADOS 
FROM ASEGURADOS 
WHERE 2019-YEAR(CTFNAC)>=0 
GROUP BY 2019-YEAR(CTFNAC) 
 ) AG 
 INNER JOIN (
     SELECT 2019-YEAR(CTFNAC) AS EDAD, SUM(SNIMPORTE) AS COSTE 
FROM PRESTACIONES 
INNER JOIN ASEGURADOS ON CTCONTRATO=SNCONTRATO
WHERE YEAR(SNFACTO)=2019 
GROUP BY 2019-YEAR(CTFNAC)
 ) SN ON SN.EDAD=AG.EDAD


--Codigo que se colocara en R
Modelo <-lm(log(COSTEMEDIO) ~ EDAD, InputDataSet) --calculamos el log de costemedio segun la edad
# log(COSTEMEDIO) = A + B x EDAD --> COSTEMEDIO = EXP(A) x EXP(BxEDAD)
R2 <-summary(Modelo)$r.squared
A <-exp(Modelo$coefficients[1])
B <-Modelo$coefficients[2]
OutputDataSet<-data.frame(R2,A,B)




--EJECUTO EL CODIGO COMPLETO Y ANALIZO LOS RESULTADOS
EXECUTE[sys].[sp_execute_external_script] 
@input_data_1 = N'
SELECT AG.EDAD, ISNULL(SN.COSTE,0)ASCOSTE, AG.ASEGURADOS,
 ISNULL(SN.COSTE,0)/AG.ASEGURADOS AS COSTEMEDIO
 FROM (
SELECT 2019-YEAR(CTFNAC) AS EDAD, COUNT(*) AS ASEGURADOS 
FROM ASEGURADOS 
WHERE 2019-YEAR(CTFNAC) BETWEEN 11 AND 80
GROUP BY 2019-YEAR(CTFNAC) 
 ) AG 
 INNER JOIN (
     SELECT 2019-YEAR(CTFNAC) AS EDAD, SUM(SNIMPORTE) AS COSTE 
FROM PRESTACIONES 
INNER JOIN ASEGURADOS ON CTCONTRATO=SNCONTRATO
WHERE YEAR(SNFACTO)=2019 
GROUP BY 2019-YEAR(CTFNAC)
 ) SN ON SN.EDAD=AG.EDAD
',
@language =N'R', 
@script =N'
Modelo <-lm(log(COSTEMEDIO) ~ EDAD, InputDataSet) 
# log(COSTEMEDIO) = A + B x EDAD  --> COSTEMEDIO = EXP(A) x EXP(BxEDAD)
R2 <-summary(Modelo)$r.squared
A <-exp(Modelo$coefficients[1])
B <-Modelo$coefficients[2]
OutputDataSet<-data.frame(R2,A,B)'
WITH RESULT SETS (([R2] [float],[A] [float],[B] [float]))




-------------------
--PROGRAMACION SQL
USE CUNEFDS

SELECT * FROM sys.tables
--Sys.table recupera todas las tablas creadas en la base de datos

--name, objetct id, principal_id, parent_object_id
--type typo de usuario (usuario)
--numero de esquema en la base de datos
--parent_object_id


SELECT * FROM sys.types 

--tipos de datos que podemos utilizar, por ej image, text, date, uniqueidetifier, time, datetime2, smallint (numeros entre 0 y 65535), int, real, float, money(pare temas economicas y contables), float es el tipo mas importante para flotantes (tiene la mejor precision), sql_variante admite textos entre otras cosas, bigint(es el mayor numero entero que se puede fabricar), varbinary(permite guardar un archivo binario exadecimal dentro del campo)
-- varchar(campo de texto de longitud variable), nvarchar, nchar(permite guardar caracteres unicod), texto como xml


SELECT * FROM sys.columns
--system type id que se relaciona con sys.type, precision nos da el tamanio del campo en cuestion.



SELECT TAB.[name], TAB.create_date, COL.column_id, COL.[name] 
FROM sys.tables TAB
INNER JOIN sys.columns COL ON COL.object_id=TAB.object_id
ORDER BY TAB.[name], COL.column_id


--SELECT adicional para saber el tamaño de la columna. HACERLO EN CASA

---------------

--DEFINICION DE VARIABLES
--Es conveniente en algunos casos
--con el comando DEclare defino una variable

DECLARE @i FLOAT --con esto la variable i queda declarada 
SELECT @i = 3. / 100. --con select o set da igual
SELECT POWER(1+@i, -3)--el segundo select nos muestra @i. Para ver la diferencia mejor usar set. Las variables existen dentro del comando o script SQL que se ejecuta


---CALCULO FINANCIERO CON LA TABLA PRESTAMOS
DECLARE @i FLOAT --con esto la variable i queda declarada 
SELECT @i = 3. / 100.
SELECT CONTRATO, SUM(CUOTA * POWER(1+@i, -t)) AS CAPITAL --traeme contrato, duracion, cuota y periodo, valor actual financiero de cada una de las cuotas al 3%
FROM PRESTAMO 
INNER JOIN T ON t BETWEEN 1 AND DURACION --hasta duracion que para esa fila es el año numero 7. Traeme todos los valores de t hasta la duracion del contrato
WHERE CONTRATO=4155903 --para este contrato calculamos 7 filas (el periodo)
GROUP BY CONTRATO--ahora sumamos los VAC y vemos el capital principal del contrato


-- Aqui calculamos 1818709 de registros para calcular el capital de todos los prestamos
DECLARE @i FLOAT --con esto la variable i queda declarada 
SELECT @i = 3. / 100.
SELECT COUNT(*) --CONTRATO, SUM(CUOTA * POWER(1+@i, -t)) AS CAPITAL --traeme contrato, duracion, cuota y periodo, valor actual financiero de cada una de las cuotas al 3%
FROM PRESTAMO 
INNER JOIN T ON t BETWEEN 1 AND DURACION --hasta duracion que para esa fila es el año numero 7. Traeme todos los valores de t hasta la duracion del contrato
--WHERE CONTRATO=4155903 --para este contrato calculamos 7 filas (el periodo)
--GROUP BY CONTRATO--ahora sumamos los VAC y vemos el capital principal del contrato



--CALCULAMOS EL CAPITAL PARA TODOS LOS CONTRATOS
DECLARE @i FLOAT --con esto la variable i queda declarada 
SELECT @i = 3. / 100.
SELECT CONTRATO, SUM(CUOTA * POWER(1+@i, -t)) AS CAPITAL --traeme contrato, duracion, cuota y periodo, valor actual financiero de cada una de las cuotas al 3%
FROM PRESTAMO 
INNER JOIN T ON t BETWEEN 1 AND DURACION --hasta duracion que para esa fila es el año numero 7. Traeme todos los valores de t hasta la duracion del contrato
--WHERE CONTRATO=4155903  
GROUP BY CONTRATO--ahora sumamos los VAC y vemos el capital principal del contrato


--Quiero recuperar de algun sitio las tablas. PAra eso se utiliza un cursos para recuperar y borrar esos datos
--cursos es una vision estatica de una recuperacion de datos

USE CUNEFDSM

DECLARE @nombre VARCHAR(200)
DECLARE tabla CURSOR 
        FOR SELECT [name] FROM sys.tables -- cursos de la variable que vamos a elminiar. Tiene una columna
OPEN tabla --lee todos los nombres de tabla y los carga. Carga todos los nombres de la tabla CUNEFDSM
FETCH NEXT FROM tabla INTO @nombre; --el contenido del cursos va a la variable nombre. Con FETCH NEXT Recupero el contenido de la tabla o cursos
WHILE @@FETCH_STATUS = 0 --mientras encuentras algo, repite
        BEGIN 
                PRINT @nombre; --la mandamos a mensajes. Me muestra el nombre entre los mensajed e Azure
                FETCH NEXT FROM tabla INTO @nombre; -- para recuperar el siguiente registro
        END 
CLOSE tabla;
DEALLOCATE tabla; --eliminamos el cursos



