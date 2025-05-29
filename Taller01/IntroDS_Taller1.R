rm(list=ls())
setwd("~/Desktop/IntroDS/Taller01")  # Definan aquí su directorio de trabajo.

# Este script tiene como encoding UTF-8
# Si leen mal los acentros, utilicen ""Reopen with Encoding >>> UTF 8" en RStudio.

# APELLIDO Y NOMBRE DE LOS INTEGRANTES DEL GRUPO:
#	Integrante 1: Gaspar Hayduk
#	Integrante 2: Luis Gonzalez Lelong
#	Integrante 3:

# IMPORTANTE: el script que resuelve el taller debe ser entregado por mail a
# introdsutdt@gmail.com. Como asunto del mismo debe decir "Intro DS - Taller 01" y
# en el cuerpo del mismo deben figurar los nombres de quienes los resolvieron y
# TAMBIÉN SUS LEGAJOS (todos los integrantes del taller deben estar copiados como
# destinatarios en el correo de entrega). Tiene tiempo para entregarlo hasta
# el 25/03/2021 (inclusive)

#~~ Intro a Data Science: Taller 01 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Fuente de los datos: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HG7NV7

## Resuelva los siguientes puntos.

# 1: Carguen en un data.frame llamado vuelos_01_12 los datos que se
# encuentran en el archivo de texto "vuelos_01_12.txt". Vean qué estructura
# tiene el data.frame cargado (las variables de texto deben ser cargadas como
# factor)
vuelos_01_12 <- read.table("vuelos_01_12.txt", sep = ";", header =  TRUE,
                  stringsAsFactors = TRUE) #cargamos los datos

str(vuelos_01_12) #veamos qué tipo de variables tiene. 
head(vuelos_01_12) #veamos una previsualizacion del dataset. 

# 2: Responda: datos de cuántos vuelos tienen el data.frame "vuelos_01_12"?
nrow(vuelos_01_12) 
#el dataset tiene datos de 3318080 vuelos

# 3: Responda: cuántas columnas tiene el data.frame "vuelos_01_12"?
ncol(vuelos_01_12)
#el dataset tiene 28 columnas/variables

# 4: Consideraremos un vuelo como demorado si el valor de "ArrDelay" es superior
# a 25 minutos. 
#Teniendo esto en cuenta, ¿qué aerolinea (variables UniqueCarrier) tiene menor PROPORCIÓN de vuelos demorados? 

vuelos_01_12$is.demorado <- vuelos_01_12$ArrDelay > 25 #creamos una nueva columna que indica si el vuelo fue demorado o no.

tabla_demora_aerolinea = prop.table(table(vuelos_01_12$UniqueCarrier, vuelos_01_12$is.demorado), margin = 1) 
#devuelve una tabla con la proporcion de vuelos demorados por aerolinea. 
#TRUE indica las demoras y FALSE los vuelos que salieron de forma puntual (entendiendo una demora menor a 25 minutos como puntual)
#si vemos esta tabla, podemos ver que US es la erolinea con menor proporcion de vuelos demorados. 


# 5: ¿En qué aeropuerto de origen es menos común (en términos relativos) que salga un avión
# que eventualmente tendrá una demora (utilice la definición de demora que uso en el punto anterior)?

#debemos hacer una tabla de contingencia combinando la variable Origin (que indica de donde sale el avion)
# y la variable is.demorado:

tabla_demora_aeropuerto = prop.table(table(vuelos_01_12$Origin, vuelos_01_12$is.demorado), margin = 1)

#esta tabla nos dice, para cada aeropuerto, qué proporcion de los vuelos salieron con demora y qué proporcion no tuvieron demora. 
#al igual aue antes, la columna TRUE indica las demora y la columna FALSE indica los vuelos que no tuvieron demora. 
#en el dataset hay 228 aeropuertos de origen (podemos verlo haciendo 'str(vuelos_01_12)' y viendo cuantós niveles tiene Origin) 
#por tanto, es imposible identificar a ojo qué aeropuerto tiene menor proporcion de vuelos demorados. 
#para solucionar esto, debemos pasar tabla_demora_aeropuerto a formato dataframe para poder trabajar con los indices. 

tabla_demora_aeropuerto <- as.data.frame(tabla_demora_aeropuerto) 

# tabla_demora_aeropuerto ahora es un dataframe donde Var1 indica el nombre del aeropuerto, 
#Var2 indica TRUE (es decir, indica vuelos demorados) y Freq indica la proporcion de vuelos demorados que salen de cada aeropuerto. 
#como queremos saber qué aeropuerto tiene menor proporcion de vuelos demorados, debemos ver qué aeropuerto tiene menor Freq:

min(tabla_demora_aeropuerto$Freq) #esto nos dice cuál es la menor proporcion de vuelos demorados. 
#buscamos a qué aeropuerto corresponde:

tabla_demora_aeropuerto[tabla_demora_aeropuerto$Freq == min(tabla_demora_aeropuerto$Freq), ] #esto nos devuelve la fila completa 
#corriendo esto, podemos ver que el aeropuerto SQR es el aeropuerto con menor proporcion de vuelos demorados. 
# Más específicamente, el aeropuerto SQR tiene una proporcion de vuelos demorados igual a 0. 



# 6: Responda: ¿ es más o menos común que haya vuelos demorados los fines de semana respecto a los días de semana? (si lo considera necesario, puede crear una nueva columna en el
# data.frame que indique si un vuelo fue o no realizado un día de fin de semana) 

#creamos una nueva columna que indica si el vuelo ocurrió durante un fin de semena o no:
vuelos_01_12$is.finde <- vuelos_01_12$DayOfWeek >= 6 
#esta columna valdra TRUE si el vuelo corresponde al fin de semana (dias sabado y domingo, 6 y 7 en DayOfWeek) y FALSE si corresponde a un dia de la semana (lunes a viernes)

# creamos una tabla de contingencia que combine is.demorado con is.finde:
#ponemos a 
prop.table(table(vuelos_01_12$is.finde, vuelos_01_12$is.demorado), margin = 1)
# los (FALSE, TRUE) en vertical hacer referencia a si el vuelo ocurrió durante el fin de semana. TRUE indica que sí y FALSE que no
#y los (FALSE, TRUE) en horizontal hacen referencia a si el vuelo tuvo demora o no. TRUE indica que tuvo demora y FALSE que no tuvo demora. 

#Inspeccionando la tabla, debemos notar que:
# - durante el fin de semana, el 86% de los vuelos no tienen demora y el 13,8% sí tiene demora
# - en los días de semana (lunes a viernes), el 85% de los vuelos no tienen demora y el 14,6% sí tiene demora. 

#Por tanto, es mas comun que haya vuelos demorados los dias de semana.  



# 7: Para los casos en que hay minutos de demora debido a "LateAircraftDelay", cuál es la mediana de esas demoras? (OJO: piense si tiene sentido
# considerar los ceros en este cálculo).

#debemos calcular la mediana de LateAircraftDelay. Para ello, ignoramos los NA y y los casos donde LateAircraftDelay sea cero, pues cuando vale 0 es porque no hay demora.  

median(vuelos_01_12$LateAircraftDelay[vuelos_01_12$LateAircraftDelay > 0], na.rm = TRUE)
#corriendo esto, vemos que la mediana de LateAircraftDelay, excluyendo cuando vale 0, es 27. 

# 8: ¿Cuál es el número de vuelo (FlightNum) del vuelo qué más tiempo pasó en el aire en los datos?
# Note que el tiempo en el aire de cada vuelo figura en la columna AirTime

max(vuelos_01_12$AirTime, na.rm = TRUE) #nos dice cual es el maximo de AirTime (excluyendo los NAs).
#debemos notar tambien que cuando comparemos un valor de AirTime con el maximo, debemos excluir los NAs, pues si comparamos un NA con el maximo, R no sabe qué hacer.
#entonces, para obtener el FlightNum del vuelo que más tiempo pasó en el aire debemos pedirle que no haya NA en AirTime y que AirTime sea el maximo
vuelos_01_12[is.na(vuelos_01_12$AirTime) == FALSE & vuelos_01_12$AirTime == max(vuelos_01_12$AirTime, na.rm = TRUE), "FlightNum" ]
#haciendio esto, el FlightNum del vuelo que más tiempo pasó en el aire es 73. 

# 9: Haga una tabla de origen-destino de los vuelos. La misma debe tener como
# nombre de filas los distintos aeropuertos de origen y como nombre de
# columnas los distintos aeropuertos de destino. El contenido de dicha tabla
# debe contener la cantidad de vuelos que se efecturan para cada combinación de
# origen y destino. Guarde esta tabla en memoria utilizando una variable.

tabla_origen.destino <- table(vuelos_01_12$Origin, vuelos_01_12$Dest)


# 10: transforme la tabla que armó en el punto anterior en un data.frame utilizando la función
# as.data.frame y guarde dicho objeto en una variable. Una vez hecho esto, elimine de la
# memoria el objeto que guardo en el punto anterior. Inspeccione y entienda qué es lo que hizo la
# función as.data.frame en este caso.

tabla_origen.destino <- as.data.frame(tabla_origen.destino)

#notemos que cambió la forma de la tabla. La misma tiene tres columnas: la primera es el aeropuerto de origen, 
#la segunda es el aeropuerto de destino y la tercera es la cantidad de veces que se hizo ese viaje. 


# 11: Responda: Cuál es el tramo en el que se hicieron más vuelos?
# Guarde de alguna manera este resultado, de modo de poder reusarlo en el siguiente punto.

#debemos buscar el maximo de Freq:
max(tabla_origen.destino$Freq) #esto nos da cuántas veces se hace el tramo mas repetida. 
#buscamos cuál es el tramo:
tabla_origen.destino[tabla_origen.destino$Freq == max(tabla_origen.destino$Freq),]
tabla_origen.destino[tabla_origen.destino$Freq == max(tabla_origen.destino$Freq), ] #esto nos devuelve la fila completa. 
#El tramo mas repetido es LAX-SAN. 

#creamos una lista vacia de 3 elementos para guardar el tramo mas frecuente 
tramo_frecuente <- vector("list", length = 3)

tabla_origen.destino[tabla_origen.destino$Freq == max(tabla_origen.destino$Freq), "Var1"] 
tabla_origen.destino[tabla_origen.destino$Freq == max(tabla_origen.destino$Freq), "Var2"]
tabla_origen.destino[tabla_origen.destino$Freq == max(tabla_origen.destino$Freq), "Freq" ]

#completamos los elementos de la lista.
#En el primer elemento tendremos el aeropuerto de origen del tramo mas frecuente, en el segundo elemento el
#aeropuerto de destino del tramo mas frecuente, y en el tercero la cantidad de veces que se repite ese tramo. 
tramo_frecuente[[1]] = tabla_origen.destino[tabla_origen.destino$Freq == max(tabla_origen.destino$Freq), "Var1"] 
tramo_frecuente[[2]] = tabla_origen.destino[tabla_origen.destino$Freq == max(tabla_origen.destino$Freq), "Var2"]
tramo_frecuente[[3]] = tabla_origen.destino[tabla_origen.destino$Freq == max(tabla_origen.destino$Freq), "Freq"]





# 12: Tomando como input los vuelos correspondientes al tramo más frecuente y
# que no fueron cancelados, haga un boxplot que permita ver las distribución de
# la variable ActualElapsedTime. ¿A qué considera que pueden deberse los outliers?

boxplot(vuelos_01_12[vuelos_01_12$Origin == tramo_frecuente[[1]] & vuelos_01_12$Dest == tramo_frecuente[[2]] & vuelos_01_12$Cancelled == 0, "ActualElapsedTime"], ylabel = "Tiempo de duración en minutos")

# 13: Haga un gráfico que permita ver por mes qué proporción de todos los vuelos registrados en los
# datos tienen valor de UniqueCarrier igual a AA y qué proporción tienen valor de UniqueCarrier igual a MQ.
# Es más que probable que el gráfico que realicen requiera de leyendas para ser entendido, inspeccionen
# esta página para ver cómo usarlas: http://www.sthda.com/english/wiki/add-legends-to-plots-in-r-software-the-easiest-way.
# El primer ejemplo debería resultarles útil

#primero debemos agregar una columna extra indicando si el vuelo corresponde a AA y otra si correspone a MQ:

vuelos_01_12$is.AA <- vuelos_01_12$UniqueCarrier == "AA"
vuelos_01_12$is.MQ <- vuelos_01_12$UniqueCarrier == "MQ"

#creamos la tabla para AA:

AA.permonth <- prop.table(table(vuelos_01_12$Month, vuelos_01_12$is.AA), margin = 1)
# AA.permonth[,2] indica la proporcion de vuelos que pertenecen a AA por cada mes.

#hacemos lo mismo para MQ:

MQ.permonth <- prop.table(table(vuelos_01_12$Month, vuelos_01_12$is.MQ), margin = 1)
# MQ.permonth[,2] indica la proporcion de vuelos que pertenecen a MQ por cada mes.

#graficamos: revisar 

# Data:
plot(1:12, AA.permonth[,2], type="b", pch=19, col="red", xlab="Month", ylab="Number of flights (% of total)")
# Lines:
lines(1:12,AA.permonth[,2], MQ.permonth[,2], pch=18, col="blue", type="b", lty=2)
# Legend:
legend(1, 95, legend=c("% de vuelos de AA", "% de vuelos de MQ"),
       col=c("red", "blue"), lty=1:2, cex=0.8)













