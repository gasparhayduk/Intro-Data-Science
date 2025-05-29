rm(list=ls())
setwd("/Users/luis/Documents/Universidad/Introducción a la Data Science")  # Definan aquí su directorio de trabajo.

# Este script tiene como encoding UTF-8
# Si leen mal los acentros, utilicen ""Reopen with Encoding >>> UTF 8" en RStudio.

# APELLIDO Y NOMBRE DE LOS INTEGRANTES DEL GRUPO:
#	Integrante 1: Gaspar Hayuk
#	Integrante 2: Luis Gonzalez Lelong
#	Integrante 3:

# IMPORTANTE: el script que resuelve el taller debe ser entregado por mail a
# introdsutdt@gmail.com. Como asunto del mismo debe decir "Intro DS - Taller 01" y
# en el cuerpo del mismo deben figurar los nombres de quienes los resolvieron y
# TAMBIÉN SUS LEGAJOS (todos los integrantes del taller deben estar copiados como
# destinatarios en el correo de entrega). Tiene tiempo para entregarlo hasta
# el 25/03/2022 (inclusive)

#~~ Intro a Data Science: Taller 01 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Fuente de los datos: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HG7NV7

## Resuelva los siguientes puntos.

# 1: Carguen en un data.frame llamado vuelos_01_12 los datos que se
# encuentran en el archivo de texto "vuelos_01_12.txt". Vean qué estructura
# tiene el data.frame cargado (las variables de texto deben ser cargadas como
# factor)
vuelos_01_12 <- read.table("vuelos_01_12.txt", header = TRUE, sep = ";", stringsAsFactors = TRUE)

# 2: Responda: datos de cuántos vuelos tienen el data.frame "vuelos_01_12"?
nrow(vuelos_01_12)
# Con esto podemos ver cuantas filas tiene el data.frame, es decir, la cantidad de vuelos registrados.
# el dataset tiene datos de 3318080 vuelos

# 3: Responda: cuántas columnas tiene el data.frame "vuelos_01_12"?
ncol(vuelos_01_12)
# Así podemos ver la cantidad de columnas que tiene el data.frame
# El dataset tiene 28 columnas/variables

# 4: Consideraremos un vuelo como demorado si el valor de "ArrDelay" es superior
# a 25 minutos. Teniendo esto en cuenta, qué aerolinea (variables UniqueCarrier) tiene
# menor PROPORCIÓN de vuelos demorados?

# Creamos una nueva columna que vale "TRUE" si el vuelo estuvo demorado y "FALSE" en caso contrario
vuelos_01_12$is.delayed <- vuelos_01_12$ArrDelay > 25
# Creamos una tabla que muestra la cantidad de vuelos demorados (bajo la columna "TRUE") y no demorados (bajo
# la columna "TRUE") por cada aerolínea
demoras.totales <- table(vuelos_01_12$UniqueCarrier, vuelos_01_12$is.delayed)
# Hacemos la misma tabla que antes pero ahora con valores relativos. Usamos margin = 1 para que los valores
# de cada fila sumen 1 y así poder saber el porcentaje de demora de cada aerolínea.
demoras.totales.adj <- prop.table(demoras.totales, margin = 1) #devuelve una tabla con la proporcion de vuelos demorados por aerolinea. 
# TRUE indica las demoras y FALSE los vuelos puntuales (con una demora menor a 25 minutos)
# Viendo la tabla vemos que US es la aerolínea que tiene un menor porcentaje de vuelos demorados. 


# 5: En qué aeropuerto de origen es menos común (en términos relativos) que salga un avión
# que eventualmente tendrá una demora (utilice la definición de demora que uso en el punto anterior)?

# Generamos una tabla con valores relativos que muestra los porcentajes de vuelos demorados por aeropuerto
demora.aeropuerto <- prop.table(table(vuelos_01_12$Origin, vuelos_01_12$is.delayed), margin = 1)
# Creamos un data.frame a partir de la tabla anterior, para poder trabajar con los indices ya que es imposivle ver a ojo qué aeropuerto tiene menor proporcion de vuelos demorados.
demora.aeropuertodf <- as.data.frame(demora.aeropuerto)
# Debemos notar que la tabla cambió su apariencie. 
# En la primer columna está el nombre del aeropuerto; en la segunda una columna que indica TRUE, es decir, indica los vuelos demorados; 
# y la tercer columna (Freq) indica la proporcion de vuelos demorados que salen de cada aeropuerto. 
# Para identificar cuál es el aeropuerto con menor proporcion de vuelos demorados, debemos buscar el minimo de la columna Freq. Esto se hace con min(demora.aeropuertodf$Freq) 
# Ahora buscamos la fila donde está el aeropuerto con la menor proporcion de vuelos demorados. 
# Indexamos el data.frame de tal forma que nos muestre la fila que se corresponde al aeropuerto que tiene el menor porcentaje de vuelos demorados.

demora.aeropuertodf[demora.aeropuertodf$Freq == min(demora.aeropuertodf$Freq),] #nos devuelve la fila completa del aeropuerto con menor proporcion de vuelos demorados
# El aeropuerto SQR es el que tiene menor porcentaje de vuelos demorados (0%)

# 6: Responda: es más o menos común que haya vuelos demorados los fines de
# semana respecto a los días de semana? (si lo considera necesario, puede crear una nueva columna en el
# data.frame que indique si un vuelo fue o no realizado un día de fin de semana)

# Creamos una nueva columna que nos dice si un vuelo fue realizado en fin de semana ("TRUE") o no ("FALSE")
vuelos_01_12$is.wknd <- vuelos_01_12$DayOfWeek == c(6,7)
# Creamos una tabla cruzada y calculamos su distribución. 
prop.table(table(vuelos_01_12$is.wknd, vuelos_01_12$is.delayed), margin = 1)
# Si no es fin de semana, un 14,53% de los vuelos tienen demoras. Si es fin de semana, un 13,89% de los vuelos
# tienen demoras. Es más común que haya vuelos demorados en los días de semana.


# 7: Para los casos en que hay minutos de demora debido a "LateAircraftDelay", cuál
# es la mediana de esas demoras? (OJO: piense si tiene sentido
# considerar los ceros en este cálculo).

# Calculamos la mediana sin tener en cuenta los ceros ya que solo consideramos los casos "en que hay minutos
# de demora debido a 'LateAircraftDelay'". Para poder hacer el cálculo tampoco consideramos los NA.
median(vuelos_01_12$LateAircraftDelay[vuelos_01_12$LateAircraftDelay > 0], na.rm = TRUE,)
# La mediana sin tener en cuenta los ceros es 27


# 8: ¿Cuál es el número de vuelo (FlightNum) del vuelo qué más tiempo pasó en el aire en los datos?
# Note que el tiempo en el aire de cada vuelo figura en la columna AirTime

# Indexamos de tal forma que muestre las filas que tengan el valor de AirTime igual al maximo de la columna
# AirTime (sin tener en cuenta los NAs; pues cuando hay NA en AirTime, R no sabrá compararlo con el maximo de AirTime), y solo la columna FlightNum.
vuelos_01_12[vuelos_01_12$AirTime == max(vuelos_01_12$AirTime, na.rm = TRUE) 
             & !is.na(vuelos_01_12$AirTime), "FlightNum"]
# El vuelo número 73 es el que pasó mayor cantidad de tiempo en el aire.


# 9: Haga una tabla de origen-destino de los vuelos. La misma debe tener como
# nombre de filas los distintos aeropuertos de origen y como nombre de
# columnas los distintos aeropuertos de destino. El contenido de dicha tabla
# debe contener la cantidad de vuelos que se efecturan para cada combinación de
# origen y destino. Guarde esta tabla en memoria utilizando una variable.

origin.dest <- table(vuelos_01_12$Origin, vuelos_01_12$Dest)


# 10: transforme la tabla que armó en el punto anterior en un data.frame utilizando la función
# as.data.frame y guarde dicho objeto en una variable. Una vez hecho esto, elimine de la
# memoria el objeto que guardo en el punto anterior. Inspeccione y entienda qué es lo que hizo la
# función as.data.frame en este caso.

origin.dest.df <- as.data.frame(origin.dest)
rm(origin.dest)
# La función as.data.frame convirtió la tabla original de 228x230 en un data frame de tres columnas, donde la
# primera es el aeropuerto de origen, la segunda el de destino, y la tercera es la cantidad de veces que sucedió
# el vuelo correspondiente.


# 11: Responda: Cuál es el tramo en el que se hicieron más vuelos?
# Guarde de alguna manera este resultado, de modo de poder reusarlo en el siguiente punto.

#Debemos identificar cuál es el valor maximo de Freq. Esto lo sabemos haciendo max(origin.dest.df$Freq). 

# Indexamos de tal forma que muestre la fila cuyo valor de "Freq" sea igual al máximo valor que alcanza "Freq"
origin.dest.df[origin.dest.df$Freq == max(origin.dest.df$Freq),] #esto nos devuelve la fila completa.
# El tramo en el que más vuelos se hicieron es el de LAX a SAN

# Ahora guardamos el tramo mas frecuente en una lista de tres elementos. 
# En el primer elemento tendremos el aeropuerto de origen del tramo más frecuente; en el segundo elemento tendremos el aeropuerto de 
# destino del tramo más frecuente; y en el tercer elemento tendremos la cantidad de veces que se repite ese tramo. 

tramo_mas_frecuente <- vector("list", length = 3) #creamos una lista vacia de tres elementos. 

# Completamos la lista:
tramo_mas_frecuente[[1]] <- origin.dest.df[origin.dest.df[origin.dest.df$Freq == max(origin.dest.df$Freq), "Var1"] # aeropuerto de origen
tramo_mas_frecuente[[2]] <- origin.dest.df[origin.dest.df[origin.dest.df$Freq == max(origin.dest.df$Freq), "Var2"] # aeropuerto de destino 
tramo_mas_frecuente[[3]] <- origin.dest.df[origin.dest.df[origin.dest.df$Freq == max(origin.dest.df$Freq), "Freq"] # cantidad de veces que se repite ese tramo. 


# Otra forma de guardar el resultado es guardando la fila completa: 

tramo_mas_frecuente.row <- origin.dest.df[origin.dest.df$Freq == max(origin.dest.df$Freq),]

# 12: Tomando como input los vuelos correspondientes al tramo más frecuente y
# que no fueron cancelados, haga un boxplot que permita ver las distribución de
# la variable ActualElapsedTime. ¿A qué considera que pueden deberse los outliers?

boxplot(vuelos_01_12[vuelos_01_12$Origin == "LAX" & vuelos_01_12$Dest == "SAN" & vuelos_01_12$Cancelled == 0, 
                     "ActualElapsedTime"],ylab = "Tiempo de duración (minutos)")


# Tambien podemos hacer el grafico llamando a la lista donde guardamos los datos del tramo mas frecuente: 

boxplot(vuelos_01_12[vuelos_01_12$Origin == ramo_mas_frecuente[[1]] & vuelos_01_12$Dest == ramo_mas_frecuente[[2]] & vuelos_01_12$Cancelled == 0, 
                     "ActualElapsedTime"],ylab = "Tiempo de duración (minutos)")


# 13: Haga un gráfico que permita ver por mes qué proporción de todos los vuelos registrados en los
# datos tienen valor de UniqueCarrier igual a AA y qué proporción tienen valor de UniqueCarrier igual a MQ.
# Es más que probable que el gráfico que realicen requiera de leyendas para ser entendido, inspeccionen
# esta página para ver cómo usarlas: http://www.sthda.com/english/wiki/add-legends-to-plots-in-r-software-the-easiest-way.
# El primer ejemplo debería resultarles útil

# Creamos una tabla que muestre el porcentaje de vuelos realizados por AA en cada mes bajo la columna "TRUE"
# y el porcentaje de vuelos realizados por el resto bajo la columna "FALSE". Luego, creamos otra tabla que solo
# contenga los valores de la columna "TRUE" de tal forma de quedarnos con el porcentaje del total de vuelos 
# realizados por AA para cada mes.
AA.per.month <- prop.table(table(vuelos_01_12$Month, vuelos_01_12$UniqueCarrier == "AA"), margin = 1)
AA.per.month <- AA.per.month[,2]

# Hacemos lo mismo para MQ.
prop.table(table(vuelos_01_12$Month, vuelos_01_12$UniqueCarrier == "MQ"), margin = 1)
MQ.per.month <- prop.table(table(vuelos_01_12$Month, vuelos_01_12$UniqueCarrier == "MQ"), margin = 1)
MQ.per.month <- MQ.per.month[,2]

# Ahora realizamos el gráfico correspondiente. Ajustamos la escala, nombramos los ejes, y agregamos una leyenda.
plot(1:12, AA.per.month, type = "b", ylim = c(0.13, 0.19), col = "red", xlab = "Month",
     ylab = "Number of flights (% of total)")
lines(1:12, MQ.per.month, type = "b", col = "blue")
legend(x = "bottomleft", legend = c("AA", "MQ"), col = c("red", "blue"), lty = 2, lwd = 1)


