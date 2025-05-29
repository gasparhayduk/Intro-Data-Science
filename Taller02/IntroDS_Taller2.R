rm(list=ls())
setwd("~/Desktop/IntroDS/Taller02") # Definan el directorio de trabajo

# Importe aquí todas las librerías que use:
library("dplyr")
library("ggplot2")



# APELLIDO Y NOMBRE DE LOS INTEGRANTES DEL GRUPO:
#	Integrante 1:
#	Integrante 2:
#	Integrante 3:

# IMPORTANTE: el script que resuelve el taller debe ser entregado por mail a
# introdsutdt@gmail.com. Como asunto del mismo debe decir "Intro DS - Taller 02" y
# en el cuerpo del mismo deben figurar los nombres de quienes los resolvieron y
# TAMBIÉN SUS LEGAJOS (todos los integrantes del taller deben estar copiados como
# destinatarios en el correo de entrega). Tienen tiempo para entregarlo hasta
# el 12/04/2021 (inclusive)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                                 Taller 2 de Intro a Data Science                                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# El objetivo de este taller es llevar adelante un análisis exploratorio de un conjunto de
# datos. No se pide que se use ninguna librería en particular para resolver las siguientes
# consignas (pero sí puede usar las que ustedes consideren necesarias).

# 1. Carguen en memoria la información contenida en "recorridos-realizados-2019.RData"
# (noten que al hacer esto se cargará en memoria un data.frame guardado en la 
# variable "datos_bicis"). El archivo puede descargarse en el link de Dropbox provisto.

datos_bicis <- load("recorridos-realizados-2019.RData")


# 2. Calcule cuántas filas y cuántas columnas tiene el data.frame datos_bicis.

# Cantidad de filas:
nrow(datos_bicis) 
# Hay 2473042 filas/observaciones

#Cantidad de columnas/variables:
ncol(datos_bicis)

# Hay 12 columnas. 

# 3. Genere una variable llamada "bici_tiempo_uso_mins" que indique la duración de cada viaje en
# minutos (es importante que la variable creadas pertenezca a las clase "numeric").

datos_bicis$bici_tiempo_uso_mins <- as.numeric((datos_bicis$fecha_destino_recorrido - datos_bicis$fecha_origen_recorrido))

# 4. Arme una nueva variable llamada "weekday" que indique el día de la semana de extracción de 
# cada bicicleta (se pide que los días de la semana estén expresados en números, siendo 0 el código
# asociado al domingo). Vean: https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html

datos_bicis$weekday <- as.numeric(strftime(datos_bicis$fecha_origen_recorrido, "%w"))


# 5. Arme una nueva columna llamada "fin_de_semana" que tenga el siguiente comportamiento: si la
# bicicleta fue extraída durante el fin de semana valga "Fin de semana", y que en caso contrario
# valga "Día de semana". Haga que la variable sea del tipo "factor"

datos_bicis$fin_de_semana <- ifelse(datos_bicis$weekday %in% c(6,0), "Fin de semana", "Día de semana")

# 6. Arme una variable llamada "hour" que contenga únicamente la hora de extracción de cada
# bicicleta (e.g.: 0, 1, ..., 23). Haga que la variable hour sea numérica.

datos_bicis$hour <- as.numeric(strftime(datos_bicis$fecha_origen_recorrido, "%H"))

# 7. Arme una variable llamada "misma_estacion", que valga "Misma estación" si la bicicleta
# se extrajo y se devolvió en la misma estación y "Distinta extación" en caso contrario.
# Haga que la variable sea de tipo "factor";

datos_bicis$misma_estacion <- ifelse(datos_bicis$nombre_estacion_origen == datos_bicis$nombre_estacion_destino, "Misma estación", "Distinta estación")

# 8. Arme una nueva variable llamada "fecha" que contenga la fecha de extracción con
# formato %Y-%m-%d.

datos_bicis$fecha <- strftime(datos_bicis$fecha_origen_recorrido, format = "%Y-%m-%d") 

# 9. Arme una figura que permita ver la distribución de la duración de los viajes (en minutos),
# según si la extracción haya sido hecha un fin de semana o no. Pueden ver ejemplos de cómo
# cambiar los límites de los ejes en ggplot en el siguiente link:
# https://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots
# ¿Nota diferencias en la distribución según si el día es o no fin de semana?



#CONSULTAR

ggplot(data = datos_bicis) + 
  aes(x = fin_de_semana, y = bici_tiempo_uso_mins, ylab = "Duracióm de viajes (en minutos") + 
  geom_boxplot() + 
  ylim(0,300)

# 10. Arme una figura que permita ver cómo se comporta la cantidad de extracciones (de todo el
# sistema) de un día típico/promedio a lo largo de las horas del día, permitiendo diferenciar
# el comportamiento de los días de semana de los que son fin de semana. Por ejemplo, debe
# permitir ver cuántas extracciones se realizan en un día de semana típico/promedio a las
# 10 am, a las 11 am y así. 

#Primero agrupamos por dia, hora y finde, y luego agrupamos por finde y hora. 

by_dia_hora_finde <- datos_bicis %>% 
  group_by(weekday, fin_de_semana, hour) %>% 
  summarise(cant = n())

by_hora_finde <- by_dia_hora_finde %>% 
  group_by(hour, fin_de_semana) %>% 
  summarise(extrac_prom = mean(cant)) 

# Ya tenemos el dataset para hacer el grafico, ahora graficamos:

ggplot(data = by_hora_finde, aes(x = hour, y = extrac_prom, colour = fin_de_semana)) + geom_line()


# 11. Filtren el data.frame datos_bicis de modo que el mismo sólo contenga extracciones
# realizadas en estaciones en las cuales se hayan extraído 5000 o más bicicletas durante el
# periodo analizado. Guarde estos datos en un nuevo data.frame que se llame
# "datos_est_grandes" (este data.frame sólo deberá ser usado en el siguiente punto)



datos_est_grandes <- datos_bicis %>% 
  group_by(nombre_estacion_origen) %>% 
  filter(n() > 5000)


prueba <- datos_bicis %>% 
  group_by(nombre_estacion_origen) %>% 
  summarise(cantidad_extracciones = n()) %>% 
  filter(cantidad_extracciones > 5000)

#PREGUNTAR SI ESTÁ BIEN


# 12. Utilizando los datos contenidos en "datos_est_grandes" armen una tabla que permita
# ver para cada estación, qué proporción de todas las personas que utilizaron dicha
# estación (i.e., que extrajeron al menos una bicicleta en la misma una bicicleta) la
# utilizan más de una vez.

prop.table(table(datos_est_grandes$nombre_estacion_origen, datos_est_grandes$id_usuario), margin = 1 )

#CONSULTAR

# 13. Armen una tabla que permita ver para las estaciones con 10000 o más extracciones
# la proporción de sus extracciones que tuvieron un tiempo de viaje mayor a 30 minutos.
# Ordenen los datos de manera que la tabla tenga como primeras filas aquellas estaciónes
# para los cuales el valor de esta proporción es mayor.

#creamos el dataset con los datos de las estaciones con mas de 10000 extracciones:
datos_ej13 <- datos_bicis %>% 
  group_by(nombre_estacion_origen) %>% 
  filter(n() > 10000)

#creamos una columna que indique si el viaje duró mas de 30 ninutos:
datos_ej13$duracion_mayor_30_minutos <- datos_ej13$bici_tiempo_uso_mins > 30
#creamos la tabla
tabla_ej13 <- prop.table(table(datos_ej13$nombre_estacion_origen, datos_ej13$duracion_mayor_30_minutos), margin = 1)

#lo pasamos a dataframe para ordenarlo:
tabla_ej13 <- as.data.frame(tabla_ej13)

#CONSULTAR


# 14. Arme una figura con facets en dónde los paneles distingan horizontalmente si se trata
# de un viaje en donde las bicicletas se extrajeron y devolvieron en la misma estación o no,
# y verticalmente distinga si la extracción fue hecha durante el fin de semana o no. A su vez,
# cada panel debe contener un gráfico que permita ver la distribución de la duración de los
# viajes (en minutos). Para hacer esta figura filtre todos los viajes con una duración mayor
# a 120 minutos.

#filtramos los viajes con una duracion mayor a 120 minutos:

bicis_120 <- filter(datos_bicis, bici_tiempo_uso_mins > 120)

#construimos el boxplot
ggplot(data = bicis_120) + aes(y = bici_tiempo_uso_mins) + 
  geom_boxplot() + 
  facet_wrap(misma_estacion ~ fin_de_semana) +
  ylim(0,1000)

#CONSULTAR

# 15. Guarde la figura del punto 14 en un archivo con formato jpg.

g <- ggplot(data = bicis_120) + aes(y = bici_tiempo_uso_mins) + 
  geom_boxplot() + 
  facet_wrap(misma_estacion ~ fin_de_semana) +
  ylim(0,1000)

ggsave("fig_Ej14.jpg", g, width = 15, height = 10, units = "cm")

# 16. Haga una figura que permita ver las 15 estaciones de origen con mayor cantidad de
# extracciones. Los nombres de las estaciones se deben ver de forma descendente segun
# la cantidad de extracciones (la cual debe estar expresada en miles).

# agrupamos por estaciones:

datos_ej16 <- datos_bicis %>% 
  group_by(nombre_estacion_origen) %>% 
  summarise(cantidad_extracciones = n())

#ordenamos por cantidad de extracciones:
datos_ej16 <- arrange(datos_ej16, desc(cantidad_extracciones))

#nos quedamos con las 15 primeras:

datos_graf16 <- datos_ej16[1:15,] 

# graficamos:
ggplot(datos_graf16, aes(x=reorder(nombre_estacion_origen, -cantidad_extracciones), y=cantidad_extracciones, fill = nombre_estacion_origen)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=cantidad_extracciones), color = "black", vjust = 1 ) 

#se puede mejorar la estetica, pero se muestra lo que pide el ejercicio. 


















