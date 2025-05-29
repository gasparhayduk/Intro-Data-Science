rm(list=ls())
# setwd("/Users/luis/Documents/Universidad/Introducción a la Data Science")  # Definan el directorio de trabajo

setwd("~/Desktop/IntroDS/Taller02")

# Importe aquí todas las librerías que use
library("dplyr")
library("ggplot2")

# APELLIDO Y NOMBRE DE LOS INTEGRANTES DEL GRUPO:
#	Integrante 1: Luis Gonzalez Lelong  
#	Integrante 2: Gaspar Hayduk

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

#  datos_bicis <- load("/Users/luis/Documents/Universidad/Introducción a la Data Science/recorridos-realizados-2019.RData")

datos_bicis <- load("recorridos-realizados-2019.RData")

# 2. Calcule cuántas filas y cuántas columnas tiene el data.frame datos_bicis.

nrow(datos_bicis)
# Hay 2473042 filas/observaciones.

ncol(datos_bicis)
# Hay 12 columnas.


# 3. genere una variable llamada "bici_tiempo_uso_mins" que indique la duración de cada viaje en
# minutos (es importante que la variable creadas pertenezca a las clase "numeric").

datos_bicis$bici_tiempo_uso_mins <- 
  as.numeric((datos_bicis$fecha_destino_recorrido - datos_bicis$fecha_origen_recorrido))

# 4. Arme una nueva variable llamada "weekday" que indique el día de la semana de extracción de 
# cada bicicleta (se pide que los días de la semana estén expresados en números, siendo 0 el código
# asociado al domingo). Vean: https://stat.ethz.ch/R-manual/R-devel/library/base/html/strptime.html

datos_bicis$weekday <- as.numeric(strftime(datos_bicis$fecha_origen_recorrido, "%w"))

# 5. Arme una nueva columna llamada "fin_de_semana" que tenga el siguiente comportamiento: si la
# bicicleta fue extraída durante el fin de semana valga "Fin de semana", y que en caso contrario
# valga "Día de semana". Haga que la variable sea del tipo "factor"

datos_bicis$fin_de_semana <- ifelse(datos_bicis$weekday %in% c(6,0), "Fin de semana",
                                    "Día de semana")

# 6. Arme una variable llamada "hour" que contenga únicamente la hora de extracción de cada
# bicicleta (e.g.: 0, 1, ..., 23). Haga que la variable hour sea numérica

datos_bicis$hour <- as.numeric(strftime(datos_bicis$fecha_origen_recorrido, "%H"))

# 7. Arme una variable llamada "misma_estacion", que valga "Misma estación" si la bicicleta
# se extrajo y se devolvió en la misma estación y "Distinta estación" en caso contrario.
# Haga que la variable sea de tipo "factor";

datos_bicis$misma_estacion <- ifelse(datos_bicis$nombre_estacion_origen == datos_bicis$nombre_estacion_destino,
                                     "Misma estación", "Distinta estación")

# 8. Arme una nueva variable llamada "fecha" que contenga la fecha de extracción con
# formato %Y-%m-%d.

datos_bicis$fecha <- strftime(datos_bicis$fecha_origen_recorrido, "%Y-%m-%d")

# 9. Arme una figura que permita ver la distribución de la duración de los viajes (en minutos),
# según si la extracción haya sido hecha un fin de semana o no. Pueden ver ejemplos de cómo
# cambiar los límites de los ejes en ggplot en el siguiente link:
# https://stackoverflow.com/questions/3606697/how-to-set-limits-for-axes-in-ggplot2-r-plots
# ¿Nota diferencias en la distribución según si el día es o no fin de semana?

# Realizamos el gráfico usando la librería ggplot. Elegimos crear una figura tipo boxplot.
# Cambiamos los ejes para eliminar los outliers del gráfico.

ggplot(data = datos_bicis) +
  geom_boxplot(mapping = aes(x = fin_de_semana, y= bici_tiempo_uso_mins)) +
  coord_cartesian(ylim = c(0, 100)) +
  ggtitle("Distribución de la duración de los viajes") +
  theme_bw() +
  ylab("Duración del viaje (mins)") + xlab("Día") +
  theme(plot.title = element_text(hjust = 0.5))

# Se puede apreciar como en los fines de semana la media de la duración de los viajes es más
# alta y además las observaciones se encuentran menos concentradas alrededor de la media en 
# comparación con los días de semana.


# 10. Arme una figura que permita ver cómo se comporta la cantidad de extracciones (de todo el
# sistema) de un día típico/promedio a lo largo de las horas del día, permitiendo diferenciar
# el comportamiento de los días de semana de los que son fin de semana. Por ejemplo, debe
# permitir ver cuántas extracciones se realizan en un día de semana típico/promedio a las
# 10 am, a las 11 am y así.

# Como vamos a necesiar hacer un promedio, primero calculamos cuántos días de semana y 
# cuántos sábados y domingos tenemos en la muestra. Para esto creamos dos variables
# agrupadas por la fecha y filtradas de acuerdo a sí son fin de semana o día de semana. 


n_finde <-
  nrow(datos_bicis %>% group_by(fecha) %>% filter(fin_de_semana == "Fin de semana") %>%
  summarise(n()) %>% ungroup())

n_sem <- 
  nrow(datos_bicis %>% group_by(fecha) %>% filter(fin_de_semana == "Día de semana") %>%
                summarise(n()) %>% ungroup())

# Ahora creamos dos data frames que nos permita saber cuántas observaciones hay para cada
# hora, uno para los días de semana y otro para los fines de semana. Dividimos las 
# observaciones de cada hora por el total de días de acuerdo a la categoría para obtener el 
# promedio.
hora_ext_semana <- as.data.frame(datos_bicis %>% group_by(hour) %>% 
                                   filter(fin_de_semana == "Día de semana") %>%
                                   summarise(extracciones_sem = n()/n_sem) %>% ungroup())

hora_ext_finde <- as.data.frame(datos_bicis %>% group_by(hour) %>% 
                                   filter(fin_de_semana == "Fin de semana") %>%
                                   summarise(extracciones_finde = n()/n_finde) %>% ungroup())

# Unimos los dos data frames en uno para simplificar el uso de ggplot.

hora_ext <- left_join(hora_ext_semana, hora_ext_finde, by = "hour")

# Usamos ggplot para crear el gráfico.

ggplot(hora_ext, aes(x = hour)) + 
  geom_line(aes(y = extracciones_sem, color = "Día de semana")) + 
  geom_line(aes(y = extracciones_finde, color = "Fin de semana")) + 
  xlab("Hora") + ylab("Número de extracciones promedio") + 
  theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("red", "blue")) +
  ggtitle("Cantidad de extracciones promedio por hora")


# 11. Filtren el data.frame datos_bicis de modo que el mismo sólo contenga extracciones
# realizadas en estaciones en las cuales se hayan extraído 5000 o más bicicletas durante el
# periodo analizado. Guarde estos datos en un nuevo data.frame que se llame
# "datos_est_grandes" (este data.frame sólo deberá ser usado en el siguiente punto)

# Agrupamos por el nombre de la estación de origen y luego filtramos para quedarnos con 
# aquellas que tengan 5000 extracciones o más.

datos_est_grandes <- 
  datos_bicis %>% group_by(nombre_estacion_origen) %>% filter(n() >= 5000) %>% ungroup()

# 12. Utilizando los datos contenidos en "datos_est_grandes" armen una tabla que permita
# ver para cada estación, qué proporción de todas las personas que utilizaron dicha
# estación (i.e., que extrajeron al menos una bicicleta en la misma estación) la
# utilizan más de una vez.

# Creamos otro data frame que tenga las observaciones agrupadas por el nombre de la estación
# y el ID del usuario. De esta forma podemos determinar cuántas veces una misma persona está
#utilizando una determinada estación. 

datos_est_grandes2 <- 
  datos_est_grandes %>% 
  group_by(nombre_estacion_origen, id_usuario) %>% summarise(N = n()) %>% ungroup()

# Como queremos analizar la cantidad de gente que utilizan la misma estación más de una vez
# creamos una columna que indique "TRUE" si la persona usó la estación más de una vez y
# "FALSE" en caso contrario.

datos_est_grandes2$repite <- (datos_est_grandes2$N > 1)

# Agrupamos por el nombre de la estación y por la columna que indica si la persona usó la 
# estación más de una vez  o no. Luego creamos una tabla que nos muestre las probabilidades
# de que una persona la use más de una vez o no para cada estación.

datos_est_grandes2 %>% group_by(nombre_estacion_origen, repite) %>% summarise(n()) %>% ungroup()

prop_rep <- prop.table(table(datos_est_grandes2$nombre_estacion_origen, datos_est_grandes2$repite),
           margin = 1)
prop_rep <- na.omit(prop_rep)

# FALSE = la utilizan una sola vez
# TRUE = la utilizan más de una vez


# 13. Armen una tabla que permita ver para las estaciones con 10000 o más extracciones
# la proporción de sus extracciones que tuvieron un tiempo de viaje mayor a 30 minutos.
# Ordenen los datos de manera que la tabla tenga como primeras filas aquellas estaciónes
# para los cuales el valor de esta proporción es mayor.

# Creamos un nuevo data frame, agrupamos por estación de origen, y filtramos para quedarnos
# con las que tengan más de 10000 extracciones.

datos_13 <- 
  datos_bicis %>% group_by(nombre_estacion_origen) %>% filter(n() >= 10000) %>% ungroup()

# Creamos una nueva columna que indique "TRUE" si la bicicleta se utilizó por más de 30 
# minutos y "FALSE" en caso contrario.

datos_13$is.more.30 = (datos_13$bici_tiempo_uso_mins > 30)

# Creamos una tabla que nos indique cuáles son las probabilidades de que un viaje dure más 
# de 30 minutos y de que dure menos de 30 minutos para cada estación.

prop_30 <- prop.table(table(datos_13$nombre_estacion_origen, datos_13$is.more.30),
           margin = 1)
prop_30 <- na.omit(prop_30)

# Convertimos la tabla en un data frame para poder ordernarla.
prop_30_df <-
as.data.frame.matrix(prop_30) %>% arrange(desc(prop_30[,2]))


# 14. Arme una figura con facets en dónde los paneles distingan horizontalmente si se trata
# de un viaje en donde las bicicletas se extrajeron y devolvieron en la misma estación o no,
# y verticalmente distinga si la extracción fue hecha durante el fin de semana o no. A su vez,
# cada panel debe contener un gráfico que permita ver la distribución de la duración de los
# viajes (en minutos). Para hacer esta figura filtre todos los viajes con una duración mayor
# a 120 minutos.

# Creamos un nuevo data frame que elimine las observaciones que duren más de 120 minutos.

datos_14 <-
  datos_bicis %>% filter(bici_tiempo_uso_mins < 120)

# Creamos una variable g que guarde el gráfico creado utilizando ggplot2.

g <-
ggplot(datos_14, aes(y=bici_tiempo_uso_mins)) +
  geom_boxplot() +
  facet_grid(rows = vars(fin_de_semana), cols = vars(misma_estacion)) +
  theme_bw() + xlab("Tiempo de uso (mins)") +
  ggtitle("Distribución de la duración de los viajes") +
  theme(plot.title = element_text(hjust = 0.5))
  

# 15. Guarde la figura del punto 14 en un archivo con formato jpg.

ggsave("fig1.jpg", g)

# 16. Haga una figura que permita ver las 15 estaciones de origen con mayor cantidad de
# extracciones. Los nombres de las estaciones se de deben ver de forma descendente segun
# la cantidad de extracciones (la cual debe estar expresada en miles).

# Creamos un nuevo data frame. Primero agrupamos por el nombre de la estación, y luego usamos
# summarise para contar la cantidad de extracciones que tiene cada estación. Luego empleamos
# mutate para expresarlas en unidades de 1000. Finalmente ordenamos de forma que queden las
# estaciones con mayor cantidad de extracciones primero y nos quedamos con las primeras 15.

datos_16 <-
  datos_bicis %>% group_by(nombre_estacion_origen) %>% summarise(count = n()) %>%
  mutate(count = count/1000) %>%
  arrange(desc(count)) %>%
  head(15) %>%
  ungroup()

# Graficamos utilizando ggplot2.

ggplot(datos_16, aes(x = reorder(nombre_estacion_origen, count), y = count)) +
  geom_col() +
  coord_flip() +
  theme_bw() + 
  ylab("Cantidad de extracciones (miles)") + xlab("Estación de origen") +
  ggtitle("Cantidad de extracciones por estación") +
  theme(plot.title = element_text(hjust = 0.5))
