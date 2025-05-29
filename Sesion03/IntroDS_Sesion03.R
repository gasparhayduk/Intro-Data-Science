rm(list=ls())
setwd("~/Desktop/IntroDS/Sesion03") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Bajamos los datos
ggal <- read.table("https://query1.finance.yahoo.com/v7/finance/download/GGAL?period1=1616358367&period2=1647894367&interval=1d&events=history",
                   header=TRUE, sep=",")
#notar que buscamos los datos en una direccion de internet. Es la direccion de yahoo finance. 

bbva <- read.table("https://query1.finance.yahoo.com/v7/finance/download/BBAR?period1=1616358367&period2=1647894367&interval=1d",
                   header=TRUE, sep=",")

# Ponemos agregamos una columna con el nombre, para que no haya lio al unir ambos dataframes. 
ggal$quote <- factor("GGAL")
bbva$quote <- factor("BBVA")

# Ahora unimos los data.frames poniendo uno debajo del otro. 
quoteData <- rbind(ggal, bbva)

# Veamos cómo usar cbind con dos data.frames de juguete. cbind() es peligroso de usar 
df1 <- data.frame(var11=1:5,var12=6:10)
df2 <- data.frame(var21=letters[1:5], var22=letters[6:10])
cbind(df1, df2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~bicisEnriquecido.RData~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

load("bicisEnriquecido.RData") #es el dataset de la Sesion02 que modificamos al final. 

estaciones <- read.table("estaciones-de-bicicletas-publicas.csv",
                         sep=";", header=TRUE, stringsAsFactors = TRUE)

# Hago el merge
bicis <- merge(bicis,
               estaciones[,c("origenestacionid", "latitud", "longitud")],
               by.x="destinoestacionid",
               by.y=c("origenestacionid") , all.x=TRUE)
# 'bicis' es el dataset que vamos a a unirle una variable o agregar una columna. Es el 'X'
#el 'by=c("origenestacionid")' indica por la variable que vamos a unir. 
#el 'estaciones[,c("origenestacionid", "latitud", "longitud")]' son las variable que vamos a agregarle a X. Si poniamos solo estaciones, agregaba todas las variables de estaciones. 
# 'all.x=TRUE' indica que mantenemos todas las filas del dataset X

#~~~~~~~~~~~~~~~~~Reshape (pasar de formato wide a long)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#instalamos la libreria 'reshape2':
install.packages("reshape2")  # Esto se debe hacer una única vez por computadora. Al hacer esto, ya queda instalada
library("reshape2")  # Esto se debe hacer cuando se inicia la sesión. Aun cuando ya este instalada, hay que cargarla y activarla. 

# update.packages(ask = FALSE) . Esto sirve para actualizar las librerias que tengamos instaladas. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
 
head(airquality) # airquality es un dataset de R. Para cada dia, tenemos cuatro mediciones. Está en formato wide. 
#queremos pasar de formato wide a long: 
aql <- melt(airquality) #asi solo está mal. Hay que pasarle mas informacion. Por ejemplo, no incorpora qué dia es cada fila. 
head(aql)
tail(aql)

#forma correcta: 
aql <- melt(airquality, id.vars = c("Month", "Day"), 
            measure.vars = c("Wind", "Temp"), #solo le pasamos estas variables
            variable.name = "climate_variable", 
            value.name = "climate_value")
head(aql)

#forma completa, todas las variables: 
aql <- melt(airquality, id.vars = c("Month", "Day"),
            variable.name = "climate_variable", #identificador para la variable
            value.name = "climate_value") #identificador para el valor de la variable
head(aql)  # Noten que contiene la misma información que el df original
#esta ultima forma es la que usa ggplot. 

#~~~~~~~~~~~~~~~~~~~~~~~Reshape (pasar de formato long a wide)~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

aqw <- dcast(aql, Month + Day ~ climate_variable,
             value.var = "climate_value")
#aql es el dataset en formato long 
#Month y Day son las variables identificadores. Seran las primeras dos columnas. 
#climate_variable indica donde estan las variables que seran futuras columnas 
# valur.bar = "climate_value" indica de donde saldran los valores para completar. 

#IMPORTANTE REFERENCIAR COLUMNAS POR NOMBRE, NO POR POSICION; PUES EL ORDEN DE LAS COLUMNAS PUEDE ALTERARSE CON DIFERENTES COLUMNAS. 
head(aqw)

#~~~~~~~~~~~~~~~~~~~~~~~~~DYPLYR: MANIPULACION DE DATAFRAMES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Instalamos y activamos las librerias: 
install.packages("dplyr") 
install.packages("nycflights13") # es un dataset que se encuentra en una libreria. 

# Activamos dichas librerias: 
library("dplyr")
library("nycflights13")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~FILTER~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


class(flights)
head(as.data.frame(flights))

class(as.data.frame(flights))

#filter sirve para filtrar filas (observaciones)

filter(flights, month == 1) #nos quedamos con los filas del Mes 1 (enero). 
filter(flights, month == 2, day == 3) #nos quedamos con las filas que cumplan month=2 y day=3. Util para multiple condiciones. 
filter(flights, month != 1, day > 10)
filter(flights, month %in% c(3, 4), day <= 10)

#para usar una condicion de 'o' (es decir, union), hacemos un | 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ARRANGE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#arrange sirve para ordenar filas

arrange(flights, dep_time) #ordenamos las filas por departure_time. Por default, ordena de menor a mayor, de forma ascendente
tail(arrange(flights, dep_time))  # Los NA quedan al final
arrange(flights, year, month, day) #ordena primero por year, luego por mes y luego por dia. 
arrange(flights, desc(dep_time)) #ordena las filas por departure time de forma descendente, del mayor a menor. 
tail(arrange(flights, desc(dep_time))) # Los también NA quedan al final
arrange(flights, desc(carrier), dep_time)
arrange(flights, desc(arr_time - dep_time))  # Se puede poner operaciones

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~SELECT~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#el select sirve para seleccionar columnas de un dataframe

select(flights, year) #seleccionamos la columna year
select(flights, year, month, day) #seleccionamos las columnas year, month y day 
select(flights, year:day) #seleccionamos las columnas entre year y day, incluyendolas. 
select(flights, -month) #seleccionamos todas las columnas excepto month
select(flights, -year, -month) #seleccionamos todas las columnas excepto year y month 
select(flights, -(year:day))

#rebombrar columnas:
rename(flights, tail_num = tailnum) #tail_num es como se llamaba y tailnum como queremos que se llame
rename(flights, mes = month, dia = day) #podemos renombrar varias columnas a la vez. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~MUTATE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#el mutate sirve para crear o modificar variables. Genera nuevas columnas 

flights_sml <- select(flights, year:day, dep_delay,
                      arr_delay, distance, air_time) #creamos un dataset para modificarlo. 

mutate(flights_sml,
       gain = arr_delay - dep_delay, #creamos la variable 'gain'
       speed = distance / air_time * 60) #creamos la variable 'speed' 
# notar que no creamos un dataset nuevo, solo nos lo muestra. 

# mutate nos permite crear variables en una sola linea de codigo. 

mutate(flights_sml,
       gain = arr_delay - dep_delay,
       hours = air_time / 60,
       gain_per_hour = gain / hours)  # Se usa la variable creada en el mismo mutate. Mas rapido

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~SUMMARISE~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# el summarise sirve para calcular metricas. Es MUY util combinado con un group_by. 

summarise(flights, delay = mean(dep_delay, na.rm = TRUE)) #calculamos la media de departure delay

#podemos calcular varias metricas a la vez:
summarize(flights,
          mean_delay = mean(dep_delay, na.rm = TRUE),
          median_delay = median(dep_delay, na.rm = TRUE))


by_month <- group_by(flights, year, month) # crea grupos con las combinaciones de year y mes. No cambio mucho el dataset

# el group_by 

by_month

#lo que cambia es que las operaciones pasan a hacerse por grupo. 
summarise(by_month, delay = mean(dep_delay, na.rm = TRUE), cant = n()) 
#nos calcula la media de dep_delay para cada grupo. 
#Es decir, tenemos dep_delay para el mes 1 del year 2013, para el mes 2 de year 2013 y asi con todos los grupos. 
#el cant = n() sirve para que nos diga cuantas filas (observaciones) tiene cada grupo. 


# no es necesario crear un dataset nuevo agrupado, podemos calcular metricas por grupo sobre el dataset completo. 


# Otra cosa util es que podemos combinar las funciones de DYPLYR anidandolas. Sin embargo, se hace dificil de leer. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# DYPLYR tiene una forma de combinar sus funciones mediante %>% (pipes): 
 

by_dest <- group_by(flights, dest)

delay <- summarise(by_dest, count = n(),
                   dist = mean(distance, na.rm = TRUE),
                   delay = mean(arr_delay, na.rm = TRUE))

delay <- filter(delay, count > 20, dest != "HNL")
#notar que hicimos una operacion en tres lineas. 

# La idea de pipes es juntar (encadenar) operaciones que se hacen en muchas lineas y hacerlo todo en una. 
# Ahora hacemos lo que hicimos en tres lineas diferentes en una sola linea de codigo. 

flights %>%
  group_by(dest) %>% 
  summarise(count = n(), dist = mean(distance, na.rm = TRUE), delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "WHL") 



flights %>% group_by(year, month, day) %>% 
    summarise(mean = mean(dep_delay, na.rm = TRUE))
# a flight lo agrupo por year, month, day, y hago una media de dep_delay para cada grupo. 
flights
flights %>% group_by(dest)
flights %>% group_by(dest) %>% filter(n() > 365)
flights %>% group_by(dest) %>% filter(n() > 365) %>% ungroup() #con el final desagrupamos. 

# es buena practica desagrupar al final, porque nos podemos olvidar y luego calculamos cosas por grupo cuando queremos calcular no por grupo. 

#~~~~~~~~~~~~~~~~PARA HACER: DYPLYR ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# En una línea seleccionen todos los vuelos de aerolíneas (carrier) 
# que en la base tienen un promedio de dep_delay mayor a 18 minutos.


flights %>% 
  group_by(carrier) %>%
  filter(mean(dep_delay, na.rm = TRUE) > 18) %>% #la operacion de mean(dep_delay) se hace POR GRUPO DE AEROLINEA. 
  ungroup()

# otra solucion:
flights %>% 
  group_by(carrier) %>%
  mutate(prom_dep_delay = mean(dep_delay, na.rm = TRUE)) %>% #aca creamos una nueva columna donde nos diga cual es el promedio de dep_delay de la aerolinea del vuelo
  filter(prom_dep_delay > 18) %>% ungroup() 





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~GGPLOT2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

install.packages("ggplot2") # instalamos ggplot2, se hace una unica vez
library("ggplot2") #activamos la libreria, se hace siempre que se va a usar. 

?mpg # dataset que esta en R
head(mpg)
# displ hace referencia al tamaño del motor
# hwy hace referencia a la eficiencia de consumo de nafta

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# con ggplot(data = mpg) le damos el input de los datos. 'data' es el parametro, le indicamos que es mpg. 
# geom_point(mapping = aes(x = displ, y = hwy)) le indicamos que tipo de grafico vamos a hacer. geom_point es la nube de puntos. 
# a geom_point le indicamos qué va em cada eje. eso se hace con 'aes()'. 
# mapping es el mapeo. 
# le indicamos que en el eje de las X va displ, y que en el eje de la Y va hwy. displ y hwl son columnas del dataset. 
# el + es para juntar operaciones. 


# ver la cheat sheet para ver qué le podemos pasar a geom_point: color, fill, etc...

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class))
# haceoms lo mismo que antes, pero el color de los puntos vienen dados por la columna class. 
# esto se lo indicamos con 'color'
# automaticamente te agrega una leyenda que te indica a qué pertenece cada color. 

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class, size=cyl)) 
# ahora, le agregamos un nuevo componente. El tamaño de los puntos vendra dado por la columna cyl. 
# esto se lo indicamos con 'size' 
# este tipo de graficos donde el tamaño de los puntos viene dado por el valor de una variable es que podemos tapar puntos.
# podemos agregar transparencia para evitar esto. EN GGPLOT2, ESTO SE HACE CON "alpha" y va FUERA del aes()

# por default, alpha vale 1. Con hacer alpha = 0.3, le damos mas transparencia

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, size=cyl), #notar que el color de cada punto NO va por el valor de una columna
             # el tamano de cada punto viene dado por la variable numerica cyl.
             alpha=0.3, color="red")  # Si algo es fijo para todas las observaciones, va fuera de aes(). 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# FACETS:

# facet_wrap: sirve para hacer un grafico para cada clase. nos muestra diferentes paneles.
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, col = trans)) + 
  facet_wrap(~ class, nrow = 3) # esto ultimo nos dice que hace un panel de acuerdo a la variable categorica 'class'.
# el  nrow = 3 indica que divide los paneles en tres filas. Ir cambiando para ver como queda. 
# como hay siete categorias de class, nos mostrara siete paneles. Una nube de puntos para cada categoria. 


# facet_grid: hace paneles para cada combinacion de variables. Generalmente se hace en funcion de dos variables
# las variables que van en el face_grid NO son las que van en el geom(). 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(fl ~ cyl) 
# el 'fl' indica el eje horizontal
# el 'cyl' indica el eje vertical. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# GEOM() 

ggplot(data = mpg) + geom_point(mapping = aes(x=displ, y=hwy)) +
  geom_smooth(mapping = aes(x=displ, y=hwy)) # se pueden combinar diferentes geom()
#geom_smooth() nos da como una tendencia suave de como se distribuyen los puntos. 


# si ponemos methos = "lm" dentro del geom_smooth(), hace la tendencia de acuerdo a una regresion lineal. 
# el SE dentro de geom_smooth( nos da los intervalos de confianza. 

# ver ?geom_smooth para mas detalles. 

ggplot(data = mpg, mapping = aes(x=displ, y=hwy)) #podemos indicar los ejes dentro de ggplot(), pero hay que indicarle luego que mapeo se hace. 
+ geom_point() + geom_smooth() # Se comparte entre geoms_

ggplot(data = mpg, mapping = aes(x=displ, y=hwy, color=drv)) +
  geom_point() + geom_smooth() #cuando la agregamos los ejes en ggplot(), todas las capas que haremos se haran sobre esos ejos. 

# este ultimo grafico le da color a los puntos por una categoria, el geom_smooth() hace la tendencia para cada categoria de 'drv'. 

ggplot(mpg, aes(x = hwy)) + geom_density(fill="red", alpha = 0.3) 
#geom_density() nos grafica la densidad. 
# aca va 'hwy' en el eje X y densidad en el eje Y. 

ggplot(mpg, aes(x = hwy)) + geom_density(aes(fill=drv), alpha = 0.3) #aca hace una densidad para cada 'drv'. eso se lo indicamos en el fill. 

ggplot(mpg, aes(x = hwy)) + geom_histogram(fill = "red", alpha = 0.3)

ggplot(mpg, aes(x = factor(cyl), y = hwy)) + geom_boxplot()

ggplot(mpg, aes(x = factor(cyl), y = hwy)) + geom_boxplot() + 
  xlab("Cilindros") + ylab("Eficiencia") + coord_flip() +
  ggtitle("Distribución de eficiencia según cilindrada")+ theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

g <- ggplot(mpg, aes(x = factor(cyl), y = hwy)) + geom_boxplot() + 
       xlab("Cilindros") + ylab("Eficiencia") + coord_flip() +
       ggtitle("Distribución de eficiencia según cilindrada")+ theme_bw()  

# en 'g' guardamos las instrucciones del grafico. 

ggsave("fig1.pdf", g, width = 15, height = 10, units = "cm")  #guarda el grafico como pdf. 
# ggsave() esta dentro de ggplot2

ggsave("fig1.jpg", g, width = 15, height = 10, units = "cm") #guarda el grafico en formato jpg, tiene mas calidad que pdf. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
