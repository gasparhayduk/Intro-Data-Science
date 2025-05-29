rm(list=ls())
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Bajen el archivo "recorrido-bicis-2015.csv" a su computadora 
#inspeccionar getwd() 
setwd("C:Users/gasparhayduk/Home/Desktop/intro_datascience")# En lugar de ... pongan el directorio donde está el archivo. Terminar de completar

#seteamos el directorio:
setwd("~/Desktop/intro_datascience")

bicis <- read.table("recorrido-bicis-2015.csv", header = TRUE,
                    sep = ";", stringsAsFactors = TRUE) # Vean ?read.table
#cargamos el dataset. 'bicis' es el nombre del dataset.
#readtable() es la funcion mas general para cargar datasets. 
#el header= TRUE significa que la primera fila es el nombre de las columnas. 
#el stringsAsFactors = TRUE significa 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Revisamos un poco el dataset...
head(bicis) #hacemos una previsualizacion 
dim(bicis) #obtenemos la cantidad de filas y columnas. 

# ¿En qué estación se retiran más bicicletas?
which(table(bicis$nombreorigen) == max(table(bicis$nombreorigen))) #obtenemos en que estacion se retiran mas bicis
# el which() sirve para preguntar en qué posicion estan ubicados los TRUE de algo.
which.max(table(bicis$nombreorigen))  # Lo mismo pero más simple

# ¿En qué estación es más común que se saque una bicicletas y se vuelva a devolver en la misma estación?
bicis$mismaest <- bicis$origenestacionid == bicis$destinoestacionid #creamos una nueva variable en el dataset (una nueva columna) que valga TRUE si se saca y devuelve una bici en la misma estacion.
#si creamos una nueva variables afuera del dataset podemos tener problemas con las observaciones si luego queremos ordenar por algo. 
#si queremos crear una nueva variable siempre es util insertarla en el dataset. 

tablaCruzada <- table(bicis$nombreorigen, bicis$mismaest) 
#creamos una tabla donde para cada estacion nos diga cuantas extracciones se devolvieron en la misma estacion (le segunda columna, TRUE) y cuantas no (FALSE)
#si queremos 
names(which.max(prop.table(tablaCruzada, margin = 1)[,2])) 
# con 'prop.table(tablaCruzada, margin = 1)', obtenemos una tabla que nos dice, para cada estacion, qué % de las extracciones de bicis se devuelvenen la misma estacion. usamos el '[,2]' para quedarnos con la segunda columna. 
# y luego vemos en qué estación es más común que se saque una bicicletas y se vuelva a devolver en la misma estación

prop.table(table(origen = bicis$nombreorigen,
                 destino = bicis$nombredestino), margin = 1) 
#creamos una tabla que nos diga, para cada estacion, qué de las extracciones terminan en las otras estaciones. Con el margin = 1 marginaliza por fila. 



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

str(bicis) #vemos la estructura de los datos del dataset, qué tipo de datos son. 

facVar <- as.factor(c("alto", "alto", "bajo", "mediano")) #creamos un vector [alto, alto, bajo, mediano] como factor. por eso el 'as.factor'
levels(facVar) #qué valores/niveles hay en facVar
facVar == "alto" 

facVar[2] <- "altísimo" # Ya vamos a ver que es NA
facVar
#si queremos agragar un nuevo nivel/valor, primero tenemos que qgregar ese nuevo nivel y luego agregarlo al vector.
levels(facVar) <- c(levels(facVar), "altísimo") #cargamos un nuevo nivel
facVar[2] <- "altísimo" #lo agregamos al vector. 
facVar

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Manipulacion de fechas:

Sys.time() #nos dice qué horario es ahora. 
as.numeric(Sys.time()) #obtenemos el horario de ahora en forma numerica, por swgundos desde 1970
#strptime(): le pasamos una feche en formato character, le pasamos el formato de fecha que queramos  y lo transforma a formato fecha.
strptime("22:05:32", "%H:%M:%OS") #el "%H:%M:%OS" setea el formato fecha hora, minuto y segundo
strptime("2015/5/6", "%Y/%m/%d", tz = "America/Argentina/Buenos_Aires") 
strptime("2015/5/6 - 22:05:32", "%Y/%m/%d - %H:%M:%OS", tz = "GMT")
#si hacemos as.numeric() y le pasamos una fecha (el resultado de strptime()), lo transforma a numero, a segundos desde 1970. 

# Vean ?strptime y ?strftime
strftime(Sys.time(), format = "%w")  # Devuelve el día de la semana de hoy, si ponemos "%A" nos dice 'Jueves'
strftime(Sys.time(), format = "%W")  # Devuelve la semana del año de hoy. 
strftime(Sys.time(), format = "%OS") # Devuelve el segundo de este momento.
strftime(Sys.time(), format = "%d")  # Devuelve el día.
strftime(Sys.time(), format = "%m")  # Devuelve el mes que estamos hoy.
strftime(Sys.time(), format = "%Y")  # Devuelve el año.
strftime(Sys.time(), format = "%Y-%m-%d")  # Devuelve la fecha.

# Reemplacen XXX por su fecha de nacimiento en formato Año-MES-DIA, ¿qué devuelve la siguiente línea? Devuelve los segundos que vivimos
z <- Sys.time() - strptime("XXX", format = "%Y/%m/%d")
as.numeric(z, units = "hours") #transformamos los segundos que vivimos a horas. 
# Vean "Data Manipulation with R. Cap 4."

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Mejoramos las clases de las variables del dataset:

# Pasemos a factor lo que debe ser factor, pasamos a factor lo que sea id, ya que no vamos a sumar ni operar con ellos. 
bicis$bicicletaid <- as.factor(bicis$bicicletaid)
bicis$usuarioid <- as.factor(bicis$usuarioid)
bicis$origenestacionid <- as.factor(bicis$origenestacionid)
bicis$destinoestacionid <- as.factor(bicis$destinoestacionid)
#bicis$nombreorigen <- as.factor(bicis$nombreorigen)
#bicis$nombredestino <- as.factor(bicis$nombredestino)

# Pasemos a variables de tiempo (que son character hasta ahora) lo que corresponde que sea tiempo, a formato fecha. 'POSIXlt' es el nombre que le da R a las fechas en formato fecha. 
bicis$origenfecha <- strptime(bicis$origenfecha,
                              format = "%Y-%m-%d %H:%M:%OS",
                              tz = "America/Argentina/Buenos_Aires")
bicis$destinofecha <- strptime(bicis$destinofecha,
                               format = "%Y-%m-%d %H:%M:%OS",
                               tz = "America/Argentina/Buenos_Aires")

# Creo variables de hora y día de la semana de las extracciones. asaeran factores. 
bicis$origenhora <- factor(strftime(bicis$origenfecha, "%H")) #nos da la hora de la extraccion.
bicis$diaSemana <- factor(strftime(bicis$origenfecha, "%w")) #nos da el dia de la semana de la extraccion. 
bicis$finSemana <- bicis$diaSemana %in% c("6", "7") #creamos una variable que nos diga si la extraccion fue durante el fin de semana.


# Comprobemos que haya quedado bien
str(bicis)


#EJERCICIO PARA HACER: 
# bicis$destinofecha - bicis%origenfecha #obtenemos el tiempo de uso en segundos, multiplicando por 60 lo pasamos a minutos. 

#podemos pisar la variable tiempouso y pasarla a minutos:
bicis$tiempouso = (bicis$destinofecha - bicis$origenfecha)/60


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Guardamos archivos:

write.table(bicis, "bicisEnriquecido.txt", sep = "\t", row.names = FALSE) #el problema de esto es que perdemos el tipo de dato asignado a cada variables, hay que cambiar el formato de fecha otra vez. 

save(list = c("bicis"), file = "bicisEnriquecido.RData") #lo guarda en formato R, 
#en el primer argumento ponemos lo que queremos guardar, el elemento: el dataset en ese caso. Conserva el formato (clase) de los datos, los cambios que hayamos hecho. 

load("bicisEnriquecido.RData") #es MUCHO mas rapido que read.table()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#NA, Inf y NaN:

Inf + 1; Inf - 1; Inf - Inf

NaN + 1; NaN - 1; NaN - NaN

NA + 1; NA - 1; NA - NA

c(1/0, 0/0, NA)
is.na(c(1/0, 0/0, NA)) #da TRUE cuando hay NA o NaN, por eso da TRUE para los dos ultimas
is.nan(c(1/0, 0/0, NA))
c(1/0, 0/0, NA, -1/0)
is.infinite(c(1/0, 0/0, NA, -1/0))

sum(is.na(c(1,NA,20,NA)))
#como TRUE vale 1. aca podemos ver cuantos NA hay. 

anyNA(c(1, 2, 3))
anyNA(c(NaN, 2, 3))
anyNA(c(Inf, 2, 3))
anyNA(c(NA, 2, 3))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

inc <- read.table("income.txt", sep = "\t", header =  TRUE,
                  stringsAsFactors = TRUE) 
#cargamos el dataset. 

head(income) #vemos una previsualizacion del dataset. 

summary(inc) #Resumen estadistico de las variables del dataset. Nos dice cuantos missings (NA) hay para cada variable. 

#la funcion complete.cases() recibe un dataset, o un vector, como parametro y chequea fila por fila y devuelve un vector con valores TRUE y FALSE y nos dice si la fila estq completa (no hay ningun NA)
complete.cases(inc) 
head(inc[complete.cases(inc),], 30) #con inc[complete.cases(inc),] obtenemos el dataset con las filas sin ningun NA. 
#si hacemos inc[!complete.cases(inc),] obtenemos el dataset con filas que tengan al menor un NA. 
head(inc[!complete.cases(inc),], 30)

inc[inc$native.country == "Germany",] # ¿Está necesariamente mal? Notar que habra NAs pues hay filas donde hay NA en country, y compara un NA con Germany, y eso da NA. Lo incorpora. 
#R incorpora las filas con NA cuando filtramos por columna, pues no sabe si deberia estar o no.
inc[inc$native.country=="Germany" & !is.na(inc$native.country),] #aca nos quedamos con los que estamos seguro que son Germany, asi omitimos los NA. 

mean(inc$age) #Devuelve NA pues hay observaciones con NA en la edad. 
mean(inc$age[complete.cases(inc$age)]) #aca computamos el promedio omitiendo los NA, pero es molesto. Quisieramos que R omita los NA sin hacer esto
mean(inc$age, na.rm = TRUE) #con na.rm = TRUE omitimos los NA al tomar el promedio. ver ?mean(). sirve para otras funciones tambien. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Para hacer: NA, Inf, NaN:

#Ej1: Es más común que hombres o mujeres tengan NA en la variable workclass?

#primero hay que filtrar por sexo y luego ver cuantos NA hay en la variable workclass. 
sum(is.na(inc[inc$sex == "Female" , "workclass" ])) #para las mujeres, en terminos absolutos

sum(is.na(inc[inc$sex == "Male" , "workclass" ])) #para los hombres, en terminos absolutos.

#si queremos en porcentaje:
sum(is.na(inc[inc$sex == "Female" , "workclass" ])) / sum(inc$sex == "Female") #para las mujeres, en porcentaje.

sum(is.na(inc[inc$sex == "Female" , "workclass" ])) / sum(inc$sex == "Male") #para los hombres, en porcentaje.


#Otra forma es usar un prop.table: los table() ignoran los NA
table(inc$sex)

table(inc$sex, is.na(inc$workclass))

prop.table(table(inc$sex, is.na(inc$workclass)), margin = 1)


#Ej2: Cree una variable que se llame age_NA que valga TRUE si en age hay un NA y FALSE en caso contrario

inc$age_NA = is.na(inc$age) #con el inc%age_NA le agregamos esa vriable al dataset inc. 

#Ej3: Reemplace los valores NA de age por el promedio de age de las observaciones sin NA.
# forma 1: 
inc[is.na(inc$age), "age"] = mean(inc$age, na.rm = TRUE) 
#con is.na(inc$age) seleccionamos las filas que tengas NA en age.
# luego con inc[is.na(inc$age), "age"] nos quedamos con la columna age para las cuales hay NA en age. 

#forma 2: usamos el ifelse. ifelse(condition, accion 1, accion 2). Si la condicion 1 se cumple, hace la accion 1; sino hace la accion 2. 

ifelse(is.na(inc$age), mean(inc$age, na.rm = TRUE), inc$age)



#Ej4: Cree una variable que se llame occupation_NA que valga TRUE si en occupation hay un NA y FALSE en caso contrario


#Ej5: Reemplace los valores NA de occupation por el valor más frecuenta de occupation


#Ej6: Arme un nuevo data.frame llamado inc_NA que contenga sólo la filasque tienen NA en native.country



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Histograma base, toma ciertos parametros por default.
hist(mtcars$mpg)

# Histograma con mejor formato, lo personalizamos ,as. 
hist(mtcars$mpg, main = "Millas por galón",
     xlab = "Millas por galón", ylab = "Frecuencia", col = "grey",
     breaks = 10, xlim = c(5, 35)) 
#breaks es la cantidad de cortes. 
# xlim = c(5, 35) es los valores de Xs que toma para hacer. 


# Histograma que grafíca estimaciones de densidades
hist(mtcars$mpg, main = "Millas por galón",
     xlab = "Millas por galón", ylab = "Frecuencia", col = "grey",
     breaks = 10, xlim = c(5, 35), freq = FALSE) #con freq = FALSE lo vemos en terminos porcentuales.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

density(mtcars$mpg) #calcula la densidad de kernel

# Kernel density estimate básico
plot(density(mtcars$mpg)) #visualizamos la densidad de kernel de los mpg. 

# Kernel density estimates con mejor formato y personalizacion. 
plot(density(mtcars$mpg), main = "Millas por galón",
     xlab = "Millas por galón", col = "grey")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

boxplot(bicis[bicis$tiempouso > 0, "tiempouso"],
        ylab = "Tiempo usado (minutos)")

boxplot(log10(bicis[bicis$tiempouso > 0, "tiempouso"]),
    ylab = "Tiempo usado (minutos)")

bicis$finDeSemana <- ifelse(bicis$diaSemana == "6", "Fin de semana", "Dia de semana")

boxplot(tiempouso ~ finDeSemana,
        data = bicis[bicis$tiempouso > 0 & bicic$tiempouso <120 ,],
        ylab = "Tiempo usado (minutos)")

boxplot(I(log10(tiempouso)) ~ finDeSemana,
        data = bicis[bicis$tiempouso > 0,],
        ylab = "Tiempo usado (log de minutos)")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

barplot(table(mtcars$gear), xlab = "Cantidad de cambios",
        ylab = "Frecuencia")

barplot(table(mtcars$gear), xlab = "Frecuencia",
        ylab = "Cantidad de cambios", horiz=TRUE)

barplot(table(mtcars$gear), xlab = "Frecuencia",
        ylab = "Cantidad de cambios", horiz=TRUE, las = 1)

# Análisis de datos de bicicletas
estGrandes <- table(bicis$nombreorigen)[table(bicis$nombreorigen)>6000]
barplot(estGrandes, xlab = "Alquileres", ylab = "", horiz=TRUE)

barplot(sort(estGrandes), xlab = "Alquileres", ylab = "", 
        horiz=TRUE, main = "Préstamos por Estación", las=1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

plot(mtcars$mpg, mtcars$hp,
     main = "Relación entre hp y mpg",
     xlab = "Millas por galón",
     ylab = "Caballos de fueza", pch = 19, col = "orange")

abline(lm(hp ~ mpg, data = mtcars), col = "violet")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

alquilerPorHora <- table(bicis$origenhora)
x <- names(alquilerPorHora)
y <- as.numeric(alquilerPorHora)

plot(x, y, type = "n", xlab = "Hora", ylab = "Alquileres")
lines(x, y, type = "l", col = "red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

hist(mtcars$mpg, main = "Millas por galón",
     xlab = "Millas por galón", ylab = "Frecuencia", col = "grey",
     breaks = 10, xlim = c(5, 35), prob = TRUE)

lines(density(mtcars$mpg), col = "blue")

rug(mtcars$mpg)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Se setea la cantidad de figuras que habrá en un ventana
par(mfrow=c(2,1))

# Primer figura
plot(mtcars$mpg, mtcars$hp,
     main = "Relación entre hp y mpg",
     xlab = "Millas por galón",
     ylab = "Caballos de fueza", pch = 19, col = "orange")
abline(lm(hp ~ mpg, data = mtcars), col = "violet")

# Segunda figura
plot(density(mtcars$mpg), main = "Millas por galón",
     xlab = "Millas por galón", col = "grey")

par(mfrow=c(1,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Como JPG
jpeg("imagen01.jpg")   # abre el dispositivo
boxplot(I(log(tiempouso)) ~ finDeSemana,
    data = bicis[bicis$tiempouso > 0,],
    ylab = "Tiempo usado (log de minutos)")
dev.off()   # cierra el dispositivo

# Como pdf
pdf("imagen01.pdf")   # abre el dispositivo
boxplot(I(log(tiempouso)) ~ finDeSemana,
    data = bicis[bicis$tiempouso > 0,],
    ylab = "Tiempo usado (log de minutos)")
dev.off()   # cierra el dispositivo
  