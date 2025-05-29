#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Operaciones algebraicas básicas
1 - 2 * 3 + 5
(1 - 2) * 3 + 5
3.5 ^ (1 / 2)
3.5 ^ 1 / 2

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Ejemplo de funciones simples
sqrt(1 + 2) #calcula la raiz cuadrada de (1+2), (1+2) es el parametro de la funcion sqrt()
qnorm(p = 0.95) #como no le indico mean y sd, asume que la media es cero y la varianza uno
qnorm(p = 0.95, mean = 0, sd = 1) #hace lo mismo que la anterior solo que le cargamos explicitamente la media y varianza. 
qnorm(p = 0.95, mean = 5, sd = 2) #aca le cambiamos la media y la varianza. 

#las funciones tienen parametros, y pueden ser mas de uno. Si uno no le pone un valor a un parametro, R por default les da ciertos parametros.


?sqrt #otra forma de pedir ayuda sobre una funcion.
help(qnorm) #nos describe la funcion qnorm(), qué parametros puede tomar y qué devuelve. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Variables/Objetos: R puede guardar ciertos calculos o objetos en forma de variables la memoria RAM. Podemos acceder a ellas cuando queramos.
#las variables aparecen en el environment
var1 <- 0.5 #operacion de asignacion. le asignamos a la variables "var1" el valor de 0.5. 
pepito <- sqrt(var1) #a la variables 'pepito' le asignamos el valor de sqrt(var1), siendo var1 igual a 0.5. Guarda el VALOR. Si cambiamos var1 y no hacemos nada con pepito, pepito no cambia. 
var1 <- var1 + 1 #actualiza el valor de var1, var1 cambia su valor dentro de la memoria. 

var1 #me muestra el valor de 'var1' en la consola. 
pepito #me muestra el valor de 'pepito' en la consola. 

#Administracion de variables
ls() #me lista las variables que estan en la memoria en firna de un vector. 
rm(pepito) #elimina la variable 'pepito' de la memeria. 
#si hacemos 'rm(list=ls())' borramos todas las variables que esten en la memoria. Consultar 'help(rm)' para mas detalles. Es util usarlo como primera linea antes de empezar un code. 
ls()
?rm
rm(list = ls()) #borramos todas las variables que esten en la memoria
ls()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Tipos de datos en R

#la funcion 'class()' nos dice a qué clase pertenece la variables que le pasemos como parametro
class(0.5) 
"verde" #tira error. 
class("verde") #aca nos dira que un dato tipo 'character' 
charVar <- 'verde'
class(charVar) #es character. 
TRUE #siempre va con mayusculas. Es un valor reservado, es booleano. 
class(FALSE) #nos dice que es un LOGICAL.
F  # No se recomienda usar esta abreviatura, es una abreviacion de FALSE; podemos nombrar a una variable como 'F', por eso no se recomienda usarlo
#de la misma manera, podemos abreviar TRUE con T.
class(T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Convertir un objeto de una clase a otra. 
as.integer(2.75) #pasamos 2.75 a entero, y nos quedamos solo con el 2
as.character(1) #nos devuelve "1" y deja de ser un entero y pasa a ser un character
"2.5" + 1  # Noten cómo R muestra los errores. Es error porque sumamos un character y un entero. 
as.numeric("2.5") + 1
as.numeric("azul")

as.integer(TRUE) #si a TRUE lo convertimos en entero, nos da 1. si a FALSE lo pasamos a entero, nos da un 0
as.logical(0) #esto vale FALSE, y todo lo demas da TRUE 
as.logical(-3.25) #esto da TRUE. todos los numeros salvo en 0 valen TRUE.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #ESTRUCTURAS DE DATOS

#Estructuras homogeneas de datos: 

# vector (estructura unidimensional): #c() es una funcion qur nos crea un vector con los parametros que les pasemos. Los parametros deben ser de la misma clase, sino los promociona. 
c(1, 3, 5.5) #vector de valores numeros. es NUMERICO. 
c("a", "b", "c", "d")
c(1, "b", 3, 4) # Qué pasó acá? (esto se llama promoción).La promocion convierte todo a la clase mas general de los datos que les pasemos. 
#en este caso, lo convierte a character. si luego queremos pasarlo a numerico, a "b" no podra pasarlo y lo dejara vacio como "NA"
vecVar <- c(9, -2, 8.75) #guardampos el vector en una variable. Para acceder a los elementos del vector, indexamos con corchete '[]'
vecVar[3] #accedemos al tercer elemento del vector. 
vecVar[1] #accedemos al primer elemento. 
vecVar[30] # Esto es raro, devuelve NA. Ya vamos a ver que es un NA!
c(vecVar, vecVar, vecVar) #podemos combinar vector, los pega al lado del otro, no es que el primer elemento es un vector. 

# matrix (estructura bidimensional): son tablas con numeros. Se guardan en una categoria de DATA, como una tabla. 
matrix(c(2,4,-2,4)) #cuatro filas y una columna
matrix(c(2,4,-2,4), nrow = 2, byrow = TRUE)
matrix(c(2,4,-2,4), nrow = 2, byrow = FALSE)
matVar <- matrix(c(vecVar, vecVar, vecVar), nrow = 3, byrow = TRUE)
#podemos acceder a elementos de matrices indexandola
matVar[2,2] #el primer numero es la fila, el segundo es la columna. 
matVar[,2]  # Qué pasó acá? si dejamos la fila vacia, accede a la columna que le pasemos. es decir, aca nos da los elementos de la segunda columna.  
eigen(matrix(c(1,2,2,5), nrow = 2, byrow = TRUE)) #hay funciones que toman como input (parametro) estructuras/coleccion de datos. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#reciclaje. 
c(1, 2, 3) + 1 # Este comportamiento se llama "reciclaje" porque recicla el 1 hasta que la operacion sea realizables, que tengan la misma dimension. (SUPER ÚTIL). Aca le suma 1 a todos los elementos del vector. 
c(1, 2, 3)^2 #eleva al cuadrado todos los elementos. 
# Recicla el de menor dimension hasta que la operacion sea realizable.

round(c(1.2, 1.8, 2, 2.5)) #round() opera en cada elemento y redondea
floor(c(1.2, 1.8, 2, 2.5)) #redondea para abajo
ceiling(c(1.2, 1.8, 2, 2.5))
#hay funciones que operan elemento a elemento dentro de un vector. 

c(1, 2, 3) + c(3, 2, 1)
c(4, 8, 12) / c(2, 4, 6)
1 / c(1, 2, 3)

c(1, 2, 3, 4) + c(3, 1) # Esto es importante tenerlo en cuenta
c(3, 1) + c(1, 2, 3, 4)
c(3, 1) + c(1, 2, 3) # Noten cómo se muestra un warning, devuelve de la dimension del mas grande.

#funciones que calculan alguna metrico (devuelve un numero) sobre una coleccion de datos, sobre un vector. 
mean(c(100, -100, 50, 75, 10000)) #calcula la media entre los valores del vector
var(c(100, -100, 50, 75, 10000))
nchar(c("hola", "buenos", "la", "noches"))
sum(c(TRUE, FALSE, TRUE)) #recordar que TRUE  es 1 y FALSE es 0. 
min(c(100, -100, 50, 75, 10000)) # Qué hará max?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Como crear datos homogeneos

c(1:100) #crea un vector [1, 2, 3, ..., 100]
seq(from = 100, to = 200, by = 5) #crea una secuencia del 100 al 200 de a 5, [100, 105, 110,..,200]. Cuando lon printea, el [] es la posicion en la que esta el elemento a la derecha del []
rep(1, 20) #crea un vector de unos de 20 elementos. 
rep(c(1,2), 20)
2^c(1:10)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#Estructuras de datos heterogeneas/listas.
#Para R, una lista es en lo que python es una diccionario. Las listas son utiles para guardar elementos de diferente clase
#Las listas es la estructura mas general de R, podemos mezclar diferentes clases. 

n <- c(2, 3, 5) 
b <- c(TRUE, FALSE, TRUE, FALSE, FALSE) 
lista1 <- list(n, b, 3)
lista2 <- list(numVar = n, logVar = b, scalar = 3)
#Es util tener en cuenta que un elemento de una lista pueden ser estructuras de datos. Le podemos dar nombres a los elementos, ver el ejemplo lista2

lista1; lista2 # El ";" es para escribir más de una instrucción por línea

names(lista1) #nos devuelve 'NULL' porque lista1 no tiene nombres 
names(lista2)

lista1[[2]] #cuando indexamos sobre una lista, va un doble [], por eso el '[[]]'. Dentro del doble corcheto podemos poner operaciones
lista2[[2]] #esto da '[1]  TRUE FALSE  TRUE FALSE FALSE' 

lista1[["numVar"]] #nuevamente da NULL porque lista1 no tiene nombres
lista2[["numVar"]]

lista2$logVar #esto da '[1]  TRUE FALSE  TRUE FALSE FALSE'. Es otra forma de indexar que solo se usa cuando los elementos de la lista tienen nombres
#lista1$2

lista2$logVar[3]  # Qué hace esto? Esto da '[1] TRUE' porque accede ak tercer elemento de logVar. 

lista3 <- list(a = lista1, b = lista2) # Qué hace esta línea?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#DATA FRAMES

df <- data.frame(col1 = rep("a", 20), col2 = rep(c(1,2,3,4,5), 4),
                 col3 = rep(c(T, F), 10))
# 'col1' es la columna 1, pero puede tener otro nombre.
# rep(c(1,2,3,4,5), 4) repite el vector [1,2,3,4,5] cuatro veces. 

df #printeamos df
names(df) #accedemos a los nombres de las columnas, de los elementos que les metimos
colnames(df)
rownames(df)
rownames(df) <- letters[c(1:20)] #uno puede ponerle nombres a las filas, pero es extraño
df
rownames(df)


length(df) # Qué hace "length" si se le pasa un vector? Y una lista? Nos dice cuantas columnas tiene el daraframe
ncol(df)
nrow(df)
dim(df) #nos da la cantidad de filas y cantidad de columnas. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Indexacion de dataframes:

df[[1]] #nos devuelve la columna 1
df$col3 #accedemos a la columna 3 por el noabre
df[["col2"]] 

df[1,1] #accedemos a la posicion [1,1], la primera columna y primera columna. '[x,z]' es fila x, columna z
df[1,"col2"] #accedemos a la fila 1 de la columna 2
df[3,] #accedemos a la fila 3 entera
df[,2] #accedemos a la columna 2 entera
df[-3,] #accedemos a todo el dataframe excepto la fila 3
df[,-2]
df[c(1,3),] #accedemos a las filas 1 y 3 enteras.
df[,c(1,3)]
df[,c("col1", "col3")]
df[,c(FALSE,TRUE,TRUE)] # Esto va a ser muy útil! 
#DENTRO DE LA INDEXACION, PODEMOS METER FUNCIONES, ver lo que sigue

df[seq(from = 1, to = nrow(df), by = 2),]
df[rep(c(FALSE,TRUE), 10),] # Esto también va a ser muy útil!

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Estructuras de control:
c(1, 3, 5) == 3       # Comparación de igualdad. El '==' compara. Recicla el 3 y compara elemento a elemento. 
c("a", "3", "c") != 3 # Comparación de desigualdad. Hay algo particular? Aca promociona el 3, pasa el numero a character. Por eso da FALSE TRUE FALSE
c(1, 2, 5, 10) > 2    # Comparación de "es mayor"
c(1, 2, 5, 10) >= 2   # Comparación de "es mayor o igual"
c(1, 2, 5, 10) < 2    # Comparación de "es menor"
c(1, 2, 5, 10) <= 2   # Comparación de "es menor o igual"

c(1, 2, 5, 10) %in% c(5, 8, 10) #pregunta si los elementos del primer vector estan en el segundo. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

a <- c(T, T, F, F)
b <- c(T, F, T, F)

a & b        # "&" es el operador de conjunción. Compara elemento a elemento y pregunta si los elementos son iguales. 
a | b        # "|" es el operador de disyunción. Compara elemento a elemenot y devuelve TREU si al menos uno es TRUE
!a           # "!" es el operador de negación. 
xor(a,b)     # "xor" implementa la disyunción exclusiva. Compara elemento a elemento y devuelve TRUE si se mezclan los TRUE y FALSE.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

mtcars        # library(help = "datasets"). Cargamos un dataset que se encuentra en R.
?mtcars  #le pedimos ayuda o una descripcion a R sobre el dataframe
head(mtcars)  # Permite ver los primeros elementos de una estructura
tail(mtcars)  # Permite ver los últimos elementos de una estructura
head(mtcars, 20) #printeamos las primeras 20mobservaciones. 

colnames(mtcars) #vemos qué columnas tiene
rownames(mtcars) #vemos los nombres de los autos

summary(mtcars)   # Da medidas resumen estadistico descriptivo de cada variable
str(mtcars)       # Da detalles sobre la estructura. Nos dice, por ejemeplo, la clase de algunas variables. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Analisis basico de un dataframe. 

mtcars[mtcars$am == 1, ]  # Devuelve sólo los que tienen am == 1, los que tienen caja manual. 
mtcars[mtcars$cyl > 4, ]  # Devuelve todos lo que tengan más de 4 en cyl

mtcars[mtcars$mpg > 20,] #devuelve las observaciones con Miles/(US) gallon mayor a 20
mtcars[mtcars$mpg > 20 & mtcars$mpg <= 25,] #Devuelve las observaciones que tengan mpg 20 (sin incluir) y 25
mtcars[mtcars$carb %in% c(2,6),] #devuelve las observaciones que tengan Number of carburetors igual a 2 o 6. Podriamos haberlo escrito con disyuncion. mtcars[mtcars$carb == 2 | mtcars$carb == 6]
grepl("Merc", rownames(mtcars)) #grepl() devuelve TRUE o FALSE si el elemento contiene el string que le pasemos a grepl().Aca nos quedamos con las observaciones de autos marca Mercedes.
mtcars[grepl("Merc", rownames(mtcars)),]  #nos quedamos con las observaciones de autos mercedes
mtcars[grepl("Merc", rownames(mtcars)), "mpg"] #nos da las millas por goleon de los autos mercedes.
mean(mtcars[grepl("Merc", rownames(mtcars)), "mpg"]) #nos da la milla promedio por galeon de un auto mercedes. 

#¿Un auto promedio Fiat hace más millas por galón que un auto promedio Mercedes Benz?
#veamos las millas por galeon promedio de un auto fiat:
grepl("Fiat", rownames(mtcars)) #esto nos devuelve un vector de TRUE y FALSE donde TRUE indica que el auto es fiat. recordar que rawnames(mtcars) nos da los modelos de los autos, por eso buscamos ahi. 
mtcars[grepl("Fiat", rownames(mtcars)),] #aca accedemos a las filas de autos Fiat.
mtcars[grepl("Fiat", rownames(mtcars)), "mpg"] #nos da un vector con las millas por galeon de cada auto Fiat
mean(mtcars[grepl("Fiat", rownames(mtcars)), "mpg"]) #calculamos la milla por galeon promedio de un auto fiat. 


#¿Qué auto recorre menos millas por galón de combustible?
mtcars[mtcars$mpg == min(mtcars$mpg),] #aca vemos los autos de menor milla por galeon. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

table(mtcars$gear) # Distribución de autos por cantidad de cambios
table(mtcars$gear)/nrow(mtcars)

# Hacemos una tabla cruzada (los nombres ayudan)
tablaCruzada <- table(cambios = mtcars$gear, 
                      cilindros = mtcars$cyl)

tablaCruzada

# Calculamos la distribución por filas y por columnas
prop.table(tablaCruzada) # A qué es igual esto?
prop.table(tablaCruzada, 1) #marginaliza por columna
prop.table(tablaCruzada, 2) #marginaliza por fila

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Agregar/eliminar columnas:
#para agregar una nueva columna a un dataframe, hay dos formas:
# - mtcars[,"esMercedes"] la creamos usando los []
# - mtcars$kmpl la creamos usando el $ (es mas comoda)

mtcars[,"esMercedes"]    <- grepl("Merc", rownames(mtcars)) #agregamos una nueva columna que sera TRUE si el auto es mercedes y FALSE si no.
mtcars$kmpl              <- 1.6093 * mtcars$mpg / 3.7854 #agregamos una nueva columna donde expresamos las millas por galeon en otra unidad
mtcars[["cincoCambios"]] <- mtcars$gear == 5 #otra forma de crear una nueva columna

mtcars2 <- mtcars
mtcars2[, c(2,4)] <- NULL #asi eliminamos columnas
mtcars2[, c("wt", "esMercedes")] <- NULL # eliminamos Por nombre

mtcars[, 1]
mtcars[, 1, drop = FALSE]



#EJERCICIOS SESION 01:

# Teniendo en cuenta el dataset "swiss" (vean "help(swiss)"), se pide:

# Ej1: Cree una variable nueva llamada "is.rural" que valga TRUE si en la provincia la proporción de
#población masculina que trabaja en agricultura es mayor a la mediana de todas las provincias
#para las que se tiene datos y FALSE en caso contrario.

data <- swiss #creamos el dataset
#calculamos la mediana de la poblacion masculina todas las provincias para las que se tiene datos:
# median(data[,"Agriculture"]) es la mediana de la poblacion masculina todas las provincias para las que se tiene datos

data$is.rural <- ifelse(data$Agriculture >= median(data[,"Agriculture"]), TRUE, FALSE)
#otra forma: data$Agriculture > median(data$Agriculture)

#Ej2: 2. Cree una variable que se llame "is.catholic" que valga TRUE si la provincia tiene más del 20% de la población católica y FALSE en caso contrario.
data$is.cathaolic <- ifelse(data$Catholic > 20, TRUE, FALSE)
#otra forma: data$is.catholic <- data$Catholic > 20

#Ej3: Haga una tabla de contingencia que permite ver si cuando "is.rural" es TRUE hay mayor
#proporción de provincias con "is.catholic" con valor de TRUE comparado con cuando "is.rural"
#es FALSE.

prop.table(table(rural = swiss$is.rural, catholic = swiss$is.catholic), 1)

#Ej4:

#4.1: 
sum(data$is.rural==FALSE & data$is.cathaolic==TRUE)
#4.2:



