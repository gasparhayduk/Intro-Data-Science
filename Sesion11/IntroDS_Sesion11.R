rm(list=ls())
setwd("~/Desktop/IntroDS/Sesion11")
library(MASS)  # Sólo para tener el dataset Boston disponible

#Dataset con el que vamos a trabajar
help("Boston") # para ver qué tiene el dataset. 
head(Boston)
str(Boston)
summary(Boston)

# Veamos parte de los datos
plot(Boston[,c(1:5)])


# Para correr k-medias, le pasamos un dataset con variables numericas, un valor de k (centers), un numero maximo de iteraciones (iter.max) y cuantas veces se ejecuta el algoritmo entero (nstart) 
# Prueba de K-means
clusters <- kmeans(Boston, centers=7, iter.max=30,  nstart=20)
clusters$centers # la ubicacion de los 7 centroides. 
clusters$cluster # en qué cluster está cada observacion. 
table(clusters$cluster)

# Veamos con evoluciona la función objetivo a medida que aumenta K
evol_variabilidad <- data.frame()
for (k in c(1:20)) {
    clusters <-kmeans(Boston, centers=k, iter.max=30,  nstart=20)
    evol_variabilidad <- rbind(evol_variabilidad,
                               data.frame(k=k,
                                          var=clusters$tot.withinss)) # lo ultimo es la funcion a optimizar. 
}

plot(c(1:20), evol_variabilidad$var, type="o", xlab="# Clusters", ylab="tot.withinss") 
# en k=4 es donde ocurre el codo, ese parece ser un buen valor. 

# Veamos cómo se asignaron los clusters. Corremos k-medias con k=4
clusters <- kmeans(Boston, centers=4, iter.max=30,  nstart=20)
plot(Boston, col=clusters$cluster)




library(rpart)

Boston$cluster <- factor(clusters$cluster)
library(rpart)
library(rpart.plot)
arbol <- rpart(cluster ~ ., data= Boston, control = rpart.control(xval=0,cp=0, minsplit=1, minbucket=1, maxdepth=3))
rpart.plot(arbol) 
# Vemos que tax y black son importantes a la hora de hacer el clustering, separan bien. 

plot(Boston[,c("tax", "black")], col=clusters$cluster)

# k-medias se enfoca mucho en tax y black. 
# Estas variables tienen valores muchos mas grandes que las otras variables (las ptras son proporciones). 
# Como k-medias usa distancia euclidea, se enfoca mucho en variables de magnitudes grandes. Por ello, primero deberiamos estandarizar para evitar este problema. 
# Si hacemos esto, va a cambiar todo. 





