rm(list=ls())

#~ Matriz de varianzas y covarianzas ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

dinero_1 <- c(950, 960, 970, 990, 930)
dinero_2 <- c(950, 1850, 50, 800, 1150)

mean(dinero_1) # la media de 1 es 960
mean(dinero_2) # la media de 2 es 960.
# La media es mucho mas informativa para dinero1, pues todos sus valores estan cerca de ella; 
# para dinero2 esto no es asi, tiene mas varianza, valores mas dispersos. 

# Esto mide, para cada punto, la distancia de ese punto respecto de la media. Y promediamos para todos los puntos. 
mean((dinero_1 - mean(dinero_1))^2)
mean((dinero_2 - mean(dinero_2))^2)

var(dinero_1)
var(dinero_1)*(length(dinero_1)-1)/length(dinero_1) # Esto divide por n-1. 

h <- women$height # altura medida en pulgadas
w <- women$weight # peso medido en libras

plot(h, w)

((h-mean(h)) %*% (w-mean(w))) / (length(w) - 1) # el %*% hace la suma producto. 
cov(h, w)
cov(data.frame(h, w)) # calcula la matriz de varianza y covarianza para h y m 

wKm <- w / 2.2 # pasamos el peso de libras a kilogramo
cov(h, wKm) #calculamos la covarianza entre altura y el peso en kg. Notemos que cambia respecto a la anterior solo porque le cambiamos la unidad. Esto no es deseable. 

# Computamos el coeficiente de correlacion de Pearson, el cual se encuentra entre -1 y 1 y no cambia si cambiamos las unidades. No se ve afectada por transformaciones lineales (cambio de unidades)
cor(h, w)
cor(h, wKm) 
cor(data.frame(h, wKm)) #m atriz de correlaciones

cov(iris[iris$Species=="virginica", c(1:4)]) #vemos la matriz de varianza y covarianza de Sepal.Length, Sepal.Width, Petal.Length, Petal.Width para la especie virginia. 
sum(diag(cov(iris[iris$Species=="virginica", c(1:4)]))) #calculamos la varianza total de la matriz de var y cov anterior. 

cor(iris[iris$Species=="virginica", c(1:4)])
cov(scale(iris[iris$Species=="virginica", c(1:4)]))

plot(iris[iris$Species=="virginica", c(1:4)])

#~ Análisis de componentes principales ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

head(USArrests) # Dataset con datos de Violent Crime Rates by US State
cor(USArrests) # Viendo las correlaciones, murder, assault y rape van de la mano, muestran cosas parecidas.  
plot(USArrests)

# Ver ?prcomp
pr.out <- prcomp(USArrests, center = TRUE, scale = TRUE)
class(pr.out)
str(pr.out)
pr.out$sdev #La variabilidad que capta cada componente cae. El componente 1 capta mas variabilidad, el segundo menos y asi. 
pr.out$rotation # Esto nos muestra los coeficientes de cada componente. 

USArrests_pc <- predict(pr.out, USArrests)
head(USArrests_pc) #esto nos dice cuanto vale cada componente para cada estado.

biplot(pr.out, scale = 0)


# Escalamos las variables
prcomp(USArrests, center = TRUE, scale = TRUE)
prcomp(USArrests, center = TRUE, scale = FALSE)

pr.var <- pr.out$sdev^2 #varianza de cada componente

pve <- pr.var/sum(pr.var) #proporcion de la varianza explicada por cada componente de la varianza total. 

plot(pve, xlab = "Principal Component",
     ylab = "Proportion     Variance Explained",
     ylim = c(0,1),type = "b")

plot(cumsum(pve), xlab = "Principal Component",
     ylab = " Cumulative Proportion of Variance Explained",
     ylim=c(0,1), type="b")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Componentes principales para USJudgeRatings:
# ver ?USJudgeRatings

head(USJudgeRatings)
cor(USJudgeRatings)
plot(USJudgeRatings) # Hay muchas variables que van de la mano. 


pr_jd <- prcomp(USJudgeRatings, center = TRUE, scale = TRUE)

pr_jd$sdev # variabilidad total explicada por cada componente. 
pr_jd$rotation # coeficiente de cada componente. 

USA_jd_pc <- predict(pr_jd,USJudgeRatings )

biplot(pr_jd, scale = 0, cex=c(0.75,0.75))
# Hay un componente que capta principalmente COUNT. 
# Las observaciones a la izquierda captan jueces con calidad, y hacia abajo reflejan jueces con muchas contactos. 


pr_jd_var <- pr_jd$sdev^2 #varianza explicada por cada componente 
pve_jd <- pr_jd_var/sum(pr_jd_var) #proporcion de la varianza explicada por cada componente de la varianza total.


plot(pve_jd, xlab = "Principal Component",
     ylab = "Proportion     Variance Explained",
     ylim = c(0,1),type = "b")

plot(cumsum(pve_jd), xlab = "Principal Component",
     ylab = " Cumulative Proportion of Variance Explained",
     ylim=c(0,1), type="b") 

# Viendo estos graficos, con 2 componentes ya captamos mucha variabilidad. 


