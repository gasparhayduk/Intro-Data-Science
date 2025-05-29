rm(list=ls())
setwd("~/Desktop/IntroDS/Sesion05") #Seteamos el directorio

#~ 1er bloque ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Cargamos librerias, una de ellas trae un dataset, otra trae implementado el algoritmo de knn. 
install.packages("class")
library(class) 
install.packages("ISLR") # es un dataset. Ver en Help qué es. 
library(ISLR)
install.packages("e1071")
library(e1071)


# Estandarizamos
std.Caravan <- scale(Caravan[,-86]) # scale() estandariza las variables, crea un nuevo dataset. Estandarizamos todo menos la clase a predecir. 
# scale() toma como parametro un dataframe.
summary(std.Caravan)
apply(std.Caravan, 2, sd)

# Separo en training y testing
test_index <- sample(c(1:nrow(std.Caravan)), 500) # elegimos observaciones al azar.  
train_X <- std.Caravan[-test_index,]
test_X  <- std.Caravan[test_index,]
train_y <- Caravan[-test_index, "Purchase"]
test_y  <- Caravan[test_index, "Purchase"]

# Entreno un modelo de vecinos más cercanos 
knn_predictions <- knn(train_X, test_X, train_y, k = 5) # devuelve qué predice knn para test_X. La libreria devuelve probabilidad 

# Veo cómo fue sobre los datos de testing
print(mean(test_y == knn_predictions)) # acierta en el 92% de los casos. 

# Cómo le hubiera ido a Naïve Bayes
nb_classifier <- naiveBayes(Purchase ~ ., data=Caravan[-test_index,])
print(mean((test_y == predict(nb_classifier, newdata=Caravan[test_index,])))) 
# ¿Por qué andará tan mal?
# acierta el 19% de los veces. Anda mal porque naiveBayes cuando la variable predictora (las X) eran categoricas; aca las X son numericas, por eso anda mal. 

# Cómo le iría con distintos valores de k?
library(ggplot2)
library(reshape2)

train_acc <- c()
test_acc  <- c()
k_vals <- c(1, 3, 5, 10, 15, 30, 50, 100) # valores de k para ir probando 

for (k in k_vals) {

  print(k) # para ver por qué vuelta va

  tmp_tr_pred <- knn(train_X, train_X, train_y, k)
  tmp_ts_pred <- knn(train_X, test_X, train_y, k)

  train_acc <- c(train_acc, mean(train_y == tmp_tr_pred))
  test_acc  <- c(test_acc, mean(test_y == tmp_ts_pred))

}

experiment_data <- data.frame(k = k_vals, train_acc, test_acc)
print(experiment_data)

plot_data <- melt(experiment_data, id.vars="k", value.name="Accuracy")

ggplot(data=plot_data, aes(x=k, y=Accuracy, col=variable)) + geom_line()

#~ 2d0 bloque ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Instalamos/activamos librerias:
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

#Abrimos el dataset
data_set <- read.table("bankruptcy_data_red.txt", header=TRUE, sep="\t")
# El dataset tiene 64 atributos que intentan predecir si una empresa entra en bancarrota o no. 
data_set$class <- factor(data_set$class) # 

# Separo en training y testing
test_index <- sample(c(1:nrow(data_set)), 280) # seteamos 280 observaciones como test 
train_data <- data_set[-test_index,] # dataset de training. 
test_data  <- data_set[test_index,]

# Entreno un modelo de arbol de decision
tree_fit <- rpart(class ~ ., data = train_data,
                  control = rpart.control(maxdepth = 6, xval=0, cp = 0)) # Vean ?rpart.control
# en maxdepth seteamos la profundidad maxima. 

# Veo cómo fue sobre los datos de testing
tree_predictions <- predict(tree_fit, newdata = test_data, type = "class") 
print(mean(test_data$class == tree_predictions)) # Acierta el 78%. 

#Visualicemos el árbol
rpart.plot(tree_fit)

# Veamos la importancia de atributos
tree_fit$variable.importance
