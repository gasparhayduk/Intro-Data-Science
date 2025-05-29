#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list=ls())
setwd("~/Desktop/IntroDS/Sesion06")
install.packages("kernlab")
library(ggplot2); library(reshape2); library(rpart); library(kernlab)

data(spam) # cargamos un dataset
# Este dataset contiene datos para identificar si un mail es spam o no. Basado en caracteristicas X, queremos predecir si un mail es spam o no
# Ver ?spam. 


# Separo en training y testing: 
set.seed(12345)
test_index <- sample(c(1:nrow(spam)), 1380) # %10 para testear. 
train_data <- spam[-test_index,]
test_data  <- spam[test_index,]

train_acc <- c() # vector vacio donde guardaremos la accuracy para training
test_acc  <- c() # vector vacio donde guardaremos la accuracy para test
depths <- seq(1, 30, by=1) # Profundidades para el arbol de decision. Haremos un arbol a la vez. 

for (d in depths) {
  print(d) # en qué profundidad vamos
  tree_fit <- rpart(type ~ ., data = train_data,
                    control = rpart.control(maxdepth = d, xval=0, minsplit = 1, minbucket=1, cp=0)) # entrenamos el arbol para la profundidad d 
  train_acc <- c(train_acc, mean(train_data$type == predict(tree_fit, train_data, type="class")))
  test_acc  <- c(test_acc, mean(test_data$type == predict(tree_fit, test_data, type="class"))) #vemos la accuracy
}

experiment_data <- data.frame(depth = depths, train_acc, test_acc)
plot_data <- melt(experiment_data, id.vars="depth", value.name="Accuracy")
ggplot(data=plot_data, aes(x=depth, y=Accuracy, col=variable)) + geom_line()
# A medida que aumenta la profundidad de un arbol, aumenta la performance en los datos de training (y suele converger a 1, explicando todo perfecto) y se estanca/cae la performance en datos de test. 
# Esto es overfitting: se ajusta demasiado a los datos de training y no generaliza para datos desconocidos. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(class)

data_set <- read.table("bankruptcy_data_red.txt", header=TRUE, sep="\t", stringsAsFactors = TRUE) # usamos el dataset de bankruptcy de la Sesion05. 
data_set$class <- factor(ifelse(data_set$class, "bankruptcy", "no_bankruptcy"))

# Separo en training y holdout/validation: 
holdout_index  <- sample(nrow(data_set), 200)  # Aproximadamente un 20% de las obs.
train_index  <- setdiff(1:nrow(data_set), holdout_index)  # Aproximadamente un 20% de las obs. 

val_data    <- data_set[holdout_index,] # validation data. Las posiciones que estan en holdout. 
train_data  <- data_set[train_index,] # Training data. Son las posiciones de train_index. 

# Notar que sabemos las Y de validation data, pero hacemos como que no lo sabemos. No usamos las X de validation data para entrenar el modelo. 

predictions <- knn(train_data[,-ncol(train_data)], val_data[,-ncol(val_data)], train_data$class, k=9) # usamos knn con k = 9. 

print(mean(val_data$class == predictions))

# Repitamos 10 veces el experimento
library(dplyr)

reps <- 30 # repetimos 30 veces lo de separar en training y validation. 
vld_accuracy <- data.frame()

for (i in c(1:reps)) {

  holdout_index  <- sample(nrow(data_set), 200)  # Aproximadamente un 20% de las obs.
  train_index  <- setdiff(1:nrow(data_set), holdout_index)  # Aproximadamente un 20% de las obs.

  val_data    <- data_set[holdout_index,]
  train_data  <- data_set[train_index,]

  predictions <- knn(train_data[,-ncol(train_data)], val_data[,-ncol(val_data)], train_data$class, k=9) 

  vld_accuracy  <- rbind(vld_accuracy,
                          data.frame(rep=i,  acc=mean(val_data$class == predictions)))
}

# Promedio entre repeticiones
print(mean(vld_accuracy$acc)) 

# Con esto, tenemos una estimacion para ver cómo le iria al modelo en datos desconocidos. Podemos ir cambiando el K para ver cómo cambia. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~LOOCV~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# LOOCV
loocv_acc <- rep(NA, nrow(train_data)) # generamos un vector de NAs. 
for (i in c(1:nrow(data_set))) {  # Itero sobre cada observación. 
    train_data <- data_set[-i,]
    loocv_data  <- data_set[i,]

    tmp_vd_pred <- knn(train_data[,-ncol(train_data)], loocv_data[,-ncol(val_data)], train_data$class, k=9)

    loocv_acc[i] <- loocv_data$class == tmp_vd_pred
}

# Estimación de performance
mean(loocv_acc)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~K-FOLD CROSS VALIDATION~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Ejemplo de k-fold cv

folds <- 10
kfoldcv_acc <- rep(NA, folds)
fold_indicator <- sample(rep_len((1:folds), nrow(data_set)))

for (f in c(1:folds)) {
    train_data <- data_set[fold_indicator!=f,]
    val_data  <- data_set[fold_indicator==f,]

    tmp_vd_pred <- knn(train_data[,-ncol(train_data)], val_data[,-ncol(val_data)], train_data$class, k=9)
    kfoldcv_acc[f]  <- mean(val_data$class == tmp_vd_pred)
}

# Estimación de performance
mean(kfoldcv_acc)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Ejemplo de training, validation y testing


val_index <- sample(c(1:nrow(data_set)), 280)  # Aproximadamente un 20% de las obs.
test_index <- sample(setdiff(c(1:nrow(data_set)), val_index), 280) # Aprox un 20% de testeo
train_index <- setdiff(1:nrow(data_set), c(val_index, test_index)) # El resto son de training. 
 
val_data <- data_set[val_index,]
test_data <- data_set[test_index,]
train_data <- data_set[train_index,]

# Entreno con distintos valores K y valido
vld_accuracy <- data.frame()
k_vals <- c(1, 2, 3, 4, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50)

for (k in k_vals) {
    print(k)
    tmp_vd_pred <- knn(train_data[,-ncol(train_data)], val_data[,-ncol(val_data)], train_data$class, k=k)
    vld_accuracy <- rbind(vld_accuracy, data.frame(k=k, acc=mean(val_data$class == tmp_vd_pred)))
}

print(vld_accuracy)

# Guardo la mejor configuración
best_conf <- vld_accuracy[which.max(vld_accuracy$acc),] # mejor configuracion, el que mejor performance tiene en validacion

# Modelo final (se entrena con todos los datos). Vemos como le va al mejor modelo (el de mejor performance con validacion) con el test data. 
all_train_data <- rbind(train_data, val_data) # entrenamos para training y validacion
ts_pred <- knn(all_train_data[,-ncol(train_data)], test_data[,-ncol(test_data)], all_train_data$class, best_conf$k)

mean(test_data$class == ts_pred)
