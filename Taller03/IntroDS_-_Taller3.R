rm(list=ls())
setwd("~/Desktop/IntroDS/Taller03") # seteamos el directorio de trabajo


#Instalacion/activacion de librerias:
install.packages("ROCR") # instalamos la libreria (no estaba instalada en mi caso)
library(ROCR) # activamos la libreria. 
library(rpart)
library(rpart.plot)


# APELLIDO Y NOMBRE DE LOS INTEGRANTES DEL GRUPO:
#	Integrante 1: Luis Gonzalez Lelong 
#	Integrante 2: Gaspar Hayduk
#	Integrante 3: 

# IMPORTANTE: el script que resuelve el taller debe ser entregado por mail a
# introdsutdt@gmail.com. Como asunto del mismo debe decir "Intro DS - Taller 03" y
# en el cuerpo del mismo deben figurar los nombres de quienes los resolvieron y
# TAMBIÉN SUS LEGAJOS (todos los integrantes del taller deben estar copiados como
# destinatarios en el correo de entrega). Tienen tiempo para entregarlo hasta
# el 26/5/2022 (inclusive).

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                         Taller 3 de Intro a Data Science                              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### Fuente de los datos: https://www.kaggle.com/adammaus/predicting-churn-for-bank-customers

# Definición de funciones
load_data <- function(data_path) {
    # data_path: debe ser el path en donde se encuentra el archivo de datos
    train_data <- read.table(data_path, sep=",", header=TRUE, quote="")
    train_data$Exited <- factor(ifelse(train_data$Exited == 1, "churn", "no_churn"))
    train_data$RowNumber <- NULL
    train_data$Surname <- NULL
    return(train_data)
}

get_auc <- function(real_class, predicted_prob) {
    # real_class: debe ser un vector de 0 y 1, en donde 1 es la clase de interés
    # predicted_prob: debe ser un vector de probabilidades predichas para la clase de interés
    pred_ROCR <- prediction(predicted_prob, real_class)
    auc_ROCR <- performance(pred_ROCR, measure = "auc")
    auc_ROCR <- auc_ROCR@y.values[[1]]
    return(auc_ROCR)
}


# Se cargan los datos
churn_data <- load_data("Churn_Modelling.csv")


# A lo largo de los siguientes puntos, se pide que arme modelos
# para predecir Exited en función del resto de las variables. El foco
# del análisis debe estar puesto en predecir quienes tienen "churn"
# como valor de Exited. La métrica de performance se usará AUC (la
# función "get_auc" permite calcularla).

# A modo de recordatorio, a continuación se muestra un ejemplo de validacion
# de un modelo de bayes ingenuo utilizando un esquema de 10-fold cross
# validation.

library(e1071)

folds <- 10
indexes <- sample(rep_len((1:folds), nrow(churn_data)))

nb_vld_performance <- data.frame()

for (f in c(1:folds)) {

  train_data <- churn_data[indexes!=f,]
  val_data  <- churn_data[indexes==f,]

  tmp_nb_model <- naiveBayes(Exited ~ ., data=train_data)
  tmp_nb_preds <- predict(tmp_nb_model, newdata=val_data, type = "raw")[, "churn"]
  nb_vld_performance  <- rbind(nb_vld_performance,
                               data.frame(fold = f,
                                          auc = get_auc(as.numeric(val_data$Exited == "churn"), tmp_nb_preds)))
}

mean(nb_vld_performance$auc)

### Se pide que complete los siguientes puntos:

# 1) Validando el modelo mediante k-fold cross validation con 10
# folds evalúe cuál es la performance de un árbol de decisión 
# (de la librería rpart) que utiliza los hiperparámetros default.
folds <- 10
kfoldcv_acc <- rep(NA, folds)
fold_indicator <- sample(rep_len((1:folds), nrow(churn_data)))


for (f in c(1:folds)) {
  training_data <- churn_data[fold_indicator!=f,]
  valid_data  <- churn_data[fold_indicator==f,]
  
  tree_fit <- rpart(Exited ~ ., data = training_data)
  tree_predictions <- predict(tree_fit, newdata = valid_data, type = "class")
  kfoldcv_acc[f]  <- mean(valid_data$Exited == tree_predictions)
}

kfold_media <- mean(kfoldcv_acc) #nos da una media igual a 0.8584. 


# 2) Cree un validation-set con 1100 observaciones elegidas al azar.
val_index <- sample(c(1:nrow(churn_data)), 1100)
val_data <- churn_data[val_index,]


# 3) Cree un test-set con 1100 observaciones elegidas al azar (que
# no deben haber sido elegidas en el punto 2 como parte del conjunto
# del validation set).
test_index <- sample(setdiff(c(1:nrow(churn_data)), val_index), 1100)
test_data <- churn_data[test_index,]

# 4) Cree un training-set que sea igual a churn_data pero no incluya
# las observaciones extraídas tanto para el validation set como para
# el test set.
train_index <- setdiff(1:nrow(churn_data), c(val_index, test_index))
train_data <- churn_data[train_index,]

# 5) Lleve adelante un experimento en donde entrenando con el
# dataset creado en el punto 4 y validando con el dataset creado
# en el punto 3, entrene árboles de decisión con todas las
# combinaciones posible de minsplit entre 1 y 10, minbucket entre
# 1 y 10, y maxdepth entre 1 y 10 (es decir 1000 combinaciones
# diferentes de hiperparámetros). Como venimos haciendo en clase, use
# siempre cp = 0 y xval = 0. Se pide que evalue el AUC tanto en los datos
# de entrenamiento como de validación. Estos resultados los debe guardar
# en un data.frame que se llame "rpart_exp" que tiene que tener las
# siguientes 5 columnas: minsplit, minbucket, maxdepth, auc_tr y
# auc_vd (note que este data.frame debe tener 1000 filas).

# TIP: en este ejercicio van a tener que usar un "triple for"


#Estructura del arbol:
# tree_fit <- rpart(Exited ~ ., data = train_data,
#           control = rpart.control(minsplit = s , minbucket = b, maxdepth = d, xval=0, cp = 0)) 

# Generamos secuencias para minsplit, minbucket y maxdepth: 
split_vals <- seq(1,10,1)
bucket_vals <- seq(1,10,1) 
depth_vals <- seq(1,10,1)

# Dataframe para ir completando la informacion de cada combinacion de minsplit, minbucket y maxdepth:
rpart_exp <- data.frame()

# Loop para probar todas las combinaciones de minsplit, minbucket y maxdepth:
for (s in split_vals) {
  for (b in bucket_vals) {
    for (d in depth_vals) {
      print(c(s,b,d))
      tree_fit_exp <- rpart(Exited ~ ., data = train_data,
                        control = rpart.control(minsplit = s , minbucket = b , maxdepth = d , xval=0, cp = 0)) #entrenamos el arbol
      tree_predictions_val <- predict(tree_fit_exp, newdata = val_data, type="prob")[, "churn"] #prediccion para los datos de validación. Es un vector con la probabilidad de ser churn.
      tree_predictions_train <- predict(tree_fit_exp, newdata = train_data, type="prob")[, "churn"] #prediccion para los datos de training. Es un vector con la probabilidad de ser churn.
      rpart_exp <- rbind(rpart_exp, data.frame(minsplit = s, minbucket = b, maxdepth = d,
                                               auc_vd = get_auc(as.numeric(val_data$Exited == "churn"),
                                                                tree_predictions_val), auc_tr = get_auc(as.numeric(train_data$Exited == "churn"), tree_predictions_train)))
    }
  }
}


# 6) Identifique la mejor combinación de hiperparámetros según
# el experimento. En caso que más de una combinación de hiperparámetros
# diera el máximo valor de AUC en validación, elija cualquiera de las
# mismas.
# Se pide que para responder este punto escriba una sentencia de R que
# que guarde la mejor combinación de hiperparámetros en alguna
# estructura de datos idónea.

best_vd <- c() #el primer elemento hará referencia al minsplit, el segundo al minbucket y el tercero al maxdepth.
row_vd <- which.max(rpart_exp$auc_vd) #fila en la que se obtiene el maxor AUC en validación 
print(rpart_exp[row_vd,])

# Completamos el vector donde guardamos la mejor combinacion de minsplit, minbucket y maxdepth
w=1
while (w<=3){
  best_vd[w] <- rpart_exp[row_vd,w]
  w <- w+1
}

# La combinacion que arroja mejor AUC para los datos de validación es: minsplit=1, minbucket=7 y maxdepth=8.
# Esto puede cambiar cada vez que se corra pues qué datos son de training y validacion se determina de forma aleatoria.  


# 7) Para el modelo elegido en punto anterior, identifique el/los umbral/es de
# decisión que máximiza/n el valor de la métrica f1-score en validación (para
# ello pueden utilizar el siguiente recurso https://rdrr.io/cran/MLmetrics/man/F1_Score.html).
# Pruebe con los valores de umbrales que surgen a partir de la siguente expresión seq(0.001, 0.999, 0.001)

# Definimos una funcion que toma una matriz de confusion y calcula precision, recall y f1 para esa matriz. 

prec_recall <- function(conf_m, verbose=TRUE) {
  if (verbose) {
    print(conf_m)
  }
  prec <- conf_m["churn", "churn"] / sum((conf_m[, "churn"]))
  rec <- conf_m["churn", "churn"] / sum((conf_m["churn",]))
  f1 <- 2 * prec * rec / (prec + rec)
  return(c(f1,prec,rec)) #devuelve un vector donde el primer elemento es el f1-score, el segundo elemento es la precision y el tercer elemento es el recall.
}


# table(actual=val_data$Exited, pred=factor(ifelse(tree_predictions_val_f1 >= p, "churn", "no_churn"))) #matriz de confusion para un umbral p. 

umbrales <- seq(0.001, 0.999, 0.001) 

# Definimos el dataframe donde completaremos cuál es el f1-score, precision y recall para cada umbral.
umbral_metricas <- data.frame()

# Loop para probar con todos los umbrales
for (p in umbrales) {
  print(p)
  tree_fit_f1 <- rpart(Exited ~ ., data = val_data,
                       control = rpart.control(minsplit = best_vd[1] , minbucket = best_vd[2] , maxdepth = best_vd[3] , xval=0, cp = 0)) #entrenamos el arbol con los mejores hiperparametros del ejercicio 5
  tree_predictions_val_f1 <- predict(tree_fit_f1, newdata = val_data, type = "prob")[, "churn"]
  conf_m_ <- table(actual=val_data$Exited, pred=factor(ifelse(tree_predictions_val_f1>=p, "churn", "no_churn"))) #matriz de confusion para calcular el f1-score
  metricas <- prec_recall(conf_m_,verbose = TRUE)
  umbral_metricas <- rbind(umbral_metricas, data.frame(umbral=p, f1_score=metricas[1], precision=metricas[2], recall=metricas[3]))
}


row_max_f1 <- which.max(umbral_metricas$f1_score) #obtenemos la fila en la que ocurre el mayor f1-score
print(umbral_metricas[row_max_f1,])
umbral_maxf1 <- umbral_metricas[row_max_f1, "umbral"] #umbral que maximiza el f1-score. El umbral es de 0.33 (raro que sea bajo)
print(umbral_maxf1)


# 8) Qué valores de precisión y recall arroja el modelo identificado en el punto
# anterior? (si fue más de un umbral el que maximiza F1 en el puntos anterior,
# elija el menor de los umbrales que encontró como solución de dicho punto).
prec_maxf1 <- umbral_metricas[row_max_f1, "precision"] #precision asociado al umbral que maximiza el f1-score
recall_maxf1 <- umbral_metricas[row_max_f1, "recall"] #recall asociado al umbral que maximiza el f1-score
#la precision asociada al umbral que maximiza el f1-score es 0.76,
# mientras que la recall asociada al umbral que maximiza el f1-score es 0.75


# 9) Entrene un árbol con los mejores hiperparámetros encontrados
# pero utilizando la suma de los datos de training y validation.
# NO deben escribir a mano el valor de los mejores hiperparámetros
# encontrados, deben aprovechar la estructura creada en el
# ejercicio 5.

# Creamos el dataset que combina los datos de training y validacion:
full_index <- c(val_index,train_index)
full_data <- churn_data[full_index,]


# Entrenamos el arbol:
tree_fit_full <- rpart(Exited ~ ., data = full_data,
                       control = rpart.control(minsplit = best_vd[1] , minbucket = best_vd[2] , maxdepth = best_vd[3] , xval=0, cp = 0))
# Este es el arbol con los mejores hiperparametros entrenado sobre la suma de los datos de validacion y training.


# 10) Calcule cuál es el valor de AUC que obtiene en el test set el
# modelo entrenado en el paso anterior 

# Vector que contiene la probabilidad predicha por el arbol entrenado con los mejores hiperparametros y la suma de 
#los datos de validacion y training para los datos de testing:
tree_predictions_test <- predict(tree_fit_full, newdata = test_data, type="prob")[, "churn"] 

#Calculamos el AUC:
auc_test_full <- get_auc(as.numeric(test_data$Exited == "churn"), tree_predictions_test)
#El arbol entrenado sobre la suma de los datos de training y validacion con los mejores hiperparametros 
# obtiene un AUC de 0.853 para los datos de testing.

# 11) Grafique el árbol obtenido en el punto 9.
rpart.plot(tree_fit_full)

# 12) Obtenga la importancia de atributos que se deriva del árbol
# obtenido en el punto 9. A su criterio, tiene sentido lo que se obtiene?

tree_fit_full$variable.importance

#Analizando la información, la varible con mayor relevancia en el modelo 
#es la variable de edad, es decir que haciendo particiones sobre la edad
#el error cuadratico se minimiza de manera relevante. Esto tiene sentido ya
#es logico pensar que un individuo se vaya a dar de baja o no según la edad que tenga
