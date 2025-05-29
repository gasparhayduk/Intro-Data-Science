rm(list=ls())
setwd("~/Desktop/IntroDS/Taller03") 

library(ROCR)

# APELLIDO Y NOMBRE DE LOS INTEGRANTES DEL GRUPO:
#	Integrante 1: 
#	Integrante 2: 
#	Integrante 3: 

# IMPORTANTE: el script que resuelve el taller debe ser entregado por mail a
# introdsutdt@gmail.com. Como asunto del mismo debe decir "Intro DS - Taller 03" y
# en el cuerpo del mismo deben figurar los nombres de quienes los resolvieron y
# TAMBIÉN SUS LEGAJOS (todos los integrantes del taller deben estar copiados como
# destinatarios en el correo de entrega). Tienen tiempo para entregarlo hasta
# el 27/5/2021 (inclusive).

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#                         Taller 3 de Intro a Data Science                              #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

### Fuente de los datos: https://www.kaggle.com/adammaus/predicting-churn-for-bank-customers

data_path <- "/Users/Usuario/Desktop/Intro DS Taller 3/Churn_Modelling.csv"
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
    # predicted_prob: de ser un vector de probabilidades predichas para la clase de interés
    pred_ROCR <- prediction(predicted_prob, real_class)
    auc_ROCR <- performance(pred_ROCR, measure = "auc")
    auc_ROCR <- auc_ROCR@y.values[[1]]
    return(auc_ROCR)
}

# Cargo los datos
churn_data <- load_data("Churn_Modelling.csv")

# A lo largo de los siguientes puntos, se pide que arme modelos
# para predecir Exited en función del resto de las variables. El foco
# del análisis debe estar puesto en predecir quienes tienen "churn"
# como valor de Exited. La métrica de performance se usará AUC (la
# función "get_auc" permite calcularla).

# A modo de recordatorio, a continuación se muestra un ejemplo de validacion
# de un modelo de bayes ingenuo utilizando un esquema de 5-fold cross
# validation.

library(e1071)

folds <- 5
indexes <- sample(rep_len((1:folds), nrow(churn_data)))

nb_vld_accuracy <- data.frame()

for (f in c(1:folds)) {

  print(f)
  train_data <- churn_data[indexes!=f,]
  val_data  <- churn_data[indexes==f,]

  tmp_nb_model <- naiveBayes(Exited ~ ., data=train_data)
  tmp_nb_preds <- predict(tmp_nb_model, newdata=val_data, type = "raw")[, "churn"]
  nb_vld_accuracy  <- rbind(nb_vld_accuracy,
                            data.frame(fold = f,
                                       auc = get_auc(as.numeric(val_data$Exited == "churn"), tmp_nb_preds)))

}

print(mean(nb_vld_accuracy$auc))

### Se pide que complete los siguientes puntos:

# 1) Validando el modelo mediante k-fold cross validation con 10
# folds evalúe cuál es la performance de un árbol de decisión 
# (de la librería rpart) que utiliza los hiperparámetros default.

library(rpart)
library(rpart.plot)

folds <- 10
indexes <- sample(rep_len((1:folds), nrow(churn_data)))

t.m_vld_acc <- data.frame()

for (f in c(1:folds)) {
  
  print(f)
  train_data <- churn_data[indexes!=f,]
  val_data  <- churn_data[indexes==f,]
   
  tree_model <- rpart(Exited ~ ., data = train_data, control = rpart.control())

t.m_pred <- predict(tree_model, newdata = val_data, type = "prob")[, "churn"]


t.m_vld_acc <- rbind(t.m_vld_acc, data.frame(
  fold = f, auc = get_auc(as.numeric(val_data$Exited == "churn"), t.m_pred)))
}  

print(mean(t.m_vld_acc$auc))


# 2) Cree un validation-set con 1000 observaciones elegidas al azar.

indices <- sample(c(1:nrow(churn_data)))

validation_set <- churn_data[indices[c(1:1000)],]

# 3) Cree un test-set con 1000 observaciones elegidas al azar (que
# no deben haber sido elegidas en el punto 2 como parte del conjunto
# del validation set).

test_set <- churn_data[indices[c(1001:2000)],]

# 4) Cree un training-set que sea igual a churn_data pero no tenga
# las observaciones extraídas tanto para el validation set como para
# el test set (note que este conjunto debe tener 8000 observaciones).

training_set <- churn_data[indices[c(2001:nrow(churn_data))],]


# 5) Lleve adelante un experimento en donde entrenando con el
# dataset creado en el punto 4 y validando con el dataset creado
# en el punto 3, entrene árboles de decisión con todas las
# combinaciones posible de minsplit entre 1 y 10, minbucket entre
# 1 y 10, y maxdepth entre 1 y 10 (es decir 1000 combinaciones
# diferentes de hiperparámetros). Como hicimos en clase, use
# siempre cp = 0 y xval = 0. Se pide que evalue el AUC tanto en los datos
# de entrenamiento como de validación. Estos resultados los debe guardar
# en un data.frame que se llame "rpart_exp" que tiene que tener las
# siguientes 5 columnas: minsplit, minbucket, maxdepth, auc_tr y
# auc_vd.

maxdepth <- c(1:10)
minbucket <- c(1:10)
minsplit <- c(1:10)

rpart_exp <- data.frame()

for (d in minsplit) {
     
  for (f in minbucket) {
       
     for (k in maxdepth) {
          print(c(d, f, k))
  
tree_model <- rpart(Exited ~ ., data = training_set, control = rpart.control(
  minsplit = d, minbucket = f, maxdepth = k, xval = 0, cp = 0)) 

t.m_pred_vd <- predict(tree_model, newdata = validation_set, type = "prob")[, "churn"] 
                           
t.m_pred_tr <- predict(tree_model, newdata = training_set, type = "prob")[, "churn"]

rpart_exp <- rbind(rpart_exp, data.frame(minsplit = d, minbucket = f, maxdepth = k,
                   auc_vd = get_auc(as.numeric(validation_set$Exited == "churn"),
                                    t.m_pred_vd), auc_tr = get_auc(as.numeric(training_set$Exited == "churn"), t.m_pred_tr)))

}  }  }

print(rpart_exp)

# TIP: en este ejercicio van a tener que usar un "triple for"


# 6) Identifique la mejor combinación de hiperparámetros según
# el experimento.

Optimal_values <- rpart_exp[which.max(rpart_exp$auc_vd),]

print(Optimal_values)

# Se pide que para responder este punto escriba una sentencia de R que
# que guarde la mejor combinación de hiperparámetros en alguna
# estructura de datos idónea.

# 7) Entrene un árbol con los mejores hiperparámetros encontrados
# pero utilizando la suma de los datos de training y validation.
# NO deben escribir a mano el valor de los mejores hiperparámtros
# encontrados, deben aprovechar la estructura creada en el
# ejercicio 5.

all_train_data <- rbind(training_set, validation_set)

tree_model_opt <- rpart(Exited ~ ., data = all_train_data, control = rpart.control(
                  maxdepth = Optimal_values$maxdepth, minbucket = Optimal_values$minbucket
                  , minsplit = Optimal_values$minsplit, xval = 0, cp = 0))

# 8) Calcule cuál es el valor de AUC que obtiene en el test set el
# modelo entrenado en el paso anterior 

auc_ts <- c()

t.m_pred_ts <- predict(tree_model_opt, newdata = test_set, 
                       type = "prob")[, "churn"]

auc_ts <- rbind(auc_ts, data.frame(auc = get_auc
                                   (as.numeric(test_set$Exited == "churn"),
                                     t.m_pred_ts)))

print(auc_ts)

# 9) Grafique el árbol obtenido en el punto 7.

rpart.plot(tree_model_opt)

# 10) Obtenga la importancia de atributos que se deriva del árbol
# obtenido en el punto 7. Tiene sentido lo que se obtiene?

tree_model_opt$variable.importance

#Analizando la información, la varible con mayor relevancia en el modelo 
#es la variable de edad, es decir que haciendo particiones sobre la edad
#el error cuadratico se minimiza de manera relevante. Esto tiene sentido ya
#es logico pensar que un individuo se vaya a dar de baja o no según la edad que tenga.