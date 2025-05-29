rm(list=ls())

setwd("~/Desktop/IntroDS/Sesion10")
library(caret)
library(dplyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1er bloque

data_set <- read.table("bankruptcy_data.txt", sep="\t", header=TRUE) # datos de empresar que hacen bancarrota y algunas caracteristicas de cada empresar. 

#queremos predecir si la empresa hace bancarrota o no:
table(data_set$class)
data_set$class <- factor(ifelse(data_set$class, "y", "n"), levels=c("y", "n"))


# ver ?trainControl
# Primero se debe definir el esquema que se utilizará para validar el modelo
fitControl <- trainControl(method = "cv", #cv es cross-validation
                           number = 3, # cantidad de folds
                           verboseIter = TRUE) #para que imprima por qué vuelta va 
# Ver ?train
# Pruebas de búsqueda de hiperparámetros (grid search). Cambia el hiperparametro cp. 
rpart_fit <- train(x = data_set[,-ncol(data_set)], #variables predictoras
                   y = data_set$class, #variable a predecir
                   method = "rpart", #aca le indicamos el modelo. Puede ser rpart, knn, etc. 
                   trControl = fitControl)

# Caret ya devuelve el mejor hiperparametro (el que da mejor accuracy) y entrena un modelo con ese hiperparametro

print(rpart_fit)  # Qué métricas calcula sobre validación? Accuracy y kappa. 

rpart_fit$results[which.max(rpart_fit$results$Accuracy),] 

# Probemos otra estructura de experimento: cambiamos el metodo de validacion 
fitControl <- trainControl(method = "LGOCV",  # Dejamos un conjunto de validación fuera
                           number = 1,
                           p = 0.8, #porcentaje del dataset que va a training 
                           classProbs = TRUE,  # Pedimos que devuelva las probabilidades estimadas
                           verboseIter = TRUE,
                           summaryFunction = twoClassSummary)  # Pedimos que calcula otras métricas sobre validación

#Cambiamos el metodo de estimacion a KNN: 
knn_fit <- train(x = data_set[,-ncol(data_set)],
                 y = data_set$class,
                 method = "knn",
                 tuneLength = 4,  # Pedimos que prueba 4 valores de vecinos
                 trControl = fitControl,
                 metric = "ROC")  # Pedimos que elija el mejor modelo en base a AUC
#deberiamos probar valores mas altos de k. 
grilla <- data.frame(k=seq(5,20,5))
knn_fit <- train(x = data_set[,-ncol(data_set)],
                 y = data_set$class,
                 method = "knn",
                 tuneLength = 4,  # Pedimos que prueba 4 valores de vecinos
                 trControl = fitControl,
                 metric = "ROC",
                 tuneGrid = grilla) #con esto ultimo le indicamos que valores k debe probar 



print(knn_fit)  # Qué métricas calcula sobre validación?

knn_fit$results[which.max(knn_fit$results$ROC),]

# Predecir con un modelo entrenado es muy simple
predict(rpart_fit, newdata = data_set)
predict(rpart_fit, newdata = data_set, type = "prob")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2do bloque

# Modelo con las variables normalizadas
knn_fit_scaled <- train(x=data_set %>% select(-class), y=data_set$class,
                        method="knn",
                        trControl = fitControl,  # Mantenemos el trainControl de antes
                        preProcess = c("center","scale"), #Caret permite pre-procesar los datos. Aca le indicamos que centre y escale las X 
                        metric = "ROC")

knn_fit_scaled$results[which.max(knn_fit_scaled$results$ROC),] #el modelo mejora su perfomance al reescalar las X. 

# Modelo con las variables en logaritmos
data_set_log <- data_set
for (v in setdiff(names(data_set_log), "class")) {
    data_set_log[[v]] <- log(data_set_log[[v]] + 1 - min(data_set_log[[v]]))
} #generamos un nuevo dataset donde todas las variables, salvo class, estan en logaritmos

knn_fit_log <- train(x=data_set_log %>% select(-class), y=data_set_log$class,
                     method="knn",
                     trControl = fitControl,
                     metric = "ROC")

knn_fit_log$results[which.max(knn_fit_log$results$ROC),] #datos en logaritmos mejora la perfomance respecto de la version base pero no mejora la version de re-escalar. 
# Podriamos probar logaritmos + re-escalar. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3er bloque

nb_fit <- train(x = data_set %>% select(-class),
                y = data_set$class,
                method = "naive_bayes",
                trControl = fitControl,
                metric = "ROC") #bayes ingenuo con los datos tal cual vienen 

nb_fit$results[which.max(nb_fit$results$ROC),]

discretize <- function(input_data, bins=10, type="equal") {

    # Función que dado un vector de input lo discretiza
    if (type == "equal") {
        cut_points <- seq(from=min(input_data), to=max(input_data), length.out=bins+1)
    } else if (type == "freq") {
        cut_points <- unique(quantile(input_data, prob=seq(from=0, to=1, length.out=bins+1)))
    } else {
        return(NULL)
    }
    cut_points[1] <- -Inf
    cut_points[length(cut_points)] <- Inf
    return(cut(input_data, breaks = cut_points))
}

# Discretizamos por intervarlos de igual largo. Pasamos de variables numericas a categoricas
data_set_disc_eq <- data_set
for (v in setdiff(names(data_set_log), "class")) {
    data_set_disc_eq[[v]] <- discretize(data_set_disc_eq[[v]], bins = 60, "equal") #la cantidad de bins es algo a probar, un hiperparametro
}

nb_fit_disc_equal <- train(x = data_set_disc_eq %>% select(-class),
                           y = data_set_disc_eq$class,
                           method="naive_bayes",
                           trControl = fitControl,
                           metric = "ROC")

nb_fit_disc_equal$results[which.max(nb_fit_disc_equal$results$ROC),] #mejora mucho la performance. La mejor performance hasta ahora

#Discretizamos por frecuencia
data_set_disc_fq <- data_set
for (v in setdiff(names(data_set_log), "class")) {
    data_set_disc_fq[[v]] <- discretize(data_set_disc_fq[[v]], bins = 60, "freq")
}

nb_fit_disc_freq <- train(x = data_set_disc_fq %>% select(-class),
                          y = data_set_disc_fq$class,
                          method="naive_bayes",
                          trControl = fitControl,
                          metric = "ROC")

nb_fit_disc_freq$results[which.max(nb_fit_disc_freq$results$ROC),]


predict(nb_fit_disc_equal, type = "prob")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 4to bloque

when <- data.frame(time = factor(c("afternoon", "night", "afternoon",
                                   "morning", "morning", "morning",
                                   "morning", "afternoon", "afternoon")),
                   day = factor(c("Mon", "Mon", "Mon",
                                  "Wed", "Wed", "Fri",
                                  "Sat", "Sat", "Fri")),
                   num_var = c(1:9))

levels(when$day) <- list(Mon="Mon", Tue="Tue", Wed="Wed", Thu="Thu",
                         Fri="Fri", Sat="Sat", Sun="Sun")

head(when)

mainEffects <- dummyVars(~ day + time, data = when)
mainEffects
predict(mainEffects, when)

mainEffects <- dummyVars(~ ., data = when) #one-hot-encoding de todas las variables
mainEffects
predict(mainEffects, when)

when2 <- when
when2[1, 1] <- NA
predict(mainEffects, when2)
#si una variable tiene NA, todas tendran NA si hacemos one-hot-encoding sobre esa variable

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 5to bloque

# Simulamos un dataset totalmente aleatorio
y <- factor(ifelse(sample(rep_len(c(0, 1), 2500)), "y", "n"))
X <- matrix(rnorm(25000000), ncol=10000) #matriz de 2500 observaciones y 10000 columnas

# Seleccionamos atributos en base a t-tests
var_imp <- apply(X, 2, function(x) {t.test(x[y=="y"], x[y!="y"])$p.value}) #calculamos el p-value para cada columna de X
top_vars <- order(var_imp)[c(1:150)] #ordenamos los p-value. obtenemos los 150 mas pequeños

# Ordenamos para que sea un data.frame entrenable y separamos en training y validation
X <- data.frame(X[,top_vars]) #dataset con los p-value mas relevantes. 
names(X) <- paste("Col", c(1:ncol(X)), sep="")

#Entrenamos con la miad de las X relevantes y validamos con la otra
fit_on <- list("holdout" = seq(1, nrow(X)/2))
val_on <- list("holdout" = seq(nrow(X)/2 + 1, nrow(X)))

# Definimos la estructura del esquema de validación y entrenamos
fitControl <- trainControl(method = "cv",  ## El método da igual podría ser LOOCV
                           index = fit_on, indexOut = val_on,
                           verboseIter = TRUE)

xgb_sim_fit <- train(x = X,
                     y = y,
                     method="xgbTree", trControl=fitControl)

xgb_sim_fit$results[which.max(xgb_sim_fit$results$Accuracy),]  # Obtuvimos información del azar!
# Obtuvimos un modelo que obtiene una performance del 67%, pero no deberia ser asi porque las X son aleatorias; no deberian predecir las Y.
# Esto es un problema. Con nuevos datos va a dar mal. 
# Hay algo en la seleccion de atributos que estuvo mal. Antes de separar en training y validation, elegimos las variables. 
# Me quedo con las variables mas predictivas viendo entrenamiento Y VALIDACION, cuando deberiamos actuar como que los datos de validacion son desconocidos. 
# Es decir, estamos seleccionando las variables predictoras con datos que no deberiamos ver. El conjunto de validacion ya no sirve. 
# Primero deberiamos haber partido a la mitad y despues hacer seleccion de atributos. 
