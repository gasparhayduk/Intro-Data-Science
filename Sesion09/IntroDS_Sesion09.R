rm(list=ls())

library(dplyr)
library(xgboost)
library(pROC)

setwd("~/Desktop/IntroDS/Sesion09")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1er bloque
install.packages("Matrix")
library('Matrix')

M1 <- matrix(0, nrow = 1000, ncol = 1000) #matriz de 1000 filas y 1000 columnas llena de ceros. Ocupa mucho esa=pacio
M2 <- Matrix(0, nrow = 1000, ncol = 1000, sparse = TRUE) #misma matriz anterior pero en formato sparse

#Cuanto espacio ocupan estas matrices: 
object.size(M1)  # Para ver en mb format(object.size(M1), units= "Mb")
object.size(M2)
#La matriz M2 pesa muuuucho menos. 

M1[500, 500] <- 1
M2[500, 500] <- 1
 
object.size(M1)
object.size(M2)

# Creamos matrices de numeros aleatorios
M1 <- matrix(rnorm(1000000), nrow = 1000, ncol = 1000)
M2 <- Matrix(rnorm(1000000), nrow = 1000, ncol = 1000, sparse = TRUE)

# Las matrices malas (sparces) solo guardan los numeros distintos de ceros y la posicion que ocupan dentro de la matriz. 
# Estas matrices son utiles solo cuando vamos a tener muchos ceros. 

format(object.size(M1), units= "Mb")
format(object.size(M2), units= "Mb") 
#Como en este caso de numeros aleatorios hay pocos ceros, la matriz sparce ocupa mas espacio que la matriz normal. 

M1 <- sparseMatrix(i=c(2, 20), j=c(3, 5), x=c(4, 8))
# creamos una matriz sparce que tenga un 4 en la fila 2 y columna 3, y que tenga un 8 en la fila 20 y columna 5 
class(M1)
print(M1)

M2 <- sparseMatrix(i=c(2, 20), j=c(3, 5), x=c(4, 8), dims = c(25, 10)) #mismo que antes pero le indicamos que la matriz tenga 25 filas y 10 columnas
print(M2)

M3 <- sparseMatrix(i=c(2, 20), j=c(3, 5), x=c(4, 8), dims = c(25000, 10000)) #mismo que antes pero le indicamos que la matriz tenga 25000 filas y 10000 columnas
print(M3)

format(object.size(M1), units= "Mb")
format(object.size(M2), units= "Mb")
format(object.size(M3), units= "Mb")
#Vemos que estas matrices pesan muy poco. La M3 pesaria mucho si no estuviera en formaro sparce. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2do bloque: hacer one-hot-encoding.
# Si tenemos una cateogoria que tome k valores diferentes que queremos pasarlos a one-hot-encoding,
# estamos agregando k columnas nuevas con k unos. Los demas serán ceros. 
# Si hay un NA en la columna que indica la categoria, cuando se pase a one-hot-encoding esa fila deberia estar llena de NAs;
# esta forma de guardar a los NAs es mala porque no se ahorra espacio, nosotros vamos a agregar una columna que indique que en esa fila hay un NA.
# tratamos a los NA como una categoria mas. Esto permite trabajar con muchos datos usando sparces. 




#Esta funcion toma un dataframe y devuelve ese dataset en formato matriz rala. Hay que asegurarse que las categorias esten como factor. . Si esta como texto, la ignora. 
one_hot_sparse <- function(data_set) {
    require(Matrix)
    require(data.table)

    data_set <- as.data.table(data_set)


    created <- FALSE

    if (sum(sapply(data_set, is.numeric)) > 0) {  # Si hay, Pasamos los numéricos a una matriz esparsa (sería raro que no estuviese, porque "Label"  es numérica y tiene que estar sí o sí)
        out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.numeric), with = FALSE]), "dgCMatrix")
        created <- TRUE
    }

    if (sum(sapply(data_set, is.logical)) > 0) {  # Si hay, pasamos los lógicos a esparsa y lo unimos con la matriz anterior
        if (created) {
            out_put_data <- cbind2(out_put_data,
                                    as(as.matrix(data_set[,sapply(data_set, is.logical),
                                                           with = FALSE]), "dgCMatrix"))
        } else { 
            out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.logical), with = FALSE]), "dgCMatrix")
            created <- TRUE
        }
    }

    # Identificamos las columnas que son factor (OJO: el data.frame no debería tener character)
    fact_variables <- names(which(sapply(data_set, is.factor)))

    # Para cada columna factor hago one hot encoding
    i <- 0

    for (f_var in fact_variables) {

        f_col_names <- levels(data_set[[f_var]])
        f_col_names <- gsub(" ", ".", paste(f_var, f_col_names, sep = "_"))
        j_values <- as.numeric(data_set[[f_var]])  # Se pone como valor de j, el valor del nivel del factor
        
        if (sum(is.na(j_values)) > 0) {  # En categóricas, trato a NA como una categoría más
            j_values[is.na(j_values)] <- length(f_col_names) + 1
            f_col_names <- c(f_col_names, paste(f_var, "NA", sep = "_"))
        }

        if (i == 0) {
            fact_data <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                      x = rep(1, nrow(data_set)),
                                      dims = c(nrow(data_set), length(f_col_names)))
            fact_data@Dimnames[[2]] <- f_col_names
        } else {
            fact_data_tmp <- sparseMatrix(i = c(1:nrow(data_set)), j = j_values,
                                          x = rep(1, nrow(data_set)),
                                          dims = c(nrow(data_set), length(f_col_names)))
            fact_data_tmp@Dimnames[[2]] <- f_col_names
            fact_data <- cbind(fact_data, fact_data_tmp)
        }

        i <- i + 1
    }

    if (length(fact_variables) > 0) {
        if (created) {
            out_put_data <- cbind(out_put_data, fact_data)
        } else {
            out_put_data <- fact_data
            created <- TRUE
        }
    }
    return(out_put_data)
}


weather_data <- read.table("weatherAUS.csv", sep = ",", header = TRUE, stringsAsFactors = TRUE) #cargamos el dataset. 
#transformamos un poco el dataset
weather_data$Date <- as.Date(weather_data$Date)
weather_data$year <- as.numeric(format((weather_data$Date), "%Y"))
weather_data$month <- as.numeric(format((weather_data$Date), "%m"))
weather_data$Date <- as.numeric(weather_data$Date)
weather_data$RainTomorrow <- ifelse(weather_data$RainTomorrow=="Yes", 1, 0)
weather_data$RISK_MM <- NULL

# one-hot-encodigns Versión de Caret. Instalar la libreria primero 
library(caret)
one_hot_model <- dummyVars(~ ., data = weather_data)
weather_one_hot_dense <- predict(one_hot_model, weather_data)
head(weather_one_hot_dense)

# Función one_hot_sparse. Esta version ocupa menos espacio. 
weather_one_hot_sparse <- one_hot_sparse(weather_data)
head(weather_one_hot_sparse)
colnames(weather_one_hot_sparse)

format(object.size(weather_one_hot_dense), units = "Mb")
format(object.size(weather_one_hot_sparse), units = "Mb")

# Separo un conjunto de validación
training_set <-  weather_one_hot_sparse[weather_one_hot_sparse[,"year"] < 2016,]
validation_set <- weather_one_hot_sparse[weather_one_hot_sparse[,"year"] == 2016,]
testing_set <- weather_one_hot_sparse[weather_one_hot_sparse[,"year"] == 2017,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 3er bloque: Random search



# funcion que tira puntos aleatorios en una dimension 
random_grid <- function(size,
                        min_nrounds, max_nrounds,
                        min_max_depth, max_max_depth,
                        min_eta, max_eta,
                        min_gamma, max_gamma,
                        min_colsample_bytree, max_colsample_bytree,
                        min_min_child_weight, max_min_child_weight,
                        min_subsample, max_subsample) {

    rgrid <- data.frame(nrounds = if (min_nrounds == max_nrounds) {
                                      rep(min_nrounds, size)
                                  } else {
                                      sample(c(min_nrounds:max_nrounds),
                                             size = size, replace = TRUE)
                                  },
                        max_depth = if (min_max_depth == max_max_depth) {
                                      rep(min_max_depth, size)
                                  } else {
                                      sample(c(min_max_depth:max_max_depth),
                                             size = size, replace = TRUE)
                                  },
                        eta = if (min_eta == max_eta) {
                                      rep(min_eta, size)
                                  } else {
                                      round(runif(size, min_eta, max_eta), 5)
                                  },
                        gamma = if (min_gamma == max_gamma) {
                                      rep(min_gamma, size)
                                  } else {
                                      round(runif(size, min_gamma, max_gamma), 5)
                                  },
                        colsample_bytree = if (min_colsample_bytree == max_colsample_bytree) {
                                      rep(min_colsample_bytree, size)
                                  } else {
                                      round(runif(size, min_colsample_bytree, max_colsample_bytree), 5)
                                  },
                        min_child_weight = if (min_min_child_weight == max_min_child_weight) {
                                      rep(min_min_child_weight, size)
                                  } else {
                                      round(runif(size, min_min_child_weight, max_min_child_weight), 5)
                                  },
                        subsample = if (min_subsample == max_subsample) {
                                      rep(min_subsample, size)
                                  } else {
                                      round(runif(size, min_subsample, max_subsample), 5)
                                  })
    return(rgrid)
}

#esta funcion corre el modelo para cada uno de los puntos. los puntos a probar se ven en rgrid. 
# Guarda toda la info de los parametros y auc para validacion y training en una lista
train_xgboost <- function(data_train, data_val, rgrid) {

    watchlist <- list(train = data_train, valid = data_val)

    predicted_models <- list()

    for (i in seq_len(nrow(rgrid))) {
        print(i)
        print(rgrid[i,])
        trained_model <- xgb.train(data = data_train,
                                   params=as.list(rgrid[i, c("max_depth",
                                                             "eta",
                                                             "gamma",
                                                             "colsample_bytree",
                                                             "subsample",
                                                             "min_child_weight")]),
                                   nrounds = rgrid[i, "nrounds"],
                                   watchlist = watchlist,
                                   objective = "binary:logistic",
                                   eval.metric = "auc",
                                   print_every_n = 10)

        eval_log <- as.data.frame(trained_model$evaluation_log)
        perf_tr <- eval_log[trained_model$niter,grep("train", colnames(eval_log))]
        perf_vd <- eval_log[trained_model$niter,grep("valid", colnames(eval_log))]
        print(c(perf_tr, perf_vd))

        predicted_models[[i]] <- list(results = data.frame(rgrid[i,],
                                                           perf_tr = perf_tr,
                                                           perf_vd = perf_vd),
                                      model = trained_model)
        rm(trained_model)
        gc()
    }

    return(predicted_models)
}


#esta funcion pasa a tabla lo que devuelve la funcion train_xgboost. Esta ordenada de forma decreciente en validacion. 
# la primera fila sera el modelo con mayor auc en validacion. El i dice qué orden de modelo es. 
result_table <- function(pred_models, higher_is_better = TRUE) {
    res_table <- data.frame()
    i <- 1
    for (m in pred_models) {
        res_table <- rbind(res_table, data.frame(i = i, m$results))
        i <- i + 1
    }

    hib <- if (higher_is_better) -1 else 1

    res_table <- res_table[order(hib * res_table$perf_vd),]
    return(res_table)
}


# Corro el experimento. Vamos a hacer el experimento para los puntos que nos da esta funcion
rgrid <- random_grid(size = 5, #en size va cuantos puntos/modelos queremos
                     min_nrounds = 50, max_nrounds = 300, #entre que valores se va a mover nrounds
                     min_max_depth = 2, max_max_depth = 12, #entre que valores se va a mover maxdepth
                     min_eta = 0.001, max_eta = 0.125, #entre que valores se va a mover eta 
                     min_gamma = 0, max_gamma = 1, #entre que valores se va a mover gamma 
                     min_colsample_bytree = 0.5, max_colsample_bytree = 1,
                     min_min_child_weight = 0, max_min_child_weight = 2,
                     min_subsample = 0.5, max_subsample = 1)

#pasamos los datos para poder hacer XGBoost
dtrain <- xgb.DMatrix(data = training_set[,setdiff(colnames(training_set), "RainTomorrow")],
                      label = training_set[,"RainTomorrow"])

dvalid <- xgb.DMatrix(data = validation_set[,setdiff(colnames(validation_set), "RainTomorrow")],
                      label = validation_set[,"RainTomorrow"])

predicted_models <- train_xgboost(dtrain, dvalid, rgrid) 
#aca hay una lista donde cada elemento nos dicen el modelo entrenado y sus resultados 

# Veo los resultados
results <- result_table(predicted_models)
print(results)

# Obtengo las predicciones del mejor modelo en validación
predict(predicted_models[[results[1, "i"]]]$model, dvalid)

# Veo cómo le va en testing
dtest <- xgb.DMatrix(data = testing_set[,setdiff(colnames(testing_set), "RainTomorrow")],
                     label = testing_set[,"RainTomorrow"])

preds_testing_probs <- predict(predicted_models[[results[1, "i"]]]$model, dtest)

auc(roc(testing_set[,"RainTomorrow"], preds_testing_probs))
