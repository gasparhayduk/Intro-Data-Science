rm(list=ls())
library(lightgbm)

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


random_grid <- function(size,
                        min_num_iterations, max_num_iterations,
                        min_num_leaves, max_num_leaves,
                        min_learning_rate, max_learning_rate,
                        min_min_gain_to_split, max_min_gain_to_split,
                        min_feature_fraction, max_feature_fraction,
                        min_min_data_in_leaf, max_min_data_in_leaf,
                        min_bagging_fraction, max_bagging_fraction) {

    rgrid <- data.frame(num_iterations = if (min_num_iterations == max_num_iterations) {
                                             rep(min_num_iterations, size)
                                         } else {
                                             sample(c(min_num_iterations:max_num_iterations),
                                                    size = size, replace = TRUE)
                                         },
                        num_leaves = if (min_num_leaves == max_num_leaves) {
                                         rep(min_num_leaves, size)
                                     } else {
                                         sample(c(min_num_leaves:max_num_leaves),
                                                size = size, replace = TRUE)
                                     },
                        learning_rate = if (min_learning_rate == max_learning_rate) {
                                            rep(min_learning_rate, size)
                                        } else {
                                            round(runif(size, min_learning_rate, max_learning_rate), 5)
                                        },
                        min_gain_to_split = if (min_min_gain_to_split == max_min_gain_to_split) {
                                                rep(min_min_gain_to_split, size)
                                            } else {
                                                round(runif(size, min_min_gain_to_split, max_min_gain_to_split), 5)
                                            },
                        feature_fraction = if (min_feature_fraction == max_feature_fraction) {
                                               rep(min_feature_fraction, size)
                                           } else {
                                               round(runif(size, min_feature_fraction, max_feature_fraction), 5)
                                           },
                        min_data_in_leaf = if (min_min_data_in_leaf == max_min_data_in_leaf) {
                                               rep(min_min_data_in_leaf, size)
                                           } else {
                                         sample(c(min_min_data_in_leaf:max_min_data_in_leaf),
                                                size = size, replace = TRUE)
                                           },
                        bagging_fraction = if (min_bagging_fraction == max_bagging_fraction) {
                                               rep(min_bagging_fraction, size)
                                           } else {
                                               round(runif(size, min_bagging_fraction, max_bagging_fraction), 5)
                                           })
    return(rgrid)
}


train_lightgbm <- function(data_train, data_val, rgrid) {

    watchlist <- list(train = data_train, valid = data_val)

    predicted_models <- list()

    for (i in seq_len(nrow(rgrid))) {
        print(i)
        print(rgrid[i,])

        rgrid_i <- rgrid[i, c("num_iterations",
                              "num_leaves",
                              "learning_rate",
                              "min_gain_to_split",
                              "feature_fraction",
                              "bagging_fraction",
                              "min_data_in_leaf")]
        rgrid_i$objective <- "binary"
        rgrid_i$metric <- "auc"

        trained_model <- lgb.train(data = data_train,
                                   params = as.list(rgrid_i),
                                   valids = watchlist,
                                   eval_freq = 30)

        perf_tr <- trained_model$eval_train()[[1]]$value
        perf_vd <- trained_model$eval_valid()[[1]]$value
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

#-------------------------------------------------------------------------------------------------#

df <- readRDS("data.RDS")

# Sólo hace one hot encoding de los factors (a modo de prueba sólo paso domain_id a factor)
df$domain_id <- factor(df$domain_id)

df <- one_hot_sparse(df)

train_df <- df[is.na(df[, "ROW_ID"]),]
colnames(train_df) <- NULL  # lightgbm no deja usar caracteres especiales en los nombres, los nombres de columnas quedan guardados en df
test_df <- df[!is.na(df[, "ROW_ID"]),]
colnames(test_df) <- NULL

valid_indexes <- sample(nrow(train_df), 1800)

dtrain <- lgb.Dataset(data = train_df[-valid_indexes, colnames(df) != "conversion"],
                      label = train_df[-valid_indexes, colnames(df) == "conversion"],
                      params = list(feature_pre_filter = FALSE))

dvalid <- lgb.Dataset(data = train_df[valid_indexes, colnames(df) != "conversion"],
                      label = train_df[valid_indexes, colnames(df) == "conversion"],
                      params = list(feature_pre_filter = FALSE))

# Corro el experimento
rgrid <- random_grid(size = 5,
                     min_num_iterations = 100, max_num_iterations = 100,
                     min_num_leaves = 2, max_num_leaves = 50,
                     min_learning_rate = 0.001, max_learning_rate = 0.125,
                     min_min_gain_to_split = 0, max_min_gain_to_split = 1,
                     min_feature_fraction = 0.5, max_feature_fraction = 1,
                     min_min_data_in_leaf = 1, max_min_data_in_leaf = 3,
                     min_bagging_fraction = 0.5, max_bagging_fraction = 1)

predicted_models <- train_lightgbm(dtrain, dvalid, rgrid)

res_table <- result_table(predicted_models)

preds <- data.frame(ROW_ID = df[!is.na(df[, "ROW_ID"]), "ROW_ID"],
                    conversion = predict(predicted_models[[res_table[1,"i"]]]$model,
                                         test_df[, colnames(df) != "conversion"]))

options(scipen=10)
write.table(preds, "basic_submission.csv", sep=",", row.names=FALSE, quote=FALSE)
options(scipen=0)
