rm(list=ls())
library(xgboost)
library(ggplot2)
library(dplyr)
library(stringr)
library(tm)
library(caret) 
# Ver la libreria LightGBM

setwd("~/Desktop/IntroDS/Taller04") # Seteamos el directorio

# Funciones ****************************************************************** #

one_hot_sparse <- function(data_set) {
  require(Matrix)
  require(data.table)
  
  data_set <- as.data.table(data_set)
  
  
  created <- FALSE
  
  if (sum(sapply(data_set, is.numeric)) > 0) { 
    out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.numeric), with = FALSE]), "dgCMatrix")
    created <- TRUE
  }
  
  if (sum(sapply(data_set, is.logical)) > 0) {  
    if (created) {
      out_put_data <- cbind2(out_put_data,
                             as(as.matrix(data_set[,sapply(data_set, is.logical),
                                                   with = FALSE]), "dgCMatrix"))
    } else {
      out_put_data <- as(as.matrix(data_set[,sapply(data_set, is.logical), with = FALSE]), "dgCMatrix")
      created <- TRUE
    }
  }
  
  fact_variables <- names(which(sapply(data_set, is.factor)))
  
  i <- 0
  
  for (f_var in fact_variables) {
    
    f_col_names <- levels(data_set[[f_var]])
    f_col_names <- gsub(" ", ".", paste(f_var, f_col_names, sep = "_"))
    j_values <- as.numeric(data_set[[f_var]])  
    
    if (sum(is.na(j_values)) > 0) { 
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

gen_bow_vars <- function(char_vector, var_name, min_wordLengths,
                         min_bound, remove_stopwords) {
  
  corpus <- VCorpus(VectorSource(char_vector))
  dt.mat <- DocumentTermMatrix(corpus,
                               control = list(stopwords = remove_stopwords,
                                              tolower=TRUE,
                                              removePunctuation = TRUE,
                                              wordLengths = c(min_wordLengths, Inf),
                                              bounds = list(global=c(min_bound, Inf))))
  
  var_names <- paste(var_name, colnames(dt.mat), sep="_")
  dt.mat <- sparseMatrix(i=dt.mat$i, j=dt.mat$j, x=dt.mat$v, dims = dim(dt.mat))
  colnames(dt.mat) <- var_names
  return(dt.mat)
}


# **************************************************************************** #
#-------------------------------------------------------------------------------------------------#


df <- readRDS("data.RDS") # Cargamos los datos 

#Eliminamos columnas innecesarias: 
df$accepts_mercadopago <- NULL #son todos 1, por lo que no tiene poder predictor
df$boosted <- NULL #son todos 1
df$user_id <- NULL #aleatorio
df$product_id <- NULL #aleatorio
df$item_id <- NULL #aleatorio
df$main_picture <- NULL #un URL
df$uid <- NULL #aleatorio
df$site_id <- NULL #todos iguales


# fechas ********************************************************************* #
df$print_server_timestamp <- as.numeric(as.POSIXct(df$print_server_timestamp))
df$print_server_timestamp <- (df$print_server_timestamp-min(df$print_server_timestamp))/
  (max(df$print_server_timestamp)-min(df$print_server_timestamp))

df$date <- as.numeric(as.POSIXct(df$date))
df$date <- (df$print_server_timestamp-min(df$date))/
  (max(df$date)-min(df$date))

# **************************************************************************** #
df$full_name <- word(df$full_name, sep = "->") # Solo nos quedamos con la categoría principal (para poder hacer OHE)
df$warranty <- ifelse(df$warranty == "Sin garantía" | is.null(df$warranty) == TRUE, 0, 1)


# One Hot Encoding *********************************************************** #
df3 <- as.data.frame(df$full_name)            # demasiadas variables (32)
dummy_full_name <- dummyVars("~ .", data = df3)
df3 <- data.frame(predict(dummy_full_name, newdata = df3))
df <- cbind(df, df3)

df2 <- as.data.frame(df$domain_id)             #demasiadas variables
dummy_domain_id <- dummyVars(~ ., data = df2)
df2 <- data.frame(predict(dummy_domain_id, newdata = df2))

df4 <- as.data.frame(df$listing_type_id)
dummy_listing_type_id <- dummyVars("~ .", data = df4)
df4 <- data.frame(predict(dummy_listing_type_id, newdata = df4))
df <- cbind(df, df4)

df5 <- as.data.frame(df$platform)
dummy_platform <- dummyVars("~ .", data = df5)
df5 <- data.frame(predict(dummy_platform, newdata = df5))
df <- cbind(df, df5)

df6 <- as.data.frame(df$logistic_type)
dummy_logistic_type <- dummyVars("~ .", data = df6)
df6 <- data.frame(predict(dummy_logistic_type, newdata = df6))
df <- cbind(df, df6)


# **************************************************************************** #

# One Hot Encoding Sparse **************************************************** #

df$deal_print_id <- as.factor(df$deal_print_id)
df$domain_id <- as.factor(df$domain_id)
df$category_id <- as.factor(df$category_id)
df$etl_version <- as.factor(df$etl_version)
df$free_shipping <- as.numeric(df$free_shipping)
df$fulfillment <- as.numeric(df$fulfillment)
df$full_name <- as.factor(df$full_name)
df$is_pdp <- as.numeric(df$is_pdp)
df$listing_type_id <- as.factor(df$listing_type_id)
df$logistic_type <- as.factor(df$logistic_type)
df$platform <- as.factor(df$platform)

# df$category_id <- NULL
# df$deal_print_id <- NULL
# df$tags <- NULL
# df$title <- NULL

dfMatrix <- one_hot_sparse(df)


# **************************************************************************** #
# Bag-of-words *************************************************************** #

# Hcaer bag of words sobre los tags. 



dfMatrix <- cbind(dfMatrix, bow_title)

# **************************************************************************** #
# dfMatrix <- as.matrix(cbind(df[,sapply(df, is.numeric)], sapply(df[,sapply(df, is.logical)], as.numeric)))



train_df <- dfMatrix[is.na(df[, "ROW_ID"]),]
test_df <- dfMatrix[!is.na(df[, "ROW_ID"]),]

valid_indexes <- sample(nrow(train_df), 1800)

dtrain <- xgb.DMatrix(data = train_df[-valid_indexes, colnames(train_df) != "conversion"],
                      label = train_df[-valid_indexes, colnames(train_df) == "conversion"])

dvalid <- xgb.DMatrix(data = train_df[valid_indexes, colnames(train_df) != "conversion"],
                      label = train_df[valid_indexes, colnames(train_df) == "conversion"])

watchlist <- list(train = dtrain, valid = dvalid)

trained_model <- xgb.train(data = dtrain,
                            nrounds = 90,
                            watchlist = watchlist,
                            objective = "binary:logistic",
                            eval.metric = "auc",
                            print_every_n = 10)

preds <- data.frame(ROW_ID = test_df[, "ROW_ID"],
                    conversion = predict(trained_model, test_df[,colnames(train_df) != "conversion"]))

options(scipen=10)  # Para evitar que guarde los datos en notación científica
write.table(preds, "basic_submission.csv", sep=",", row.names=FALSE, quote=FALSE)
options(scipen=0) # Para cancelar el comportamiento que hace que no se guarde en notación científica

xgb.importance(colnames(dtrain), model = trained_model)
