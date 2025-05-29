rm(list=ls())
setwd("~/Desktop/IntroDS/Taller04")
library(xgboost)
library(dplyr)


#-------------------------------------------------------------------------------------------------#
# Cargamos los datos: 
df <- readRDS("data.RDS")

# Deberiamos pasar las variables categoricas a numericas y hacer one-hot-encoding para incluir algunas de las variables categoricas. 
# El titulo no sirve, 

df <- as.matrix(cbind(df[,sapply(df, is.numeric)], sapply(df[,sapply(df, is.logical)], as.numeric))) 
#seleccionamos las variables numericas y pasamos las variables logicas a numericas. Me quedo solo con variables numericas y logicas.  

train_df <- df[is.na(df[, "ROW_ID"]),] #el conjunto de entrenamiento es lo que tiene NA en row_id
test_df <- df[!is.na(df[, "ROW_ID"]),] #el conjunto de testing es lo que no tiene NA en row_id

valid_indexes <- sample(nrow(train_df), 1800)

#pasamos los datasets de training y testing a formato XGBoost. 
dtrain <- xgb.DMatrix(data = train_df[-valid_indexes, colnames(train_df) != "conversion"],
                      label = train_df[-valid_indexes, colnames(train_df) == "conversion"])

dvalid <- xgb.DMatrix(data = train_df[valid_indexes, colnames(train_df) != "conversion"],
                      label = train_df[valid_indexes, colnames(train_df) == "conversion"])

watchlist <- list(train = dtrain, valid = dvalid)

# Entrenamos el modelo
trained_model <- xgb.train(data = dtrain,
                            nrounds = 30,
                            watchlist = watchlist,
                            objective = "binary:logistic",
                            eval.metric = "auc",
                            print_every_n = 10)
# A este modelo le falta buscar mejores hiperparametros con random search. Hay que separar entre training y validacion primero.
# Tambien es util utilizar ingenieria de atributos. 
# Para identificar qué variables incluir, es util explorar graficos:
df %>% group_by(is_pdp) %>% summarise(mean(conversion, na.rm=TRUE))

#predecimos para testing
preds <- data.frame(ROW_ID = test_df[, "ROW_ID"],
                    conversion = predict(trained_model, test_df[,colnames(train_df) != "conversion"]))

xgb.importance(model=trained_model) # nos dice la importancia de los atributos. Buscar Product Description Page. 

#guardamos el archivo con las predicciones. Es para subir a Kaggle.
options(scipen=10)
write.table(preds, "basic_submission.csv", sep=",", row.names=FALSE, quote=FALSE)
options(scipen=0)
