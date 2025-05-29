rm(list=ls())

library(caret)
library(dplyr)
library(tm)
library(Matrix)
library(stringr)

setwd("~/Desktop/IntroDS/Sesion12")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Construcción de una dt.mat

# Buscamos hacer un modelo que avise cuando se está hablando mal de la comida.

guia_oleo <- readRDS("guia_oleo.RDS")
head(guia_oleo)
# 
corpus <- VCorpus(VectorSource(guia_oleo$comment))  # Se indica de dónde viene el corpus

corpus[[45]]
corpus[[45]]$meta
corpus[[45]]$content

# Paso a minúscula
corpus <- tm_map(corpus, content_transformer(tolower))
corpus[[45]]$content

# Quito signos de puntuación
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus[[45]]$content

# Quito stopwords
print(stopwords("spanish"))
stp_words <- stopwords("spanish")[stopwords("spanish") != "no"]
corpus <- tm_map(corpus, content_transformer(function(x) removeWords(x, stp_words)))
corpus[[45]]$content

# Quito doble espacio y espacio al final (esto es de TOC)
corpus <- tm_map(corpus, content_transformer(function(x) gsub("\\s+", " ", str_trim(x))))
corpus[[45]]$content

# Pruebo el tokenizador
scan_tokenizer(corpus[[45]]$content)
rm(corpus)
gc()

# Función para hacer bag of words
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

# min_wordLenghts hace que se ignoren las palabras de cierta cantidad de caracteres.
# min_bound mínimo de documento en los que tienen que aparecer las palabtras.
# var_name es la palabra que aparece antes en el título de la columna de la matriz.


bow_comments <- gen_bow_vars(guia_oleo$comment, "comment", 1, 15, FALSE)

# Paso a formato denso para trabajar más simple con caret, pero en el tp esto no sería buena idea
bow_comments <- as.data.frame(as.matrix(bow_comments))  # OJO con esto, puede no ser buena idea pasar a matriz densa los datos
bow_comments <- cbind(data.frame(id_comentario=guia_oleo$id_comentario,
                                 clase_comentario=guia_oleo$clase_comentario),
                      bow_comments)

bow_comments$clase_comentario <- factor(ifelse(bow_comments$clase_comentario == "Malo", "malo", "no_malo"))

head(bow_comments[, c("comment_pizza", "comment_zona", "comment_rico", "comment_feo", "clase_comentario")], 300)

# Entreno un modelo que detecte sentimiento
fitControl <- trainControl(method = "LGOCV", number = 1, p = 0.75,
                           verboseIter = TRUE, classProbs = TRUE,
                           summaryFunction = twoClassSummary)

# xgboost
xgbTreeFit <- train(x = bow_comments %>% select(-clase_comentario, -id_comentario),
                    y = bow_comments$clase_comentario,
                    method = "xgbTree",
                    trControl = fitControl,
                    tuneGrid = data.frame(eta=0.1, max_depth=3,
                                          gamma=0.25, colsample_bytree=0.8,
                                          min_child_weight=1,
                                          subsample=0.75, nrounds=150),
                    metric = "ROC")

xgbTreeFit$results[which.max(xgbTreeFit$results$ROC),]

plot(varImp(xgbTreeFit), top = 30)

rm(fitControl, xgbTreeFit)
gc()
