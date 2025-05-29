rm(list=ls())

setwd("~/Desktop/IntroDS/Sesion07") # seteamos el directorio

# Instalamos/activamos las librerias:
library(rpart)
install.packages("pROC")
library(pROC)
library(ggplot2)


data_bank <- read.table("bank-full.csv", sep=";", header=TRUE) # abrimos la base de datos. Contiene datos de individuos (caracteristicas X) y si tomó o no un producto y. 

prop.table(table(data_bank$y)) #vemos cuantos tomaron el producto y. Si clasificabamos a todos como NO, tendriamos un accuracy de 0.88

# Separamos en training y evaluation
train_index <- sample(c(1:nrow(data_bank)), 36200)
training_bank <- data_bank[train_index,]
evaluation_bank <- data_bank[-train_index,]

tree_fit <- rpart(y ~ ., data = training_bank,
                  control = rpart.control(maxdepth = 30, xval=0, cp=0)) #entrenamos un arbol de profundidad 30. 

preds_p <- predict(tree_fit, newdata=evaluation_bank)[, "yes"]

# Definimos una funcion que toma una matriz de confusion y calcula precision, recall, f1 y fpr para esa matriz. 
prec_recall <- function(conf_m, verbose=TRUE) {
  if (verbose) {
    print(conf_m)
  }
  prec <- conf_m["yes", "yes"] / sum((conf_m[, "yes"]))
  rec <- conf_m["yes", "yes"] / sum((conf_m["yes",]))
  f1 <- 2 * prec * rec / (prec + rec)
  fpr <- conf_m["no", "yes"] / sum((conf_m["no",]))
  return(data.frame(prec=prec, rec=rec, f1=f1, tpr=rec, fpr=fpr))
}

# Distribución de las probabilidades predichas
ggplot(data.frame(p=preds_p, clase=evaluation_bank$y), aes(x=p, col=clase, fill=clase)) +
    geom_density(alpha=0.5) # no parece malo el modelo, ahora vemos cómo nos dan las metricas. 

# Valor predefinido. Calculamos precision, recall, f1 para un umbral de 0.5
prec_recall(table(actual=evaluation_bank$y,
                  pred=factor(ifelse(preds_p >= 0.5, "yes", "no")))) #table(actual=evaluation_bank$y,pred=factor(ifelse(preds_p >= 0.5, "yes", "no"))) es la matriz de confusion para un umbral de decision de 0.5

# Valor que debería aumentar recall
prec_recall(table(actual=evaluation_bank$y,
                  pred=factor(ifelse(preds_p >= 0.3, "yes", "no"))))

# Valor que debería aumentar precision
prec_recall(table(actual=evaluation_bank$y,
                  pred=factor(ifelse(preds_p >= 0.7, "yes", "no"))))

# Grafico la curva ROC. Creamos un loop para diferentes umbrales de decision. 
data_roc <- data.frame()
for (p in seq(0.001, 0.999, by=0.001)) {
  data_roc <- rbind(data_roc,
                    prec_recall(table(actual=evaluation_bank$y,
                                      pred=factor(ifelse(preds_p > p,
                                            "yes", "no"))),
                                verbose=FALSE)[, c("fpr", "tpr")])
}

plot(data_roc, type="l", xlim=c(0, 1), ylim=c(0, 1))
abline(c(0,1))

# Cálculo del AUC
auc(roc(ifelse(evaluation_bank$y=="yes", 1, 0), preds_p))
as.numeric(auc(roc(ifelse(evaluation_bank$y=="yes", 1, 0), preds_p)))  # Para obtener un número
