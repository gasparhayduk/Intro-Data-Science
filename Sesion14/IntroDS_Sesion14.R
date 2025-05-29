rm(list=ls())

library(tm)
library(Matrix)
library(ROCR)
library(ISLR)
library(glmnet)
setwd("~/Desktop/IntroDS/Sesion14")

#~~~~~~~~~~~~~~~~~~Regresión Logistica~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Tenemos una variable 𝑦𝑖 que vale 1 si la observación pertenece a la clase que
# estamos analizando y 0 si no pertenece (trabajaremos sólo con el caso binario).

# Buscamos predecir la probabilidad de que 𝑦𝑖 valga 1 dados los valores de una serie de variables x_i
# (una probabilidad condicional). De modo que asumimos que y_ depende de una función f() de x_i. 

# Una función que tiene esta forma es la función logística (o sigmoidea), que en
# el caso que tenga como input un función lineal de una única variable explicativa tiene la siguiente expresión:

# p(y_i=1| x_i) = 1 / (1 + e^(-beta0 + beta1*x_i)) 
# si los betas son positivos, tiende a cero. Si son positivos, tiende a 1. 

# LogLoss es nuestra funcion de costos. La vamos a minimizar. 

.




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Simulo datos

X <- matrix(rnorm(1000), ncol=10, byrow=TRUE) # matriz de numeros aleatorios
y <- matrix(sample(c(1, 0), size = 100, replace=TRUE), ncol=1) #matriz de ceros y unos aleatorios. 
coefs <- matrix(rnorm(10), ncol=1)
bias <- rnorm(1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Regresión logística

sigmoid <- function(z) {
  return(1/(1 + exp(-z)))
}

activation <- function(X, coefs, bias) {
  return(X %*% coefs + bias)
}

logistic_pred <- function(X, coefs, bias) {
  return(sigmoid(activation(X, coefs, bias)))
}

logloss <- function(y, preds_probs) {
  return(-(sum(log(preds_probs[y==1])) + sum(log(1-preds_probs[y==0]))) / length(y))
} #logloss es nuestra funcion de costos. La vamos a minimizar. 

preds_probs <- logistic_pred(X, coefs, bias) #predicciones dados x, y y los betas que les dimos. 
logloss(y, preds_probs) #calculamos el LogLoss para las predicciones que hicimos. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Descenso gradiente: optimizar una funcion que no puede derivarse e igualar a cero. 

funcion_a_minimizar <- function(x) {
  return(2 * x ^ 2 + 20 * x - 5)
}


derivada_de_f <- function(x) {
  return(4 * x + 20)
}

plot(seq(-10, 0, 0.1), funcion_a_minimizar(seq(-10, 0, 0.1)), type = "l")
x <- runif(1)
parar <- FALSE
gam <- 0.3
criterio_de_parada <- 0.000001

i <- 1
while (parar == FALSE) {
  x_anterior <- x
  x <- x_anterior - gam * derivada_de_f(x_anterior)
  print(i)
  if (sqrt((x_anterior - x) ^ 2) < criterio_de_parada) {
    parar <- TRUE
  }
  i <- i + 1
}

print(x)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Descenso gradiente en regresión logística

derivative_of_logloss <- function(X, y, preds_probs) {
  derivative_coefs <- (t(X) %*% (preds_probs - y)) / length(y)
  derivative_bias  <- mean(preds_probs - y)
  return(list(derivative_coefs = derivative_coefs,
              derivative_bias=derivative_bias))
}

train_gd_logistic <- function(X, y, coefs, bias, learning_rate = 0.1,
                              stopCriterion = 0.0000001, verbose = TRUE,
                              print_every = 3000) {
  
  n_param <- length(coefs) + 1
  end_while <- FALSE
  preds_probs <- logistic_pred(X, coefs, bias)
  
  i <- 1
  while (end_while == FALSE) {
    
    old_coefs <- coefs
    old_bias <- bias
    
    der_logloss <- derivative_of_logloss(X, y, preds_probs)
    
    # Actualizo los coeficientes y el sesgo
    coefs <- old_coefs - learning_rate * der_logloss$derivative_coefs
    bias <- old_bias - learning_rate * der_logloss$derivative_bias
    
    # Veo las nuevas predicciones
    preds_probs <- logistic_pred(X, coefs, bias)
    
    # Imprimo el nuevo error
    if (verbose & !(i %% print_every)) {print(logloss(y, preds_probs))}
    
    # Veo si parar el while
    if ((1 / n_param * sqrt(sum((old_coefs - coefs) ^ 2) + (old_bias - bias) ^ 2)) < stopCriterion) {
      end_while <- TRUE
    }
    i <- i + 1
  }
  return(list(coefs=coefs, bias=bias))
} # Entrena una regresion logistica dado un vector x, un vector y, unas probabilidades predichas, un valor inicial, un learning rate y un criterio de parada. 

# Resultados sobre los datos simulados
trained_logistic <- train_gd_logistic(X, y, coefs, bias)
preds_probs <- logistic_pred(X, trained_logistic$coefs, trained_logistic$bias)

logistic_R <- glm(data=data.frame(X=X, y=y), formula="y ~ .", family=binomial)
preds_probs_R <- matrix(predict(logistic_R, data.frame(X=X, y=y), type = "response"), ncol=1)

plot(preds_probs_R, preds_probs, xlim=c(0, 1), ylim=c(0, 1))
abline(0, 1)


# Datos de "verdad"
Hitters <- Hitters[complete.cases(Hitters),] # Dataset con jugadores de beisbol y sus
Hitters$Salary <- ifelse(Hitters$Salary > quantile(Hitters$Salary, 0.75), 1, 0)
# Vamos a predecir si el salario de un jugador está en el cuantil 75. 

# Escalamos las variables
X_hitters <- model.matrix(Salary ~ . - 1, data=Hitters)
X_hitters <- scale(X_hitters, center=TRUE, scale=TRUE)
y_hitters <- matrix(Hitters$Salary, ncol = 1)

coefs <- matrix(rnorm(ncol(X_hitters)), ncol = 1) # Tiramos unos coeficientes. 
bias <- rnorm(1)
trained_logistic <- train_gd_logistic(X_hitters, y_hitters, coefs, bias, learning_rate = 0.2)
preds_probs <- logistic_pred(X_hitters, trained_logistic$coefs, trained_logistic$bias)

logistic_R <- glm(data=Hitters, formula="Salary ~ .", family=binomial)
preds_probs_R <- matrix(predict(logistic_R, Hitters, type = "response"), ncol=1)

plot(preds_probs_R, preds_probs, xlim=c(0, 1), ylim=c(0, 1))
abline(0, 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Regresión logística regularizada: esto es para que el modelo no se ajuste tanto a los datos de training. Para evitar overfittins. 

# un beta1 grande tiende a sobreajustar, pues la probabilidad sube de golpe. 
# En LogLoss2, aumentar el lamda penaliza el costo. 
# Tenemos un trade off: betas 'grandes' ajustan bien los datos y logloss es chico, pero si los betas son 'grandes' hay riesgo de sobreajustar. 
# En LogLoss2, recompensamos que tan bien prediga pero castigamos en la medida que los betas sean diferente de ceros y sobreajustamos. 

# Si los coeficientes son diferentes de ceros, subir lamda sube LogLoss2. 

penalized_logloss <- function(y, preds_probs, coefs, lambda) {
  
  fit_loss <- -(sum(log(preds_probs[y==1])) + sum(log(1-preds_probs[y==0]))) / length(y)
  penalization_loss <- 0.5 * lambda * sum(coefs ^ 2)
  
  return(fit_loss + penalization_loss)
}

coefs <- matrix(rnorm(10), ncol=1)
bias <- rnorm(1)

preds_probs <- logistic_pred(X, coefs, bias)

logloss(y, preds_probs)
penalized_logloss(y, preds_probs, coefs, lambda=0)
penalized_logloss(y, preds_probs, coefs, lambda=0.5)
penalized_logloss(y, preds_probs, coefs, lambda=1)
penalized_logloss(y, preds_probs, coefs, lambda=10)

derivative_of_penalized_logloss <- function(X, y, preds_probs, coefs, lambda) {
  derivative_coefs <- (t(X) %*% (preds_probs - y)) / length(y) + lambda * coefs
  derivative_bias  <- mean(preds_probs - y)
  return(list(derivative_coefs = derivative_coefs, derivative_bias=derivative_bias))
}

train_gd_pen_logistic <- function(X, y, coefs, bias,
                                  learning_rate = 0.1,
                                  stopCriterion = 1e-09, verbose = TRUE,
                                  lambda = 0,
                                  print_every = 3000,
                                  max_iter = Inf) {
  
  n_param <- length(coefs) + 1
  end_while <- FALSE
  preds_probs <- logistic_pred(X, coefs, bias)
  
  i <- 1
  while (end_while == FALSE) {
    old_coefs <- coefs
    old_bias <- bias
    
    der_pen_logloss <- derivative_of_penalized_logloss(X, y, preds_probs, coefs, lambda)
    
    # Actualizo los coeficientes y el sesgo
    coefs <- old_coefs - learning_rate * der_pen_logloss$derivative_coefs
    bias <- old_bias - learning_rate * der_pen_logloss$derivative_bias
    
    # Veo las nuevas predicciones
    preds_probs <- logistic_pred(X, coefs, bias)
    
    # Imprimo el nuevo error
    if (verbose & !(i %% print_every)) {print(penalized_logloss(y, preds_probs, coefs, lambda))}
    
    # Veo si parar el while porque no se modifican los coeficientes
    if ((1 / n_param * sqrt(sum((old_coefs - coefs) ^ 2) + (old_bias - bias) ^ 2)) < stopCriterion) {
      end_while <- TRUE
    }
    
    # Veo si parar el while porque por iteraciones
    if (i == max_iter) {
      end_while <- TRUE
    }
    
    i <- i + 1
  }
  return(list(coefs=coefs, bias=bias))
}

coefs <- matrix(rnorm(ncol(X_hitters)), ncol = 1)
bias <- rnorm(1)
trained_logistic_L2 <- train_gd_pen_logistic(X_hitters, y_hitters, coefs, bias, lambda=0.01)
preds_probs_L2 <- logistic_pred(X_hitters, trained_logistic_L2$coefs, trained_logistic_L2$bias)

plot(preds_probs_R, preds_probs_L2, xlim=c(0, 1), ylim=c(0, 1))

trained_logistic_L2_R <- glmnet(X_hitters, y_hitters,
                                "binomial",
                                alpha = 0, lambda = 0.01)

preds_probs_L2_R <- predict(trained_logistic_L2_R, X_hitters, type = "response")

plot(preds_probs_L2_R, preds_probs_L2, xlim=c(0, 1), ylim=c(0, 1))
abline(0, 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~PROBLEMA DE CLASES~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Genero la bolsa de palabras

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

guia_oleo <- readRDS("guia_oleo.RDS")
X <- gen_bow_vars(guia_oleo$comment, "comment", 1, 15, FALSE)
y <- ifelse(guia_oleo$clase_comentario == "Malo", 1, 0)

## Entrenamos un modelo de regresión logística regularizado

# Separo en training y validation
valid_index <- sample(c(1:nrow(X)), size = 1300)
X_train <- X[-valid_index,]
y_train <- y[-valid_index]

X_valid <- X[valid_index,]
y_valid <- y[valid_index]

# Predecimos con nuestra implementación de regresión logística
trained_logistic <- glmnet(X_train, y_train, "binomial", alpha = 0)

get_auc <- function(real_class, predicted_prob) {
  pred_ROCR <- prediction(predicted_prob, real_class)
  auc_ROCR <- performance(pred_ROCR, measure = "auc")
  auc_ROCR <- auc_ROCR@y.values[[1]]
  return(auc_ROCR)
}

best_auc <- -Inf
for (s in trained_logistic$lambda) {
  s_preds <- predict(trained_logistic, X_valid, type = "response", s = s)
  lambda_auc <- get_auc(y_valid, s_preds[, 1])
  if (lambda_auc > best_auc) {
    best_auc <- lambda_auc
    best_lambda <- s
  }
}

print(best_auc)

# Tarea: entrenen un modelo LightGBM y vean qué performance
# tiene (no va a ser dramáticamente superior).