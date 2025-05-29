rm(list=ls())
par(mfrow=c(1,1))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

set.seed(1234)

datos_lineales <- data.frame(x1=runif(400, min=-1, max=1),
                             x2=runif(400, min=-1, max=1))

X <- as.matrix(datos_lineales[,c("x1", "x2")])

a1 <- 10 / 100
b1 <- -1.5

datos_lineales$y1 <- as.numeric((a1 + b1 * datos_lineales$x1) > datos_lineales$x2)

plot(datos_lineales[,c("x1", "x2")],
     col=factor(datos_lineales$y1), pch=datos_lineales$y1 + 2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

sigmoid <- function(z) {
    return(1/(1 + exp(-z)))
}

logistic_pred <- function(X, coefs, bias) {
    return(sigmoid(X %*% coefs + bias))
}

logloss <- function(y, preds_probs) {
    return(-(sum(log(preds_probs[y==1])) + sum(log(1-preds_probs[y==0]))) / length(y))
}

derivative_of_logistic <- function(X, y, preds_probs) {
    derivative_coefs <- (t(X) %*% (preds_probs - y)) / length(y)
    derivative_bias  <- mean(preds_probs - y)
    return(list(derivative_coefs = derivative_coefs, derivative_bias=derivative_bias))
}

plot_logistic_frontier <- function(coefs, bias) {
    a <- -bias/coefs[2]
    b <- -coefs[1] / coefs[2]
    abline(a, b, col="blue")
}

train_gd_logistic_with_plot <- function(X, y, coefs, bias, learning_rate = 0.1,
                                        stopCriterion = 0.000001, verbose = TRUE,
                                        print_every = 3000) {

    end_while <- FALSE
    preds_probs <- logistic_pred(X, coefs, bias)

    i <- 1
    while (!end_while) {
        old_coefs <- coefs
        old_bias <- bias
        
        der_logistic <- derivative_of_logistic(X, y, preds_probs)

        # Actualizo los coeficientes y el sesgo
        coefs <- coefs - learning_rate * der_logistic$derivative_coefs
        bias <- bias - learning_rate * der_logistic$derivative_bias

        # Veo las nuevas predicciones
        preds_probs <- logistic_pred(X, coefs, bias)

        # Imprimo el nuevo error
        if (verbose & !(i %% print_every)) {
            print(logloss(y, preds_probs))
            plot(X, col=factor(y), pch=y + 2, xlim=c(-1, 1), ylim=c(-1, 1))
            plot_logistic_frontier(coefs, bias)
        }

        # Veo si parar el while
        if (sqrt(sum((old_coefs - coefs) ^ 2) + (old_bias - bias) ^ 2) < stopCriterion) {
            end_while <- TRUE
        }
        i <- i + 1
    }
    return(list(coefs=coefs, bias=bias))
}


coefs <- matrix(rnorm(ncol(datos_lineales[,c("x1", "x2")])), ncol = 1)
bias <- rnorm(1)

trained_logistic <- train_gd_logistic_with_plot(X,
                                                datos_lineales$y1,
                                                coefs, bias,
                                                learning_rate = 0.01,
                                                print_every=1000,
                                                stopCriterion = 0.00005)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

a2 <- 15 / 100
b2 <- -0.5

datos_lineales$y2 <- as.numeric((a2 + b2 * datos_lineales$x1) > datos_lineales$x2)

datos_lineales$y3 <- ifelse(!datos_lineales$y1 & datos_lineales$y2, 1, 0)

plot(datos_lineales[,c("x1", "x2")], col=factor(datos_lineales$y3),
     pch=datos_lineales$y3 + 2)

one_hidden_layer_pred <- function(X, coefs1, bias1, coefs2, bias2) {

    n_obs <- nrow(X)

    # First layer
    n_hidden <- nrow(coefs1)
    z1 <- coefs1 %*% t(X) + matrix(rep(bias1, n_obs), nrow = n_hidden)
    a1 <- sigmoid(z1)

    # Second layer
    n_out <- nrow(coefs2)
    z2 <- coefs2 %*% a1 + matrix(rep(bias2, n_obs), nrow = n_out)
    preds_probs <- t(sigmoid(z2)) 

    return(list(preds_probs = preds_probs,
                z1 = z1,
                a1 = a1))
}

logloss_multiclass <- function(y, preds_probs) {
    return(-sum(log(rowSums(preds_probs * y))) / nrow(y))
}

derivative_of_sigmoid <- function(z) {
    return(sigmoid(z) * (1 - sigmoid(z)))
}

derivative_of_one_hidden_layer <- function(X, y, preds_probs, coefs2, a1, z1) {

    n_obs <- nrow(X)
    n_out <- ncol(preds_probs)

    # Derivative of output layer
    dz2 <- t(preds_probs) - t(y)
    derivative_coefs2 <- dz2 %*% t(a1) / n_obs
    derivative_bias2  <- matrix(rowMeans(t(preds_probs) - t(y)), ncol=1)

    # Derivative of hidden layer
    dz1 <- t(coefs2) %*% dz2 * derivative_of_sigmoid(z1)
    derivative_coefs1 <- dz1 %*% X / n_obs
    derivative_bias1 <- matrix(rowMeans(dz1), ncol = 1)

    return(list(derivative_coefs1 = derivative_coefs1,
                derivative_bias1 = derivative_bias1,
                derivative_coefs2 = derivative_coefs2,
                derivative_bias2 = derivative_bias2))
}


train_gd_one_hidden <- function(X, y, coefs1, bias1, coefs2, bias2,
                                learning_rate = 0.1,
                                stopCriterion = 0.000001, verbose = TRUE,
                                print_every = 3000,
                                twoPlots = FALSE,
                                multiclass = FALSE,
                                savePlot = FALSE,
                                noPlot = FALSE,
                                maxEpochs = NULL) {

    if (twoPlots) {par(mfrow=c(1,2))} else {par(mfrow=c(1,1))}

    end_while <- FALSE
    preds_and_cache <- one_hidden_layer_pred(X, coefs1, bias1, coefs2, bias2)

    i <- 1
    j <- 1

    while (!end_while) {
        old_coefs1 <- coefs1
        old_bias1 <- bias1
        old_coefs2 <- coefs2
        old_bias2 <- bias2

        der_one_hidden <- derivative_of_one_hidden_layer(X,
                                                         y,
                                                         preds_and_cache$preds_probs,
                                                         coefs2,
                                                         preds_and_cache$a1,
                                                         preds_and_cache$z1)

        # Actualizo los coeficientes y el sesgo
        coefs1 <- coefs1 - learning_rate * der_one_hidden$derivative_coefs1
        bias1 <- bias1 - learning_rate * der_one_hidden$derivative_bias1
        coefs2 <- coefs2 - learning_rate * der_one_hidden$derivative_coefs2
        bias2 <- bias2 - learning_rate * der_one_hidden$derivative_bias2

        # Veo las nuevas predicciones
        preds_and_cache <- one_hidden_layer_pred(X, coefs1, bias1, coefs2, bias2)

        # Imprimo el nuevo error
        if (verbose & !(i %% print_every)) {

            if (!multiclass) {
                ll <- logloss(y, preds_and_cache$preds_probs)
            } else {
                ll <- logloss_multiclass(y, preds_and_cache$preds_probs)
            }
            print(ll)

            if (!noPlot) {
                if (savePlot) {
                    jpeg(paste('nnet_gif/one_hidden_', sprintf("%06d", j), '.jpg', sep=""),
                         width = 480*2, height = 480)
                    j <- j + 1
                }
                if (twoPlots) {
                    par(mfrow=c(1,2))} else {par(mfrow=c(1,1))
                }
                if (!multiclass) {
                    plot(X, col=factor(y), xlim=c(-1, 1), ylim=c(-1, 1), pch=y + 2)
                } else {
                    y_plot <- apply(y, 1, function(x) which(x==1))-1
                    plot(X, col=factor(y_plot), xlim=c(-1, 1), ylim=c(-1, 1), pch=y_plot + 2)
                }

                # Plot of features
                for (k in c(1:nrow(coefs1))) {
                    plot_logistic_frontier(coefs1[k,], bias1[k])
                }

                # Plot if hidden activation space
                if (twoPlots) {
                    plot(t(preds_and_cache$a1), col=factor(y), pch=y + 2, xlim=c(0, 1),
                         ylim=c(0, 1), xlab="a1", ylab="a2")
                    for (k in c(1:nrow(coefs2))) {
                        plot_logistic_frontier(coefs2[k,], bias2[k])
                    }
                }
                if (savePlot) {
                    dev.off()
                    if (ll < 0.005) {return()}
                }
            }
        }

        # Veo si parar el while
        if (sqrt(sum((old_coefs1 - coefs1) ^ 2) + sum((old_bias1 - bias1) ^ 2) +
                 sum((old_coefs2 - coefs2) ^ 2) + sum((old_bias2 - bias2) ^ 2)) < stopCriterion) {
            end_while <- TRUE
        } else if (!is.null(maxEpochs)) {
            if (i == maxEpochs) {
                end_while <- TRUE
            }
        }
        i <- i + 1
    }

    par(mfrow=c(1,1))

    return(list(coefs1=coefs1, bias1=bias1,
                coefs2=coefs2, bias2=bias2))
}

y <- matrix(datos_lineales[,"y3"], ncol=1)
n_hidden <- 2
n_vars <- 2
n_out <- 1

coefs1 <- matrix(3 * rnorm(n_vars * n_hidden), nrow = n_hidden) * 0.3
bias1 <- matrix(3 * rnorm(n_hidden), ncol = 1) * 0.1

coefs2 <- matrix(3 * rnorm(n_hidden * n_out), nrow = n_out) * 0.3
bias2 <- matrix(3 * rnorm(n_out), ncol = 1) * 0.1

train_gd_one_hidden(X, y, coefs1, bias1, coefs2, bias2, learning_rate = 0.6,
                    stopCriterion = 0.00001, verbose = TRUE, print_every = 200, twoPlots=TRUE)
# Esto lo que hace es encontrar dos lineas que dividen mejor a los datos. Redes neuronales con dos capas. 
# Lo que hace redes neuronales es apilar varias regresiones logisticas. 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

datos_lineales$y4 <- as.numeric(apply(datos_lineales[,c("x1", "x2")], 1,
                                function(x) sqrt(sum(x^2))) < 0.55)

plot(datos_lineales[,c("x1", "x2")], col=factor(datos_lineales$y4),
     pch=datos_lineales$y4 + 2) # Vemos que los puntos se dividen en una especie de triangulo 

y <- matrix(datos_lineales[,"y4"], ncol=1)

n_hidden <- 5
n_vars <- 2
n_out <- 1

coefs1 <- matrix(3 * rnorm(n_vars * n_hidden), nrow = n_hidden) * 0.3
bias1 <- matrix(3 * rnorm(n_hidden), ncol = 1) * 0.1

coefs2 <- matrix(rnorm(n_hidden * n_out), nrow = n_out) * 0.3
bias2 <- matrix(rnorm(n_out), ncol = 1) * 0.1

train_gd_one_hidden(X, y, coefs1, bias1, coefs2, bias2, learning_rate = 0.4,
                    stopCriterion = 0.000001, verbose = TRUE, print_every = 300)
# Aca armamos a1, a2 y a3 que separan mejor a los datos. 
# Pagina 14: cuantos parametros tiene el modelo? 13 parametros. 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Lo mismo que antes pero dividimos en una especie de pentagono, agregamos mas lineas
# A mayores lineas, se adapta mejor a los datos y quizas tenemos overfitting. 


# En lo siguiente tenemos tres clases a predecir, multiclase (pagina 16)
# Esto tiene 21  parametros
a3 <- 120 / 100
b3 <- 1.5

datos_lineales$y5 <- as.numeric((a3 + b3 * datos_lineales$x1) > datos_lineales$x2)
datos_lineales$y6 <- ifelse(!datos_lineales$y5, 0,
                            ifelse(!datos_lineales$y1 & datos_lineales$y2, 1, 2))

plot(datos_lineales[,c("x1", "x2")], col=factor(datos_lineales$y6),
     pch=datos_lineales$y6 + 2)

n_hidden <- 3 # 3 neuronas
n_vars <- 2 # 2 variables x
n_out <- 3 # 3 clases a predecir

# A esto le agregm=amos la constante en las x, una constante en las neuronas y tenemos todos los parametros. 

coefs1 <- matrix(3 * rnorm(n_vars * n_hidden), nrow = n_hidden) * 0.5
bias1 <- matrix(3 * rnorm(n_hidden), ncol = 1) * 0.1

coefs2 <- matrix(3 * rnorm(n_hidden * n_out), nrow = n_out) * 0.5
bias2 <- matrix(3 * rnorm(n_out), ncol = 1) * 0.1

y <- matrix(t(sapply(datos_lineales$y6 + 1, function(x) as.numeric(seq(1, 3)==x))), ncol=n_out)

train_gd_one_hidden(X, y, coefs1, bias1, coefs2, bias2, learning_rate = 0.5,
                    stopCriterion = 0.000001, verbose = TRUE, print_every = 300, multiclass=TRUE)

# Pagina 14: cuantos parametros tiene el modelo? 13 parametros. 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Predicción en iris

plot(iris[,c(1:4)], pch=as.numeric(iris$Species), col=iris$Species)

valid_index <- sample(c(1:nrow(iris)), 50)

iris_shuffled <- iris[sample(c(1:nrow(iris))),]
scaled_iris_X <- scale(iris_shuffled[,-5], center=TRUE, scale=TRUE)

train_X <- as.matrix(scaled_iris_X[-valid_index,])
valid_X <- as.matrix(scaled_iris_X[valid_index,])

iris_y <- matrix(t(sapply(as.numeric(iris_shuffled$Species),
                          function(x) as.numeric(seq(1, 3)==x))), ncol=n_out)

train_y <- iris_y[-valid_index,]
valid_y <- iris_y[valid_index,]

n_hidden <- 10 #neuronas
n_vars <- 4 #las x
n_out <- 3 # clases a predecir. 

coefs1 <- matrix(rnorm(n_vars * n_hidden), nrow = n_hidden) * 0.5
bias1 <- matrix(rnorm(n_hidden), ncol = 1) * 0.1

coefs2 <- matrix(rnorm(n_hidden * n_out), nrow = n_out) * 0.5
bias2 <- matrix(rnorm(n_out), ncol = 1) * 0.1

iris_trained <- train_gd_one_hidden(train_X, train_y, coefs1, bias1, coefs2, bias2,
                                    learning_rate = 0.4, stopCriterion = 0.00001,
                                    verbose = TRUE, print_every = 300, multiclass=TRUE,
                                    noPlot=TRUE, maxEpochs = 80000)

# Predicciones en validación
y_pred <- one_hidden_layer_pred(valid_X, iris_trained$coefs1, iris_trained$bias1,
                                iris_trained$coefs2, iris_trained$bias2)$preds_probs

# Accuracy en validación
mean(apply(y_pred, 1, which.max) == apply(valid_y, 1, function(x) which(x==1)))

# Para poder hacer esto, necesitamos una funcion de costos que sea derivable en cada uno de los parametros. 



# Pagina 18:
# Para ver la cantidad de parametros: la respuesta es 210. 
