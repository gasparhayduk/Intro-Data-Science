rm(list=ls())

#~ Bloque: 01 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(e1071)
library(mlbench)

data(HouseVotes84, package = "mlbench")

# Separo en training y testing
set.seed(1234)
test_index <- sample(c(1:nrow(HouseVotes84)), 50)
train_data <- HouseVotes84[-test_index,]
test_data  <- HouseVotes84[test_index,]

# Entreno un modelo de bayes ingenuo sobre train
nb_classifier <- naiveBayes(Class ~ ., data = train_data)

# Veo cómo fue sobre los datos de testing
preds <- predict(nb_classifier, newdata = test_data)
table(predicted = preds, actual = test_data$Class)
print(mean(preds == test_data$Class))

# Cómo lo hizo sobre los datos con los que se entrenó el modelo
print(mean(predict(nb_classifier, newdata = train_data) == train_data$Class))

# Se puede pedir que se devuelvan las probabilidades estimadas
head(predict(nb_classifier, newdata = test_data, type = "raw"))

#~ Bloque: 02 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

discretize <- function(input_data, bins=10, type="equal_len") {
    # Función que dado un vector de input lo discretiza
    if (type == "equal_len") {
        cut_points <- seq(from=min(input_data), to=max(input_data), length.out=bins+1)
    } else if (type == "equal_freq") {
        cut_points <- unique(quantile(input_data, prob=seq(from=0, to=1, length.out=bins+1)))
    } else {
        return(NULL)
    }
    cut_points[1] <- -Inf
    cut_points[length(cut_points)] <- Inf
    return(cut(input_data, breaks = cut_points))
}

# Creamos datos sintéticos
x_sint <- abs(rnorm(10000, 5, 2))^2

# Mismo largo
discretize(x_sint, bins=15, type="equal_len")
table(discretize(x_sint, bins=15, type="equal_len"))

# Misma frecuencia
discretize(x_sint, bins=15, type="equal_freq")
table(discretize(x_sint, bins=15, type="equal_freq"))

# Ayuda para ejercicio al final

bankruptcy <- read.table("bankruptcy_data_red.txt", sep="\t", header=TRUE, stringsAsFactors=TRUE)

bankruptcy$class <- factor(ifelse(bankruptcy$class == TRUE, "yes", "no"))

bankruptcy_disc <- bankruptcy
for (v in colnames(bankruptcy_disc)) {
    if (is.numeric(bankruptcy_disc[[v]]) == TRUE) {
        bankruptcy_disc[[v]] <- discretize(bankruptcy_disc[[v]], bins=15, type="equal_freq")
    }
}
