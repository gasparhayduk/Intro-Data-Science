rm(list=ls())

setwd("~/Desktop/IntroDS/Sesion08")
install.packages("xgboost")
library("xgboost")
library("ggplot2")
library("dplyr")
library("pROC")
install.packages("caret")
library("caret")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Fuente del dataset https://www.kaggle.com/jsphyg/weather-dataset-rattle-package

data_set <- read.table("weatherAUS.csv", sep=",", header=TRUE)

# Veamos características general del data_set
nrow(data_set) #cantidad de filas/observaciones
str(data_set) #qué tipo de datos contiene
head(data_set) #pequeña visualizacion del dataset
summary(data_set)  # Hay missings!

# Veamos cómo se distribuye la clase
prop.table(table(data_set$RainTomorrow)) #vamos a predecir estas clases
prop.table(table(RainTomorrow=data_set$RainTomorrow, RainToday=data_set$RainToday), 2)  # Si llovió hoy es más probable que llueva mañana
# si hoy llovio, la probabilidad de que llueva mañana es 0.46. 

# Pasamos algunas variables de fecha
data_set$Date <- as.Date(data_set$Date)
data_set$year <- as.numeric(format((data_set$Date), "%Y"))
data_set$month <- as.numeric(format((data_set$Date), "%m"))
data_set$Date <- as.numeric(data_set$Date)  # Para capturar alguna tendencia

# Hacemos algunos gráficos exploratorios

ggplot(data_set, aes(x=RainTomorrow, y=MaxTemp)) + geom_boxplot() # vemos como se distribuye la temperatura maxima cuando llueveop no mañana

ggplot(data_set %>% group_by(month, RainTomorrow) %>%
         summarize(MaxTemp=mean(MaxTemp, na.rm = TRUE)),
       aes(x=month, y=MaxTemp, color=RainTomorrow)) + geom_line()
# vemos como se distribuye la temperatura maxima por mes cuando llueve o no mañana. 

ggplot(data_set, aes(x=RainTomorrow, y=WindSpeed9am)) +
    geom_boxplot() + facet_wrap(~WindDir9am, scale="free_y")

# el viento puede predecir si va a llover mañana o no
ggplot(data_set %>% group_by(Location) %>%
           summarize(RainTomorrow = mean(RainTomorrow=="Yes")) %>%
           mutate(Location=reorder(Location, RainTomorrow)),
       aes(x=Location, y=RainTomorrow)) + 
    geom_bar(position=position_dodge(), stat="identity") + coord_flip()
# vemos como la proporcion de dias que llueve depende de la ciudad (geografia)


#tenemos variables para predecir si mañana llueve: si hoy llovió, la ciudad, el mes, la temperatura maxima, etc. 


# Hacemos una mini limpieza de variables
data_set$RainTomorrow <- ifelse(data_set$RainTomorrow=="Yes", 1, 0)  # Predice 0 ó 1.
data_set$RISK_MM <- NULL  # La página dice que es una variable a quitar si no se quiere hacer trampa

# Separamos un conjunto de entrenamiento, validación y otro de testeo
table(data_set$year)

train_set <-  data_set %>% filter(year < 2016)  # Dejo 2017 para testear
val_set <- data_set %>% filter(year == 2016)  # Dejo 2016 para validar
test_set <- data_set %>% filter(year == 2017)  # Dejo 2017 para testear

# Hacemos one-hot-encoding (solo tomos los valores que están en train)
one_hot_model <- dummyVars(~ ., data = train_set)

train_set <- predict(one_hot_model, train_set)  # Ojo, ya no son data.frames, son matrices
val_set <- predict(one_hot_model, val_set)
test_set <- predict(one_hot_model, test_set)
head(train_set)

# Pasamos los datos a formato de xgboost
dtrain <- xgb.DMatrix(data = train_set[,setdiff(colnames(train_set), "RainTomorrow")],
                      label = train_set[,"RainTomorrow"])

dvalid <- xgb.DMatrix(data = val_set[,setdiff(colnames(val_set), "RainTomorrow")],
                      label = val_set[,"RainTomorrow"]) #labes es la Y 

# Entrenamos un modelo de xgboost (como tarea prueben si rpart funcionaría)
model_1 <- xgb.train(data = dtrain,
                     params = list(max_depth = 5,  # Profundidad máxima
                                   eta = 0.1,  # Learnig rate
                                   gamma = 0.2,  # Reducción mínima requerida en el erro para aceptar la partición
                                   colsample_bytree = 0.9,  # Columnas que se muestrean al construir casa árbol
                                   subsample = 0.9,  # Observaciones que se muestrean al construir cada árbol
                                   min_child_weight = 1),  # Equivalente a cantidad de observaciones que debe tener una hoja para ser aceptada
                     nrounds = 100,  # Cantidad de árboles a entrenar, podemos ir cambiandolo. 
                     watchlist = list(train = dtrain, valid = dvalid),
                     objective = "binary:logistic", #predecir clasificacion binaria. ver en help qué poner en objective para regresion. 
                     eval.metric = "auc",
                     print_every_n = 10) #imprimir la performance cada 10 arboles. 

# Vemos la importancia de atributos
xgb.importance(model = model_1)

# Vemos cómo separa las clases la variable más importante
ggplot(data.frame(train_set) %>%
           mutate(RainTomorrow = RainTomorrow == 1),
       aes(x=Humidity3pm, fill = RainTomorrow)) +
    geom_density(alpha=0.2)

# Vemos cómo funciona en los datos de testeo
dtest <- xgb.DMatrix(data = test_set[,setdiff(colnames(test_set), "RainTomorrow")])

pred_probs <- predict(model_1, newdata = dtest)

auc(roc(test_set[,"RainTomorrow"], pred_probs))

#~ Ejercicios ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Ejercicio 1: respecto al punto anterior, mejora la performance (en validación) si los árboles
# tienen profundida 7 y se usan 700 árboles. Notan algún comportamiento extraño en cómo evoluciona
# auc en validación?

# Ejercicio 2: Busquen una buena combinación de max_depth y nrounds para este problema.

# Ejercicio 3: busquen las mejor combinación de max_depth, eta, gamma, colsample_bytree,
# subsample y nrounds para este problema.
