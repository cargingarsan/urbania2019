#https://xgboost.readthedocs.io/en/stable/R-package/discoverYourData.html

library(xgboost)

data(agaricus.train, package='xgboost')
data(agaricus.test, package='xgboost')
train <- agaricus.train
test <- agaricus.test

str(train$data)
dim(train$data)
str(test$data)
dim(test$data)

class(train$data)
class(train$label)

bstSparse <- xgboost(data = train$data, 
                     label = train$label, 
                     max.depth = 2, eta = 1, 
                     nthread = 2, 
                     nrounds = 2, 
                     objective = "binary:logistic")
## [0]	train-error:0.046522

bstDense <- xgboost(data = as.matrix(train$data), 
                    label = train$label,
                    max.depth = 2, 
                    eta = 1, 
                    nthread = 2, 
                    nrounds = 2, 
                    objective = "binary:logistic")


# verbose = 0, no message
dtrain <- xgb.DMatrix(data = train$data, 
                      label = train$label)

bst <- xgboost(data = dtrain, 
               max.depth = 2, 
               eta = 1, 
               nthread = 2, 
               nrounds = 2, 
               objective = "binary:logistic", 
               verbose = 0)


# verbose = 2, also print information about tree
bst <- xgboost(data = dtrain, 
               max.depth = 2, 
               eta = 1, 
               nthread = 2, 
               nrounds = 2, 
               objective = "binary:logistic", 
               verbose = 2)

#El propósito del modelo que hemos construido es clasificar nuevos datos. 
#Como se explicó anteriormente, utilizaremos el testconjunto de datos para este paso.

pred <- predict(bst, test$data)

# size of the prediction vector
print(length(pred))


## limit display of predictions to the first 10
print(head(pred))

prediction <- as.numeric(pred > 0.5)
print(head(prediction))


#Para medir el rendimiento del modelo, calcularemos una métrica simple, el error promedio .

err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))

#Lo más importante que hay que recordar es que para hacer una clasificación, 
#simplemente se hace una regresión label y luego se aplica un umbral .


#Preparación del conjunto de datos

dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)


watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, 
                 max.depth=2, 
                 eta=1, 
                 nthread = 2, 
                 nrounds=2, 
                 watchlist=watchlist, 
                 objective = "binary:logistic")



bst <- xgb.train(data=dtrain, 
                 max.depth=2, 
                 eta=1, 
                 nthread = 2, 
                 nrounds=2, 
                 watchlist=watchlist, 
                 eval.metric = "error", 
                 eval.metric = "logloss", 
                 objective = "binary:logistic")


bst <- xgb.train(data=dtrain, 
                 booster = "gblinear", 
                 nthread = 2, 
                 nrounds=2, 
                 watchlist=watchlist, 
                 eval.metric = "error", 
                 eval.metric = "logloss", 
                 objective = "binary:logistic")


xgb.DMatrix.save(dtrain, "dtrain.buffer")

#extraccion de informacion
label = getinfo(dtest, "label")
pred <- predict(bst, dtest)
err <- as.numeric(sum(as.integer(pred > 0.5) != label))/length(label)
print(paste("test-error=", err))

#La importancia de la característica es similar a la influencia relativa del paquete R gbm (rel.inf).

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#Puedes volcar el árbol que aprendiste a usar xgb.dumpen un archivo de texto.

xgb.dump(bst, with_stats = TRUE)

#Puedes trazar los árboles desde tu modelo usando ```xgb.plot.tree``

xgb.plot.tree(model = bst) # no funciona

# save model to binary local file, cuando el modelo es grande
xgb.save(bst, "xgboost.model")

#Una prueba interesante para ver qué tan idéntico es nuestro modelo guardado 
#al original sería comparar las dos predicciones.

# load binary model to R
bst2 <- xgb.load("xgboost.model")
pred2 <- predict(bst2, test$data)

# And now the test
print(paste("sum(abs(pred2-pred))=", sum(abs(pred2-pred))))

# save model to R's raw vector
rawVec <- xgb.save.raw(bst)

# print class
print(class(rawVec))

# load binary model to R
bst3 <- xgb.load(rawVec)
pred3 <- predict(bst3, test$data)

# pred3 should be identical to pred
print(paste("sum(abs(pred3-pred))=", sum(abs(pred3-pred))))


##########################

require(xgboost)
require(Matrix)
require(data.table)
if (!require('vcd')) {
  install.packages('vcd')
}

#Conversión de variables categóricas a numéricas

data(Arthritis)
df <- data.table(Arthritis, keep.rownames = FALSE)
head(df)

str(df)

head(df[, AgeDiscret := as.factor(round(Age / 10, 0))])

head(df[, AgeCat := as.factor(ifelse(Age > 30, "Old", "Young"))])

df[, ID := NULL]

levels(df[, Treatment])


sparse_matrix <- sparse.model.matrix(Improved ~ ., data = df)[, -1]
head(sparse_matrix)

output_vector <- df[, Improved] == "Marked"

bst <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
               eta = 1, nthread = 2, nrounds = 10, objective = "binary:logistic")

#Un valor pequeño para el error de entrenamiento puede ser un síntoma de sobreajuste,
#lo que significa que el modelo no predecirá con precisión valores invisibles.


#Cree la tabla de datos de importancia de características

importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
head(importance)

#Trazar la importancia de la característica

xgb.plot.importance(importance_matrix = importance)

#Un Chi2 más alto significa una mejor correlación.

c2 <- chisq.test(df$Age, output_vector)#correl. entre edad y desaparicon enfermedad
print(c2)

# correlac. AgeDiscret y desaparicion dela enferm (output_vector)
c2 <- chisq.test(df$AgeDiscret, output_vector)
print(c2)

c2 <- chisq.test(df$AgeCat, output_vector)
print(c2)
#una baja correlac. 2.36 es indicativo que la edad de corte en 30 años no es relevante.


#rf vs Xgboost
data(agaricus.train, package = 'xgboost')
data(agaricus.test, package = 'xgboost')
train <- agaricus.train
test <- agaricus.test

#Random Forest - 1000 trees
bst <- xgboost(
  data = train$data
  , label = train$label
  , max_depth = 4
  , num_parallel_tree = 1000
  , subsample = 0.5
  , colsample_bytree = 0.5
  , nrounds = 1
  , objective = "binary:logistic"
)

## [1]  train-logloss:0.456201

#Boosting - 3 rounds
bst <- xgboost(
  data = train$data
  , label = train$label
  , max_depth = 4
  , nrounds = 3
  , objective = "binary:logistic"
)



##########################
library(data.table)
library(xgboost)

if (!file.exists("./dermatology.data")) {
  download.file(
    "https://archive.ics.uci.edu/ml/machine-learning-databases/dermatology/dermatology.data",
    "dermatology.data",
    method = "curl"
  )
}

df <- fread("dermatology.data", sep = ",", header = FALSE)

df[, `:=`(V34 = as.integer(ifelse(V34 == "?", 0L, V34)),
          V35 = V35 - 1L)]

idx <- sample(nrow(df), size = round(0.7 * nrow(df)), replace = FALSE)

train <- df[idx, ]
test <- df[-idx, ]

train_x <- train[, 1:34]
train_y <- train[, V35]

test_x <- test[, 1:34]
test_y <- test[, V35]

xg_train <- xgb.DMatrix(data = as.matrix(train_x), label = train_y)
xg_test <- xgb.DMatrix(as.matrix(test_x), label = test_y)

params <- list(
  objective = 'multi:softmax',
  num_class = 6,
  max_depth = 6,
  nthread = 4,
  eta = 0.1
)

watchlist <- list(train = xg_train, test = xg_test)

bst <- xgb.train(
  params = params,
  data = xg_train,
  watchlist = watchlist,
  nrounds = 5
)

pred <- predict(bst, xg_test)
error_rate <- sum(pred != test_y) / length(test_y)
print(paste("Test error using softmax =", error_rate))

# do the same thing again, but output probabilities
params$objective <- 'multi:softprob'
bst <- xgb.train(params, xg_train, nrounds = 5, watchlist)

pred_prob <- predict(bst, xg_test)

pred_mat <- matrix(pred_prob, ncol = 6, byrow = TRUE)
# validation
# rowSums(pred_mat)

pred_label <- apply(pred_mat, 1, which.max) - 1L
error_rate <- sum(pred_label != test_y) / length(test_y)
print(paste("Test error using softprob =", error_rate))

#####################################


set.trainset.seed(1)
df <- winetaste
nobs <- nrow(df)
itrain <- sample(nobs, 0.8 * nobs)
train <- df[itrain, ]
test <- df[-itrain, ]




E:/proyectos/rstudio_git/ml/urbania2019/data/vino