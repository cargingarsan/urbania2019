#https://rstudio-pubs-static.s3.amazonaws.com/1137511_88738466546e481ab193a81de322f857.html

library(caret)
names(getModelInfo("xgb"))
modelLookup("xgbTree")


library(tidyverse)
library(corrplot)
library(plotly)

red <-read.csv("E:/proyectos/rstudio_git/ml/urbania2019/data/vino/winequality-red.csv", header = T,sep=";")
white <- read.csv("E:/proyectos/rstudio_git/ml/urbania2019/data/vino/winequality-white.csv", header = T,  sep=";")

red$type <- c("red")
white$type <- c("white")


dim(red); dim(white)
wine <- rbind(red, white)

wine <- wine[rowSums(is.na(wine)) != ncol(wine),]
wine <- wine[, colSums(is.na(wine)) != nrow(wine)]
dim(wine)
#[1] 6497   12

any(is.na(wine))

#Remove duplicate rows
wine1 <- wine |> distinct()
dim(wine1)
#[1] 5320  12

summary(wine1)

#Classes of wine1 dataset
map(wine1, class)

#A new binary column type_bin is created to differentiate red and white wines. 
#(“red - 1”, “white - 0”)

wine1$type_bin <- ifelse(wine1$type == "red", 1, 0)
head(wine1)

data <- wine1


#Visualization of all Variables-Histogram
numerical_vars <- c(
  "fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide", 
  "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol", "quality"
)

data <- read.csv("wine_cleaned.csv")
library(ggplot2)

# Define colors for 'red' and 'white' types
type_colors <- c("red" = "darkred", "white" = "skyblue")

for (var in numerical_vars) {
  print(
    ggplot(data, aes(x = get(var), fill = type)) +
      geom_histogram(binwidth = 1, color = "white") +
      scale_fill_manual(values = type_colors) +
      labs(title = paste("Histogram: ", var), x = var, y = "Count") + theme_minimal()
  )
}


#visualizacion barchart
# Visualization of Wine Quality by Wine Type - Bar Chart
#data <- read.csv("wine_cleaned.csv")
ggplot(data, aes(x = factor(quality), fill = factor(type))) + geom_bar(position = "dodge", alpha = 0.7) +
  labs(title = "Bar Chart of Wine Quality by Wine Type", x = "Quality", y = "Count") + theme_minimal()


# fixed acidity, alcohol and residual sugar determine the taste of the Wine 
numerical_vars <- c("fixed.acidity", "alcohol", "residual.sugar")
data_long <- tidyr::gather(data, key = "variable", value = "value", all_of(numerical_vars))
ggplot(data_long, aes(x = type, fill = variable, y = value)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_fill_manual(values = c("fixed.acidity" = "orange", "alcohol" = "darkblue", "residual.sugar" = "purple")) +
  facet_wrap(~variable, scales = "free_y", ncol = 1) +
  labs(title = "Grouped Bar Plot of Chemical Attributes that Determine Wine Taste",
       x = "Wine Type",
       y = "Variable Value") +
  theme_minimal()


#Visualization - Box Plot
# Bivariate Analysis
#data <- read.csv("wine_cleaned.csv")
ggplot(data, aes(x = type, y = citric.acid, fill = type)) +
  geom_boxplot(alpha = 1.0, color = "black") + 
  labs(title = "Distribution of Citric Acid by Wine Type", x = "Wine Type", y = "Citric Acid") +
  scale_fill_manual(values = c("red" = "darkred", "white" = "lightblue")) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

#Visualization - Scatter Plot
#Multivariate Analysis
#data <- read.csv("wine_cleaned.csv")
selected_vars <- c("fixed.acidity", "alcohol", "volatile.acidity", "sulphates", "quality", "type")
ggplot(data, aes(x = .data[[selected_vars[1]]], y = .data[[selected_vars[2]]], color = type)) +
  geom_point(alpha = 0.5) +
  labs(title = "Scatter Plot: Selected Variables by Wine Type", x = selected_vars[1], y = selected_vars[2]) +
  theme_minimal()


#correlacion entre varibles
correlation_matrix <- cor(data[, sapply(data, is.numeric)])

par(mar = c(6, 6, 6, 6))  
corrplot(correlation_matrix, method = "color", type = "upper", addCoef.col = "black", number.cex = 0.7)

#Model Building:

#Necessary libraries like “rpart”, “rattle”, “randomForest”, “xgboost”, “caret”, “e1071”, “caTools” are used.

library(caTools)
set.seed(123)
#wine quality is turned into nominal data
data$qualitytype<-ifelse(data$quality<6,'bad','good')
data$qualitytype[data$quality==6]<-'normal'
data$qualitytype<-as.factor(data$qualitytype)
ggplot(data=data)+geom_bar(mapping = aes(x=quality, fill = factor(quality)), stat = "count") +
  scale_fill_manual(values = c("3" = "red", "4" = "blue", "5" = "green", "6" = "orange", "7" = "purple", "8" = "pink"))+
  labs(x = "Quality", y = "Count") +
  theme_minimal()


library(rattle)
part<-sample.split(data$fixed.acidity,SplitRatio = 0.7)
train<-data[part,]
test<-data[!part,]
library(rpart) 
library(plotly) 
library(rpart.plot)

#Determine the essential physicochemical properties 
#Load necessary libraries
library(randomForest)
library(caret)

# Read the cleaned wine data
wine_data <- data
wine_data$quality <- as.factor(wine_data$quality)
# Split the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(wine_data$quality, p = 0.7, list = FALSE)
trainData <- wine_data[trainIndex, ]
testData <- wine_data[-trainIndex, ]
# Train the model
rfModel <- randomForest(quality ~ ., data = trainData, importance = TRUE, ntree = 500)
# View the model results
print(rfModel)

# Make predictions
predictions <- predict(rfModel, newdata = testData)
# Evaluate the model
confMatrix <- confusionMatrix(predictions, testData$quality)
print(confMatrix)

# Get the importance matrix
importanceMatrix <- importance(rfModel)
# Filter out 'type' and 'type_bin' from the importance matrix
varImportance <- data.frame(Variable = rownames(importanceMatrix), Importance = importanceMatrix[, "MeanDecreaseGini"])
varImportance <- varImportance[!varImportance$Variable %in% c("type", "type_bin", "qualitytype"), ]
# Visualize the importance (Using the correct importance measure)
ggplot(varImportance, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_minimal() +
  scale_fill_gradient(low = "Lavender", high = "Thistle") +
  xlab("Physicochemical Properties") +
  ylab("Essential") +
  ggtitle("Essential physicochemical properties using the Random Forest Model\n")

#To build regression models for predicting wine quality


#Random Forest for predicting the quality of wine:

# RANDOM FOREST - Regression

# Ensure the 'quality' column is treated as numeric
wine_data$quality <- as.numeric(wine_data$quality)

# Split the data into training and testing sets
set.seed(123) # Setting seed for reproducibility
indexes <- createDataPartition(wine_data$quality, p = 0.7, list = FALSE)
train_set <- wine_data[indexes, ]
test_set <- wine_data[-indexes, ]

# Train the Random Forest regression model
rf_model <- randomForest(quality ~ ., data = train_set)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test_set)

# Calculate RMSE
rmse <- sqrt(mean((test_set$quality - predictions)^2))

# Return the RMSE
rmse

#En el contexto de la regresión, el modelo destaca por realizar predicciones precisas 
#sobre la calidad del vino como variable continua. Su bajo RMSE indica un 
#error de predicción mínimo y un buen ajuste a los datos. El modelo RF supera 
#al de los modelos de regresión Decision Tree y XGBoost


# XGBoost-Regression

set.seed(123)

# Define predictors and responses
train_x <- data.matrix(train[,-c(12, 13, 15)])
train_y <- data.matrix(train[, 12])
test_x <- data.matrix(test[,-c(12, 13, 15)])
test_y <- data.matrix(test[, 12])

# Load the xgboost library
library(xgboost)

# Create DMatrix objects for training and testing data
train_xgb <- xgb.DMatrix(data = train_x, label = train_y)
test_xgb <- xgb.DMatrix(data = test_x, label = test_y)

# Train the XGBoost model
xgb <- xgb.train(
  data = train_xgb,
  max.depth = 3,
  watchlist = list(train = train_xgb, test = test_xgb),
  nrounds = 100,
  verbose = 0
)

# Determine the number of rounds with the minimum RMSE
xgb_final <- xgboost(data = train_xgb, max.depth = 3, nrounds = 88, verbose = 0)

# Make predictions
pred_xgb <- predict(xgb_final, newdata = test_xgb)

# Calculate Mean Squared Error (MSE)
mse <- mean((test_y - pred_xgb)^2)

# Calculate Mean Absolute Error (MAE)
mae <- caret::MAE(test_y, pred_xgb)

# Calculate Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((test_y - pred_xgb)^2))  
# Convert numeric predictions to a factor with thresholds 
pred_xgb_factor <- as.factor(ifelse(pred_xgb < 5, 'Low', ifelse(pred_xgb > 7, 'High', 'Medium')))

# Convert the actual numeric values to the same categorical levels for consistency
test_y_factor <- as.factor(ifelse(test_y < 5, 'Low', ifelse(test_y > 7, 'High', 'Medium')))

# Create a confusion matrix
conf_matrix_xgb <- confusionMatrix(pred_xgb_factor, test_y_factor)

# Output the confusion matrix and RMSE
print(conf_matrix_xgb)

print(paste("RMSE:", rmse)) 

#arbol de decisiones prediciendo la calidada del vino


wine_data$quality <- as.factor(wine_data$quality)

set.seed(123)
dtModel<-rpart(quality~.,data = train[,-c(14,15)])
dtModel

#visualize the decision tree
fancyRpartPlot(dtModel)

#model evaluation
pred_dtModel<-predict(dtModel,newdata = test)
#summary of the prediction vs summary of the real test data
summary(pred_dtModel)

summary(test$quality)

RMSE_dtModel<-sqrt(mean((pred_dtModel-test$quality)^2))
print("RMSE of the model")

#data frame with real response and the prediction
dat<-data.frame(test$quality,pred_dtModel)
head(dat)



#Tabla de comparación de modelos de regresión:

# RMSE for the Random Forest model
rmse_rf <- 0.287581

# RMSE for the Decision Tree model (Regression)
rmse_dt <- 0.7730765

# RMSE for the XGBoost model (Regression)
rmse_xgb <- 0.7085322

# Create a data frame with the model performance metrics based on RMSE
model_comparison_rmse <- data.frame(
  Model = c("Random Forest (Regression)", "XGBoost (Regression)", "Decision Tree (Regression)"),
  RMSE = c(rmse_rf, rmse_xgb, rmse_dt)
)

# 
knitr::kable(model_comparison_rmse, format = "pipe", caption = "Comparative Analysis of Models Based on RMSE")


#1. Bosque aleatorio (regresión):
#  RMSE = 0.287581
#Interpretación: El modelo de regresión Random Forest tiene un RMSE ligeramente 
#más bajo que los modelos de regresión XGBoost y Decision Tree. Esto muestra que 
#Random Forest realiza las predicciones más precisas con el menor error de predicción.




#Objetivo 3: Construir modelos de clasificación para predecir el tipo de vino.
#Árbol de decisión para predecir el tipo de vino:

#Decision Tree
#predict the type of wine

ctree1<-rpart(type_bin~.,data=train[,-c(13,15)])
ctree1
fancyRpartPlot(ctree1)

pred_ctree1<-predict(ctree1,newdata = test[,-13])
pred_ctree1<-ifelse(pred_ctree1>0.5,1,0)
#accuracy, specificity, precision ,etc of the model
conf_ctree1<-confusionMatrix(as.factor(test$type_bin),as.factor(pred_ctree1), mode = "everything")
conf_ctree1

preci<-precision(conf_ctree1$table)
paste("precision is",preci)
## [1] "precision is 0.983870967741935"
recal<-recall(conf_ctree1$table)
paste("recall is ",recal)
## [1] "recall is  0.967445742904841"
fscore<-2*preci*recal/(preci+recal)
paste("f1score is ",fscore)

#Esto sugiere que el modelo es capaz de identificar y clasificar consistentemente 
#tipos de vino en función de sus propiedades químicas.


#SVM
library(e1071)
train$type_bin <- as.factor(train$type_bin)
test$type_bin <- as.factor(test$type_bin)
#Fit the SVM model on the training data
svm_model <- svm(type_bin ~ ., data = train[,-c(12,13,15)], kernel = "radial")

#Predict the wine type on the test data
svm_predictions <- predict(svm_model, newdata = test[,-c(12,13,15)])
#Evaluate the model's performance
svm_conf_matrix <- confusionMatrix(svm_predictions, test$type_bin)
print(svm_conf_matrix)


# Extract precision, recall, and F1 score from the confusion matrix
precision_value <- svm_conf_matrix$byClass["Pos Pred Value"]
recall_value <- svm_conf_matrix$byClass["Sensitivity"]
f1_score <- 2 * (precision_value * recall_value) / (precision_value + recall_value)

# Print precision, recall, and F1 score
print(paste("Precision:", precision_value))
## [1] "Precision: 0.994067796610169"
print(paste("Recall:", recall_value))
## [1] "Recall: 0.995755517826825"
print(paste("F1 Score:", f1_score))
## [1] "F1 Score: 0.994910941475827"

#SVM logrando una impresionante tasa de precisión de más del 99,25 %



#KNN para predecir calidad de vinos 

library(class)
library(caret)

numeric_train_data <- train[, sapply(train, is.numeric)]
numeric_test_data <- test[, sapply(test, is.numeric)]

maxs <- apply(numeric_train_data, 2, max)
mins <- apply(numeric_train_data, 2, min)

scaled_train_data <- as.data.frame(scale(numeric_train_data, center = mins, scale = maxs - mins))

# Scaling the test data using the same scaling parameters
scaled_test_data <- as.data.frame(scale(numeric_test_data, center = mins, scale = maxs - mins))

# Train the k-NN model
set.seed(123) # for reproducibility
k <- 5 

# Train the model using scaled training data and original labels
knn_pred <- knn(train = scaled_train_data, test = scaled_test_data, cl = train$type_bin, k = k)

# Evaluate the model's performance
knn_conf_matrix <- confusionMatrix(knn_pred, test$type_bin)
knn_conf_matrix

# Output precision, recall, and F1 score
preci <- precision(knn_conf_matrix$table)
recal <- recall(knn_conf_matrix$table)
fscore <- 2 * preci * recal / (preci + recal)

# Print the performance metrics
print(paste("Precision is", preci))
## [1] "Precision is 0.987320371935757"
print(paste("Recall is", recal))
## [1] "Recall is 0.99151103565365"
print(paste("F1 score is", fscore))
## [1] "F1 score is 0.989411266412537"

#KNN es una opcion confiable para predecir y clasificar vinos


#Análisis comparativo de los modelos de clasificación:


# Performance metrics for Decision Tree
accuracy_dt <- "96.36%"
kappa_dt <- "0.9038"
sensitivity_dt <- "96.74%"
specificity_dt <- "95.18%"
ppv_dt <- "98.39%" 
npv_dt <- "90.58%" 
f1_dt <- "97.55%" 

#Performance metrics for SVM 
accuracy_svm <- "99.25%"
kappa_svm <- "0.9804"
sensitivity_svm <- "99.58%"
specificity_svm <- "98.31%"
ppv_svm <- "99.41%"
npv_svm <- "98.79%"
f1_svm <- "99.49%"

#Performance metrics for k-NN
accuracy_knn <- "98.43%"
kappa_knn <- "0.959"
sensitivity_knn <- "99.15%"
specificity_knn <- "96.38%"
ppv_knn <- "98.73%" # Positive Predictive Value is equivalent to Precision
npv_knn <- "97.56%" # Negative Predictive Value
f1_knn <- "98.94%"

# Create a data frame with the model performance metrics
model_comparison <- data.frame(
  Metric = c("Accuracy", "Kappa", "Sensitivity (Recall)", "Specificity", "Positive Predictive Value (Precision)", "Negative Predictive Value", "F1 Score"),
  `Model 1 (Decision Tree)` = c(accuracy_dt, kappa_dt, sensitivity_dt, specificity_dt, ppv_dt, npv_dt, f1_dt),
  `Model 2 (SVM)` = c(accuracy_svm, kappa_svm, sensitivity_svm, specificity_svm, ppv_svm, npv_svm, f1_svm),
  `Model 3 (k-NN)` = c(accuracy_knn, kappa_knn, sensitivity_knn, specificity_knn, ppv_knn, npv_knn, f1_knn)
)

#Comparison Table 
knitr::kable(model_comparison, format = "pipe", caption = "Comparative Analysis of Decision Tree Models, SVM, and k-NN")

