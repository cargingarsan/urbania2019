
library(tidymodels)  # metapackage of all the tidymodels packages
library(tidyverse)   # metapackage of all tidyverse packages
library(skimr)
library(caret)
library(corrplot)
library(randomForest)
library(ggplot2)
library(naniar)
library(dplyr)
library(knitr)
library(tidyr)

library(DataExplorer) # For automated data exploration
options(scipen = 999)

#######################

#random forest para hallar las variables importantes

# Ensuring that the dataset is correct and then fitting the Random Forest model
rf_model <- randomForest(SalePrice ~ ., data = new_train_data, ntree = 100, importance = TRUE)

# Extracting and displaying feature importance
importance <- importance(rf_model)
feature_importance <- data.frame(Feature = rownames(importance), Importance = importance[, '%IncMSE'])

# Displaying the most important features sorted by importance
top_features <- feature_importance |>
  arrange(desc(Importance)) |>
print()

#graficamos las 15 caracteristicas mas importantes

top_features |> head(15)|>
  ggplot(aes(x= reorder(Feature, Importance), y=Importance))+
  geom_col()+
  labs(x="Feature", y ="Importance(%IncMSE)")+
  coord_flip()+
  theme(legend.position = "none")

#el area construida habitable GrLivAreaes la var mas imoportante
#en las decisioiones de compra/venta de viviendas

#'GrLivArea' is the top feature 
ggplot(new_train_data, aes(x = GrLivArea, y = SalePrice)) + geom_point()

#existe una relacion directa entre ambas variables


#ingenieria de caracteristicas (crear nuevas vars)

#En este caso concreto se diseñan dos novedades: T#otalBathrooms y TotalSquareFeet.


new_train_data$TotalBathrooms <- new_train_data$FullBath + (new_train_data$HalfBath*0.5)+
 new_train_data$BsmtFullBath + (new_train_data$BsmtHalfBath*0.5)

new_test_data$TotalBathrooms <- new_test_data$FullBath + (new_test_data$HalfBath*0.5) +
 new_test_data$BsmtFullBath + (new_test_data$BsmtHalfBath*0.5)

new_train_data$TotalSquareFeet <- new_train_data$GrLivArea + new_train_data$TotalBsmtSF

new_test_data$TotalSquareFeet <- new_test_data$GrLivArea + new_test_data$TotalBsmtSF

ggplot(new_train_data, aes(x = TotalSquareFeet, y = SalePrice)) + geom_point()


#Correlation Analysis

#To understand the strength of these newly created features in 
#predicting SalePrice, correlation coefficients are calculated.

cor(new_train_data$SalePrice, new_train_data$TotalBathrooms, use= "pairwise.complete.obs")
#[1] 0.6317311

cor(new_train_data$SalePrice, new_train_data$TotalSquareFeet, use= "pairwise.complete.obs")
#[1] 0.7789588
#relacion directa, casas mas grandes se venden a precios mas altos

ggplot(new_train_data, aes(y = SalePrice)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Sale Prices")

#We are going to use the library outliers to remove the outliers from the SalePrice.

library(outliers)
# Detect outliers based on the interquartile range (IQR)
Q1 <- quantile(new_train_data$SalePrice, 0.25)
Q3 <- quantile(new_train_data$SalePrice, 0.75)
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR


# Identify indices of outliers
outlier_indices <- which(new_train_data$SalePrice < 
                           lower_bound | new_train_data$SalePrice > upper_bound)
outlier_indices

new_train_data <- new_train_data[-outlier_indices, ]
dim(new_train_data)

#despues de remover los valores atipicos, hacemos otros boxplot

ggplot(new_train_data, aes(y = SalePrice)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of Sale Prices")

#Asimetría en el precio de venta
qqnorm(new_train_data$SalePrice)
qqline(new_train_data$SalePrice)

#precio de venta no esta distribuido normalmente

#Modeloamos

library(recipes)
library(ranger)

# Definition of the recipe
data_recipe <- recipe(SalePrice ~ ., data = new_train_data) |>
  # Step to impute missing data using KNN
  step_impute_knn(all_predictors()) |>
  step_center(all_numeric(), -all_outcomes()) |>
  # Preparing the recipe with training data
  prep(training = new_train_data, retain = TRUE)

# Preparing the recipe with training data
prepared_recipe <- prep(data_recipe, training = new_train_data, retain = TRUE)

# Apply the recipe to the training data
train_baked <- bake(prepared_recipe, new_data = NULL)  # NULL implies original training data

# test_data is prepared similarly and needs the same transformations
test_baked <- bake(prepared_recipe, new_data = new_test_data)



train_baked$SalePrice <- log(train_baked$SalePrice + 1)

# Check the new distribution
ggplot(train_baked, aes(x = SalePrice)) +
  geom_histogram(bins = 30, fill = "green", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Log of SalePrice", x = "Log Sale Price", y = "Frequency")

# Calculate skewness of the transformed data
new_skewness <- moments::skewness(train_baked$SalePrice)
print(paste("Skewness of Log SalePrice:", new_skewness))



# Set up a Random Forest model specification
rf_spec <- rand_forest(trees = 1000) %>%
  set_mode("regression") %>%
  set_engine("ranger")

# Fit the model
rf_fit <- fit(rf_spec, SalePrice ~ ., data = train_baked)

# Predict on the test data
test_predictions_rf <- predict(rf_fit, test_baked, type = "numeric")


#modelo de regresion lineal

lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Fit the model
lm_fit <- fit(lm_spec, SalePrice ~ ., data = train_baked)

# Predict on the test data
test_predictions_lm <- predict(lm_fit, test_baked, type = "numeric")


#los valores estimados y los reales son graficados

rf_fit

broom::augment(rf_fit, new_data = train_baked) %>%
  ggplot( aes (x = SalePrice, y = `.pred`)) +
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) +
  labs(y = "Predicted values of SalePrice", x = "SalePrice (observed values)") +
  ggtitle('Predicted vs. Real Values - the random forest model')



lm_fit

glance(lm_fit) |>
  select(r.squared, adj.r.squared)

broom::augment (lm_fit, new_data = train_baked) %>%
  ggplot( aes (x = SalePrice, y = `.pred`)) +
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) +
  labs(y = "Predicted values of SalePrice", x = "SalePrice (observed values)") +
  ggtitle('Predicted vs. Real Values - the linear model')


#Gradient Boosting Machine (GBM) Model

library(xgboost)

# Set up a GBM model specification
gbm_spec <- boost_tree(
  trees = 1000,
  tree_depth = 6,
  min_n = 10,
  loss_reduction = 0.01,
  sample_size = 0.75
) |>
  set_mode("regression") |>
  set_engine("xgboost")

# Fit the model
gbm_fit <- fit(gbm_spec, SalePrice ~ ., data = train_baked)

# Predict on the test data
test_predictions_gbm <- predict(gbm_fit, test_baked, type = "numeric")

# View the model object
gbm_fit

#Shows predicted versus actual values for the GBM model.

broom::augment(gbm_fit, new_data = train_baked) %>%
  ggplot( aes (x = SalePrice, y = `.pred`)) +
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) +
  labs(y = "Predicted values of SalePrice", x = "SalePrice (observed values)") +
  ggtitle('Predicted vs. Real Values - the GBM model')

#HIPERTUNING¶
#Para realizar un ajuste de hiperparámetros simple para nuestros modelos 
#especificados (Random Forest y Gradient Boosting Machine (GBM) usando XGBoost), 
#podemos configurar una cuadrícula de ajuste básica para cada modelo y usar 
#validación cruzada para evaluar el rendimiento en diferentes conjuntos de parámetros.


library(dials)

# Define a recipe for preprocessing the data
tuning_data_recipe <- recipe(SalePrice ~ ., data = train_baked)|>
  step_dummy(all_nominal(), -all_outcomes()) |> # Convert all categorical variables
  step_normalize(all_numeric(), -all_outcomes())  # Normalize numeric variables

# Prepare the recipe
prepared_recipe <- prep(tuning_data_recipe)

# Random Forest Model Setup
rf_spec_param <- rand_forest(
  mtry = tune(),    # Hyper-parameter to be tuned
  trees = 1000,
  min_n = tune()    # Hyper-parameter to be tuned
) |>
  set_engine("ranger") |>
  set_mode("regression")
rf_spec_param

#Configuración del flujo de trabajo: un flujo de trabajo que combina el modelo y la receta.

wf_rf <- workflow() |>
  add_model(rf_spec_param) |>
  add_recipe(tuning_data_recipe)


set.seed(1234)
rf_grid <- dials::grid_random(
  finalize(mtry(), train_baked |> select (-SalePrice)),
  min_n(),  
  size = 50)  # the number should be larger, but it would take longer
rf_grid

#configurar la validacion cruzada

cv_folds <- vfold_cv(train_baked, v = 5, repeats = 1)

tuning_results <- tune_grid(
  wf_rf,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metric_set(rmse, rsq)
)

# Review tuning results
collect_metrics(tuning_results)


#imprimiendo en pantalla los resultados
# Collect and view tuning metrics
tuning_metrics <- collect_metrics(tuning_results)
print(tuning_metrics)

autoplot(tuning_results, metrics = "rmse")

#ahora identificamos los mejores hiperparametros para RMSE 

# Identify the best hyperparameters
best_params <- select_best(tuning_results)

# Review the best parameters
print(best_params)

# A tibble: 1 × 3
#mtry min_n .config              
#<int> <int> <chr>                
 # 1    54     3 Preprocessor1_Model30


#El flujo de trabajo se finaliza utilizando los mejores parámetros y luego 
#el modelo final se ajusta utilizando una parte reservada de los datos.

# Finalize the workflow with the best parameters
final_rf_model <- finalize_workflow(wf_rf, best_params)


# Create a final training and testing split
set.seed(1234)
split <- initial_split(train_baked, prop = 0.8)
train_data_hyper <- training(split)
test_data_hyper <- testing(split)


# Fit the final model using the full training data and evaluate on the test set
final_fit_results <- last_fit(final_rf_model, split = split)

#We extract and display RMSE and R-squared values to evaluate model performance.

# Extract the results
results_rf <- collect_metrics(final_fit_results)

# Results contain rows for each metric and columns like 'estimate'
rmse_value_rf <- results_rf |> filter(.metric == "rmse") |> pull(.estimate)
rsq_value_rf <- results_rf |> filter(.metric == "rsq") |> pull(.estimate)

# Print the RMSE and R^2
cat("RMSE on Test Set:", rmse_value_rf, "\n")
#RMSE on Test Set: 0.009730876 
cat("R^2 on Test Set:", rsq_value_rf, "\n")
#R^2 on Test Set: 0.8620958 

# Extract the fitted model from the final fit results
final_model_rf <- final_fit_results$.workflow[[1]]$fit$fit

#Then, we make predictions on the test_baked data and display them.

# Predict on the test data
test_predictions_tuned_rf <- predict(final_model_rf, new_data = test_baked, type = "numeric")
# View the first few predictions
head(test_predictions_tuned_rf)


#### el codigo presenta problemas  
####


# Predict on the test data
test_predictions_tuned_rf <- predict(final_model_rf, new_data = test_baked, type = "numeric")


#revertir la transformacion logaritmica
#aplicar funcion exponencial para revertirla

# Reverse log transformation because SalePrice was log-transformed
original_scale_predictions <- exp(test_predictions_tuned_rf$.pred) - 1

# View the predictions on the original scale
head(original_scale_predictions)


#Reverse log transformation because SalePrice was log-transformed
#original_scale_predictions <- exp(test_predictions_tuned_rf$.pred) - 1

# View the predictions on the original scale
head(original_scale_predictions)

str(original_scale_predictions)

#Then, we create a data.frame with the Id from our test dataset and the 
#predictions adjusted back to the original scale:
  
  # original_scale_predictions is a vector of predictions already in the correct scale
  submission_tuned_rf <- data.frame(
    Id = test_data$Id, 
    SalePrice = original_scale_predictions  # Directly use the vector of predictions
  )





# YPERTUNING XGB

library(dials)  # for setting up parameter objects
library(tune)   # for tuning methods

### XGBoost models
xgb_spec_tuning <- boost_tree(
  trees = 700, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune()                          ## step size
) |> 
  set_engine("xgboost") |> 
  set_mode("regression")

xgb_spec_tuning

#The workflow combines the XGBoost model specification with the preprocessing recipe.
#This allows the model to be fit on data that has been preprocessed consistently.

wf_xgb <- workflow() |>
  add_model(xgb_spec_tuning) |>
  add_recipe(prepared_recipe)

set.seed(1234)
xgb_grid <- dials::grid_random(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_baked |> select (-SalePrice)),
  learn_rate(),
  size = 100   
)
xgb_grid


cv_folds <- vfold_cv(train_baked, v = 5, repeats = 1)

tuning_results <- tune_grid(
  wf_xgb,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = metric_set(rmse, rsq)
)

# Review tuning results
collect_metrics(tuning_results)











