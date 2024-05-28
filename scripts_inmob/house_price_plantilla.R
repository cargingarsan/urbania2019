#https://www.kaggle.com/code/cginogaribottos/house-prices-eda-lm-rf-xgb/edit

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


list.files(path = " ../data/input_data")

train <- read_csv('E:/proyectos/rstudio_git/ml/urbania2019/data/input_data/train.csv')
test <- read_csv('E:/proyectos/rstudio_git/ml/urbania2019/data/input_data/test.csv')

skimr::skim(train)


set.seed(1234)

glimpse(train)
train_data <- train


set.seed(1234)

glimpse(test)
test_data <-test

#Ahora comprobaremos qué columnas tienen valores faltantes tanto en train_data como en test_data.

NAcol <- which(colSums(is.na(train_data)) > 0)
sort(colSums(sapply(train_data[NAcol], is.na)), decreasing = TRUE)

valores_faltantes <- map_dbl(train_data, ~sum(is.na(.))) |>
  sort(decreasing = TRUE) 

#Esto selecciona las columnas de train_data que tienen al menos un valor NA, y luego 
#calcula la cantidad de valores NA en esas columnas, ordenándolos en orden descendente.

NAcol <- which(colSums(is.na(train_data)) > 0)
sort(colSums(sapply(train_data[NAcol], is.na)), decreasing = TRUE)

NAcol_test <- which(colSums(is.na(test_data)) > 0)
sort(colSums(sapply(test_data[NAcol], is.na)), decreasing = TRUE)


#Como podemos ver hasta ahora, existen algunas diferencias entre el número total 
#de valores faltantes en train_data y test_data.

#Usaremos GG para trazar los valores faltantes para poder verlos mejor.

gg_miss_var(train_data) #funcion del paquete naniar

gg_miss_var(test_data)


#vemos que hay muchas columnas con valores perdidos en ambos datasets

cat('There are', length(NAcol), 'columns with missing values')


plot_missing(train_data) #dataExplorer

plot_missing(test_data) #dataExplorer

#Conociendo las proporciones de los valores faltantes, vayamos un poco más allá 
#y exploremos la distribución de SalePrice en train_data usando un ggplot para 
#comprender su asimetría e identificar posibles valores atípicos.

ggplot(train_data, aes(x = SalePrice)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Sale Prices", x = "Sale Price", y = "Frequency")
#como se puede ver, hay dos casas con el recio que supera los 600 mil $


# Numeric variable distributions
plot_histogram(train_data)

# Categorical variable analysis
plot_bar(train_data)

#A partir de los gráficos, identificamos que la mayoría de los NA no son 
#realmente valores faltantes, pero en la mayoría de los casos significa "Ninguno"


cols <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", "GarageType", 
          "GarageFinish", "GarageQual", "GarageCond", "BsmtExposure", "BsmtFinType1", 
          "BsmtFinType2", "BsmtQual", "BsmtCond", "MasVnrType")

train_data <- train_data |>
  mutate(across(all_of(cols), ~replace_na(., "None")))

#lo mismo con test_data
cols <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", "GarageType", 
          "GarageFinish", "GarageQual", "GarageCond", "BsmtExposure", "BsmtFinType1", 
          "BsmtFinType2", "BsmtQual", "BsmtCond", "MasVnrType")

test_data <- test_data |>
  mutate(across(all_of(cols), ~replace_na(., "None")))

sum(is.na(train_data))

sum(is.na(test_data))

#datos de prueba 342 tiene menos valores perdidos que datos de entrenamiento 342


#vamos a trazar la proporción de ellos nuevamente, para que podamos decidir qué 
#tipo de imputación elegir para estos valores faltantes.


plot_missing(train_data)

plot_missing(test_data)


NAcol <- which(colSums(is.na(train_data)) > 0)
sort(colSums(sapply(train_data[NAcol], is.na)), decreasing = TRUE)

NAcol <- which(colSums(is.na(test_data)) > 0)
sort(colSums(sapply(test_data[NAcol], is.na)), decreasing = TRUE)

str(train_data)
summary(train_data)

str(test_data)
summary(test_data)


#Formato y categorización de datos¶
#Conversión a factores y factores ordenados

#Variables nominales (sin orden): variables como MSZoning, Street y Neighborhood 
#representan categorías sin ningún orden inherente. Los convertiremos en factores 
#para garantizar que los algoritmos de modelado los traten como categorías 
#distintas en lugar de números con magnitudes.

#Variables ordinales (el orden importa): variables como ExterQual, BsmtQual y 
#HeatingQC poseen un orden significativo en sus categorías. Los convertiremos en 
#factores ordenados para preservar este orden.


#Calidad de la piscina

#PoolQC: Calidad del grupo Ex Excelente Gd Bueno TA Promedio/típico 
#Fa Aceptable NA Sin grupo

#Convertido a un factor ordenado: aplica el orden: especifica la clasificación 
#de calidad inherente a los datos ("Ex" es mejor que "Gd", etc.)


cols <- c("MSZoning", "MiscFeature", "Alley", "Street", "LandContour", "Utilities", 
          "LotConfig", "Neighborhood", "BldgType", "RoofStyle", "RoofMatl", 
          "Exterior1st", "Exterior2nd", "Foundation", "Heating", "CentralAir", 
          "GarageType", "SaleType", "SaleCondition", "Fence", "Condition1", "Condition2")

train_data <- train_data %>%
  mutate(across(all_of(cols), as.factor)) |> as_data_frame()

#lo mismo con test_data

cols <- c("MSZoning", "MiscFeature", "Alley", "Street", "LandContour", "Utilities", 
          "LotConfig", "Neighborhood", "BldgType", "RoofStyle", "RoofMatl", 
          "Exterior1st", "Exterior2nd", "Foundation", "Heating", "CentralAir", 
          "GarageType", "SaleType", "SaleCondition", "Fence", "Condition1", "Condition2")

test_data <- test_data %>%
  mutate(across(all_of(cols), as.factor)) |> as_data_frame()

#Realizamos la conversión tanto en train_data como en test_data para mantener la 
#coherencia para el modelado posterior. Veamos cómo se ve ahora la estructura 
#train_data con nuestros factores.

# Define las columnas y los niveles correspondientes
cols_levels <- list(
  PoolQC = c("None", "Fa", "TA", "Gd", "Ex"),
  LotShape = c("Reg", "IR1", "IR2", "IR3"),
  LandSlope = c("Gtl", "Mod", "Sev"),
  HouseStyle = c("1Story", "1.5Unf", "1.5Fin", "2Story", "2.5Unf", "2.5Fin", "SFoyer", "SLvl"),
  MasVnrType = c("None", "CBlock", "BrkCmn", "BrkFace", "Stone"),
  ExterQual = c("Po", "Fa", "TA", "Gd", "Ex"),
  ExterCond = c("Po", "Fa", "TA", "Gd", "Ex"),
  BsmtQual = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  BsmtCond = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  BsmtExposure = c("None", "No", "Mn", "Av", "Gd"),
  BsmtFinType1 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  BsmtFinType2 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  HeatingQC = c("Po", "Fa", "TA", "Gd", "Ex"),
  Electrical = c("FuseP", "FuseF", "FuseA", "SBrkr", "Mix"),
  KitchenQual = c("Po", "Fa", "TA", "Gd", "Ex"),
  Functional = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"),
  FireplaceQu = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  GarageFinish = c("None", "Unf", "RFn", "Fin"),
  GarageQual = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  GarageCond = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  PavedDrive = c("N", "P", "Y")
)

#cols_levels <- as.data.frame(cols_levels)

# Verifica si las columnas existen en los datos
#cols_exist <- names(cols_levels) %in% names(train_data)
cols_levels <- gsub("`", "", cols_levels)

# Aplica la función ordered a cada columna con sus niveles correspondientes
train_data <- train_data %>%
  mutate(across(names(cols_levels), ~ordered(., levels = cols_levels[[.]])))

# Define las columnas y los niveles correspondientes
cols_levels <- list(
  PoolQC = c("None", "Fa", "TA", "Gd", "Ex"),
  LotShape = c("Reg", "IR1", "IR2", "IR3"),
  LandSlope = c("Gtl", "Mod", "Sev"),
  HouseStyle = c("1Story", "1.5Unf", "1.5Fin", "2Story", "2.5Unf", "2.5Fin", "SFoyer", "SLvl"),
  MasVnrType = c("None", "CBlock", "BrkCmn", "BrkFace", "Stone"),
  ExterQual = c("Po", "Fa", "TA", "Gd", "Ex"),
  ExterCond = c("Po", "Fa", "TA", "Gd", "Ex"),
  BsmtQual = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  BsmtCond = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  BsmtExposure = c("None", "No", "Mn", "Av", "Gd"),
  BsmtFinType1 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  BsmtFinType2 = c("None", "Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"),
  HeatingQC = c("Po", "Fa", "TA", "Gd", "Ex"),
  Electrical = c("FuseP", "FuseF", "FuseA", "SBrkr", "Mix"),
  KitchenQual = c("Po", "Fa", "TA", "Gd", "Ex"),
  Functional = c("Sal", "Sev", "Maj2", "Maj1", "Mod", "Min2", "Min1", "Typ"),
  FireplaceQu = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  GarageFinish = c("None", "Unf", "RFn", "Fin"),
  GarageQual = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  GarageCond = c("None", "Po", "Fa", "TA", "Gd", "Ex"),
  PavedDrive = c("N", "P", "Y")
)

cols_levels <- gsub("`", "", cols_levels)

# Aplica la función ordered a cada columna con sus niveles correspondientes
test_data <- test_data %>%
  mutate(across(names(cols_levels), ~ordered(., levels = cols_levels[[.]])))

#En este código, across es una función de dplyr que aplica una función a múltiples 
#columnas. names(cols_levels) selecciona todas las columnas en la lista
#cols_levels, y ~ordered(., levels = cols_levels[[.]])) es la función que se 
#aplica a cada una de esas columnas

str(train_data)

#Volviendo a los valores faltantes, arreglémoslos tanto en train_data como en test_data.

NAcol <- which(colSums(is.na(train_data)) > 0)
sort(colSums(sapply(train_data[NAcol], is.na)), decreasing = TRUE)


#2. MasVnrArea

#Dado que MasVnrArea (área de revestimiento de mampostería en pies cuadrados) 
#es numérico y le faltan algunos valores, estos podrían imputarse mediante una 
#medida de tendencia central.

# Imputation of MasVnrArea
train_data$MasVnrArea[is.na(train_data$MasVnrArea) & train_data$MasVnrType == "None"] <- 0
train_data$MasVnrArea[is.na(train_data$MasVnrArea)] <- median(train_data$MasVnrArea, na.rm = TRUE)
# Imputation of MasVnrArea
test_data$MasVnrArea[is.na(test_data$MasVnrArea) & test_data$MasVnrType == "None"] <- 0
test_data$MasVnrArea[is.na(test_data$MasVnrArea)] <- median(test_data$MasVnrArea, na.rm = TRUE)







NAcol <- which(colSums(is.na(test_data)) > 0)
sort(colSums(sapply(test_data[NAcol], is.na)), decreasing = TRUE)


#1. Frente al lote

#El número de valores faltantes aquí es relativamente alto. Dado que el LotFrontage 
#(los pies lineales de la calle conectada a la propiedad) puede variar según el 
#vecindario, una estrategia común es imputar los valores faltantes en función 
#del LotFrontage mediana del vecindario.

train_data <- train_data |>
  group_by(Neighborhood) |>
  mutate(LotFrontage = ifelse(is.na(LotFrontage), median(LotFrontage, na.rm = TRUE), LotFrontage)) |>
  ungroup()


test_data <- test_data |>
  group_by(Neighborhood) |>
  mutate(LotFrontage = ifelse(is.na(LotFrontage), median(LotFrontage, na.rm = TRUE), LotFrontage)) |>
  ungroup()


impute_MasVnrArea <- function(data) {
  data$MasVnrArea[is.na(data$MasVnrArea) & data$MasVnrType == "None"] <- 0
  data$MasVnrArea[is.na(data$MasVnrArea)] <- median(data$MasVnrArea, na.rm = TRUE)
  return(data)
}

# Aplicar la función a los conjuntos de datos
train_data <- impute_MasVnrArea(train_data)
test_data <- impute_MasVnrArea(test_data)

#Código generado por IA. Revisar y usar cuidadosamente. Más información sobre 
#preguntas frecuentes.
#Este código hace exactamente lo mismo que el tuyo, pero es más conciso y evita 
#la repetición. Además, si necesitas realizar la misma imputación en otros 
#conjuntos de datos en el futuro, puedes reutilizar la función impute_MasVnrArea.


mode_electrical <- names(which.max(table(train_data$Electrical)))
train_data$Electrical[is.na(train_data$Electrical)] <- mode_electrical


impute_Electrical <- function(data) {
  mode_electrical <- names(which.max(table(data$Electrical)))
  data <- data %>%
    mutate(Electrical = replace(Electrical, is.na(Electrical), mode_electrical))
  return(data)
}

# Aplicar la función a los conjuntos de datos
train_data <- impute_Electrical(train_data)
test_data <- impute_Electrical(test_data)


#4. Imputar GarageYrBlt con YearBuilt

#Para las casas donde falta GarageYrBlt, usaremos el año en que se 
#construyó la casa (YearBuilt).Esto supone que el garaje se construyó al mismo 
#tiempo que la casa, ya que inspeccionamos los conjuntos de datos y notamos que,
#con frecuencia, YearBuilt es similar a GarageYrBlt.


# Imputation of GarageYrBlt using YearBuilt for missing values
train_data$GarageYrBlt[is.na(train_data$GarageYrBlt)] <- train_data$YearBuilt[is.na(train_data$GarageYrBlt)]
# Imputation of GarageYrBlt using YearBuilt for missing values
test_data$GarageYrBlt[is.na(test_data$GarageYrBlt)] <- test_data$YearBuilt[is.na(test_data$GarageYrBlt)]
# Verifying that no missing values remain in GarageYrBlt
sum(is.na(train_data$GarageYrBlt))

# Verifying that no missing values remain in GarageYrBlt
sum(is.na(test_data$GarageYrBlt))

# Checking for any garages built before the house was built
sum(train_data$GarageYrBlt < train_data$YearBuilt)
# Checking for any garages built before the house was built
sum(test_data$GarageYrBlt < test_data$YearBuilt)

#Hay 9 casos en los que GarageYrBlt es anterior a YearBuilt. Potencialmente, esto 
#podría ser un error de entrada de datos o podría indicar que el garaje de la propiedad 
#se conservó de una estructura anterior cuando se construyó la casa actual, lo 
#que podría ser un escenario plausible en algunas renovaciones o reconstrucciones.


sapply(train_data, function(x) sum(is.na(x)))

valores_faltantes_train <- map_dbl(as_tibble(train_data), ~sum(is.na(.)))|>
sort(decreasing = TRUE) 
valores_faltantes

#para ratificar con un grafico
plot_missing(train_data)


valores_faltantes_test <- map_dbl(as_tibble(test_data), ~sum(is.na(.)))|>
  sort(decreasing = TRUE) 
valores_faltantes_test

plot_missing(test_data)

#Podemos ver que todavía faltan algunos valores en test_data, así que imputémoslos 
#manualmente y usando una función auxiliar que calculo:la moda.

#MSZoning - Imputación con la moda (valor más común) de esta variable categórica.

#Utilidades: como suele ser 'AllPub', imputación con el modo.

#Exterior1º y Exterior2º - Imputación con la moda de estas variables categóricas.

#BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF: falta, probablemente no haya 
#sótano; establecido en 0.

#BsmtFullBath y BsmtHalfBath: faltan, probablemente no haya sótano; establecido en 0.

#KitchenQual - Imputación con la moda al ser una variable categórica.

#Funcional: imputación con 'Typ', ya que es la categoría más común que indica 
#una funcionalidad típica.

#GarageCars y GarageArea: faltantes, probablemente no haya garaje; establecido en 0.

#SaleType - Imputación con la moda.



#como es var categorica imputamos con la moda, usamos la funcion impute_mode

# Function to calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# Applying imputations
test_data <- test_data %>%
  mutate(
    MSZoning = replace_na(MSZoning, Mode(MSZoning)),
    Utilities = replace_na(Utilities, "AllPub"),
    Exterior1st = replace_na(Exterior1st, Mode(Exterior1st)),
    Exterior2nd = replace_na(Exterior2nd, Mode(Exterior2nd)),
    BsmtFinSF1 = replace_na(BsmtFinSF1, 0),
    BsmtFinSF2 = replace_na(BsmtFinSF2, 0),
    BsmtUnfSF = replace_na(BsmtUnfSF, 0),
    TotalBsmtSF = replace_na(TotalBsmtSF, 0),
    BsmtFullBath = replace_na(BsmtFullBath, 0),
    BsmtHalfBath = replace_na(BsmtHalfBath, 0),
    KitchenQual = replace_na(KitchenQual, Mode(KitchenQual)),
    Functional = replace_na(Functional, "Typ"),
    GarageCars = replace_na(GarageCars, 0),
    GarageArea = replace_na(GarageArea, 0),
    SaleType = replace_na(SaleType, Mode(SaleType))
  )

# Checking if all NAs are handled
sum(is.na(test_data))

plot_missing(test_data)

#"Detectar valores atípicos

#Usando DataExplorer nuevamente, veamos la distribución de características numéricas 
#y también generaremos diagramas de caja para todas las variables numéricas para 
#visualizar posibles valores atípicos.


# Ploting numeric feature distributions after factor conversion
plot_bar(train_data)

# Generating boxplots for all numeric variables
plot_boxplot(train_data, by = "SalePrice")

# Calculating correlation matrix
numeric_data <- select(train_data, where(is.numeric))
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Ploting the correlation matrix
corrplot(cor_matrix, method = "color", order = "hclust", type = "upper", tl.col = "black", tl.srt = 45)

#Resaltemos las correlaciones específicamente relacionadas con SalePrice, con el 
#propósito de identificar correlaciones altas para priorizar la selección de funciones.

# Correlation with SalePrice
saleprice_correlations <- cor_matrix["SalePrice",]
saleprice_correlations <- sort(saleprice_correlations, decreasing = TRUE)

# Displaying correlations
print(saleprice_correlations)


#hacer una correlacion entre todas laas vars excepto SalePrice

# Calculating the full feature correlation matrix (excluding SalePrice)
feature_cor_matrix <- cor(select(train_data, where(is.numeric), -SalePrice), use = "pairwise.complete.obs")

# Identifying indices of highly correlated features
high_corr <- findCorrelation(feature_cor_matrix, cutoff = 0.75, verbose = TRUE)

# Printing the names of highly correlated features based on indices
high_corr_features <- names(select(train_data, where(is.numeric), -SalePrice))[high_corr]
print(high_corr_features)

# Visualizing high correlations
corrplot(feature_cor_matrix[high_corr, high_corr], type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, method = "color", addCoef.col = "black")


numericVars <- which(sapply(train_data, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(train_data, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 
    'categorical variables')

numericVars <- which(sapply(test_data, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(test_data, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', length(factorVars), 
    'categorical variables')

#eliminando variables redundantes y colineales con otras
new_train_data <- select(train_data, -c("1stFlrSF", "GarageArea", "GarageYrBlt", "2ndFlrSF", "3SsnPorch"))
new_test_data <- select(test_data, -c("1stFlrSF", "GarageArea", "GarageYrBlt", "2ndFlrSF", "3SsnPorch"))

colnames(new_train_data)
colnames(new_test_data)

sum(is.na(new_train_data))
sum(is.na(new_test_data))
