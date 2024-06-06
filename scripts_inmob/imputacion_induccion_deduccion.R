
library(VIM)
library(mice)
library(missForest)
library(tidyverse)

list.files(path = " ../data/input_data")

train <- read_csv('E:/proyectos/rstudio_git/ml/urbania2019/data/input_data/train.csv')
test <- read_csv('E:/proyectos/rstudio_git/ml/urbania2019/data/input_data/test.csv')

test_labels <- test$Id

test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
all <- rbind(train, test)
dim(all)

head(all)

all1 <- all |> select(-SalePrice)

numericVars <- which(sapply(all1, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
## There are 37 numeric variables


varchar <- which(sapply(all1, is.character)) #index vector numeric variables
varcharNames <- names(varchar) #saving names vector for use later on
cat('There are', length(varchar), 'character variables')
## There are 42 numeric variables

all1 <- all1 |> 
  mutate(across(all_of(varchar), as.factor))

all_factores <- all1 |>
  select(where(is.factor))
print(names(all_factores))

str(all1)

aggr(all1)

nrow(na.omit(all1))
all1

data_imp <- mice(all1)

attributes(all1_imp)


library(missForest)

data_imp_all1 <- missForest(as.data.frame(all1))

table(all1$LotFrontage)

plot(density(all1$LotFrontage, na.rm = TRUE)) 
lines(density(as.numeric(data_imp_all1$xim$LotFrontage)), col="red", lty=3, lwd=2)

table(round(data_imp_all1$ximp$LotFrontage, 2))

sum(table(round(data_imp_all1$ximp$LotFrontage, 2)))
# 2919

#graficando

# Combinamos ambos conjuntos de datos

all1$type <- c("value")
data_imp_all1$ximp$type <- c("Imputed")

data_combined <- rbind(all1, data_imp_all1$ximp, id.vars = NULL)

# Eliminamos los NA de los datos originales



data_combined$type_bin <- ifelse(data_combined$type == "value", 1, 0)

#data_combined <- data_combined[!is.na(data_combined$value) | data_combined$Type == "Imputed", ]


head(data_combined,2)
names(data_combined)

# Obtén los nombres de las columnas
column_names <- data_combined |> select(where(is.numeric)) |> select(-type_bin)
column_names <- names(column_names)

# Crea un pdf para guardar todos los gráficos
pdf("density_plots.pdf")

# Itera sobre cada columna
for (col_name in column_names) {
  # Crea un nuevo gráfico de densidad para la columna actual
  plot(density(all1[[col_name]], na.rm = TRUE), main = col_name)
  lines(density(as.numeric(data_imp_all1$xim[[col_name]])), col = "red", lty = 3, lwd = 2)
}

# Cierra el pdf
dev.off()


save(data_combined, file = "data_combined.RData")

#load(data_combined.RData)

################################################################################################

#https://github.com/oyaxbell/curso_R/blob/main/Clase_10.R

#DATOS PERDIDOS E IMPUTACIÓN MÚLTIPLE
#Ponente: Luisa Fernández Chirino, 16 de junio de 2021
#Datos perdidos: no hay alguna observación dentro de la variable de interés
#Problema común 

#PAQUETERÍAS
#Recordamos el install.packages()
library(mice); library(miceadds); library(finalfit); library(mitools); library(VIM);  
library(tidyverse); library(datasets); library(devtools)

#1. Limpieza de la base
# Create some extra missing data
## MCAR
set.seed(1)
colon_s$smoking_mcar <- sample(c("Smoker", "Non-smoker", NA), 
                               dim(colon_s)[1], replace=TRUE, 
                               prob = c(0.2, 0.7, 0.1)) %>% factor() %>% ff_label("Smoking (MCAR)")

## Datos perdidos condicionados al sexo del paciente
colon_s$smoking_mar[colon_s$sex.factor == "Female"] <- 
  sample(c("Smoker", "Non-smoker", NA), 
         sum(colon_s$sex.factor == "Female"), 
         replace = TRUE,
         prob = c(0.1, 0.5, 0.4))

colon_s$smoking_mar[colon_s$sex.factor == "Male"] <- 
  sample(c("Smoker", "Non-smoker", NA), 
         sum(colon_s$sex.factor == "Male"), 
         replace=TRUE, prob = c(0.15, 0.75, 0.1))
colon_s$smoking_mar = factor(colon_s$smoking_mar) %>% 
  ff_label("Smoking (MAR)")

# Visualización
explanatory <- c("age", "sex.factor", 
                 "nodes", "obstruct.factor",  
                 "smoking_mcar", "smoking_mar")
dependent <- "mort_5yr"

colon_s %>% 
  ff_glimpse(dependent, explanatory)

#2. Identificación y visualización de datos perdidos

colon_s %>%
  missing_plot()

set.seed(123)
aggr_plot<-aggr(colon_s, col=c('navyblue','red'), 
                numbers=TRUE, sortVars=TRUE, labels=names(colon_s), 
                cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
print(aggr_plot)


#3. Visualización de patrones

md.pattern(colon_s, rotate.names = TRUE)

explanatory <- c("age", "sex.factor", 
                 "obstruct.factor",  
                 "smoking_mcar", "smoking_mar")
dependent <- "mort_5yr"

colon_s %>% 
  missing_pattern(dependent, explanatory)

#Incluir datos perdidos al realizar las tablas de población

#Variables explicatorias o confusoras
explanatory <- c("age", "sex.factor", 
                 "nodes",  
                 "smoking_mcar", "smoking_mar")

# Variable dependiente de interés
dependent <- "obstruct.factor" # Obstrucción intestinal

colon_s %>% 
  summary_factorlist(dependent, explanatory, 
                     na_include=TRUE, p=TRUE)

#4. Buscar asociaciones entre datos perdidos y observados

explanatory <- c("age", "sex.factor", 
                 "nodes", "obstruct.factor",  
                 "smoking_mcar", "smoking_mar")
dependent <- "mort_5yr"
colon_s %>% 
  missing_pairs(dependent, explanatory)

colon_s %>% 
  missing_pairs(dependent, explanatory, position = "fill")

explanatory <- c("age", "sex.factor", 
                 "nodes", "obstruct.factor")
dependent <- "smoking_mcar"
colon_s %>% 
  missing_compare(dependent, explanatory)

dependent <- "smoking_mar"
colon_s %>% 
  missing_compare(dependent, explanatory) 

#Si, puedo usar una prueba de hipótesis para identificar MCAR
#Se llama prueba de Little
install_github("cran/MissMech")
library(MissMech)

explanatory <- c("age", "nodes")
dependent <- "mort_5yr" 

colon_s %>% 
  select(explanatory) %>% 
  MissMech::TestMCARNormality()

#5. Decidir qué hacer con los datos perdidos

#1.1 Listwise deletion
explanatory <- c("age", "sex.factor", 
                 "nodes", "obstruct.factor",  
                 "smoking_mcar")
dependent <- "mort_5yr"
colon_s %>% 
  finalfit(dependent, explanatory)

#1.2 Imputación múltiple
explanatory = c("age", "sex.factor", 
                "nodes", "obstruct.factor", "smoking_mar")
dependent = "mort_5yr"

#Elegimos no imputar valores perdidos para variable dependiente de interés y variable respuesta. 
#Sin embargo se incluyen en el algoritmo.

colon_s %>% 
  select(dependent, explanatory) %>% 
  missing_predictorMatrix(
    drop_from_imputed = c("obstruct.factor", "mort_5yr")) -> predM

fits = colon_s %>% 
  select(dependent, explanatory) %>% 
  mice(m = 5, predictorMatrix = predM) %>% 
  # Vamos a correr una regresión logística con cada base imputada
  with(glm(formula(ff_formula(dependent, explanatory)), 
           family="binomial"))

#Extracción de métricas
## BICs
fits %>% 
  getfit() %>% 
  purrr::map(BIC)

# Estadístico C
fits %>% 
  getfit() %>% 
  purrr::map(~ pROC::roc(.x$y, .x$fitted)$auc)

# Pool  results
fits_pool <- fits %>% 
  pool()

## Plot de OR
colon_s %>% 
  or_plot(dependent, explanatory, glmfit = fits_pool, table_text_size=4)

fit_imputed = fits_pool %>%                                  
  fit2df(estimate_name = "OR (multiple imputation)", exp = TRUE)

# Vamos a unir y comparar resultados
fit_imputed <- fits_pool %>%                                  
  fit2df(estimate_name = "OR (multiple imputation)", exp = TRUE)

colon_s %>% 
  summary_factorlist(dependent, explanatory, fit_id = TRUE) -> summary1

colon_s %>% 
  glmuni(dependent, explanatory) %>% 
  fit2df(estimate_suffix = " (univariable)") -> fit_uni

#colon_s %>% 
#  glmmulti(dependent, explanatory) %>% 
#  fit2df(estimate_suffix = " (multivariable inc. smoking)") -> fit_multi

explanatory <- c("age", "sex.factor", 
                 "nodes", "obstruct.factor")
colon_s %>% 
  glmmulti(dependent, explanatory) %>% 
  fit2df(estimate_suffix = " (multivariable)") -> fit_multi_r

# Combinar a tabla resumen
summary1 %>% 
  ff_merge(fit_uni) %>% 
  ff_merge(fit_multi_r) %>% 
#  ff_merge(fit_multi) %>% 
  ff_merge(fit_imputed) %>% 
  select(-fit_id, -index)




