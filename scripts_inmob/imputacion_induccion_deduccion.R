
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





