library(knitr)
library(ggplot2)
library(plyr)
library(dplyr)
library(corrplot)
library(caret)
library(gridExtra)
library(scales)
library(Rmisc)
library(ggrepel)
library(randomForest)
library(psych)
library(xgboost)


train <- read.csv("E:/proyectos/rstudio_git/ml/urbania2019/data/input_data/train.csv", stringsAsFactors = F)
test <- read.csv("E:/proyectos/rstudio_git/ml/urbania2019/data/input_data/test.csv", stringsAsFactors = F)

dim(train)
str(train[,c(1:10, 81)])

#Deshacerse de las identificaciones pero mantener las identificaciones de 
#prueba en un vector. Estos son necesarios para componer el archivo de envío.

test_labels <- test$Id

test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
all <- rbind(train, test)
dim(all)

head(all)

#explorando las vars mas importantes

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

summary(all$SalePrice)

#Altogether, there are 10 numeric variables with a correlation of at least 0.5 with SalePrice. All those correlations are positive.

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
## There are 37 numeric variables

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#Precio de venta y calidad general,  correlacion 
ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
#a mayor calidad mayor precio

#area construida y precio de venta
ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))
#a mayor area construida, mayor precio

#Correlations with SalePrice
charVars <- which(sapply(all, is.character)) #index vector numeric variables
charVarNames <- names(charVars) #saving names vector for use later on
cat('There are', length(charVars), 'numeric variables')


NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
NAcol #sirve para saber el orden del index de la columna en el dataset


# Lista de columnas
NAcol <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", "LotFrontage", "GarageType", 
           "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond", "BsmtExposure", "BsmtFinType2", 
           "BsmtQual", "BsmtCond", "BsmtFinType1", "MasVnrType", "MasVnrArea", "Electrical")


2919*.2
1459*.2

sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE) > 291

subset("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", "LotFrontage")

# Hacemos un subset de las variables de interés
vars <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", "LotFrontage")
df_subset <- df %>% select(all_of(vars))

library(purrr)
Nacol <-  which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE) > 291  |>
 # select(PoolQC, MiscFeature, Alley, Fence, FireplaceQu, LotFrontage) |>
    map_df(~replace(., is.na(.), "none"))

# Aplicamos la función map para rellenar los valores NA con "none"
Nacol80_subset_filled <- Nacol80_subset %>% map_Nacol80(~replace(., is.na(.), "none"))


Asegúrate de tener tus datos en un data.frame llamado 'Nacol80'
# Convierte PoolQC a un factor ordenado
Nacol80$PoolQC <- factor(Nacol80$PoolQC, levels = c("None", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)
Nacol80_SalePrice <- all[!is.na(all$SalePrice),]

# Calcula el coeficiente de correlación de Spearman
correlation <- cor.test(Nacol80_SalePrice, Nacol80$PoolQC, method = "spearman")

# Imprime el coeficiente de correlación y el valor p
print(paste("Coeficiente de correlación de Spearman: ", correlation$estimate))


all$PoolQC[is.na(all$PoolQC)] <- 'None'

table(as.factor(Nacol80$PoolQC))
sum(table(Nacol80$PoolQC))


# Función para calcular la moda
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Reemplazar NA con la moda en las columnas especificadas
all <- all |>
  mutate(across(all_of(NAcol), function(col) {
    replace(col, is.na(col), getmode(col[!is.na(col)]))
  }))

# Imprimir el dataframe
print(all)


str(all)

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)
NaCol #sirve para saber el orden del index de la columna en el dataset





all <- all |>
  mutate(across(all_of(all), ~replace(., is.na(.), "0")))
#ahora toca escoger algunas vars que tienen valores faltantes (na)


cat('There are', length(NAcol), 'columns with missing values')


Charcol <- names(all[,sapply(all, is.character)])

all <- all |>
  mutate(across(all_of(Charcol), as.factor)) 

all_factores <- all |>
  select(where(is.factor))
print(names(all_factores))







#algunas variables pueden pasar de factores ordinales a numericas como PoolQC 
all$PoolQC[is.na(all$PoolQC)] <- 'None'
Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

all$PoolQC<-as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)


all[all$PoolArea>0 & all$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]

all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 3
all$PoolQC[2600] <- 2

all$Fence[is.na(all$Fence)] <- 0
table(all$Fence)

vector[is.na(vector)] <- 0






#factores que pueden pasar a ordinales y luego
#convertirlas a numericas con as.integer()



table(all$Foundation)

Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
table(all$MasVnrType)
table(all$PavedDrive)

# Asegúrate de tener tus datos en un data.frame llamado 'df'
df <- as.data.frame(cbind(all$SalePrice, Nacol80$PoolQC))

# Convierte PoolQC a un factor ordenado
df$PoolQC <- factor(df$PoolQC, levels = c("None", "Fa", "TA", "Gd", "Ex"), ordered = TRUE)

# Calcula el coeficiente de correlación de Spearman
correlation <- cor.test(df$SalePrice, df$PoolQC, method = "spearman")

# Imprime el coeficiente de correlación y el valor p
print(paste("Coeficiente de correlación de Spearman: ", correlation$estimate))
print(paste("Valor p: ", correlation$p.value))