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
library(mice)
library(VIM)
library(outliers)



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



#grafica de valores faltantes con VIM
aggr(all,numbers=T,sortVar=T)


#Imputación con la media con mice


columns <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", "LotFrontage", "GarageType", 
             "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond", "BsmtExposure", "BsmtFinType2", 
             "BsmtQual", "BsmtCond", "BsmtFinType1", "MasVnrType", "MasVnrArea", "Electrical")

imputed_data <- mice(all[,names(all) %in% columns],m = 1,
                     maxit = 1, method = "mean",seed = 2018,print= FALSE)

complete.data <- mice::complete(imputed_data)

xyplot(imputed_data,GarageYrBlt ~LotFrontage)

mosaicMiss(imputed_data, Alley, LotFrontage)




# Supongamos que 'df' es tu dataframe y 'num_var' es tu variable numérica
outliers <- boxplot.stats(all$SalePrice)$out

# Guarda el valor original de 'max.print'
original_max_print <- getOption("max.print")

# Establece 'max.print' a un número grande
options(max.print = 99999)

# Ejecuta tu función
psych::describe(all)

# Restaura el valor original de 'max.print'
options(max.print = original_max_print)

#tomando el log de las var num, para compartir un solo grafico
all_long <- log(all_numVar) |> 
  gather(variable, value, everything())

# Crea el boxplot para todas las variables
ggplot(all_long, aes(x=variable, y=value)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





