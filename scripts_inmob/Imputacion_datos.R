#https://rpubs.com/ydmarinb/429757
#https://stefvanbuuren.name/fimd/

#aplicando esta tecnicas al conjunto de datos ames houses


library(kableExtra)

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

base <- all
kable(base[1:4,1:10],"markdown")


#Contar el total de NAs en la base de datos
sum(is.na(base))

#Saber el número de NAs por columna
sort(colSums(is.na(base)), decreasing = TRUE)


#Omitir las filas con observaciones NA
base1 <- na.omit(base)


#Clasificación de los datos perdidos

#MCAR (Missing Completely At Random): La probabilidad de que una respuesta a una 
#variable sea dato faltante es independiente tanto del valor de esta variable 
#como del valor de otras variables del conjunto de datos.

#MAR (Missing At Random): La probabilidad de que una respuesta sea dato faltante 
#es independiente de los valores de la misma variable pero es dependiente de los
#valores de otras variables del conjunto de datos.

#NMAR (Not Missing At Random): La probabilidad de que una respuesta a una 
#variable sea dato faltante es dependiente de los valores de la variable.

library(VIM)
aggr(base,numbers=T,sortVar=T)


#Imputación con la media
library(mice)

columns <- c("PoolQC", "MiscFeature", "Alley", "Fence", "FireplaceQu", "LotFrontage", "GarageType", 
           "GarageYrBlt", "GarageFinish", "GarageQual", "GarageCond", "BsmtExposure", "BsmtFinType2", 
           "BsmtQual", "BsmtCond", "BsmtFinType1", "MasVnrType", "MasVnrArea", "Electrical")

imputed_data <- mice(base[,names(base) %in% columns],m = 1,
                     maxit = 1, method = "mean",seed = 2018,print= FALSE)

complete.data <- mice::complete(imputed_data)

xyplot(imputed_data,GarageYrBlt ~LotFrontage)

mosaicMiss(imputed_data, Alley, LotFrontage)


#graficar dos variables categoricas, usar mosaicplot de paquete vcd 

# Carga el paquete
library(vcd)

# Convierte las variables a factores
base$Alley <- as.factor(base$Alley)
base$LotFrontage <- as.factor(base$LotFrontage)

# Crea la tabla de contingencia
tab <- table(base$Alley, base$Fence)

# Crea el mosaicplot
mosaicplot(tab, main="Mosaic Plot of Alley and Fence", xlab="Alley", ylab="Fence", color=TRUE)


summary(imputed_data$data$GarageYrBlt)

#el valor atipico de GarageYrBlt sale en 2207 cuando deberia ser 2007
which (imputed_data$data$GarageYrBlt > 2200)
#ahora remplazo por el valor mas cercano
imputed_data$data$GarageYrBlt[2593] <- 2007
#[1] 2207




par(mfrow=c(1,2))
plot(density(base$GarageYrBlt,na.rm = T),col=2,main="GarageYrBlt")
lines(density(complete.data$GarageYrBlt),col=3)
plot(density(base$LotFrontage,na.rm = T),col=2,main="LotFrontage")
lines(density(complete.data$LotFrontage),col=3)



#Imputación mediante regresión
impute_arg1  <- mice(base[,names(base) %in% columns],m = 1,
                     maxit = 1, method = "norm.predict",seed = 2018,print=F)

impute_arg <- mice::complete(impute_arg1)

mosaicplot(impute_arg1,GarageYrBlt ~LotFrontage)

#graficando
par(mfrow=c(1,2))
plot(density(base$GarageYrBlt,na.rm = T),col=2,main="GarageYrBlt")
lines(density(impute_arg$GarageYrBlt),col=3)
plot(density(base$LotFrontage,na.rm = T),col=2,main="LotFrontage")
lines(density(impute_arg$LotFrontage),col=3)



#Imputación mediante regresión estocastica

imputed_data1 <- mice(base[,names(base) %in% columns],m = 1,
                      maxit = 1, method = "norm.nob",seed = 2018,print=F)
complete.data1 <- mice::complete(imputed_data1)
xyplot(imputed_data1,GarageYrBlt ~LotFrontage)

par(mfrow=c(1,2))
plot(density(base$GarageYrBlt,na.rm = T),col=2,main="GarageYrBlt")
lines(density(complete.data1$GarageYrBlt),col=3)
plot(density(base$LotFrontage,na.rm = T),col=2,main="LotFrontage")
lines(density(complete.data1$LotFrontage),col=3)

aggr(complete.data1)
aggr(complete.data3)
skimr::skim(complete.data1)
skimr::skim(complete.data3)

table(aggr(c(base,complete.data1),numbers=T,sortVar=T))


#Imputación LOCF

#Son métodos de imputación ad hoc para datos longitudinales. La idea es tomar el 
#valor observado anterior como un reemplazo de los datos faltantes. Cuando faltan 
#varios valores en sucesión, el método busca el último valor observado.

library(tidyr)

imputar <- tidyr::fill(base, PoolQC)





#imputacion multiple

imputed_data3 <- mice(base[,names(base) %in% columns], seed=2018,print = F,
                      m = 30)
complete.data3<- mice::complete(imputed_data3)

xyplot(imputed_data3,GarageYrBlt ~LotFrontage)


par(mfrow=c(1,2))
plot(density(base$GarageYrBlt,na.rm = T),col=2,main="GarageYrBlt")
lines(density(complete.data3$GarageYrBlt),col=3)
plot(density(base$LotFrontage,na.rm = T),col=2,main="LotFrontage")
lines(density(complete.data3$LotFrontage),col=3)


#Imputacion Aleatorio
rand.imput <-function(x){
  missing <- (is.na(x)) #vector booleano
  n.missing <- sum(missing)#Numero de NA’s
  x.obs <- x[!missing]#Datos no NA
  imputed <- x
  imputed[missing] <- sample(x.obs,n.missing,replace = T)
  #Se extrae una muestra aleatoria conocida y se remplazan estos en los NA
  return(imputed)}

complete.data4 <- rand.imput(base$GarageYrBlt)
complete.data5 <- rand.imput(base$LotFrontage)

par(mfrow=c(1,2))

plot(density(base$GarageYrBlt,na.rm = T),col=2,main="GarageYrBlt")
lines(density(complete.data4),col=3)
plot(density(base$LotFrontage,na.rm = T),col=2,main="LotFrontage")
lines(density(complete.data5),col=3)


############################################################################

library(VIM)
sapply(base, class)
aggr(data)

nrow(na.omit(data))
data

data_imp <- mice(data)

attributes(data_imp)

#complete

data_complete <- complete(data_imp)

aggr(data_complete)

plot(density(data$LotFrontage, na.rm = TRUE)) 
lines(density(data_complete$LotFrontage), col="red", lty=3, lwd=2)

#############################
aggr(penguins)

library(missForest)

data_imp_peng <- missForest(as.data.frame(penguins))

table(penguins$sex)

table(data_imp_peng$ximp$sex)  


################################