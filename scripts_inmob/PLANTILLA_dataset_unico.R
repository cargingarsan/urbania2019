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

#explorando las vars mas importantes

ggplot(data=all[!is.na(all$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

summary(all$SalePrice)

#Correlations with SalePrice
numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

#calidad o estado general de la viv o construccion
ggplot(data=all[!is.na(all$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#La variable numérica con la segunda mayor correlación con el precio de venta es 
#Above Grade Living Area. Esto tiene mucho sentido; las casas grandes suelen ser más caras.

ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))


#candidatas a ser eliminadas como valores atipicos
all[c(524, 1299), c('SalePrice', 'GrLivArea', 'OverallQual')]


#En primer lugar, me gustaría ver qué variables contienen valores perdidos.

NAcol <- which(colSums(is.na(all)) > 0)
sort(colSums(sapply(all[NAcol], is.na)), decreasing = TRUE)

cat('There are', length(NAcol), 'columns with missing values')


#Calidad de la piscina y la variable PoolArea
all$PoolQC[is.na(all$PoolQC)] <- 'None'

Qualities <- c('None' = 0, 'Po' = 1, 'Fa' = 2, 'TA' = 3, 'Gd' = 4, 'Ex' = 5)

all$PoolQC<-as.integer(revalue(all$PoolQC, Qualities))
table(all$PoolQC)

#hay una segunda variable que se relaciona con las Piscinas. Se trata de la variable PoolArea (en pies cuadrados).

all[all$PoolArea>0 & all$PoolQC==0, c('PoolArea', 'PoolQC', 'OverallQual')]

#imputacion directa en base a la moda
all$PoolQC[2421] <- 2
all$PoolQC[2504] <- 3
all$PoolQC[2600] <- 2


####Miscellaneous Feature
#Miscellaneous feature no incluídos en otras categorías

all$MiscFeature[is.na(all$MiscFeature)] <- 'None'
all$MiscFeature <- as.factor(all$MiscFeature)

ggplot(all[!is.na(all$SalePrice),], aes(x=MiscFeature, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))

table(all$MiscFeature)


#Tipo de callejón de acceso a la propiedad

all$Alley[is.na(all$Alley)] <- 'None'
all$Alley <- as.factor(all$Alley)

ggplot(all[!is.na(all$SalePrice),], aes(x=Alley, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 200000, by=50000), labels = comma)

table(all$Alley)


#Calidad de la valla, Con Fence, hay 2348 NAs. Los valores parecen ser ordinales. 

all$Fence[is.na(all$Fence)] <- 'None'
table(all$Fence)

all[!is.na(all$SalePrice),] |> 
  group_by(Fence) |>
  summarise(median = median(SalePrice), counts=n())

#Mi conclusión es que los valores no parecen ordinales (ninguna valla es mejor).
#Por lo tanto, voy a convertir Fence en un factor.

all$Fence <- as.factor(all$Fence)


#Calidad de la chimenea, y Número de chimeneas

all$FireplaceQu[is.na(all$FireplaceQu)] <- 'None'
all$FireplaceQu<-as.integer(revalue(all$FireplaceQu, Qualities))
table(all$FireplaceQu)

#Chimeneas es una variable entera, y no hay valores perdidos.

table(all$Fireplaces)
sum(table(all$Fireplaces))


#3 variables. Una con 1 NA, y 2 variables completas.
#LotFrontage: Pies lineales de calle conectados a la propiedad

#486 NAs. La imputación más razonable parece tomar la mediana por barrio.

ggplot(all[!is.na(all$LotFrontage),], aes(x=as.factor(Neighborhood), y=LotFrontage)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

for (i in 1:nrow(all)){
  if(is.na(all$LotFrontage[i])){
    all$LotFrontage[i] <- as.integer(median(all$LotFrontage[all$Neighborhood==all$Neighborhood[i]], na.rm=TRUE)) 
  }
}

#No NAs. Los valores parecen ordinales (Regular=best)

all$LotShape<-as.integer(revalue(all$LotShape, c('IR3'=0, 'IR2'=1, 'IR1'=2, 'Reg'=3)))
table(all$LotShape)

sum(table(all$LotShape))

#LotConfig: Configuración de la parcela

#No NAs. The values seemed possibly ordinal to me, but the visualization does 
#not show this. Therefore, I will convert the variable into a factor.

ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(LotConfig), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..))


all$LotConfig <- as.factor(all$LotConfig)
table(all$LotConfig)


####Garage variables

#En total, hay 7 variables relacionadas con los garajes

all$GarageYrBlt[is.na(all$GarageYrBlt)] <- all$YearBuilt[is.na(all$GarageYrBlt)]

#comprobar si las 157 NAs son las mismas observaciones entre las variables con 157/159 NAs
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & 
               is.na(all$GarageCond) & is.na(all$GarageQual)))


#Encontrar 2 NAs adicionales
kable(all[!is.na(all$GarageType) & is.na(all$GarageFinish), 
          c('GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

#Las diferencias se encuentran en las casas 2127 y 2577. Como se puede ver, 
#la casa 2127 parece tener un garaje y la casa 2577 no. Por lo tanto, debería 
#haber 158 casas sin Garaje. Para arreglar la casa 2127, imputaré los valores 
#más comunes (modos) para GarageCond, GarageQual y GarageFinish.

##Imputing modes.
all$GarageCond[2127] <- names(sort(-table(all$GarageCond)))[1]
all$GarageQual[2127] <- names(sort(-table(all$GarageQual)))[1]
all$GarageFinish[2127] <- names(sort(-table(all$GarageFinish)))[1]

#display "fixed" house
kable(all[2127, c('GarageYrBlt', 'GarageCars', 'GarageArea', 'GarageType', 'GarageCond', 'GarageQual', 'GarageFinish')])

#fixing 3 values for house 2577
all$GarageCars[2577] <- 0
all$GarageArea[2577] <- 0
all$GarageType[2577] <- NA

#comprobar si los NAs de las variables de carácter son ahora todos 158
length(which(is.na(all$GarageType) & is.na(all$GarageFinish) & is.na(all$GarageCond) & is.na(all$GarageQual)))

all$GarageType[is.na(all$GarageType)] <- 'No Garage'
all$GarageType <- as.factor(all$GarageType)
table(all$GarageType)


#GarageFinish: Acabado interior del garaje; Los valores son ordinales.

all$GarageFinish[is.na(all$GarageFinish)] <- 'None'
Finish <- c('None'=0, 'Unf'=1, 'RFn'=2, 'Fin'=3)

all$GarageFinish<-as.integer(revalue(all$GarageFinish, Finish))
table(all$GarageFinish)


#GarageQual: Calidad del garaje

#Otra variable que puede hacerse ordinal con el vector Cualidades.

all$GarageQual[is.na(all$GarageQual)] <- 'None'
all$GarageQual<-as.integer(revalue(all$GarageQual, Qualities))
table(all$GarageQual)

all$GarageCond[is.na(all$GarageCond)] <- 'None'
all$GarageCond<-as.integer(revalue(all$GarageCond, Qualities))
table(all$GarageCond)


#En total, hay 11 variables relacionadas con los sótanos de una casa
#Cinco de ellos tienen 79-82 NAs, seis tienen uno o dos NAs.

#comprobar si las 79 NAs son las mismas observaciones entre las variables con 80+ NAs
length(which(is.na(all$BsmtQual) & is.na(all$BsmtCond) & is.na(all$BsmtExposure)
             & is.na(all$BsmtFinType1) & is.na(all$BsmtFinType2)))

#Find the additional NAs; BsmtFinType1 is the one with 79 NAs
all[!is.na(all$BsmtFinType1) & (is.na(all$BsmtCond)|is.na(all$BsmtQual)|
    is.na(all$BsmtExposure)|is.na(all$BsmtFinType2)), c('BsmtQual', 'BsmtCond', 
                      'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2')]


##Imputing modes.
all$BsmtFinType2[333] <- names(sort(-table(all$BsmtFinType2)))[1]
all$BsmtExposure[c(949, 1488, 2349)] <- names(sort(-table(all$BsmtExposure)))[1]
all$BsmtCond[c(2041, 2186, 2525)] <- names(sort(-table(all$BsmtCond)))[1]
all$BsmtQual[c(2218, 2219)] <- names(sort(-table(all$BsmtQual)))[1]

#BsmtQual: Evalúa la altura del sótano

all$BsmtQual[is.na(all$BsmtQual)] <- 'None'
all$BsmtQual<-as.integer(revalue(all$BsmtQual, Qualities))
table(all$BsmtQual)

#BsmtCond: Evalúa el estado general del sótano

all$BsmtCond[is.na(all$BsmtCond)] <- 'None'
all$BsmtCond<-as.integer(revalue(all$BsmtCond, Qualities))
table(all$BsmtCond)


#BsmtExposure: Se refiere a los muros a nivel del jardín o de la calle
#Una variable que puede convertirse en ordinal.

all$BsmtExposure[is.na(all$BsmtExposure)] <- 'None'
Exposure <- c('None'=0, 'No'=1, 'Mn'=2, 'Av'=3, 'Gd'=4)

all$BsmtExposure<-as.integer(revalue(all$BsmtExposure, Exposure))
table(all$BsmtExposure)


#BsmtFinType1: Calificación de la superficie del sótano terminado

all$BsmtFinType1[is.na(all$BsmtFinType1)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all$BsmtFinType1<-as.integer(revalue(all$BsmtFinType1, FinType))
table(all$BsmtFinType1)


#BsmtFinType2: Calificación de la superficie acabada del sótano (si hay varios tipos)

all$BsmtFinType2[is.na(all$BsmtFinType2)] <- 'None'
FinType <- c('None'=0, 'Unf'=1, 'LwQ'=2, 'Rec'=3, 'BLQ'=4, 'ALQ'=5, 'GLQ'=6)

all$BsmtFinType2<-as.integer(revalue(all$BsmtFinType2, FinType))
table(all$BsmtFinType2)

#Ahora todavía hay que lidiar con esas 6 variables que tienen 1 o 2 NAs.

#mostrar los restantes NA. Utilizando BsmtQual como referencia para las 79 viviendas sin sótano acordadas anteriormente
all[(is.na(all$BsmtFullBath)|is.na(all$BsmtHalfBath)|is.na(all$BsmtFinSF1)|
       is.na(all$BsmtFinSF2)|is.na(all$BsmtUnfSF)|is.na(all$TotalBsmtSF)), 
    c('BsmtQual', 'BsmtFullBath', 'BsmtHalfBath', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'TotalBsmtSF')]


#BsmtFullBath: Baños completos en el sótano

#Una variable entera.

all$BsmtFullBath[is.na(all$BsmtFullBath)] <-0
table(all$BsmtFullBath)

#BsmtHalfBath: Medios baños en el sótano

#Una variable entera. a los Na se le da el valor de 0

all$BsmtHalfBath[is.na(all$BsmtHalfBath)] <-0
table(all$BsmtHalfBath)

all$BsmtFinSF1[is.na(all$BsmtFinSF1)] <-0

all$BsmtFinSF2[is.na(all$BsmtFinSF2)] <-0

all$BsmtUnfSF[is.na(all$BsmtUnfSF)] <-0

all$TotalBsmtSF[is.na(all$TotalBsmtSF)] <-0


#Variables de mampostería

#El tipo de revestimiento de mampostería tiene 24 NAs. El área de revestimiento de mampostería tiene 23 NAs.

#compruebe si las 23 casas con superficie de chapa de madera NA son también NA en el tipo de chapa de madera
length(which(is.na(all$MasVnrType) & is.na(all$MasVnrArea)))


#encontrar el que debe tener un MasVnrType
all[is.na(all$MasVnrType) & !is.na(all$MasVnrArea), c('MasVnrType', 'MasVnrArea')]

##fijar este tipo de chapa imputando el modo
all$MasVnrType[2611] <- names(sort(-table(all$MasVnrType)))[2] 
#taking the 2nd value as the 1st is 'none'
all[2611, c('MasVnrType', 'MasVnrArea')]

#Tipo de revestimiento de mampostería

all$MasVnrType[is.na(all$MasVnrType)] <- 'None'

all[!is.na(all$SalePrice),] |> group_by(MasVnrType) |>
  summarise(median = median(SalePrice), counts=n()) |> 
  arrange(median)

#Masonry <- c('None'=0, 'BrkCmn'=0, 'BrkFace'=1, 'Stone'=2)
all$MasVnrType<-as.integer(revalue(all$MasVnrType, Masonry))
table(all$MasVnrType)


#MasVnrArea: Área de revestimiento de mampostería en pies cuadrados

all$MasVnrArea[is.na(all$MasVnrArea)] <-0


#MSZoning: Identifica la clasificación zonal general de la venta

#imputing the mode
all$MSZoning[is.na(all$MSZoning)] <- names(sort(-table(all$MSZoning)))[1]
all$MSZoning <- as.factor(all$MSZoning)
table(all$MSZoning)

sum(table(all$MSZoning))


#Kitchen quality

all$KitchenQual[is.na(all$KitchenQual)] <- 'TA' #replace with most common value
all$KitchenQual<-as.integer(revalue(all$KitchenQual, Qualities))
table(all$KitchenQual)

sum(table(all$KitchenQual))


#Number of Kitchens above grade

table(all$KitchenAbvGr)

sum(table(all$KitchenAbvGr))


#Utilities: Type of utilities available

table(all$Utilities)
#encontrar el Na
kable(all[is.na(all$Utilities) | all$Utilities=='NoSeWa', 1:9])

all$Utilities <- NULL


#Functional: Home functionality

#impute mode for the 1 NA
all$Functional[is.na(all$Functional)] <- names(sort(-table(all$Functional)))[1]

all$Functional <- as.integer(revalue(all$Functional, 
        c('Sal'=0, 'Sev'=1, 'Maj2'=2, 'Maj1'=3, 'Mod'=4, 'Min2'=5, 'Min1'=6, 'Typ'=7)))
table(all$Functional)

sum(table(all$Functional))


#Exterior1st: Revestimiento exterior de la casa

#imputing mode
all$Exterior1st[is.na(all$Exterior1st)] <- names(sort(-table(all$Exterior1st)))[1]

all$Exterior1st <- as.factor(all$Exterior1st)
table(all$Exterior1st)

sum(table(all$Exterior1st))


#Exterior2nd: Revestimiento exterior de la casa (si hay más de un material)

#imputing mode
all$Exterior2nd[is.na(all$Exterior2nd)] <- names(sort(-table(all$Exterior2nd)))[1]

all$Exterior2nd <- as.factor(all$Exterior2nd)
table(all$Exterior2nd)

sum(table(all$Exterior2nd))


#ExterQual: Evalúa la calidad del material en el exterior

all$ExterQual<-as.integer(revalue(all$ExterQual, Qualities))

table(all$ExterQual)
sum(table(all$ExterQual))


#ExterCond: Evalúa el estado actual del material en el exterior

all$ExterCond<-as.integer(revalue(all$ExterCond, Qualities))

table(all$ExterCond)
sum(table(all$ExterCond))


#Electrical: Sistema eléctrico; 1 NA. Los valores son categóricos.

#imputing mode
all$Electrical[is.na(all$Electrical)] <- names(sort(-table(all$Electrical)))[1]

all$Electrical <- as.factor(all$Electrical)
table(all$Electrical)
 
sum(table(all$Electrical))


#Tipo y condición de venta

#imputing mode
all$SaleType[is.na(all$SaleType)] <- names(sort(-table(all$SaleType)))[1]

all$SaleType <- as.factor(all$SaleType)
table(all$SaleType)
sum(table(all$SaleType))


#SaleCondition: Condición de venta;No hay NAs. Los valores son categóricos.

all$SaleCondition <- as.factor(all$SaleCondition)
table(all$SaleCondition)
sum(table(all$SaleCondition))


#Codificación de etiquetas/factorización de las restantes variables de caracteres 

Charcol <- names(all[,sapply(all, is.character)])
Charcol

cat('There are', length(Charcol), 'remaining columns with character values')
  
#Foundation: Tipo de cimiento

#No ordinality, so converting into factors
all$Foundation <- as.factor(all$Foundation)
table(all$Foundation)

sum(table(all$Foundation))

Charcol <- c("MSZoning", "MiscFeature", "Alley", "Street", "LandContour",  
          "LotConfig", "Neighborhood", "BldgType", "RoofStyle", "RoofMatl", 
          "Exterior1st", "Exterior2nd", "Foundation", "Heating", "CentralAir", 
          "GarageType", "SaleType", "SaleCondition", "Fence", "Condition1", 
          "Condition2", "LandSlope" ,"HouseStyle", "MasVnrType", "ExterCond",
          "HeatingQC" , "PavedDrive")

all <- all |>
  mutate(across(all_of(Charcol), as.factor)) |> as_data_frame()

all_factores <- all %>%
  select(where(is.factor))
print(names(all_factores))


unique(all$SaleCondition)
table(all$SaleCondition)
sum(table(all$SaleCondition))



#5.4 Changing some numeric variables into factors
#However, there are 3 variables that are recorded numeric but should actually be categorical.

str(all$YrSold)
str(all$MoSold)

all$MoSold <- as.factor(all$MoSold)

#la estacionalidada es importante para las ventas, indep. de la crisis 2008

ys <- ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(YrSold), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

ms <- ggplot(all[!is.na(all$SalePrice),], aes(x=MoSold, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue')+
  scale_y_continuous(breaks= seq(0, 800000, by=25000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..)) +
  coord_cartesian(ylim = c(0, 200000)) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

grid.arrange(ys, ms, widths=c(1,2))


#5.4.2 MSSubClass; MSSubClass: Identifies the type of dwelling involved in the sale.

str(all$MSSubClass)

all$MSSubClass <- as.factor(all$MSSubClass)

#revalue for better readability
all$MSSubClass<-revalue(all$MSSubClass, c('20'='1 story 1946+', '30'='1 story 1945-', 
            '40'='1 story unf attic', '45'='1,5 story unf', '50'='1,5 story fin', 
            '60'='2 story 1946+', '70'='2 story 1945-', '75'='2,5 story all ages', 
            '80'='split/multi level', '85'='split foyer', '90'='duplex all style/age', 
            '120'='1 story PUD 1946+', '150'='1,5 story PUD all', 
            '160'='2 story PUD 1946+', '180'='PUD multilevel', '190'='2 family conversion'))

str(all$MSSubClass)

#Visualization of important variables

numericVars <- which(sapply(all, is.numeric)) #index vector numeric variables
factorVars <- which(sapply(all, is.factor)) #index vector factor variables
cat('There are', length(numericVars), 'numeric variables, and', 
    length(factorVars), 'categoric variables')


str(all)


#nuevamente correlacionamos

all_numVar <- all[, numericVars]
cor_numVar <- cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt", tl.cex = 0.7,cl.cex = .7, number.cex=.7)

## Variables importantes

set.seed(2018)
quick_RF <- randomForest(x=all[1:1460,-79], y=all$SalePrice[1:1460], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + 
  geom_bar(stat = 'identity') + 
  labs(x = 'Variables', y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + theme(legend.position="none")



#Above Ground Living Area, y otras variables relacionadas con la superficie (en pies cuadrados)

s1 <- ggplot(data= all, aes(x=GrLivArea)) +
  geom_density() + labs(x='Square feet living area')
s2 <- ggplot(data=all, aes(x=as.factor(TotRmsAbvGrd))) +
  geom_histogram(stat='count') + labs(x='Rooms above Ground')
s3 <- ggplot(data= all, aes(x=X1stFlrSF)) +
  geom_density() + labs(x='Square feet first floor')
s4 <- ggplot(data= all, aes(x=X2ndFlrSF)) +
  geom_density() + labs(x='Square feet second floor')
s5 <- ggplot(data= all, aes(x=TotalBsmtSF)) +
  geom_density() + labs(x='Square feet basement')
s6 <- ggplot(data= all[all$LotArea<100000,], aes(x=LotArea)) +
  geom_density() + labs(x='Square feet lot')
s7 <- ggplot(data= all, aes(x=LotFrontage)) +
  geom_density() + labs(x='Linear feet lot frontage')
s8 <- ggplot(data= all, aes(x=LowQualFinSF)) +
  geom_histogram() + labs(x='Low quality square feet 1st & 2nd')

layout <- matrix(c(1,2,5,3,4,8,6,7),4,2,byrow=TRUE)
multiplot(s1, s2, s3, s4, s5, s6, s7, s8, layout=layout)


#ejemplo de multicolinealidad en este dataset

cor(all$GrLivArea, (all$X1stFlrSF + all$X2ndFlrSF + all$LowQualFinSF))

head(all[all$LowQualFinSF>0, c('GrLivArea', 'X1stFlrSF', 'X2ndFlrSF', 'LowQualFinSF')])


###La variable categórica más importante;Neighborhood

#El primer gráfico muestra la mediana del precio de venta por barrio.
#La frecuencia (número de casas) de cada barrio en el conjunto de entrenamiento se muestra en las etiquetas.

n1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=Neighborhood, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

n2 <- ggplot(data=all, aes(x=Neighborhood)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(n1, n2)


#Calidad global y otras variables de calidad

q1 <- ggplot(data=all, aes(x=as.factor(OverallQual))) +
  geom_histogram(stat='count')
q2 <- ggplot(data=all, aes(x=as.factor(ExterQual))) +
  geom_histogram(stat='count')
q3 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')
q4 <- ggplot(data=all, aes(x=as.factor(KitchenQual))) +
  geom_histogram(stat='count')
q5 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
q6 <- ggplot(data=all, aes(x=as.factor(FireplaceQu))) +
  geom_histogram(stat='count')
q7 <- ggplot(data=all, aes(x=as.factor(PoolQC))) +
  geom_histogram(stat='count')

layout <- matrix(c(1,2,8,3,4,8,5,6,7),3,3,byrow=TRUE)
multiplot(q1, q2, q3, q4, q5, q6, q7, layout=layout)


#La segunda variable categórica más importante; MSSubClass

#la mayoria de las casas son nuevas y tienen uno o dos pisos

ms1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=MSSubClass, y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice
ms2 <- ggplot(data=all, aes(x=MSSubClass)) +
  geom_histogram(stat='count')+
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(ms1, ms2)


#Garaje variables

#correct error
all$GarageYrBlt[2593] <- 2007 #this must have been a typo. GarageYrBlt=2207, YearBuilt=2006, YearRemodAdd=2007.

g1 <- ggplot(data=all[all$GarageCars !=0,], aes(x=GarageYrBlt)) +
  geom_histogram()
g2 <- ggplot(data=all, aes(x=as.factor(GarageCars))) +
  geom_histogram(stat='count')
g3 <- ggplot(data= all, aes(x=GarageArea)) +
  geom_density()
g4 <- ggplot(data=all, aes(x=as.factor(GarageCond))) +
  geom_histogram(stat='count')
g5 <- ggplot(data=all, aes(x=GarageType)) +
  geom_histogram(stat='count')
g6 <- ggplot(data=all, aes(x=as.factor(GarageQual))) +
  geom_histogram(stat='count')
g7 <- ggplot(data=all, aes(x=as.factor(GarageFinish))) +
  geom_histogram(stat='count')

layout <- matrix(c(1,5,5,2,3,8,6,4,7),3,3,byrow=TRUE)
multiplot(g1, g2, g3, g4, g5, g6, g7, layout=layout)


#Variables del sótano

b1 <- ggplot(data=all, aes(x=BsmtFinSF1)) +
  geom_histogram() + labs(x='Type 1 finished square feet')
b2 <- ggplot(data=all, aes(x=BsmtFinSF2)) +
  geom_histogram()+ labs(x='Type 2 finished square feet')
b3 <- ggplot(data=all, aes(x=BsmtUnfSF)) +
  geom_histogram()+ labs(x='Unfinished square feet')
b4 <- ggplot(data=all, aes(x=as.factor(BsmtFinType1))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 1 finished area')
b5 <- ggplot(data=all, aes(x=as.factor(BsmtFinType2))) +
  geom_histogram(stat='count')+ labs(x='Rating of Type 2 finished area')
b6 <- ggplot(data=all, aes(x=as.factor(BsmtQual))) +
  geom_histogram(stat='count')+ labs(x='Height of the basement')
b7 <- ggplot(data=all, aes(x=as.factor(BsmtCond))) +
  geom_histogram(stat='count')+ labs(x='Rating of general condition')
b8 <- ggplot(data=all, aes(x=as.factor(BsmtExposure))) +
  geom_histogram(stat='count')+ labs(x='Walkout or garden level walls')

layout <- matrix(c(1,2,3,4,5,9,6,7,8),3,3,byrow=TRUE)
multiplot(b1, b2, b3, b4, b5, b6, b7, b8, layout=layout)


##Número total de baños

all$TotBathrooms <- all$FullBath + (all$HalfBath*0.5) + all$BsmtFullBath + (all$BsmtHalfBath*0.5)

tb1 <- ggplot(data=all[!is.na(all$SalePrice),], aes(x=as.factor(TotBathrooms), y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)
tb2 <- ggplot(data=all, aes(x=as.factor(TotBathrooms))) +
  geom_histogram(stat='count')
grid.arrange(tb1, tb2)


##Añadir las variables “Edad de la casa”, “Remodelado (Sí/No)” y “Es nuevo”.

all$Remod <- ifelse(all$YearBuilt==all$YearRemodAdd, 0, 1) #0=No Remodeling, 1=Remodeling
all$Age <- as.numeric(all$YrSold)-all$YearRemodAdd
ggplot(data=all[!is.na(all$SalePrice),], aes(x=Age, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)

#As expected, the graph shows a negative correlation with Age (old house are worth less).

cor(all$SalePrice[!is.na(all$SalePrice)], all$Age[!is.na(all$SalePrice)])

#116 casas nuevas en el conjunto de datos.

all$IsNew <- ifelse(all$YrSold==all$YearBuilt, 1, 0)
table(all$IsNew)

ggplot(all[!is.na(all$SalePrice),], aes(x=as.factor(IsNew), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=6) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  theme_grey(base_size = 18) +
  geom_hline(yintercept=163000, linetype="dashed") #dashed line is median SalePrice

summary(all$SalePrice)

all$YrSold <- as.factor(all$YrSold) #the numeric version is now not needed anymore

#Binning Neighborhood

nb1 <- ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=median), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "median", fill='blue') + labs(x='Neighborhood', y='Median SalePrice') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

nb2 <- ggplot(all[!is.na(all$SalePrice),], aes(x=reorder(Neighborhood, SalePrice, FUN=mean), y=SalePrice)) +
  geom_bar(stat='summary', fun.y = "mean", fill='blue') + labs(x='Neighborhood', y="Mean SalePrice") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red") #dashed line is median SalePrice

grid.arrange(nb1, nb2)


all$NeighRich[all$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
all$NeighRich[!all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
all$NeighRich[all$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0
table(all$NeighRich)


##Pies cuadrados totales

all$TotalSqFeet <- all$GrLivArea + all$TotalBsmtSF
ggplot(data=all[!is.na(all$SalePrice),], aes(x=TotalSqFeet, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))


#Como era de esperar, la correlación con SalePrice es muy fuerte (0,78).

cor(all$SalePrice, all$TotalSqFeet, use= "pairwise.complete.obs")

#Los dos posibles valores atípicos parecen “salir” aún más que antes. 
#Al eliminar estos dos valores atípicos, la correlación aumenta un 5%.

cor(all$SalePrice[-c(524, 1299)], all$TotalSqFeet[-c(524, 1299)], use= "pairwise.complete.obs")



#consolidacion vars porche

all$TotalPorchSF <- all$OpenPorchSF + all$EnclosedPorch + all$X3SsnPorch + all$ScreenPorch

cor(all$SalePrice, all$TotalPorchSF, use= "pairwise.complete.obs")

ggplot(data=all[!is.na(all$SalePrice),], aes(x=TotalPorchSF, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = comma)


#Preparación de los datos para la modelización

##Eliminación de variables altamente correlacionadas

dropVars <- c('YearRemodAdd', 'GarageYrBlt', 'GarageArea', 'GarageCond',
              'TotalBsmtSF', 'TotalRmsAbvGrd', 'BsmtFinSF1')

all <- all[,!(names(all) %in% dropVars)]


#eliminando valores atipicos, casas gran tamaño y bajo precio
all <- all[-c(524, 1299),]


##Preprocesamiento de las variables predictoras

numericVarNames <- numericVarNames[!(numericVarNames %in% 
  c('MSSubClass', 'MoSold', 'YrSold', 'SalePrice', 'OverallQual', 'OverallCond'))] 
#numericVarNames was created before having done anything

numericVarNames <- append(numericVarNames, c('Age', 
                            'TotalPorchSF', 'TotBathrooms', 'TotalSqFeet'))

DFnumeric <- all[, names(all) %in% numericVarNames]

DFfactors <- all[, !(names(all) %in% numericVarNames)]
DFfactors <- DFfactors[, names(DFfactors) != 'SalePrice']

cat('There are', length(DFnumeric), 'numeric variables, and', 
    length(DFfactors), 'factor variables')

#calculando y seleccionando vars asimetricas
for(i in 1:ncol(DFnumeric)){
  if (abs(skew(DFnumeric[,i]))>0.8){
    DFnumeric[,i] <- log(DFnumeric[,i] +1)
  }
}

#Normalizing the data

PreNum <- preProcess(DFnumeric, method=c("center", "scale"))
print(PreNum)

DFnorm <- predict(PreNum, DFnumeric)
dim(DFnorm)

DFdummies <- as.data.frame(model.matrix(~.-1, DFfactors))
dim(DFdummies)



#Eliminación de niveles con pocas o ninguna observación en el tren o la prueba

#check if some values are absent in the test set
ZerocolTest <- which(colSums(DFdummies[(nrow(all[!is.na(all$SalePrice),])+1):nrow(all),])==0)
colnames(DFdummies[ZerocolTest])

DFdummies <- DFdummies[,-ZerocolTest] #removing predictors

#check if some values are absent in the train set
ZerocolTrain <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])==0)
colnames(DFdummies[ZerocolTrain])

## [1] "MSSubClass1,5 story PUD all"
DFdummies <- DFdummies[,-ZerocolTrain] #removing predictor
#También se eliminan las variables con menos de 10 “unos” en el conjunto de trenes.

fewOnes <- which(colSums(DFdummies[1:nrow(all[!is.na(all$SalePrice),]),])<10)
colnames(DFdummies[fewOnes])

DFdummies <- DFdummies[,-fewOnes] #removing predictors
dim(DFdummies)

#se eliminaron 49 predictores

combined <- cbind(DFnorm, DFdummies) #combining all (now numeric) predictors into one dataframe 
##Tratamiento de la asimetría de la variable de respuesta

skew(all$SalePrice)

qqnorm(all$SalePrice)
qqline(all$SalePrice)

#Para solucionar esto, estoy tomando el logaritmo de SalePrice.

all$SalePrice <- log(all$SalePrice) #default is the natural logarithm, "+1" is not necessary as there are no 0's
skew(all$SalePrice)

qqnorm(all$SalePrice)
qqline(all$SalePrice)


#Composición de los conjuntos de entrenamiento y prueba

train1 <- combined[!is.na(all$SalePrice),]
test1 <- combined[is.na(all$SalePrice),]


##Modelo de regresión Lasso

set.seed(27042018)
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))

lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], 
                   method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune

min(lasso_mod$results$RMSE)

#usando caret para varImport

lassoVarImp <- varImp(lasso_mod,scale=F)
lassoImportance <- lassoVarImp$importance

varsSelected <- length(which(lassoImportance$Overall!=0))
varsNotSelected <- length(which(lassoImportance$Overall==0))

cat('Lasso uses', varsSelected, 'variables in its model, and did not select',
    varsNotSelected, 'variables.')

#Así que el lazo hizo lo que se supone que tiene que hacer: parece haber tratado
#bien la multicolinealidad al no utilizar cerca del 45% de las variables disponibles en el modelo.

LassoPred <- predict(lasso_mod, test1)
predictions_lasso <- exp(LassoPred) #need to reverse the log to the real values
head(predictions_lasso)


##Modelo XGBoost

xgb_grid = expand.grid(
  nrounds = 1000,
  eta = c(0.1, 0.05, 0.01),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree=1,
  min_child_weight=c(1, 2, 3, 4 ,5),
  subsample=1
)

xgb_caret <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='xgbTree', trControl= my_control, tuneGrid=xgb_grid) 

xgb_caret$bestTune

#hiperparametros
Max_depth=3
eta=0.05
Min_child_weight=4

label_train <- all$SalePrice[!is.na(all$SalePrice)]

# put our testing & training data into two seperates Dmatrixs objects
dtrain <- xgb.DMatrix(data = as.matrix(train1), label= label_train)
dtest <- xgb.DMatrix(data = as.matrix(test1))














