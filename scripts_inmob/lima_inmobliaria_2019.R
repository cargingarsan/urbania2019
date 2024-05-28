#usethis::create_from_github(
#  "https://github.com/cargingarsan/urbania2019.git",
#  destdir = ("E:/proyectos/rstudio_git/ml")
#)



library(archive)
library(tidyverse)
library(DataExplorer)
library(psych)
library(leaflet)
library(collapse)


lima_urb <- read_csv(archive_read("E:/proyectos/rstudio_git/ml/urbania2019/data/archive.zip",
                         file=1), col_types = cols())
           

# elimino las filas de indices

lima_urb$`Unnamed: 0` <- NULL
lima_urb$...1 <- NULL


head(lima_urb)
str(lima_urb)


skimr::skim(lima_urb)
DataExplorer::create_report(lima_urb)
correlacion <- round(cor(lima_urb), 1)

library(archive)
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
library(tidyverse)


lima_urb <- read_csv(archive_read("E:/proyectos/rstudio_git/ml/urbania2019/data/archive.zip",
                                  file=1), col_types = cols())

# elimino las filas de indices
lima_urb$`Unnamed: 0` <- NULL
#lima_urb <- lima_urb  |> rename(id =`Unnamed: 0`) # guardar para separar entre datos prueba y entrenamiento
lima_urb$...1 <- NULL

lima_urb <-  filter(lima_urb, Provincia=="Lima" )
dim(lima_urb)
#7663-7129
#534/7663 o .0696855 = 7%


str(lima_urb)

skimr::skim(lima_urb)



lima_inmob$Dormitorios[lima_inmob$Dormitorios == "5+"] <- "5"
# Convertimos la columna a numérica
lima_inmob$Dormitorios <- as.numeric(lima_inmob$Dormitorios)

# Calculamos el tercer cuartil
tercer_cuartil <- quantile(lima_urb$Precio, 0.75)

# Filtramos los datos hasta el tercer cuartil
datos_filtrados_lima <- lima_urb[lima_urb$Precio <= tercer_cuartil,]

# Creamos el gráfico
ggplot(lima_urb, aes(x=factor(`Estado de Inmueble`), y=Precio))  +
  geom_boxplot(color = "blue", fill = "skyblue") +
  labs(x = "Estado", y = "Precio", title = "Distribución de precios hasta el tercer cuartil")

lima_inmob <- lima_urb |> select(-c(Descripcion, Anunciante, Fecha_pub,match, Direccion, latitud, longitud))



head(lima_inmob) #group_by(Provincia)
str(lima_inmob)

dim(lima_inmob)

summary(lima_inmob$Precio)
#  Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#22000    310000    579500    874293    995000 570000000 


# Calculamos el tercer cuartil
tercer_cuartil <- quantile(lima_inmob$Precio, 0.75)

# Filtramos los datos hasta el tercer cuartil
datos_filtrados <- lima_inmob[lima_inmob$Precio <= tercer_cuartil,]

# Creamos el gráfico
ggplot(datos_filtrados, aes(x = Precio)) +
  geom_histogram(binwidth = 50000, color = "blue", fill = "skyblue") +
  labs(x = "Precio", y = "Frecuencia", title = "Distribución de precios hasta el tercer cuartil")


numericVars <- which(sapply(lima_inmob, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')


lima_inmob_numVar <- lima_inmob[, numericVars]
cor_numVar <- cor(lima_inmob_numVar, use="pairwise.complete.obs") #correlations of lima_inmob numeric variables
cor_numVar

corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")

lima_inmob$Dormitorios <- as.numeric(lima_inmob$Dormitorios)



#para dividir la data se debe eliminar la variable Id
set.seed(1969)
id_train <- sample(1:nrow(lima_inmob), size = 0.7*nrow(lima_inmob), replace = FALSE)

datos_train <- datos[id_train, ]
datos_test  <- datos[-id_train, ]

