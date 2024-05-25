devtools::install_github("dgonxalex80/paqueteMODELOS")
library(paqueteMODELOS)
library(factoextra)

data("vivienda") # Datos
head(vivienda,5)
str(vivienda)

#validar datos faltantes

valores_faltantes <- map_dbl(vivienda, ~sum(is.na(.)))


#borrar piso parqueaderos y barrio, las dos primeras por exceso de datos faltantes
#la var barrio porque no aporta al analisis, es una etiqueta

borrar <- c("piso","parqueaderos","barrio")
vivienda <- vivienda[ ,!(names(vivienda) %in% borrar)]

#conteo valores faltantes
vivienda <- vivienda[complete.cases(vivienda),]
sum(is.na(vivienda)) # Conteo de valores faltantes

#se puede resumir con case when
# Zona

library(dplyr)

vivienda <- vivienda %>%
  mutate(zona = case_when(
    zona == "Zona Centro" ~ "1",
    zona == "Zona Norte" ~ "2",
    zona == "Zona Oeste" ~ "3",
    zona == "Zona Oriente" ~ "4",
    zona == "Zona Sur" ~ "5",
    TRUE ~ zona
  ))



# Tipo                         
vivienda <- vivienda %>% 
  mutate(tipo = case_when(
    tipo == "Casa" ~ "1" ,
    tipo == "Apartamento" ~ "2",
    TRUE ~ tipo
  ))

#converir character a numeric mediante as.numeric
vivienda$tipo <- as.numeric(vivienda$tipo)

head(vivienda,5) # Tabla

correlacion <- round(cor(vivienda), 1)
# corrplot(correlacion, number.cex = 0.5, method="number")
corPlot(correlacion, number.cex = 0.5)



#Entre las correlaciones más significativas,destacan:
  
#  preciom y estrato: 0.6 (positiva), a mayor estrato, mayor precio de la vivienda.
#areaconst y preciom: 0.7 (positiva), a mayor área, mayor precio de la vivienda.
#banios y preciom: 0.7 (positiva), entre más cantidade baños hayan en la vivienda, mayor es el precio de la misma.
#banios y areaconst: 0.6 (positiva), entre más grande sea el área de la vivienda, hay una mayor cantidad de baños.
#habitaciones y areaconst: 0.5 (positiva), entre más grande sea el área de la vivienda, hay una mayor cantidad de habitaciones.
#tipo y areaconst: - 0.5 (negativa), con la recodificación hecha, las casas quedaron identificadas con 1 y los apartamentos 
#con 2, por tanto, esto significa que el área de las casas es mucho más grande que el área de los apartamentos.
#habitaciones y banios: 0.6 (positiva), entre más habitaciones haya en una vivienda, mayor es el número de baños.
#habitaciones y tipo: -0.5 (negativa), con la recodificación hecha, las casas quedaron identificadas con 1 y los apartamentos con 2, 
#por tanto, esto significa que el número de habitaciones en las casas, es mayor que el número de habitaciones en los apartamentos.


ggplot(vivienda, aes(x = areaconst, y = preciom)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Agregar la línea de tendencia
  labs(title = "Gráfico de Dispersión con Línea de Tendencia: Precio vs Área Construida", 
       x = "Área Construida (metros cuadrados)", 
       y = "Precio (millones de pesos COP)")

hist(x = vivienda$zona, 
     main = "Histograma de Zonas", 
     xlab = "Zona", ylab = "Frecuencia",
     col = "purple") # zona

hist(x = vivienda$estrato, 
     main = "Histograma de Estratos", 
     xlab = "Estrato", ylab = "Frecuencia",
     col = "red") # estrato

hist(x = vivienda$preciom, 
     main = "Histograma de Precios", 
     xlab = "Precio", ylab = "Frecuencia",
     col = "green") # preciom


hist(x = vivienda$areaconst, 
     main = "Histograma de Áreas", 
     xlab = "Área", ylab = "Frecuencia",
     col = "white") # areaconst

hist(x = vivienda$banios, 
     main = "Histograma de Número de Baños", 
     xlab = "Número de Baños", ylab = "Frecuencia",
     col = "blue") # banios


hist(x = vivienda$habitaciones, 
     main = "Histograma de Número de Habitaciones", 
     xlab = "Número de Habitaciones", ylab = "Frecuencia",
     col = "pink") # habitaciones

viv_varianza <-map_dbl(vivienda, ~var(.))
                       

#Para aplicar el ACP se emplea la función prcomp(), pues por medio de esta función se pueden estandarizar
#las variables para que tengan media 0 y desviación estándar 1,
#seleccionando variables con mayor correlacion y numericas de origen

##caso 1 incluyendo todas las variables en el modelo


pca <- prcomp(vivienda[3:7], scale = TRUE)
res.pca <- prcomp(vivienda[3:7])
fviz_eig(res.pca, addlabels = TRUE)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#FF7F00",  "#034D94"),
             repel = TRUE     # Avoid text overlapping
)

k2 <- kmeans(vivienda_2, centers = 4, nstart = 25)
fviz_cluster(k2, data = vivienda[3:7])


res4 <- hcut(vivienda[3:7], k = 4, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))













##caso 2 seleccionando cinco variables de interes 
vivienda_2 <- vivienda[1:8319,3:7]

pca <- prcomp(vivienda_2, scale = TRUE)
res.pca <- prcomp(vivienda_2)
fviz_eig(res.pca, addlabels = TRUE)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#FF7F00",  "#034D94"),
             repel = TRUE     # Avoid text overlapping
)

k2 <- kmeans(vivienda_2, centers = 4, nstart = 25)
fviz_cluster(k2, data = vivienda_2)


res4 <- hcut(vivienda_2, k = 4, stand = TRUE)
fviz_dend(res4, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF","green","black"))



