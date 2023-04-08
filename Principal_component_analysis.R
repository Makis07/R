#Grettel Alexandra Ingaruca Rivera

################################################################################
################################################################################

#TEMA: TUMORES DEL CANCER DE MAMA

#Numero de casos: 569
#Numero de Variables numericas: 10
#
#Conjunto de datos de cancer de mama 
#Fuente: Kaggle
###https://www.kaggle.com/datasets/yasserh/breast-cancer-dataset?select=breast-cancer.csv

#La data esta compuesta por caracteristicas fisicas de tumores por cancer de mama.
#Que mide cada variable?
#radio_promedio: radio de lobulos
#textura_promedio: promedio de la textura de la superficie
#perimetro_promedio: perimetro exterior de los lobulos
#area_promedio: area media de los lobulos
#suavidad_promedio: media de los niveles de suavidad
#compacidad_promedio: media de compacidad
#concavidad_promedio: media de concavidad
#puntos_concavos_promedio: media de puntos cocave
#simetria_promedio: media de simetria
#dimension_fractal_promedio: media de dimension fractal

################################################################################
################################################################################

#DATA

#llamar a la libreria
library(tidyverse)
library(readr)

#leer un archivo csv en R
data_cancermama <- readr::read_csv('C:/Users/HP/OneDrive/Escritorio/breast-cancer.csv')

#convertir en DataFrame
data_cancermama <- as.data.frame(data_cancermama)

#visualizar la nueva DataFrame
View(data_cancermama)

#identificar los nombres de la columnas del DataFrame
colnames(data_cancermama)

#seleccionar y eliminar desde la columna 13 a 32
data_cancermama <- data_cancermama %>%
  select(-c(13:32))
         
#renombrar las columnas(espanol)
colnames(data_cancermama) <- c("ID","diagnostico", "radio_promedio", 
                               "textura_promedio", "perimetro_promedio", 
                               "area_promedio", "suavidad_promedio",
                               "compacidad_promedio", "concavidad_promedio", 
                               "puntos_concavos_promedio", "simetria_promedio", 
                               "dimension_fractal_promedio")

#visualizar los nuevos nombres de las columnas
colnames(data_cancermama)

################################################################################
################################################################################

#ANALISIS DE COMPONENTES PRINCIPALES

#eliminar la columna 1 y 2, al resultado se le nombra"subdata"
subdata <- data_cancermama |> select(-1,-2)

#visualizar la nueva data "subdata
subdata

#1.Analisis exploratorio de datos
#Analisis visual
#visualizar los nombres de las columnas de la nueva data "subdata"
colnames(subdata)

#visualizar la relacion entre la variable radio_promedio y perimetro_promedio en un grafico de puntos
subdata |> ggplot(aes(x=radio_promedio, y=perimetro_promedio)) + geom_point()

#visualizar la relacion entre la variable suavidad_promedio y compacidad_promedio en un grafico de puntos
subdata |> ggplot(aes(x=suavidad_promedio, y=compacidad_promedio)) + geom_point()

#visualizar la relacion entre la variable perimetro_promedio y area_promedio en un grafico de puntos
subdata |> ggplot(aes(x=perimetro_promedio, y=area_promedio)) + geom_point()

#analisis de las relaciones entre las variables, a la cual se le denomina "matrixcorrelacional"
matrixcorrelacional <- cor(subdata)

#visualizar la matriz de correlacional
matrixcorrelacional

#empleo de la funcion cor.plot() para visualizar la relacion entre las variables
cor.plot(matrixcorrelacional)

#se identifica la similitud entre las variables radio_promedio y perimetro_promedio
radio_promedio ~ perimetro_promedio

#eliminar la variable perimetro_promedio
#se modifica la subdata 
subdata <- subdata %>%
  select(-c(3))

#visualizar los nombres de la columnas de la subdata modificada
colnames(subdata)

################################################################################
################################################################################

#VERIFICACION DE SUPUESTOS SOBRE LA MATRIZ CORRELACIONAL

##Kaiser-Meyer-Olkin (KMO)
#analizar si los datos son adecuados para realizar el analisis
#si el valor es cercano al 1 significa una alta correlacion global para el analisis
psych::KMO(subdata)#Overall MSA = 0.76

#Prueba de Esfericidad de Bartlett
# H0: Es una matriz de identidad (Las variables analizadas NO están correlacionadas en la muestra).
# H1: No es una matriz de identidad (Las variables analizadas SÍ están correlacionadas en la muestra).
cortest.bartlett(matrixcor,n=dim(subdata)[1])#$p.value=0, menor a 0.05, es decir, se rechaza la H0.

################################################################################
################################################################################

#PCA
#Determinar el número de componentes
scree(subdata, pc=TRUE, factors=FALSE)#se visualiza en la grafica el 2 como codo.

#Analizar los componentes principales, se emplea la funcion prcomp(), dicho producto se denomina "pc"
pc <- prcomp(x=subdata, scale=TRUE, center=TRUE)
pc

################################################################################
################################################################################

#INTERPRETACION

#Variabilidad explicada
#indetificar la proporcion de varianza y la proporcion acumulada, se emplea la funcion summary()
summary(pc)

#PC1 y PC2 tienen 0.5302 y 0.2505 de variabilidad explicada.
#en total 0.7807.

#visualizar la varianza explicada por los componentes principales, utilizando la funcion fviz_eig()
fviz_eig(pc)

#Cargas de cada PC
#El PC1 esta fuertemente relacionado a las variables puntos_concavos_promedio, concavidad_promedio y
#compacidad_promedio y de forma inversa. Y el PC2, a la dimension_fractal_promedio.
pc$rotation
pc$x

################################################################################
################################################################################

#UTILIZACION DEL PCA

#creación de variables PC1 y PC2, las cuales se incluyeron en la data inicial "nueva_data_cancermama"
nueva_data_cancermama <- data_cancermama |> mutate( pc1 = pc$x[,1], pc2 = pc$x[,2])

#se llama a la librerya BBmisc, biblioteca empleada para la limpieza y transformacion de datos
library(BBmisc)

#creacion de la variable pc1_norm, la cual se agrego a la data 
nueva_data_cancermama$pc1_norm = normalize(nueva_data_cancermama$pc1, 
                                method = "range", 
                                margin=2, # by column
                                range = c(0, 100))

#creacion de la variable pc2_norm, la cual se agrego a la data 
nueva_data_cancermama$pc2_norm = normalize(nueva_data_cancermama$pc2, 
                                           method = "range", 
                                           margin=2, # by column
                                           range = c(0, 100))

#Visualizar "nueva_data_cancermama"
View(nueva_data_cancermama)

################################################################################
################################################################################

#ANALISIS DESCRIPTIVO

# Cuál son los 10 pacientes con mayor radio, puntos concavos y textura?
nueva_data_cancermama|> 
  select(3) |> 
  arrange(desc(radio_promedio)) |> 
  head(10)

nueva_data_cancermama |> 
  select(10) |> 
  arrange(desc(puntos_concavos_promedio)) |> 
  head(10)

nueva_data_cancermama |> 
  select(4) |> 
  arrange(desc(textura_promedio)) |> 
  head(10)

#Cuál son las caracteristicas de los 10 pacientes con menor valor según el componente (normalizado) creado?
#Qué similitudes hay?
nueva_data_cancermama |> 
  select(3,4,10,15) |> 
  arrange(pc1_norm) |> 
  head(10)
#presentan mayor tamaño de radio, textura y puntos concavos

#visualizar los resultados de analisis de los PC1 y PC2 en los datos de los pacientes
fviz_pca_ind(pc)

################################################################################
################################################################################

#EXPLORACION Y ESTANDARIZACION

#aplicar la funcion scale() a la subdata, la cual es convertida en DataFrame, y se le denomina data.est
data.est <- as.data.frame(scale(subdata))

#aplicar la funcion str() para obtener informacion sobre la estructura de un objeto
str(data.est)

################################################################################
################################################################################

#DISTANCIA EUCLIDEANA

#seleccion de euclideana como metodo de distancia, aplicado a la data.est, el producto de ello se le denomina "dist.eucl"
dist.eucl <- dist(data.est, method = "euclidean")

#aplicar la funcion matrix() a la dist.eucl, los valore sque contenga son redondeados por medio de la fauncion round()
round(as.matrix(dist.eucl), 1)

#visualizar una matriz de distancia por medio de la funcion fviz_dist()
fviz_dist(dist.eucl, 
          gradient = list(low = "white", 
                          high = "purple"))

#llamar a la libreria corrplot, la cual se emplea para visualizar matrices de correlacion
library(corrplot)

#visualizar la matriz dist.eucl, por medio de la funcion corrplot()
corrplot(as.matrix(dist.eucl), 
         is.corr = FALSE, 
         method = "color")

#Identificar el número recomendado de clusters. Aplica tanto para jerarquico alomerativo como divisivo
#emplear los tres metodos
fviz_nbclust(subdata, hcut, diss=dist.eucl,method = "gap_stat",verbose = F)#1 clusters
fviz_nbclust(subdata, hcut, diss=dist.eucl,method = "silhouette",verbose = F)#2 clusters
fviz_nbclust(subdata, hcut, diss=dist.eucl,method = "wss",verbose = F)#2 clusters

################################################################################
################################################################################

#CLUSTER JERARQUICO AGLOMERATIVO: AGNES

#seleccionar 2 clusters, la funcion a utilizar es "agnes", el metodo de distancia entre clusters es la minima distancia
#empleo de la distancia calculada anteriormente "dist.eucl, todo ello aplicado a la funcion hcut()
#al resultado se le denomina "aglomerativo"
aglomerativo = hcut(dist.eucl, k = 2, hc_func = 'agnes', hc_method = "single")

#Grafico de la silueta
#emplear la funcion fviz_silhouette
fviz_silhouette(aglomerativo, label=TRUE)

#emplear la funcion fviz_dend
fviz_dend(aglomerativo, rect = TRUE, cex = 0.5)

#guardar la columna cluster de la data aglomerativo en la columna llamada aglomerativo de la data subdata
subdata$aglomerativo = aglomerativo$cluster

#aplicar la funcion as.factor a la columna aglomerativo de la data subdata
#convertir ello en la nuevacolumna aglomerativo en la data subdata
subdata$aglomerativo = as.factor(subdata$aglomerativo)

#visualizar los 2 cluster mediante la funcion fviz_cluster
fviz_cluster(object = list(data=subdata[, -10], cluster = subdata$aglomerativo),
             geom = c("text"), 
             ellipse.type = "convex")

################################################################################
################################################################################

# K-MEANS

#llamar a la libreria factoextra, la cual proporciona herramientas para la visualizacion y analisis de resultados 
library(factoextra)

#Selección del número de clsters para el empleo del metodo K-means
fviz_nbclust(subdata[, -10],diss=dist.eucl, kmeans, method = "wss", k.max = 10) +
  labs(subtitle = "Método Elbow") + theme_bw()#2

fviz_nbclust(subdata[, -10], diss=dist.eucl, kmeans, method = "silhouette", k.max = 10) +
  labs(subtitle = "Silhouette method")#2

fviz_nbclust(subdata[, -10],diss=dist.eucl, kmeans, method = "gap_stat", k.max = 10) +
  labs(subtitle = "Método gap_stat") + theme_bw()#1

#Metodo de participacion: K-means
km <- kmeans(subdata[, -10], 
             centers = 2,     # Número de Cluster
             iter.max = 100,  # Número de iteraciones máxima
             algorithm = "Lloyd")

#Explorando el cluster creado
km$size
prop.table(km$size)
km$centers

#visualizar los 2 cluster mediante la funcion fviz_cluster
library(factoextra)
fviz_cluster(km, data = subdata[, -10], ellipse.type = "convex") +
  theme_classic()

################################################################################
################################################################################

#nombrar "p1" al grafico mostrando los clusters segun AGNES  
p1 <- fviz_cluster(object = list(data=subdata[, -10], cluster = subdata$aglomerativo),
             geom = c("text"), 
             ellipse.type = "convex") + theme_classic()  + ggtitle('AGNES')

#nombrar "p2" al grafico mostrando los clusters segun K-means  
p2 <- fviz_cluster(km, data = subdata[, -10], ellipse.type = "convex") +
  theme_classic() + ggtitle('K-mean')

#llamar a la libreria ggpur
library(ggpubr)

#aplicando la funcion ggparrange a las variables p1 y p2, para realizar la comparacion
ggarrange(p1, p2)

#Cual agrupa de mejor manera?
#el grafico de K-means genera una mejor visualizacion de los casos de cancer de mama,
#debido a que separa en dos clusters de forma notoria, y cuyos clusters demuestran una 
#concentracion de casos cada uno.
#A comparacion del grafico de AGNES, que ssi bien tambien separa en dos grupos,
#uno de estos esta compuesto por un caso. ello demuestra una erronea agrupacion. 

################################################################################
################################################################################

#visualizar los nombres de las columnas de la data llamada subdata
colnames(subdata)

#Tomar la la data denominada subdata, y crear una variable llamada k-mean, que se define
#como la variable "cluster" de "km". Agrupa dichos datos por la variable kmean. 
#calcula el promedio de las variables mencionadas, y retorna para kmean. 
resumen = subdata %>% mutate(kmean=km$cluster) %>% group_by(kmean) %>% summarise(
  radio_promedio = mean(radio_promedio),
  textura_promedio = mean(textura_promedio), 
  area_promedio=mean(area_promedio),
  suavidad_promedio=mean(suavidad_promedio),
  compacidad_promedio=mean(compacidad_promedio),
  concavidad_promedio=mean(concavidad_promedio),
  puntos_concavos_promedio=mean(puntos_concavos_promedio),
  simetria_promedio=mean(simetria_promedio),
  dimension_fractal_promedio=mean(dimension_fractal_promedio))

#visualizar "resumen"
View(resumen)

#el grupo 1 se caracteriza por altos valores en las variables radio_promedio", 
#"textura_promedio", area_promedio", "suavidad_promedio","compacidad_promedio", 
#"concavidad_promedio", "puntos_concavos_promedio", "simetria_promedio".A comparacion 
#del grupo 2 que presenta bajos valores en las variables mencionadas anteriormente. 
#el valor de la variable dimension_fractal_promedio es similar en ambos grupos, por lo 
#tanto no es una variable relevante.

################################################################################
################################################################################

#columna que visualiza el grupo al que pertenece cada paciente
km$cluster

#la columna cluster de la data km sera la nueva columna de subdata, se denominara kmean
subdata$kmean = km$cluster
#aplicar la funcion factor a la columna de kmean de subdata, y esta ser la nueva columna 
#denominada kmean de subdata
subdata$kmean = factor(subdata$kmean)

#Extraer la primera columna de pc, para que sea una nueva columna denominada pca1 en la subdata
subdata$pca1 = pc$x[,1]

#Extraer la segunda columna de pc, para que sea una nueva columna denominada pca2 en la subdata
subdata$pca2 = pc$x[,2]

#Extraer la columna diagnostico de la data original, para ser una nueva columna denominada
#"diagnostico" en la subdata
subdata$diagnostico = data_cancermama$diagnostico

ggplot(subdata) +
  geom_point( aes(x = pca1, y = pca2, color = kmean, shape = diagnostico), size = 2 ) +
  theme_bw()

#se visualiza los grupos 1 y 2, por colores,y el diagnostico se encuentra representado por figuras,
#siendo los triangulos los casos con tumores malignos, y los circulos la presencia de tumores benignos. 

#El grupo 1 identificado por k-mean identifica correctamente los casos benignos. Y el grupo 2, los casos malignos.
#Algunos casos malignos se presentan dentro del grupo mayoritariamente benigno, dicho casos malignos se
#encuentran cerca del grupo maligno, dicha motivo podria ser la razon por la que el logaritmo  no logro 
#identificar correctamente.

