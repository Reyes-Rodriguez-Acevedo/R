# para guardar el resultado en txt
# sink("Clustering y heatmaps.txt") 
# ejecutar operaciones

# Directorio de trabajo
getwd()

# Cambiar Directorio de trabajo
setwd("C:/Users/Reyes/Documents/Analisis de Datos y Big Data")

# How To Solve function Rcpp_precious_remove
install.packages("Rcpp")
library(Rcpp)

# Instalar paquete K-medoids: metodo Clara para PAM
install.packages("cluster")
library(cluster)

# Instalar paquete readxl
install.packages("xlsx")
library(xlsx)

# Instalar paquete in factoextra para calcular el analisis de correspondencia 
install.packages("FactoMineR")
library(FactoMineR)

# Instalar paquete factoextra para ayudar en la interpretacion y 
# visualizacion del analisis de correspondencia
install.packages("factoextra")
library(factoextra)

# Instalar paquete tidyverse: conjunto de librerias de R diseñadas para la 
# Ciencia de datos (Data Science)
# https://estadistica-dma.ulpgc.es/cursoR4ULPGC/5b-Tidyverse.html
install.packages("tidyverse")
library(tidyverse)
library(ggpubr)

# Instalar paquete fmsb: para hacer graficos de radar 
install.packages("fmsb")
library(fmsb)

## K-means-clustering

set.seed(101)

# Se simulan datos aleatorios con dos dimensiones
datos <- matrix(rnorm(n = 100*2), nrow = 100, ncol = 2,
                dimnames = list(NULL,c("x", "y")))
datos <- as.data.frame(datos)
class(datos)
dim(datos)
head(datos)
view(datos)
summary(datos)
datos %>% summarise(distinto = n_distinct(x), valor_n = nth(y, 79))

# Se determina la media que va a tener cada grupo en cada una de las dos
# dimensiones. En total 2*4 medias. Este valor se utiliza para separar
# cada grupo de los demas.
media_grupos <- matrix(rnorm(n = 8, mean = 0, sd = 4), nrow = 4, ncol = 2,
                       dimnames = list(NULL, c("media_x", "media_y")))
media_grupos <- as.data.frame(media_grupos)
media_grupos <- media_grupos %>% mutate(grupo = c("a","b","c","d"))
# https://rsanchezs.gitbooks.io/rprogramming/content/chapter9/pipeline.html
# El operador pipeline %>% es útil para concatenar multiples dplyr operaciones
# Con la funcion mutate() podemos computar tranformaciones de variables en un 
# data frame

# Se genera un vector que asigne aleatoriamente cada observación a uno de
# los 4 grupos
datos <- datos %>% mutate(grupo = sample(x = c("a","b","c","d"),
                                         size = 100,
                                         replace = TRUE))
# sample: obtener una serie de elementos al azar dentro de dicho vector

# Se incrementa el valor de cada observacion con la media correspondiente al
# grupo asignado
datos <- left_join(datos, media_grupos, by = "grupo")
datos <- datos %>% mutate(x = x + media_x,
                          y = y + media_y)

jpeg(filename = "plot grupos k-means.jpg", width = 800, height = 600, 
     units = "px", quality = 100)
ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 2.5) +
  theme_bw()
dev.off()
file.show("plot grupos k-means.jpg")

# kmeans(): realiza K-mean-clustering
# centers: número K de clusters que se van a generar
# nstart: determina el número de veces que se va a repetir el proceso (entre 25-50)
# considerando que todas las dimensiones tienen aproximadamente la misma magnitud
# no es necesario escalarlos ni centrarlos. Sino si habria que hacerlo

set.seed(101)
km_clusters <- kmeans(x = datos[, c("x", "y")], centers = 4, nstart = 50)
km_clusters

# Se representa el número de cluster al que se ha asignado cada observación y
# se muestra con un código de color el grupo real al que pertenece.

datos <- datos %>% mutate(cluster = km_clusters$cluster)
datos <- datos %>% mutate(cluster = as.factor(cluster),
                          grupo   = as.factor(grupo))

jpeg(filename = "clusters grupos k-means.jpg", width = 800, height = 600, 
     units = "px", quality = 100)
ggplot(data = datos, aes(x = x, y = y, color = grupo)) +
  geom_text(aes(label = cluster), size = 5) +
  theme_bw() +
  theme(legend.position = "none")
dev.off()
file.show("clusters grupos k-means.jpg")

# Analisis de la clasificacion en clusters
table(km_clusters$cluster, datos[, "grupo"],
      dnn = list("cluster", "grupo real"))
# dnn = nombres dados a las dimensions en el resultado (the dimnames names).

# K-means con USArrests
data("USArrests")
head(USArrests)
str(USArrests)

# Como la magnitud de los valores difiere notablemente entre variables, se 
# procede a escalarlas antes de aplicar el clustering
datos <- scale(USArrests)
# Por defecto la funcion rescale me realiza un escalado de la variables en un 
# rango [0,1] ya que aplica el siguiente metodo para escalar
# x.escalada = ( x−min(x) ) / ( max(x)−min(x) )

# Una forma sencilla de estimar el número K optimo de clusters cuando no se 
# dispone de informacion adicional en la que basarse, es aplicar el algoritmo de
# K-means para un rango de valores de K e identificar aquel valor a partir del 
# cual la reduccion en la suma total de varianza intra-cluster deja de ser 
# sustancial (estrategia o metodo del codo o elbow method)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "wss", k.max = 15, 
             diss = get_dist(datos, method = "euclidean"), nstart = 50)

calcular_totwithinss <- function(n_clusters, datos, iter.max=1000, nstart=50){
  # Esta función aplica el algoritmo kmeans y devuelve la suma total de
  # cuadrados internos.
  cluster_kmeans <- kmeans(centers = n_clusters, x = datos, iter.max = iter.max,
                           nstart = nstart)
  return(cluster_kmeans$tot.withinss)
}

# Se aplica esta función para diferentes valores de k
total_withinss <- map_dbl(.x = 1:15,
                          .f = calcular_totwithinss,
                          datos = datos)
total_withinss

data.frame(n_clusters = 1:15, suma_cuadrados_internos = total_withinss) %>%
  ggplot(aes(x = n_clusters, y = suma_cuadrados_internos)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:15) +
  labs(title = "Evolución de la suma total de cuadrados intra-cluster") +
  theme_bw()

# Grafico de Clusters de Componentes Principales
set.seed(123)
km_clusters <- kmeans(x = datos, centers = 4, nstart = 50)

jpeg(filename = "Clusters de Componentes Principales.jpg", width = 800, 
     height = 600, units = "px", quality = 100)
# Las funciones del paquete factoextra emplean el nombre de las filas del
# dataframe que contiene los datos como identificador de las observaciones.
# Esto permite añadir labels a los gráficos.
fviz_cluster(object = km_clusters, data = datos, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme(legend.position = "none")

dev.off()
file.show("Clusters de Componentes Principales.jpg")

# Centros de gravedad
centrosk <- km_clusters$centers
row.names(centrosk) <-c("Cluster 1","Cluster 2","Cluster 3","Cluster 4")
centrosk<- as.data.frame(centrosk)
maximos <-apply(centrosk,2,max)
minimos <- apply(centrosk,2,min)
centrosk <- rbind(minimos,centrosk)
centrosk <- rbind(maximos,centrosk)
centrosk

# Grafico de Comparacion de Clusters
# https://r-charts.com/es/ranking/radar-chart/

jpeg(filename = "Comparacion Clusters.jpg", width = 800, height = 600, 
     units = "px", quality = 100)

radarchart(centrosk, maxmin = TRUE, axistype = 4, axislabcol = "slategray4",
           centerzero = FALSE,seg = 8,cglcol = "gray67",
           pcol = c("black","red","green","blue"),
           plty = 1,
           plwd = 5,
           title = "Comparacion de Clusteres"
)

legenda <- legend(1.5,1,legend = c("Cluster 1","Cluster 2","Cluster 3","Cluster 4"),
                  seg.len=1.4,
                  title = "clusteres",
                  pch=21,
                  bty = "n",lwd = 3, y.intersp = 1,horiz = FALSE,
                  col=c("black","red","green","blue"))

dev.off()
file.show("Comparacion Clusters.jpg")


## K-medoids clustering (PAM)
data("USArrests")
str(USArrests)

datos <- scale(USArrests)
fviz_nbclust(x = datos, FUNcluster = pam, method = "wss", k.max = 15,
             diss = dist(datos, method = "manhattan"))

set.seed(123)
pam_clusters <- pam(x = datos, k = 4, metric = "manhattan")
pam_clusters

jpeg(filename = "Clustering PAM de Componentes Principales.jpg", width = 800, 
     height = 600, units = "px", quality = 100)

fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

dev.off()
file.show("Clustering PAM de Componentes Principales.jpg")

# Como en k-medoids no hay centroides, no se muestran en la representación ni
# tampoco las distancias desde este al resto de observaciones

# Como hay más de 2 variables, se están representando las 2 primeras componentes
# de un PCA. Se tienen que calcular el PCA y extraer las proyecciones almacenadas
# en el elemento x.
medoids <- prcomp(datos)$x

# Se seleccionan únicamente las proyecciones de las observaciones que son medoids
medoids <- medoids[rownames(pam_clusters$medoids), c("PC1", "PC2")]
medoids <- as.data.frame(medoids)

# Se emplean los mismos nombres que en el objeto ggplot
colnames(medoids) <- c("x", "y")

# Creación del gráfico

jpeg(filename = "Clustering PAM de Componentes Principales.jpg", width = 800, 
     height = 600, units = "px", quality = 100)

fviz_cluster(object = pam_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  # Se resaltan las observaciones que actúan como medoids
  geom_point(data = medoids, color = "firebrick", size = 2) +
  labs(title = "Resultados clustering PAM") +
  theme(legend.position = "none")

dev.off()
file.show("Clustering PAM de Componentes Principales.jpg")

## Clara: Clustering Large Applications, es un metodo que combina la idea de 
# K-medoids con el resampling para que pueda aplicarse a grandes volumenes de 
# datos
clara_clusters <- clara(x = datos, k = 4, metric = "manhattan", stand = TRUE,
                        samples = 50, pamLike = TRUE)
clara_clusters

jpeg(filename = "Clustering Clara de Componentes Principales.jpg", width = 800, 
     height = 600, units = "px", quality = 100)

fviz_cluster(object = clara_clusters, data = datos, ellipse.type = "t",
             repel = TRUE) +
  theme_bw() +
  labs(title = "Resultados clustering CLARA") +
  theme(legend.position = "none")

dev.off()
file.show("Clustering Clara de Componentes Principales.jpg")

## Hierarchical clustering
# Agglomerative clustering (bottom-up): el agrupamiento se inicia en la base del
# arbol, donde cada observacion forma un cluster individual. Los clusters se van
# combinado a medida que la estructura crece hasta converger en una unica rama 
# central
# Hay que estandarizar los datos a la misma escala


# sink()
