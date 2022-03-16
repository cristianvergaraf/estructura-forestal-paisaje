###
### Este script tiene como objetivo realizar un análisis exploratorio 
### De un conjunto de datos multivariantes.

# Cargamos las paquetes necesarios

library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)

# Directorio de trabajo

setwd("~/GitHub/estructura-forestal-paisaje")

# Cargamos los datos desde un archivo csv.
pl_metricas<- read.csv("metricas_seleccion_ejercicio.csv")

# Seleccionamos solo las filas númericas

X <- pl_metricas[3:18]


########
# Reescalado lineal de los datos

library(scales)
# En esta funcion creamos un dataframe con solo el nombre de las comunas
# De esta manera creamos un dataframe con el largo adecuado para poder 
# Agregar nuevas columnas

rescale.many <- function(dataframe, cols){
  dataframe1 <- data.frame(pl_metricas[1])
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col], "rescaled", sep = ".")
    dataframe1[name] <- rescale(dataframe[,col])
  }
  cat(paste("Hemos rescalada ", length(cols), " variables(s)"))
  dataframe1
}

plantaciones_rescale <- rescale.many(X, c(1:16))  

View(plantaciones_rescale)

#########################
# Normalización de las variables


pl_norm <- scale(X, center = TRUE, scale = TRUE)


## Esta función conserva el dataframe original
scale.many = function(dataframe, cols){
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col], "z", sep = ".")
    dataframe[name] <- scale(dataframe[,col])
  }
  cat(paste("Hemos normalizado ", length(cols), "variable(s)"))
  dataframe
}


############




# configuramos la ventana para hacer los gráficos, vamos agrupar 
# en ventanas de dos por dos.

par(mfrow = c(2,2))

# Una primera aproximación para conocer la distribución de las variables es un 
# Boxplot por variable

sapply(seq(1,16),function(j)boxplot(X[,j],main=colnames(X)[j],xlab="",col="yellow"))

# En un segundo paso generamos un histrograma por variable

par(mfrow=c(2,4))

sapply(seq(1,16),function(j)hist(X[,j],main=colnames(X)[j],xlab="",col="blue",breaks = "Sturges"))
sapply(seq(1,16),function(j)hist(pl_norm[,j],main=colnames(pl_norm)[j],xlab="",col="blue",breaks = "Sturges"))
sapply(seq(2,17),function(j)hist(plantaciones_rescale[,j],main=colnames(plantaciones_rescale)[j],xlab="",col="blue",breaks = "Sturges"))

## En tercer lugar podemos representar distribución de los datos
## de forma continua con una función de densidad.

par(mfrow=c(2,4))
sapply(seq(1,16),function(j)plot(density(X[,j],kernel="gaussian"),main=colnames(X)[j],xlab="",col="blue",lwd=2))

## Relaciones entre variables se pueden evaluar visualmente 
## a través de histogramas

pairs(X[1:5],pch=19,col="black",
      lower.panel = panel.smooth)

pairs(X[6:10],pch=19,col="black",
      lower.panel = panel.smooth)

## Adicionalmente podemos calcular la correlación entre variables

pl_metricas.cor <- cor(X, method = "pearson")
pl_round.cor <- round(pl_metricas.cor, digits = 2) # con esta línea dejamos solo dos dígitos para el resultado

# Definimos el marco para los gráficos
par(mfrow = c(1,1))

# Definimos una escala de colores
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))

## Podemos represesntar las correlaciones por medio de un gráfico

corrplot(pl_round.cor, method = "shade", sade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", number.digits = 2, number.cex = 0.6)

corrplot(pl_metricas.cor, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "upper", 
         diag = F, number.digits = 2, number.cex = 0.6)

######   
# Scatterplots revisar diseno separar por variables. 

# Este opción permite hacer gráficos con scaterplot para entender las relaciones entre variables



ggpairs(X, columns = c(1,4,7)) # podemos seleccionar algunas columnas del dataset


## Podemos agregar una linea de regresión

ggpairs(X, columns = c(1,4,7,8), 
        lower = list(continuous = "smooth"))

## Podemos con una función colocar colores a la linea y el histograma

lowerFn <- function(data, mapping, method = "lm", ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") + 
    geom_smooth(method = method, color = "red", ...)
  p
}

ggpairs(
  X, columns = c(1,3,11), 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 5))
)

