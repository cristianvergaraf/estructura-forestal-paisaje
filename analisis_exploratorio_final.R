### Fecha: 16/03/2022

### Este script tiene como objetivo realizar un análisis exploratorio 
### de un conjunto de datos multivariantes.

# Cargamos las paquetes necesarios

library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(scales)
library(tidyr)
#library(qgraph)

# Directorio de trabajo

setwd("~/GitHub/estructura-forestal-paisaje")
setwd("C:/Users/crist/Documents/GitHub/estructura-forestal-paisaje")

# Cargamos los datos desde un archivo csv.

pl_metricas<- read.csv("metricas_seleccion_ejercicio.csv")
X <- pl_metricas[3:18]
pl_metricas$ID_row <- seq(1:length(pl_metricas$LID))


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
    dataframe1[name] <- scales::rescale(dataframe[,col])
  }
  cat(paste("Hemos rescalada ", length(cols), " variables(s)"))
  dataframe1
}

plantaciones_rescale <- rescale.many(X, c(1:16))

#write.csv(plantaciones_rescale, file = "pl_rescale.csv")

#########################
# Normalización de las variables

## Esta función conserva el dataframe original
scale.many = function(dataframe, cols){
  dataframe1 <- data.frame(pl_metricas[1])
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col], "z", sep = ".")
    dataframe1[name] <- scale(dataframe[,col])
  }
  cat(paste("Hemos normalizado ", length(cols), "variable(s)"))
  dataframe1
}

pl_norm <- scale.many(X, c(1:16))

#write.csv(pl_norm, file = "pl_norm.csv")

#pl_norm <- scale(X, center = TRUE, scale = TRUE)

############


# Configuramos la ventana para hacer los gráficos, vamos agrupar 
# en ventanas de dos por dos.

par(mfrow = c(2,2))

# Una primera aproximación para conocer la distribución de las variables es un 
# Boxplot por variable

sapply(seq(1,16),function(j)boxplot(X[,j],main=colnames(X)[j],xlab="",col="#FF8247"))
sapply(seq(1,16),function(j)boxplot(pl_norm[,2:17][,j],main=colnames(pl_norm[,2:17])[j],xlab="",col="#00FF7F"))
sapply(seq(1,16),function(j)boxplot(plantaciones_rescale[,2:17][,j],main=colnames(plantaciones_rescale[,2:17])[j],xlab="",col="yellow"))

par(mfrow = c(1,1))

boxplot(X[,1:8], notch = TRUE, boxfill = "royalblue", whiskcol = "blue",
        pch = 16, outcol = "firebrick")

boxplot(pl_norm[,2:17], notch = TRUE, boxfill = "royalblue", whiskcol = "blue",
        pch = 16, outcol = "firebrick")

boxplot(plantaciones_rescale[,3:17], notch = TRUE, boxfill = "royalblue", whiskcol = "blue",
        pch = 16, outcol = "firebrick")


# En un segundo paso generamos un histrograma por variable

par(mfrow=c(2,4))

sapply(seq(1,16),function(j)hist(X[,j],main=colnames(X)[j],xlab="",col="#FF8247",breaks = "Sturges"))
sapply(seq(2,17),function(j)hist(pl_norm[,j],main=colnames(pl_norm)[j],xlab="",col="#00FF7F", breaks = "Sturges"))
sapply(seq(2,17),function(j)hist(plantaciones_rescale[,j],main=colnames(plantaciones_rescale)[j],xlab="",col="yellow",breaks = "Sturges"))


## En tercer lugar podemos representar distribución de los datos
## de forma continua con una función de densidad.

par(mfrow=c(2,4))
sapply(seq(1,16),function(j)plot(density(X[,j],kernel="gaussian"),main=colnames(X)[j],xlab="",col="#FF8247",lwd=2))
sapply(seq(2,17),function(j)plot(density(pl_norm[,j],kernel="gaussian"),main=colnames(pl_norm)[j],xlab="",col="#00FF7F",lwd=2))
sapply(seq(2,17),function(j)plot(density(plantaciones_rescale[,j],kernel="gaussian"),main=colnames(plantaciones_rescale)[j],xlab="",col="yellow",lwd=2))

## Relaciones entre variables se pueden evaluar visualmente 
## a través de histogramas

pairs(X[1:5],pch=19,col="black",
      lower.panel = panel.smooth)

pairs(X[6:10],pch=19,col="black",
      lower.panel = panel.smooth)

pairs(X[c(3,5,8)],pch=19,col="black",
      lower.panel = panel.smooth)


pairs(X[1:16],pch=19,col="black",
      lower.panel = panel.smooth)

#### Adicionalmente podemos calcular correlacion entre variables

pl_metricas.cor <- cor(X, method = "pearson")
pl_round.cor <- round(pl_metricas.cor, digits = 2) # con esta línea dejamos solo dos dígitos para el resultado

pl_norm_metricas.cor_pearson <- cor(pl_norm[2:17], method = "pearson", use = "complete.obs")
pl_norm_metricas.cor_spearman <- cor(pl_norm[2:17], method = "spearman", use = "complete.obs")




pl_rescale_metricas.cor <- cor(plantaciones_rescale[2:17], method = "pearson")

N_big_correlation <-function(dato, umbral, sentido){
  
  if (sentido == "mayor"){
    Big_return <- sum(dato >= abs(umbral) & dato < abs(1.1)) /2 
    return (Big_return)
  
    }
  if (sentido == "menor"){
        Big_return <- sum(dato > abs(0) & dato < abs(umbral)) /2 
        return (Big_return)
  }
  
  
}


n_correlaciones <- function(datos){
  A <-  (sum(datos>= abs(0) & datos <= abs(0.3)) /2)
  B <-  (sum(datos>= abs(0.3) & datos <= abs(0.9)) /2)
  C <-  (sum(datos > abs(0.9) & datos < abs(1)) /2)
  final = c(A,B,C)
  return(final)
}

sum(pl_norm_metricas.cor_pearson> abs(.0) & pl_norm_metricas.cor_pearson < abs(0.3))/2
sum(pl_norm_metricas.cor_pearson> abs(.3) & pl_norm_metricas.cor_pearson < abs(0.9))/2

correlaciones_normal_0.3 <- data.frame(pl_norm_metricas.cor_pearson) %>%
  mutate(Metricas_1 = rownames(pl_norm_metricas.cor_pearson)) %>% relocate(Metricas_1) %>% pivot_longer(-Metricas_1, names_to = "Metricas_2") %>%
  filter(value > -0.3 & value < 0.3)
  
  
pivot_wider(correlaciones_normal_0.3, names_from = Metricas_2, values_from = value)


correlaciones_normal_0.3

### Eliminar las duplicaciones

corr_menores_0.3 <- distinct(correlaciones_normal_0.3,value, .keep_all = TRUE)
# write.csv(corr_menores_0.3, file = "corr_menores_0.3.csv")



table(corr_menores_0.3$Metricas_1)
table(corr_menores_0.3$Metricas_2)


n_correlaciones(pl_norm_metricas.cor_pearson) ### Esto da poco hay que revisar
n_correlaciones(pl_norm_metricas.cor_spearman) ### Correlaciones poco !!!

N_M30 <- N_big_correlation(pl_metricas.cor,0.00, "mayor")
N_m30 <- N_big_correlation(pl_metricas.cor,0.30, "menor")

totR = (ncol(pl_metricas.cor)) * (ncol(pl_metricas.cor)-1)/2
print(totR)

print((N_M30)/totR)*100 ## Revisar estos resultados, parecen incorrectos
print((N_m30)/totR)*100 ## Revisar estos datos parecen incorrectos


### POsibilidades de multicolinealidad usando el determinante
det(pl_metricas.cor) 

options(scipen = 9999)
det(pl_metricas.cor) ### Si el determinante es mayor a 0.00001 entonces la multicolinealidad no es un problema
# ALERTA DE MULTICOLINEALIDAD en estos datos

## MAtrices son iguales

# Definimos el marco para los gráficos
par(mfrow = c(1,1))

# Definimos una escala de colores
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))

## Podemos represesntar las correlaciones por medio de un gráfico

windows()
corrplot(pl_norm_metricas.cor_pearson, method = "shade", sade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", number.digits = 2, number.cex = 0.6)


corrplot(pl_norm_metricas.cor_spearman, method = "shade", sade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", number.digits = 2, number.cex = 0.6)



corrplot(pl_metricas.cor, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "upper", 
         diag = F, number.digits = 2, number.cex = 0.6)


#######################################################   

### Scatterplots revisar diseno separar por variables #### 

# Este opción permite hacer gráficos con scaterplot para entender las relaciones entre variables

ggpairs(pl_metricas, 
        columns = c("PLAND","GYRATE_MN"))+ # podemos seleccionar algunas columnas del dataset
geom_text(
  label=(pl_metricas$LID),
  nudge_x = 0.25, nudge_y = 0.25,check_overlap = T
)
### Podemos agregar una línea de regresión

ggpairs(X, columns = c(1,4,7), 
        lower = list(continuous = "smooth"))

ggpairs(plantaciones_rescale[,2:5], 
        lower = list(continuous = "smooth"))


## Podemos con una función coloca6r colores a la linea y el histograma
# olvide que se coloca en mapping

lowerFn <- function(data, mapping, method = "lm", ...){
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "blue") + 
    geom_smooth(method = method, color = "red", ...)
  p
}
########3



ggpairs(
  pl_metricas, columns = c("LPI","GYRATE_MN"), 
  lower = list(continuous = wrap(lowerFn, method = "lm")),
  diag = list(continuous = wrap("barDiag", colour = "blue")),
  upper = list(continuous = wrap("cor", size = 5))+
    
)

corr_menores_0.3

ggplot(pl_metricas, aes(LPI,GYRATE_MN))+
  geom_point() + #
  geom_text(
    label=(pl_metricas$LID),
    nudge_x = 0.25, nudge_y = 0.25,
    check_overlap = T
  ) +
  geom_smooth(method="lm" , color="red", se=TRUE) +
  theme_classic() 

head(pl_metricas)

corr_menores_0.3$Metricas_1

rownames(X)

###################

library(ggplot2)
library(hrbrthemes)
library(ggrepel)
library(ggpubr)
library(tidyverse)

scatter_etiquetas <- function(datos,var1,var2, labx = 30, laby = 350, etiqueta){
ggplot(datos, aes(datos[,var1],datos[,var2]))+
  geom_point(color="black") +
  geom_smooth(method=lm , color="red", se=TRUE) +
  theme_classic()+
  ggrepel::geom_text_repel(data = datos, aes(label=datos[,etiqueta]))+
  stat_cor(method = "pearson", label.x = labx, label.y = laby)+
    xlab(var1) + ylab(var2)
    
    # ubicación de la etiquita

}  

# Encontrar una mejor forma de filtrar por casos que no son deseados
pl_m <- filter(pl_metricas, str_detect(LID, "Melipeuco|Curarrehue"))


scatter_etiquetas(pl_metricas, var1 = "LPI", var2 = "GYRATE_MN", labx = 30, laby = 350, etiqueta = "LID")
scatter_etiquetas(pl_m, var1 = "LPI", var2 = "GYRATE_MN", labx = 30, laby = 350, etiqueta = "LID")



ggplot(pl_m, aes(LPI,GYRATE_MN))+
  geom_point(color="black") +
  geom_smooth(method=lm , color="red", se=TRUE) +
  theme_classic()+
  ggrepel::geom_text_repel(data = pl_m, aes(label=LID))+
  stat_cor(method = "pearson", label.x = 30, label.y = 350)


### Revisar que pasa con el formato de pl_norm, las columnas son numericas
### pero tienen más atributos, hay que simplificar el data.frame

library(pysch)
options(scipen = 999999)


View(describe(X))
View(describe(pl_rescale[3:17]))
View(describe(pl_norm[3:17]))


################################33

library(psych)

?KMO
X1 <- select(X, -PARA_MN)
X1.cor <- cor(X1, method = "pearson")

det(pl_metricas.cor) 
det(X1.cor)
n_correlaciones(X1.cor)

KMO(select(X, -PARA_MN))
KMO(X)# El valor es de 0.63 lo que es aceptable. 
# Algunas variables tienen  menos de 0.5
cortest.bartlett(X, n = 46) # BArlett test rejected correlation matrix is an identity
a <- alpha(X)


## Mardia multivariate normality from psych package
mardia(X, na.rm = TRUE, plot = TRUE)
mardia(pl_rescale[3:18], na.rm = TRUE, plot = TRUE)
mardia(pl_norm[2:17], na.rm = TRUE, plot = TRUE)

### KORMOLOT

ks.test(X$LPI, "pnorm", mean(X$LPI, na.rm=T), sd(X$LPI,na.rm=T))
