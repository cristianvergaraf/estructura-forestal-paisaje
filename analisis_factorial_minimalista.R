# 03.18.2022 

# Este programa tendrá lo esencial para comenzar con el análisis factorial #
# Cargamos las librerias

library(psych)
library(GPArotation)
library(dplyr)

setwd("~/GitHub/estructura-forestal-paisaje")
setwd("C:/Users/crist/Documents/GitHub/estructura-forestal-paisaje")

# Cargamos los datos de las tres base de datos, la original, la normalizada, 
# la rescalada, para comparar los resultados.

pl <- read.csv("metricas_seleccion_ejercicio.csv")
pl_norm <- read.csv("pl_norm.csv")
pl_rescale <- read.csv("pl_rescale.csv")




cor_pl <- cor(pl[,3:18])
cor_pl_norm <- cor(pl_norm[,3:18])
cor_pl_rescale <- cor(pl_rescale[,3:18])

cor_pl_rescale <- cor(pl_rescale[,3:18])


## Primero queremos tener una idea de en cuantos factores podemos separar nuestros datos
## para esto podemos tener una idea teorica de cuantos, o determinarlo de forma empirica 
# segun la estructura de los datos.
# Vamos a usar un gráfico que muestra el eigenvalue para determinar el numero de factores
# El punto de corte se señala como el "Codo" que se hace en el gráfico.
# Otro criterio es el criterio de KAISER Que dice que todos los FACTORES. 

par(mfrow = c(1,1))


fa.parallel(pl[,3:18], fm = "ml", fa = "pa", n.iter = 500, ylab = "Eigenvalue")
fa.parallel(pl_norm[,3:18], fm = "ml", fa = "fa")
fa.parallel(pl_rescale[,3:18], fm = "ml", fa = "fa")

## El resultado indico que sería razonable utilizar 3 Factores

EFA_model <- fa(pl[,3:18],3, rotate = "varimax", fm = "minchi")
EFA_model1 <- fa(pl_norm[,3:18],3, rotate = "varimax", fm = "minchi") 
EFA_model2 <- fa(pl_rescale[,3:18],3, rotate = "varimax", fm = "minchi") 


### 

EFA_model2 <- fa(pl_rescale[,3:18],3, rotate = "varimax", fm = "minchi") 


# Podemos acceder a un resumen de los resultados con la función print y el nombre del
# modelo

print(EFA_model)
print(EFA_model1)
print(EFA_model2)

## El resultado de lo anterior, es un poco caótico, mejor es acceder 
## a los resultados de forma individual.

## En primer lugar nos interesan los loadings 
# ("Correlaciones de las variables con los componentes")

EFA_model
EFA_model1$loadings
EFA_model2$loadings

# podemos ver los loadings definiendo un umbral

print(EFA_model$loadings, cutoff = 0.3)


### Los resultados entre las 3 bases de datos no varían mucho en los loadings.Hemos modificado 
### La rotación para ver las diferencias.
## Los resultados no son del todo claros. En este momento hay que comenzar a modificar 
## los parametros del análisis, incluir o sacar variables, para ir logrando factores que 
## expliquen la mayor cantidad de variabilidad y que tengan la más fácil interpretación

# Veamos que pasa con los scores

EFA_model$scores
EFA_model1$scores
EFA_model2$scores

# Diagrama con los loadings
fa.diagram(EFA_model, sort = T, cut = 0.5, digits = 2, main = "Model", cex = 0.75)

# Resumen bastante confuso de los resultados

biplot.psych(EFA_model1)

# Este gráfico se puede mejorar para hacer un representación de los resultados

biplot.psych(EFA_model1,choose=c(1,2), main = "Analisis factorial",hist.col="cyan",xlim.s=c(-2,2),
             ylim.s=c(-3,3), xlim.f=c(-1,1),ylim.f=c(-0.75,0.75),
             col = c("red","black"), arrow.len = 0.05, pch = 21, cutl = 0.4, smoother = F, vars = T )



EFA_model$scores

New_Factor_DS <- tibble(obs = pl["LID"], F1 = EFA_model$scores[,1], F2 = EFA_model$scores[,1], F3 = EFA_model$scores[,1])

write.csv(New_Factor_DS, "FA_plantaciones.csv")
