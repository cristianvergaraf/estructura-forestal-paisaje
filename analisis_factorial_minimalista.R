# 03.14.2022 

# Este programa tendrá lo esencial para el análisis factorial #

library(psych)
library(GPArotation)
library(car)
library(dplyr)

setwd("~/GitHub/estructura-forestal-paisaje")



plantaciones_clase_ambas <- read.table(file = "prim_sel_metricas_completas_42_16_05_2019.csv", header = TRUE, sep =";", dec = "." )


cor_pl <- cor(plantaciones_clase_ambas[4:16], use = "pairwise.complete.obs")

eigen_pl <- eigen(cor_pl) # Esto encuentra los eigen usando PCA

scree(cor_pl, factors = FALSE)



EFA_model <- fa(plantaciones_clase_ambas[4:16],3)
EFA_model1 <- fa(plantaciones_clase_ambas[4:16],4)

EFA_model$BIC
EFA_model1$BIC


EFA_model$loadings


head(EFA_model$scores)

