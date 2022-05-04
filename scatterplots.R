###################
# Graficos de scaterplot para poder evaluar el efecto de los outliers

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

  