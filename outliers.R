library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(scales)
library(tidyr)
library(stringr)
#library(qgraph)

# ISO8859-1

# Directorio de trabajo

setwd("~/GitHub/estructura-forestal-paisaje")

# Cargamos los datos desde un archivo csv.

pl_metricas<- read.csv("metricas_seleccion_ejercicio.csv")

X <- pl_metricas[3:18]
pl_metricas$ID_row <- seq(1:length(pl_metricas$LID))

#ciudades <- read.csv("resultados_final.csv")
# Seleccionamos solo las filas númericas

## Funciones outliers #### 

out <- function(datos, n = 0){
  lower_bound <- quantile(datos[,n], 0.250) - IQR(datos[,n])
  upper_bound <- quantile(datos[,n], 0.750) + IQR(datos[,n])
  n_outlier <-which(datos[,n] < lower_bound | datos[,n] > upper_bound)
  return(data.frame(Observacion = datos[n_outlier,m], valor = datos[n_outlier,n]))
  
}

out_nombre <- function(datos,nombre,ID){
  lower_bound <- quantile(datos[,nombre], 0.250) - (1.5 * IQR(datos[,nombre]))
  upper_bound <- quantile(datos[,nombre], 0.750) + (1.5 * IQR(datos[,nombre]))
  n_outlier <-which(datos[,nombre] < lower_bound | datos[,nombre] > upper_bound)
  return(data.frame(ID_row = n_outlier, Observacion = datos[n_outlier,ID], valor = datos[n_outlier,nombre]))
}

## For loop ####
# For loop que selecciona los outliers de cada variable.
# Devuelve una lista

lista_df <- list()
for (name in colnames(pl_metricas[3:17])){
  lista_df[[name]] <- out_nombre(pl_metricas, name, "LID")
  print(lista_df)
}

# R has an interesting function called do. call. This function allows 
# you to call any R function, 
# but instead of writing out the arguments one by one, 
# you can use a list to hold the arguments of the function. 
# While it may not seem useful on the surface, 
# a simple example will help to show how powerful do


outlyiers <- do.call(rbind.data.frame, lista_df) # En este caso aplica la funcion rbind.data.frame 
                                                 # a la lista df.

## Ver los datos


# agregamos una columna que sea el nombre de las comunas para tener las metricas
# como observaciones



outlyiers$Metricas <- row.names(outlyiers) 

outlyiers$Metricas <- gsub("\\.[0???9]*$","",row.names(outlyiers))

tb_out <- tibble(outlyiers) # La estructura de datos tibble no tiene rownames

# Podemos calcular el numero de casos por con agrupando por group_by y summarise

group_by(outlyiers,ID_row) %>% summarise(n_veces = n())

# O podemos calcular el numero de casos utilizando NCasosOut

NCasosOut <- count(tb_out, ID_row)

## join para agregar los datos del numero de casos outlier por comuna
pl_metricas_out <- left_join(pl_metricas, NCasosOut, by = "ID_row") %>% 
  replace_na(list(n = 0)) %>% tibble()

## Pregunta cuales son solo los datos que son outliers

sel_row_out <- unique(outlyiers$ID_row) ## Numero de row de los outliers

df_out <- (pl_metricas_out[sel_row_out,]) ## Nuevo tibble solo con los outliers 

## los mismo usando dplyr
tb_out_complete <- filter(pl_metricas_out, ID_row %in% sel_row_out) %>%
  tibble()
  
a <- pivot_wider(tb_out, names_from = Observacion, 
            values_from = Metricas, valor)


met <- vector()
for (cnam in colnames(a)){
  met[[cnam]] <- a[!is.na(a[,cnam]),cnam]
}

met_fina <- list()
for (com in colnames(a[,2:14])){
  max_val <- max(seq_along(t(met[[com]][1])))
  met_fina[[com]] = unite(data.frame(t(met[[com]])), metricas, c(1:max_val), sep = ",", remove = TRUE) %>%
    mutate(LID = rownames(data.frame(t(met[[com]]))))
}

met_final_juntas <- do.call(rbind.data.frame, met_fina)

pl_metricas_total <- full_join(met_final_juntas, pl_metricas_out, by = ("LID")) %>%
  relocate("metricas", .after = last_col()) %>% relocate("ID_row") %>% tibble()



View(pl_metricas_total) ### Falta reemplazar el valor de los NA por 0.
                        ### Falta contar el numero de outliers que hay por variable.

## Fin del script ###

#####################################################################################










  


