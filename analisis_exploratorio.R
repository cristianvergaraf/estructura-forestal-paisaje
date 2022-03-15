library(dplyr)
library(ggplot2)

setwd("~/GitHub/estructura-forestal-paisaje")

pl <- read.table(file = "prim_sel_metricas_completas_42_16_05_2019.csv", header = TRUE, sep =";", dec = "." )

pl_small <- pl[1:7,]


### Definir un histograma
ggplot(pl, aes(GYRATE_MN)) + 
  geom_histogram(bin = 6, fill = "cornflowerblue",
                 color = "white") 

### Definir un histograma
p = ggplot(pl_small, aes(GYRATE_MN))  
  
#Density plots

b <- p + geom_density(color = "black", fill = "red", alpha = 0.5 )

b 


b + facet_grid("LID~GYRATE_MN")


p + geom_freqpoly(bins = 5, color = "cornflowerblue" )

## scatterplot

c <- ggplot(data = pl_small, aes(x= GYRATE_MN, y = GYRATE_MD)) + geom_point() 

c + facet_grid(.~LID)



### Revisar teoria boxplot y hacer uno por variable

pl_03 <- pl[pl$ENN_MN > 0.8,]

a <- ggplot(data = pl_small, aes(x = PLADJ)) + geom_histogram(bins = 25)


a + facet_grid("LID",)

ggplot(pl, aes(x = CA, y = PLAND, color = LID, shape = TYPE)) + geom_point() +
   coord_cartesian(ylim = 0, 70) # Tengo solo un punto por categoria y opor eso no

ggplot(data = pl, aes(x = TYPE, y = PLAND)) + geom_boxplot() # Hay solo un valor por categoria

ggplot(data = pl_small, aes(x = CA, y = PLAND, color = LID, shape = TYPE)) + geom_point() # Hay solo un valor por categoria

pl_small$
  
pl
ggplot(data = pl_small, aes(x = CA, y = PLAND))  

class(pl_small)

### boxplot relacionan una variable con respecto a otra ?????

