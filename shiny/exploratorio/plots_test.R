
library(dplyr)
library(tidyr)
library(tidyverse)
library(hrbrthemes)
library(viridis)

setwd("C:/Users/crist/Documents/GitHub/estructura-forestal-paisaje/shiny/exploratorio")

pl_norm_long <- pivot_longer(pl_norm, c(-LID,-X), names_to = "Metricas" )
#write.csv(pl_norm_long, file = "pl_norm_long.csv")

pl_norm <- read.csv("pl_norm.csv")
pl_norm_long <- read.csv("pl_norm_long.csv")

ggplot(pl_norm_long, aes(x = Metricas, y = value, fill = Metricas)) +
  geom_boxplot() + scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
   geom_jitter(color="black", size=0.4, alpha=0.9) theme_ipsum() +
  theme(
    legend.position="none",
   plot.title = element_text(size=11)) +
  ggtitle("Basic boxplot") +
  xlab("")

filter(pl_norm_long, Metricas == "PLAND.z")


pl_norm_long$Metricas

ggplot(pl_norm, aes(y = AI.z, fill = AI.z)) + 
  geom_boxplot() + scale_fill_viridis(discrete = TRUE, alpha = 0.6)


test <- filter(pl_norm_long, Metricas == "ENN_MN.z")


ggplot(test, aes(x = Metricas, y = value, fill = Metricas)) +
  geom_boxplot() + geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() + theme(
    legend.position="none")+
  xlab("")




# plot
 pl_norm %>%
  ggplot(aes(x=DIVISION.z)) +
  geom_histogram(binwidth= 0.5, fill="#69b3a2", color="#e9ecef", alpha=0.9) 
   
  
x <- pl_norm[,"DIVISION.z"]

library(ggplot2)
library(hrbrthemes)
library(ggrepel)
library(ggpubr)
library(tidyverse)

ggplot(pl_norm, aes(PLAND.z,GYRATE_MN.z))+
  geom_point(color="black") +
  geom_smooth(method=lm , color="red", se=TRUE) +
  theme_classic()+
  ggrepel::geom_text_repel(data = pl_norm, aes(label=LID))+
  stat_cor(method = "pearson", label.x = 2, label.y = 4)

