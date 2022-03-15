###

library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)

setwd("~/GitHub/estructura-forestal-paisaje")

pl_metricas<- read.csv("metricas_seleccion_ejercicio.csv")

X = pl_metricas[3:18]

par(mfrow = c(2,2))

sapply(seq(1,16),function(j)boxplot(X[,j],main=colnames(X)[j],xlab="",col="yellow"))


par(mfrow=c(2,4))
sapply(seq(1,16),function(j)hist(X[,j],main=colnames(X)[j],xlab="",col="blue",breaks = "Sturges"))


par(mfrow=c(2,4))
sapply(seq(1,16),function(j)plot(density(X[,j],kernel="gaussian"),main=colnames(X)[j],xlab="",col="blue",lwd=2))


pairs(X[1:5],pch=19,col="black",
      lower.panel = panel.smooth)

pl_metricas.cor <- cor(X, method = "pearson")
pl_round.cor <- round(pl_metricas.cor, digits = 1)

pl_metricas.cor

par(mfrow = c(1,1))

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))

corrplot(pl_round.cor, method = "shade", sade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", number.digits = 2, number.cex = 0.7)


corrplot(pl_metricas.cor, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45,
         col = col(200), addCoef.col = "black", addcolorlabel = "no",
         order = "AOE", type = "upper", 
         diag = F, number.digits = 2, number.cex = 0.7)

######   
# Scatterplots revisar diseno separar por variables. 

ggpairs(pl_metricas)

ggpairs(X[1:8])
ggpairs(X[9:16])

ggpairs(X[1:16])

