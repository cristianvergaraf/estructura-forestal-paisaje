#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
library(viridis)



options(scipen = 9999)

pl_norm <- read.csv("pl_norm.csv", header = TRUE, stringsAsFactors = F)
metrica <- names(pl_norm[,3:18])
pl_norm_long <- read.csv("pl_norm_long.csv", header = TRUE, stringsAsFactors = F)


#######################################################################################


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Histograma metricas del paisaje"),
    
    # Sidebar with a slider input for number of bins
    
    sidebarPanel("My sidebar", width = 4,
                 sliderInput("bins",
                             "Number of bins:",
                             min = 1,
                             max = 50,
                             value = 30),
                 
                 sliderInput("large",
                             "Largo de los bins",
                             min = 0.1,
                             max = 5,
                             value = 0.5),
    
        
    #Sidebar with a slider input for number of bin
        selectInput("Met",
                    "Elige una metrica",
                    metrica, selected = "IJI.z"),
                    
         
    
        selectInput("Met1",
                    "Elige una metrica",
                    metrica, selected = "LSI.z"),
    
        
    
        selectInput("BMet",
                    "Elige una metrica",
                    metrica, selected = "PLAND.z"),
    
        selectInput("MetScat1",
                    "Elige la primera metrica para el scatterplot",
                    metrica, selected = "PLAND.z"),
    
        selectInput("MetScat2",
                    "Elige la segunda metrica para el scatterplot",
                    metrica, selected = "IJI.z")
    
    ),
            
        
        
        ## Show a plot of the generated distribution
    mainPanel("My content", width = 8,
        tabsetPanel(
        tabPanel("Histogramas",
                 plotOutput("distPlot1"),
                 plotOutput("distPlot2")
        ),
        
        tabPanel("scatterplot",
                plotOutput("distPlot5")
        
        ),
                 
        tabPanel("boxplot",
                    plotOutput(
                    outputId = "distPlot4",
                    height = "800px",
                    width = "800px")
        ),
        
        tabPanel("tabla filtrada",
                 DT::dataTableOutput("value")
                 
        ),
        
        tabPanel("tabla seleccion",
                 DT::dataTableOutput("table"),
        
                      
    ),
    
    
             
             
    )
)
)
)



