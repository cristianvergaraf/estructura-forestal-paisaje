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
    fluidRow(
        column(4,
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        )
    ),
            
            
        
        
    # Sidebar with a slider input for number of bin
        column(4,    
            selectInput("Met",
                    "Elige una metrica",
                    metrica, selected = "IJI.z")
        )
    ),           
         
        column(4,
            selectInput("Met1",
                        "Elige una metrica",
                        metrica, selected = "LSI.z")
        )
    ),
    
        fluid_rowcolumn(2,
    
            selectInput("BMet",
                    "Elige una metrica",
                    metrica, selected = "PLAND.z")
    
        )
    ),
    
        
    )


        
        
        # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(
        tabPanel("Histogramas",
                 plotOutput("distPlot1"),
                 plotOutput("distPlot2")
        ),
        
        tabPanel("tabla filtrada",
                 DT::dataTableOutput("value")
                 
        ),
        
        tabPanel("tabla seleccion",
                 DT::dataTableOutput("table"),
        
                      
    ),
    
    tabPanel("boxplot",
             plotOutput(
                 outputId = "distPlot4",
                 height = "400px",
                 width = "800px"),
             
             
    )
)
)
)
)
)

