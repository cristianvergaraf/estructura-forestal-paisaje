#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

############################################################################################################
###################################################################################################333




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    
    
    
    test <- reactive({
         datos <- filter(pl_norm_long, Metricas==input$BMet)
         return(datos)
    })
    
    
    output$value <- DT::renderDataTable(DT::datatable(test()))
    
    
    
    output$table <- DT::renderDataTable(DT::datatable(pl_norm_long)
        
    )
    
    output$distPlot1 <- renderPlot({

        # generate bins based on input$bins from ui.R
        
        # draw the histogram with the specified number of bins
        
        ggplot(pl_norm, aes_string(input$Met)) +
            geom_histogram(binwidth= input$large, fill="#69b3a2", color="#e9ecef", alpha=0.9) 
        
        
        #hist(x, breaks = bins, col = 'blue', border = 'white')

    })
    
    output$distPlot2 <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- pl_norm[, input$Met1]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'red', border = 'white')
        
})
    
    output$distPlot3 <- renderPlot({
        
        # generate bins based on input$bins from ui.R
        x    <- pl_norm[, input$Met]
        bins <- seq(min(x), max(x), length.out = input$bons + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'red', border = 'white')
        
    })
    
    output$distPlot4 <- renderPlot({
        
        # 
        
        ggplot(data = test(), aes(x = Metricas, y = value, fill = Metricas)) +
            geom_boxplot()  +
            geom_jitter(color="black", size=0.4, alpha=0.9)  +
            theme(
                legend.position="none",
                plot.title = element_text(size=18)
            ) +
            ggtitle(paste(input$BMet, "Boxplot")) +
            xlab("")
        
    })
    
    output$distPlot5 <- renderPlot({
        
        #
        
        ggplot(pl_norm, aes_string(input$MetScat1,input$MetScat2))+
            geom_point(color="black") +
            geom_smooth(method=lm , color="red", se=TRUE) +
            theme_classic()+
            ggrepel::geom_text_repel(data = pl_norm, aes(label=LID))+
            stat_cor(method = "pearson", label.x = 2, label.y = 4)
        
    })

})
