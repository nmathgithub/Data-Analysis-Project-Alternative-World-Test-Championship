# Nitesh Mathur 
# First Shiny App - Dashboard (with subpages)
# 20 April 2021 
# 
# Data: Cricket Data

library(cricketr)
library(shiny)
library(shinydashboard)
library(tidyverse)
#library(DT)

ACook <- getPlayerData(11728,dir="..",file="acook.csv",type="batting",homeOrAway=c(1,2),
                       result=c(1,2,4))
GSmith <- getPlayerData(47270,dir="..",file="acook.csv",type="batting",homeOrAway=c(1,2),
                       result=c(1,2,4))
KSang <- getPlayerData(50710,dir="..",file="acook.csv",type="batting",homeOrAway=c(1,2),
                       result=c(1,2,4))

ui <- dashboardPage(skin = "red",
                    dashboardHeader(title = "Left Handed Great Batsmen"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Alastair Cook", tabName = "Cook", icon = icon("bat")),
                        menuItem("Graeme Smith", tabName = "Smith", icon = icon("bat")),
                        menuItem("Kumar Sangakkara", tabName = "Sangakkara", icon = icon("bat"))
                        
                      
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        # tabItem("iris", box(plotOutput("correlation_plot"), width = 8),
                        #         box(
                        #           selectInput("features", "Features:", 
                        #                       c("Sepal.Width", "Petal.Length", 
                        #                         "Petal.Width")),  width = 4
                        #         )
                        #         
                        # ),
                        tabItem("Cook", 
                                fluidPage(
                                  h1("Alastair Cook"),
                                  dataTableOutput("A_Cook")
                                )
                        ),
                        
                        tabItem("Smith", 
                                fluidPage(
                                  h1("Graeme Smith"),
                                  dataTableOutput("G_Smith")
                                )
                        ),
                        
                        tabItem("Sangakkara", 
                                fluidPage(
                                  h1("Kumar Sangakkara"),
                                  dataTableOutput("K_Sang")
                                )
                        )
                      )
                      
                    )
)

server <- function(input, output){
  output$correlation_plot <- renderPlot(
    {
      plot(iris$Sepal.Length, iris[[input$features]],
           xlab = "Sepal Length", ylab = "Features")
    }
  )
  output$A_Cook <- renderDataTable(ACook)
  output$G_Smith <- renderDataTable(GSmith)
  output$K_Sang <- renderDataTable(KSang)
  
  
}

shinyApp(ui, server)

