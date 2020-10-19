library(lpSolveAPI)
library(dplyr)
library(shiny)
library(ggplot2)

source("lpsolveAPI functions.R")
#Define UI ----
ui <- fluidPage(h3('Flex the Fuel-Corn Mix'),
  sliderInput("Fuel_pct", h6("Fuel % final mix"),
              min = 20, max = 80, value = 35, step=5),
  h3(' Optimized Cost:'),
  verbatimTextOutput('optim')
  
)

## Server Side
server <- function(input, output) {
  cost <- reactive({solv_lp(14,20, in_mat, input$Fuel_pct, qtr_demand, 3)})
  output$optim <- renderPrint(cost())

}


shinyApp(ui = ui, server = server)




