library(lpSolveAPI)
library(dplyr)
library(shiny)
library(ggplot2)

source("lpsolveAPI functions.R")
#Define UI ----
ui <- fluidPage(
                sidebarPanel(h3('Q1 Demand'),
                sliderInput("Q1", h3("Fuel Corn Demand"),
                            min = 800, max = 1500, value = 1200, step=100),
                h3('Q2 Demand'),
                sliderInput("Q2", h3("Fuel Corn Demand"),
                            min = 800, max = 1500, value = 1100, step=100),
                h3('Q3 Demand'),
                sliderInput("Q3", h3("Fuel Corn Demand"),
                            min = 800, max = 1500, value = 1300, step=100),
                h3('Q4 Demand'),
                sliderInput("Q4", h3("Fuel Corn Demand"),
                            min = 800, max = 1500, value = 1000, step=100),
                hr(),hr(),hr(),
                h3('Flex the Fuel-Corn Mix'),
  sliderInput("Fuel_pct", h6("Fuel % final mix"),
              min = 20, max = 80, value = 35, step=5)),
  mainPanel(
  h3(' Optimized Cost:'),
  textOutput('optim'),
  h3('Quarter/ Buying Period Pattern'),
  tableOutput('datab')
  )
  
)

## Server Side
server <- function(input, output) {
  result <- reactive({solv_lp(14,20, in_mat, input$Fuel_pct, c(input$Q1, input$Q2, input$Q3, input$Q4), 3)})
  output$optim <- renderPrint(result()[[1]])
  output$datab <- renderTable(result()[[2]])
}


shinyApp(ui = ui, server = server)




