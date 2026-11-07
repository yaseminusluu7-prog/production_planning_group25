# app.R
library(shiny)
library(readxl)

# Define Main UI with Common Forecasting Menu
ui <- navbarPage(
  title = "Production Planning",
  
  # Preserving Assignment 1 Module
  tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
  
  # Assignment 2: Common Forecasting Module Menu
  navbarMenu(
    "Forecasting",
    tabPanel("Stationary Series", stationary_ui("stationary")),
    tabPanel("Trend & Seasonal Series", trend_seasonal_ui("trend_seasonal")),
    tabPanel("Box-Jenkins (ARIMA)", box_jenkins_ui("box_jenkins"))
  )
)

server <- function(input, output, session) {
  # Learning curve server
  learning_curve_server("learning_curve")
  
  # Assignment 2 Forecasting servers
  stationary_server("stationary")
  trend_seasonal_server("trend_seasonal")
  box_jenkins_server("box_jenkins")
}

shinyApp(ui, server)
