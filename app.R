# app.R
library(shiny)
library(readxl)

# Define Main UI with Common Menus
ui <- navbarPage(
  title = "Production Planning",
  
  # Assignment 1: Learning Curve
  tabPanel("Learning Curve", learning_curve_ui("learning_curve")),
  
  # Assignment 2: Forecasting Menu
  navbarMenu(
    "Forecasting",
    tabPanel("Stationary Series", stationary_ui("stationary")),
    tabPanel("Trend & Seasonal Series", trend_seasonal_ui("trend_seasonal")),
    tabPanel("Box-Jenkins (ARIMA)", box_jenkins_ui("box_jenkins"))
  ),
  
  # Assignment 3: Aggregate Planning Menu
  navbarMenu(
    "Aggregate Planning",
    tabPanel("Heuristic Plans", ap_heuristic_ui("ap_heuristic")),
    tabPanel("Linear Programming", ap_lp_ui("ap_lp")),
    tabPanel("Transportation Method", ap_transportation_ui("ap_transportation")),
    tabPanel("Disaggregation", ap_disaggregation_ui("ap_disaggregation"))
  )
)

server <- function(input, output, session) {
  # Assignment 1 Server
  learning_curve_server("learning_curve")
  
  # Assignment 2 Servers
  stationary_server("stationary")
  trend_seasonal_server("trend_seasonal")
  box_jenkins_server("box_jenkins")
  
  # Assignment 3 Servers
  ap_heuristic_server("ap_heuristic")
  ap_lp_server("ap_lp")
  ap_transportation_server("ap_transportation")
  ap_disaggregation_server("ap_disaggregation")
}

shinyApp(ui, server)
