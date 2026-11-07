# R/ap_disaggregation.R
library(shiny)

ap_disaggregation_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Aggregate Planning: Disaggregation (Square-Root Rule)"),
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("total_agg_prod"), "Total Aggregate Production to Disaggregate (Units)", value = 12000, min = 1),
        hr(),
        h4("Product Families Setup"),
        numericInput(ns("num_families"), "Number of Product Families", value = 3, min = 2, max = 6),
        uiOutput(ns("family_inputs")),
        hr(),
        radioButtons(ns("rule_variant"), "Square-Root Allocation Basis",
                     choices = c("Proportional to sqrt(Demand)" = "sqrt_d",
                                 "Proportional to sqrt(Demand * Setup Cost)" = "sqrt_ds"))
      ),
      mainPanel(
        h3("Disaggregated Production Quantities"),
        p("Applies the Square-Root Rule to split aggregate batch/production quantities optimally among product families, minimizing total setup and holding costs."),
        hr(),
        tableOutput(ns("disagg_table")),
        plotOutput(ns("disagg_plot"))
      )
    )
  )
}

ap_disaggregation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$family_inputs <- renderUI({
      k <- input$num_families
      lapply(1:k, function(i) {
        fluidRow(
          column(4, textInput(ns(paste0("fam_name_", i)), paste0("Family ", i, " Name"), value = paste0("Family_", LETTERS[i]))),
          column(4, numericInput(ns(paste0("fam_dem_", i)), "Annual Demand", value = 1000 * i, min = 1)),
          column(4, numericInput(ns(paste0("fam_setup_", i)), "Setup Cost ($)", value = 50 * i, min = 1))
        )
      })
    })
    
    disagg_data <- reactive({
      k <- input$num_families
      names_vec <- character(k)
      dem_vec <- numeric(k)
      setup_vec <- numeric(k)
      
      for (i in 1:k) {
        n_val <- input[[paste0("fam_name_", i)]]
        d_val <- input[[paste0("fam_dem_", i)]]
        s_val <- input[[paste0("fam_setup_", i)]]
        
        names_vec[i] <- if (!is.null(n_val) && n_val != "") n_val else paste0("Family_", LETTERS[i])
        dem_vec[i] <- if (!is.null(d_val)) as.numeric(d_val) else 1000 * i
        setup_vec[i] <- if (!is.null(s_val)) as.numeric(s_val) else 50 * i
      }
      
      # Square-Root Rule weights
      if (input$rule_variant == "sqrt_d") {
        weights <- sqrt(dem_vec)
      } else {
        weights <- sqrt(dem_vec * setup_vec)
      }
      
      shares <- weights / sum(weights)
      total_prod <- input$total_agg_prod
      allocated_prod <- total_prod * shares
      
      data.frame(
        Family = names_vec,
        Demand = dem_vec,
        Setup_Cost = setup_vec,
        Sqrt_Weight = round(weights, 2),
        Allocation_Share = paste0(round(shares * 100, 1), "%"),
        Disaggregated_Units = round(allocated_prod, 1),
        Whole_Units = round(allocated_prod)
      )
    })
    
    output$disagg_table <- renderTable({
      disagg_data()
    })
    
    output$disagg_plot <- renderPlot({
      df <- disagg_data()
      barplot(df$Disaggregated_Units, names.arg = df$Family,
              col = "#2B6CB0", border = NA,
              xlab = "Product Family", ylab = "Allocated Units",
              main = "Disaggregated Production Allocation (Square-Root Rule)")
      grid(nx = NA, ny = NULL, col = "lightgray")
    })
  })
}
