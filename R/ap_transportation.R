# R/ap_transportation.R
library(shiny)

ap_transportation_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Aggregate Planning: Transportation Method"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Plan Data (.xlsx, .xls, .csv)", accept = c(".xlsx", ".xls", ".csv")),
        hr(),
        h4("Method Option"),
        radioButtons(ns("tp_method"), "Transportation Solution Method",
                     choices = c("Optimal (Cheapest-Cell First / Minimum Cost)" = "cheapest",
                                 "North-West Corner Rule" = "nw_corner")),
        hr(),
        h4("Capacity Parameters"),
        numericInput(ns("workers"), "Fixed Workforce for Capacity", value = 15, min = 1),
        numericInput(ns("k_val"), "Productivity K (units/worker/day)", value = 1.0, min = 0.01),
        numericInput(ns("max_ot_pct"), "Max Overtime (% of Regular)", value = 25, min = 0, max = 100),
        numericInput(ns("sub_cap_per_period"), "Subcontracting Capacity per Period", value = 500, min = 0),
        numericInput(ns("init_inv"), "Initial Inventory (Period 0 Source)", value = 200, min = 0),
        hr(),
        h4("Unit Costs ($)"),
        numericInput(ns("c_reg"), "Regular Time ($/unit)", value = 20, min = 0),
        numericInput(ns("c_ot"), "Overtime ($/unit)", value = 30, min = 0),
        numericInput(ns("c_sub"), "Subcontracting ($/unit)", value = 40, min = 0),
        numericInput(ns("c_hold"), "Inventory Holding Cost ($/unit/period)", value = 4, min = 0)
      ),
      mainPanel(
        h3(textOutput(ns("method_title"))),
        plotOutput(ns("cum_plot")),
        hr(),
        h4("Transportation Allocation Table (Sources vs Demand Periods)"),
        div(style = "max-height: 300px; overflow: auto;", tableOutput(ns("alloc_matrix"))),
        hr(),
        fluidRow(
          column(7,
                 h4("Derived Aggregate Plan Schedule"),
                 tableOutput(ns("plan_table"))
          ),
          column(5,
                 h4("Cost Breakdown"),
                 tableOutput(ns("cost_table"))
          )
        )
      )
    )
  )
}

ap_transportation_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    plan_input <- reactive({
      if (is.null(input$file)) {
        data.frame(
          Period = paste0("Period_", 1:6),
          Demand = c(1800, 1500, 2100, 2500, 1900, 2200),
          WorkingDays = c(22, 19, 21, 22, 20, 22),
          stringsAsFactors = FALSE
        )
      } else {
        df <- read_ap_input_file(input$file$datapath, input$file$name)
        if (!"Period" %in% names(df)) df$Period <- paste0("Period_", 1:nrow(df))
        if (!"Demand" %in% names(df)) df$Demand <- df[[2]]
        if (!"WorkingDays" %in% names(df)) df$WorkingDays <- 20
        df
      }
    })
    
    tp_solution <- reactive({
      df <- plan_input()
      n <- nrow(df)
      W <- input$workers
      K <- input$k_val
      ot_ratio <- input$max_ot_pct / 100
      sub_cap <- input$sub_cap_per_period
      I0 <- input$init_inv
      
      c_reg <- input$c_reg
      c_ot <- input$c_ot
      c_sub <- input$c_sub
      c_h <- input$c_hold
      
      # Define Sources:
      # Source 1: Initial Inventory (Cap: I0)
      # Then for each period t (1..n):
      #   Regular t: Cap = W * K * Days[t]
      #   Overtime t: Cap = ot_ratio * Regular Cap
      #   Subcontract t: Cap = sub_cap
      
      source_names <- c("Initial_Inv")
      source_supply <- c(I0)
      source_base_cost <- c(0)
      source_period <- c(0)
      source_type <- c("Inv")
      
      for (t in 1:n) {
        reg_cap <- W * K * df$WorkingDays[t]
        ot_cap <- ot_ratio * reg_cap
        
        # Regular
        source_names <- c(source_names, paste0("P", t, "_Regular"))
        source_supply <- c(source_supply, reg_cap)
        source_base_cost <- c(source_base_cost, c_reg)
        source_period <- c(source_period, t)
        source_type <- c(source_type, "Reg")
        
        # Overtime
        source_names <- c(source_names, paste0("P", t, "_Overtime"))
        source_supply <- c(source_supply, ot_cap)
        source_base_cost <- c(source_base_cost, c_ot)
        source_period <- c(source_period, t)
        source_type <- c(source_type, "OT")
        
        # Subcontract
        source_names <- c(source_names, paste0("P", t, "_Subcontract"))
        source_supply <- c(source_supply, sub_cap)
        source_base_cost <- c(source_base_cost, c_sub)
        source_period <- c(source_period, t)
        source_type <- c(source_type, "Sub")
      }
      
      # Add Dummy Demand column if supply > demand
      total_supply <- sum(source_supply)
      total_demand <- sum(df$Demand)
      
      dest_names <- df$Period
      dest_demand <- df$Demand
      
      if (total_supply > total_demand) {
        dest_names <- c(dest_names, "Unused_Capacity")
        dest_demand <- c(dest_demand, total_supply - total_demand)
      }
      
      num_sources <- length(source_supply)
      num_dests <- length(dest_demand)
      
      # Build Cost Matrix (num_sources x num_dests)
      cost_mat <- matrix(1e8, nrow = num_sources, ncol = num_dests)
      for (i in 1:num_sources) {
        s_per <- source_period[i]
        b_cost <- source_base_cost[i]
        for (j in 1:n) { # periods 1..n
          if (s_per <= j) {
            # valid production period (or initial inventory)
            holding_periods <- j - s_per
            cost_mat[i, j] <- b_cost + holding_periods * c_h
          }
        }
        if (num_dests > n) {
          # Dummy column cost = 0 (unused capacity)
          cost_mat[i, num_dests] <- 0
        }
      }
      
      # Allocation Matrix
      alloc_mat <- matrix(0, nrow = num_sources, ncol = num_dests)
      rem_supply <- source_supply
      rem_demand <- dest_demand
      
      method_choice <- input$tp_method
      
      if (method_choice == "nw_corner") {
        # North-West Corner Algorithm
        i <- 1
        j <- 1
        while (i <= num_sources && j <= num_dests) {
          # Check feasibility (cannot produce for past periods)
          if (cost_mat[i, j] >= 1e7 && j <= n) {
            # Skip past or infeasible cell by advancing column or row
            i <- i + 1
            next
          }
          alloc <- min(rem_supply[i], rem_demand[j])
          alloc_mat[i, j] <- alloc
          rem_supply[i] <- rem_supply[i] - alloc
          rem_demand[j] <- rem_demand[j] - alloc
          
          if (rem_supply[i] == 0) i <- i + 1
          if (j <= num_dests && rem_demand[j] == 0) j <- j + 1
        }
      } else {
        # Optimal / Cheapest Cell First (Greedy / Min-Cost Heuristic)
        # Order valid cells by cost
        cell_coords <- expand.grid(i = 1:num_sources, j = 1:num_dests)
        cell_coords$cost <- mapply(function(r, c) cost_mat[r, c], cell_coords$i, cell_coords$j)
        # Filter strictly out infeasible (>1e6)
        cell_coords <- cell_coords[cell_coords$cost < 1e7, ]
        cell_coords <- cell_coords[order(cell_coords$cost), ]
        
        for (k in 1:nrow(cell_coords)) {
          r <- cell_coords$i[k]
          c <- cell_coords$j[k]
          if (rem_supply[r] > 0 && rem_demand[c] > 0) {
            alloc <- min(rem_supply[r], rem_demand[c])
            alloc_mat[r, c] <- alloc
            rem_supply[r] <- rem_supply[r] - alloc
            rem_demand[c] <- rem_demand[c] - alloc
          }
        }
      }
      
      # Aggregate into plan table
      reg_p <- numeric(n)
      ot_p <- numeric(n)
      sub_p <- numeric(n)
      
      for (i in 1:num_sources) {
        st <- source_type[i]
        sp <- source_period[i]
        if (sp >= 1 && sp <= n) {
          used_prod <- sum(alloc_mat[i, 1:n])
          if (st == "Reg") reg_p[sp] <- reg_p[sp] + used_prod
          if (st == "OT") ot_p[sp] <- ot_p[sp] + used_prod
          if (st == "Sub") sub_p[sp] <- sub_p[sp] + used_prod
        }
      }
      
      inv_track <- numeric(n)
      curr_inv <- I0
      for (t in 1:n) {
        curr_inv <- curr_inv + reg_p[t] + ot_p[t] + sub_p[t] - df$Demand[t]
        inv_track[t] <- curr_inv
      }
      
      plan_df <- data.frame(
        Period = df$Period,
        Demand = df$Demand,
        WorkingDays = df$WorkingDays,
        Workers = W,
        Hired = 0,
        Fired = 0,
        Regular_Prod = round(reg_p, 1),
        Overtime_Prod = round(ot_p, 1),
        Subcontract_Prod = round(sub_p, 1),
        Production = round(reg_p + ot_p + sub_p, 1),
        Ending_Inventory = round(inv_track, 1)
      )
      
      # Formatted allocation matrix for display
      alloc_df <- as.data.frame(round(alloc_mat, 1))
      colnames(alloc_df) <- dest_names
      alloc_df <- cbind(Source = source_names, Capacity = round(source_supply, 1), alloc_df)
      
      list(plan = plan_df, alloc_df = alloc_df)
    })
    
    output$method_title <- renderText({
      if (input$tp_method == "cheapest") "Transportation Plan: Optimal (Minimum Cost Allocation)" else "Transportation Plan: North-West Corner Starting Solution"
    })
    
    output$cum_plot <- renderPlot({
      plot_cumulative_plan(tp_solution()$plan, paste("(", input$tp_method, ")"))
    })
    
    output$alloc_matrix <- renderTable({
      tp_solution()$alloc_df
    })
    
    output$plan_table <- renderTable({
      tp_solution()$plan
    })
    
    cost_params <- reactive({
      list(
        hire = 0,
        fire = 0,
        hold = input$c_hold,
        regular = input$c_reg,
        overtime = input$c_ot,
        subcontract = input$c_sub
      )
    })
    
    output$cost_table <- renderTable({
      calc_plan_costs(tp_solution()$plan, cost_params())
    })
  })
}
