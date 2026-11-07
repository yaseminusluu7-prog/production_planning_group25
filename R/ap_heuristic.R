# R/ap_heuristic.R
library(shiny)

ap_heuristic_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Aggregate Planning: Heuristic Plans"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Plan Data (.xlsx, .xls, .csv)", accept = c(".xlsx", ".xls", ".csv")),
        helpText("Required columns: Period, Demand, WorkingDays (default example provided if empty)."),
        hr(),
        h4("Plan Selection"),
        selectInput(ns("plan_type"), "Select Strategy",
                    choices = c("Zero-Inventory (Chase) Plan" = "chase",
                                "Constant-Workforce (Level) Plan" = "level",
                                "Mixed Plan (Custom Workforce Change Periods)" = "mixed")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'mixed'", ns("plan_type")),
          textInput(ns("mixed_change_periods"), "Periods to Change Workforce (comma-separated, e.g. 1, 3, 5)", value = "1, 3, 5")
        ),
        hr(),
        h4("Plant & Initial Data"),
        numericInput(ns("init_workforce"), "Starting Workforce (W0)", value = 15, min = 0),
        numericInput(ns("init_inv"), "Starting Inventory (I0)", value = 200, min = 0),
        numericInput(ns("final_inv"), "Ending Inventory Target (In)", value = 200, min = 0),
        hr(),
        h4("Productivity (K)"),
        radioButtons(ns("k_mode"), "Productivity Input Mode",
                     choices = c("Enter Directly (units/worker/day)" = "direct",
                                 "Compute from Past Data" = "compute")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'direct'", ns("k_mode")),
          numericInput(ns("k_direct"), "K Value", value = 1.0, min = 0.01, step = 0.1)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'compute'", ns("k_mode")),
          numericInput(ns("past_units"), "Past Total Units Produced", value = 3000),
          numericInput(ns("past_workers"), "Past Number of Workers", value = 15),
          numericInput(ns("past_days"), "Past Working Days", value = 20)
        ),
        hr(),
        h4("Cost Parameters ($)"),
        numericInput(ns("c_hire"), "Hiring Cost ($/worker)", value = 500, min = 0),
        numericInput(ns("c_fire"), "Firing Cost ($/worker)", value = 800, min = 0),
        numericInput(ns("c_hold"), "Holding Cost ($/unit/period)", value = 4, min = 0),
        numericInput(ns("c_regular"), "Regular Time Cost ($/unit)", value = 20, min = 0),
        numericInput(ns("c_overtime"), "Overtime Cost ($/unit)", value = 30, min = 0),
        numericInput(ns("c_subcontract"), "Subcontracting Cost ($/unit)", value = 40, min = 0)
      ),
      mainPanel(
        h4("Editable Period & Demand Data"),
        div(style = "max-height: 200px; overflow-y: auto;", tableOutput(ns("input_data_preview"))),
        hr(),
        h3(textOutput(ns("plan_title"))),
        plotOutput(ns("cum_plot")),
        hr(),
        fluidRow(
          column(7,
                 h4("Plan Schedule (Original / Exact)"),
                 tableOutput(ns("plan_table_orig")),
                 conditionalPanel(
                   condition = "output.has_rounded_plan == true",
                   h4("Plan Schedule (Rounded Up to Whole Workers)"),
                   tableOutput(ns("plan_table_rounded"))
                 )
          ),
          column(5,
                 h4("Cost Breakdown (Original)"),
                 tableOutput(ns("cost_table_orig")),
                 conditionalPanel(
                   condition = "output.has_rounded_plan == true",
                   h4("Cost Breakdown (Whole Workers)"),
                   tableOutput(ns("cost_table_rounded"))
                 )
          )
        )
      )
    )
  )
}

ap_heuristic_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Load or default dataset
    plan_input <- reactive({
      if (is.null(input$file)) {
        # Default 6-period aggregate planning dataset
        data.frame(
          Period = paste0("Period_", 1:6),
          Demand = c(1800, 1500, 2100, 2500, 1900, 2200),
          WorkingDays = c(22, 19, 21, 22, 20, 22),
          stringsAsFactors = FALSE
        )
      } else {
        df <- read_ap_input_file(input$file$datapath, input$file$name)
        # Ensure mandatory columns exist
        if (!"Period" %in% names(df)) df$Period <- paste0("Period_", 1:nrow(df))
        if (!"Demand" %in% names(df)) df$Demand <- df[[2]]
        if (!"WorkingDays" %in% names(df)) df$WorkingDays <- 20
        df
      }
    })
    
    output$input_data_preview <- renderTable({
      plan_input()
    })
    
    k_val <- reactive({
      calc_k_value(input$k_mode, input$k_direct, input$past_units, input$past_workers, input$past_days)
    })
    
    cost_params <- reactive({
      list(
        hire = input$c_hire,
        fire = input$c_fire,
        hold = input$c_hold,
        regular = input$c_regular,
        overtime = input$c_overtime,
        subcontract = input$c_subcontract
      )
    })
    
    # Solve Heuristic Model
    solve_heuristic <- function(round_whole = FALSE) {
      df <- plan_input()
      n <- nrow(df)
      K <- k_val()
      W0 <- input$init_workforce
      I0 <- input$init_inv
      I_final <- input$final_inv
      
      ptype <- input$plan_type
      
      workers <- numeric(n)
      hired <- numeric(n)
      fired <- numeric(n)
      reg_prod <- numeric(n)
      ending_inv <- numeric(n)
      
      if (ptype == "chase") {
        # Chase (Zero-Inventory): Produce exactly what is needed to cover net demand
        curr_inv <- I0
        curr_w <- W0
        for (t in 1:n) {
          target_end <- if (t == n) I_final else 0
          needed_prod <- max(0, df$Demand[t] + target_end - curr_inv)
          w_req <- needed_prod / (K * df$WorkingDays[t])
          if (round_whole) w_req <- ceiling(w_req)
          
          workers[t] <- w_req
          if (w_req > curr_w) {
            hired[t] <- w_req - curr_w
            fired[t] <- 0
          } else {
            hired[t] <- 0
            fired[t] <- curr_w - w_req
          }
          reg_prod[t] <- w_req * K * df$WorkingDays[t]
          curr_inv <- curr_inv + reg_prod[t] - df$Demand[t]
          ending_inv[t] <- max(0, curr_inv)
          curr_w <- w_req
        }
      } else if (ptype == "level") {
        # Level (Constant Workforce): Equal workers across all periods
        total_demand <- sum(df$Demand) + I_final - I0
        total_worker_days <- sum(df$WorkingDays) * K
        w_level <- max(0, total_demand / total_worker_days)
        if (round_whole) w_level <- ceiling(w_level)
        
        curr_w <- W0
        curr_inv <- I0
        for (t in 1:n) {
          workers[t] <- w_level
          if (t == 1) {
            if (w_level > curr_w) hired[t] <- w_level - curr_w else fired[t] <- curr_w - w_level
          } else {
            hired[t] <- 0
            fired[t] <- 0
          }
          reg_prod[t] <- w_level * K * df$WorkingDays[t]
          curr_inv <- curr_inv + reg_prod[t] - df$Demand[t]
          ending_inv[t] <- curr_inv
          curr_w <- w_level
        }
      } else {
        # Mixed Plan: Workforce changes only at selected periods
        changes <- as.numeric(trimws(unlist(strsplit(input$mixed_change_periods, ","))))
        changes <- changes[!is.na(changes) & changes >= 1 & changes <= n]
        if (!1 %in% changes) changes <- c(1, changes)
        changes <- sort(unique(changes))
        
        # Segment and calculate level workforce for each segment
        curr_w <- W0
        curr_inv <- I0
        seg_starts <- changes
        seg_ends <- c(changes[-1] - 1, n)
        
        for (s in seq_along(seg_starts)) {
          s_start <- seg_starts[s]
          s_end <- seg_ends[s]
          idx <- s_start:s_end
          
          t_demand <- sum(df$Demand[idx])
          if (s == length(seg_starts)) t_demand <- t_demand + I_final
          t_wd <- sum(df$WorkingDays[idx]) * K
          w_seg <- max(0, (t_demand - (if(s==1) I0 else 0)) / t_wd)
          if (round_whole) w_seg <- ceiling(w_seg)
          
          for (t in idx) {
            workers[t] <- w_seg
            if (t == s_start) {
              if (w_seg > curr_w) hired[t] <- w_seg - curr_w else fired[t] <- curr_w - w_seg
            } else {
              hired[t] <- 0
              fired[t] <- 0
            }
            reg_prod[t] <- w_seg * K * df$WorkingDays[t]
            curr_inv <- curr_inv + reg_prod[t] - df$Demand[t]
            ending_inv[t] <- curr_inv
            curr_w <- w_seg
          }
        }
      }
      
      out_df <- data.frame(
        Period = df$Period,
        Demand = df$Demand,
        WorkingDays = df$WorkingDays,
        Workers = round(workers, 2),
        Hired = round(hired, 2),
        Fired = round(fired, 2),
        Regular_Prod = round(reg_prod, 1),
        Overtime_Prod = 0,
        Subcontract_Prod = 0,
        Production = round(reg_prod, 1),
        Ending_Inventory = round(ending_inv, 1)
      )
      list(plan = out_df, is_fractional = any(workers %% 1 != 0))
    }
    
    res_orig <- reactive({ solve_heuristic(round_whole = FALSE) })
    res_round <- reactive({ solve_heuristic(round_whole = TRUE) })
    
    output$has_rounded_plan <- reactive({
      res_orig()$is_fractional
    })
    outputOptions(output, "has_rounded_plan", suspendWhenHidden = FALSE)
    
    output$plan_title <- renderText({
      pt <- input$plan_type
      name <- if (pt == "chase") "Chase (Zero-Inventory) Strategy" else if (pt == "level") "Level (Constant-Workforce) Strategy" else "Mixed Aggregate Plan"
      paste0("Aggregate Strategy: ", name)
    })
    
    output$cum_plot <- renderPlot({
      plot_cumulative_plan(res_orig()$plan, "(Original Plan)")
    })
    
    output$plan_table_orig <- renderTable({
      res_orig()$plan
    })
    
    output$plan_table_rounded <- renderTable({
      res_round()$plan
    })
    
    output$cost_table_orig <- renderTable({
      calc_plan_costs(res_orig()$plan, cost_params())
    })
    
    output$cost_table_rounded <- renderTable({
      calc_plan_costs(res_round()$plan, cost_params())
    })
  })
}
