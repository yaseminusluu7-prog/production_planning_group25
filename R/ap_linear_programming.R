# R/ap_linear_programming.R
library(shiny)

ap_lp_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Aggregate Planning: Linear Programming (LP)"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Plan Data (.xlsx, .xls, .csv)", accept = c(".xlsx", ".xls", ".csv")),
        hr(),
        h4("LP Model Capabilities & Options"),
        checkboxInput(ns("opt_overtime"), "Allow Overtime Production", value = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("opt_overtime")),
          numericInput(ns("max_ot_pct"), "Max Overtime (% of Regular Time)", value = 25, min = 0, max = 100)
        ),
        checkboxInput(ns("opt_idle"), "Allow Idle Time (Under-utilization)", value = TRUE),
        checkboxInput(ns("opt_subcontract"), "Allow Subcontracting", value = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("opt_subcontract")),
          numericInput(ns("max_sub_per_period"), "Max Subcontract Units / Period", value = 1000, min = 0)
        ),
        hr(),
        h4("Plant & Initial Data"),
        numericInput(ns("init_workforce"), "Starting Workforce (W0)", value = 15, min = 0),
        numericInput(ns("init_inv"), "Starting Inventory (I0)", value = 200, min = 0),
        numericInput(ns("final_inv"), "Ending Inventory Target (In)", value = 200, min = 0),
        hr(),
        h4("Productivity (K)"),
        radioButtons(ns("k_mode"), "Productivity Input Mode",
                     choices = c("Enter Directly" = "direct", "Compute from Past Data" = "compute")),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'direct'", ns("k_mode")),
          numericInput(ns("k_direct"), "K (units/worker/day)", value = 1.0, min = 0.01)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'compute'", ns("k_mode")),
          numericInput(ns("past_units"), "Past Total Units", value = 3000),
          numericInput(ns("past_workers"), "Past Workers", value = 15),
          numericInput(ns("past_days"), "Past Working Days", value = 20)
        ),
        hr(),
        h4("Unit Costs ($)"),
        numericInput(ns("c_hire"), "Hiring ($/worker)", value = 500, min = 0),
        numericInput(ns("c_fire"), "Firing ($/worker)", value = 800, min = 0),
        numericInput(ns("c_hold"), "Holding ($/unit/period)", value = 4, min = 0),
        numericInput(ns("c_regular"), "Regular Time ($/unit)", value = 20, min = 0),
        numericInput(ns("c_overtime"), "Overtime ($/unit)", value = 30, min = 0),
        numericInput(ns("c_subcontract"), "Subcontracting ($/unit)", value = 40, min = 0),
        numericInput(ns("c_idle"), "Idle Time ($/worker/day)", value = 10, min = 0)
      ),
      mainPanel(
        h3("Optimal LP Aggregate Production Plan"),
        plotOutput(ns("cum_plot")),
        hr(),
        fluidRow(
          column(8,
                 h4("Optimal Production Schedule"),
                 tableOutput(ns("plan_table")),
                 conditionalPanel(
                   condition = "output.has_rounded_plan == true",
                   h4("Optimal Schedule (Whole Workers Rounded Up)"),
                   tableOutput(ns("plan_table_rounded"))
                 )
          ),
          column(4,
                 h4("Cost Breakdown"),
                 tableOutput(ns("cost_table")),
                 conditionalPanel(
                   condition = "output.has_rounded_plan == true",
                   h4("Whole Workers Cost"),
                   tableOutput(ns("cost_table_rounded"))
                 )
          )
        )
      )
    )
  )
}

ap_lp_server <- function(id) {
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
    
    k_val <- reactive({
      calc_k_value(input$k_mode, input$k_direct, input$past_units, input$past_workers, input$past_days)
    })
    
    solve_lp_model <- function(round_whole = FALSE) {
      if (!requireNamespace("lpSolve", quietly = TRUE)) {
        install.packages("lpSolve")
      }
      library(lpSolve)
      
      df <- plan_input()
      n <- nrow(df)
      K <- k_val()
      W0 <- input$init_workforce
      I0 <- input$init_inv
      I_final <- input$final_inv
      
      allow_ot <- input$opt_overtime
      allow_idle <- input$opt_idle
      allow_sub <- input$opt_subcontract
      
      ot_pct <- (input$max_ot_pct / 100)
      max_sub <- input$max_sub_per_period
      
      # Variables per period t:
      # W[t] (workforce)
      # H[t] (hired)
      # F[t] (fired)
      # I[t] (ending inventory)
      # P_reg[t] (regular production)
      # P_ot[t] (overtime production)
      # P_sub[t] (subcontract production)
      # Idle[t] (idle worker days)
      
      n_vars_per_t <- 8
      total_vars <- n * n_vars_per_t
      
      # Variable indices helper
      idx_w <- function(t) (t - 1) * 8 + 1
      idx_h <- function(t) (t - 1) * 8 + 2
      idx_f <- function(t) (t - 1) * 8 + 3
      idx_i <- function(t) (t - 1) * 8 + 4
      idx_preg <- function(t) (t - 1) * 8 + 5
      idx_pot <- function(t) (t - 1) * 8 + 6
      idx_psub <- function(t) (t - 1) * 8 + 7
      idx_idle <- function(t) (t - 1) * 8 + 8
      
      # Objective function
      obj <- numeric(total_vars)
      for (t in 1:n) {
        obj[idx_h(t)] <- input$c_hire
        obj[idx_f(t)] <- input$c_fire
        obj[idx_i(t)] <- input$c_hold
        obj[idx_preg(t)] <- input$c_regular
        obj[idx_pot(t)] <- if(allow_ot) input$c_overtime else 1e7
        obj[idx_psub(t)] <- if(allow_sub) input$c_subcontract else 1e7
        obj[idx_idle(t)] <- if(allow_idle) input$c_idle else 1e7
      }
      
      # Constraints list
      row_list <- list()
      dir_list <- c()
      rhs_list <- c()
      
      # 1. Workforce balance: W[t] - W[t-1] - H[t] + F[t] = 0
      for (t in 1:n) {
        row <- numeric(total_vars)
        row[idx_w(t)] <- 1
        if (t == 1) {
          # W0 is constant
          row[idx_h(t)] <- -1
          row[idx_f(t)] <- 1
          row_list[[length(row_list) + 1]] <- row
          dir_list <- c(dir_list, "==")
          rhs_list <- c(rhs_list, W0)
        } else {
          row[idx_w(t - 1)] <- -1
          row[idx_h(t)] <- -1
          row[idx_f(t)] <- 1
          row_list[[length(row_list) + 1]] <- row
          dir_list <- c(dir_list, "==")
          rhs_list <- c(rhs_list, 0)
        }
      }
      
      # 2. Production capacity: P_reg[t] + K*Idle[t] - K*WorkingDays[t]*W[t] = 0
      for (t in 1:n) {
        row <- numeric(total_vars)
        row[idx_preg(t)] <- 1
        row[idx_idle(t)] <- K
        row[idx_w(t)] <- -K * df$WorkingDays[t]
        row_list[[length(row_list) + 1]] <- row
        dir_list <- c(dir_list, "==")
        rhs_list <- c(rhs_list, 0)
      }
      
      # 3. Inventory balance: I[t-1] + P_reg[t] + P_ot[t] + P_sub[t] - I[t] = Demand[t]
      for (t in 1:n) {
        row <- numeric(total_vars)
        if (t > 1) row[idx_i(t - 1)] <- 1
        row[idx_preg(t)] <- 1
        row[idx_pot(t)] <- 1
        row[idx_psub(t)] <- 1
        row[idx_i(t)] <- -1
        row_list[[length(row_list) + 1]] <- row
        dir_list <- c(dir_list, "==")
        rhs_list <- c(rhs_list, if(t == 1) df$Demand[t] - I0 else df$Demand[t])
      }
      
      # 4. Final inventory constraint: I[n] >= I_final
      row_final <- numeric(total_vars)
      row_final[idx_i(n)] <- 1
      row_list[[length(row_list) + 1]] <- row_final
      dir_list <- c(dir_list, ">=")
      rhs_list <- c(rhs_list, I_final)
      
      # 5. Overtime limits
      for (t in 1:n) {
        row <- numeric(total_vars)
        row[idx_pot(t)] <- 1
        if (allow_ot) {
          row[idx_w(t)] <- -ot_pct * K * df$WorkingDays[t]
          row_list[[length(row_list) + 1]] <- row
          dir_list <- c(dir_list, "<=")
          rhs_list <- c(rhs_list, 0)
        } else {
          row_list[[length(row_list) + 1]] <- row
          dir_list <- c(dir_list, "==")
          rhs_list <- c(rhs_list, 0)
        }
      }
      
      # 6. Subcontract limits
      for (t in 1:n) {
        row <- numeric(total_vars)
        row[idx_psub(t)] <- 1
        row_list[[length(row_list) + 1]] <- row
        dir_list <- c(dir_list, "<=")
        rhs_list <- c(rhs_list, if(allow_sub) max_sub else 0)
      }
      
      # Build matrix
      mat <- do.call(rbind, row_list)
      
      # Solve LP
      res_lp <- lp("min", obj, mat, dir_list, rhs_list)
      sol <- res_lp$solution
      
      workers <- sapply(1:n, function(t) sol[idx_w(t)])
      hired <- sapply(1:n, function(t) sol[idx_h(t)])
      fired <- sapply(1:n, function(t) sol[idx_f(t)])
      p_reg <- sapply(1:n, function(t) sol[idx_preg(t)])
      p_ot <- sapply(1:n, function(t) sol[idx_pot(t)])
      p_sub <- sapply(1:n, function(t) sol[idx_psub(t)])
      inv <- sapply(1:n, function(t) sol[idx_i(t)])
      
      is_frac <- any(workers %% 1 != 0)
      
      if (round_whole && is_frac) {
        workers <- ceiling(workers)
        # recompute hires/fires with whole workers
        w_prev <- W0
        for (t in 1:n) {
          if (workers[t] > w_prev) {
            hired[t] <- workers[t] - w_prev
            fired[t] <- 0
          } else {
            hired[t] <- 0
            fired[t] <- w_prev - workers[t]
          }
          p_reg[t] <- workers[t] * K * df$WorkingDays[t]
          w_prev <- workers[t]
        }
        # recompute inventory cascade
        inv_curr <- I0
        for (t in 1:n) {
          inv_curr <- inv_curr + p_reg[t] + p_ot[t] + p_sub[t] - df$Demand[t]
          inv[t] <- inv_curr
        }
      }
      
      plan_df <- data.frame(
        Period = df$Period,
        Demand = df$Demand,
        WorkingDays = df$WorkingDays,
        Workers = round(workers, 2),
        Hired = round(hired, 2),
        Fired = round(fired, 2),
        Regular_Prod = round(p_reg, 1),
        Overtime_Prod = round(p_ot, 1),
        Subcontract_Prod = round(p_sub, 1),
        Production = round(p_reg + p_ot + p_sub, 1),
        Ending_Inventory = round(inv, 1)
      )
      
      list(plan = plan_df, is_fractional = is_frac)
    }
    
    lp_orig <- reactive({ solve_lp_model(round_whole = FALSE) })
    lp_round <- reactive({ solve_lp_model(round_whole = TRUE) })
    
    output$has_rounded_plan <- reactive({
      lp_orig()$is_fractional
    })
    outputOptions(output, "has_rounded_plan", suspendWhenHidden = FALSE)
    
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
    
    output$cum_plot <- renderPlot({
      plot_cumulative_plan(lp_orig()$plan, "(Optimal LP)")
    })
    
    output$plan_table <- renderTable({
      lp_orig()$plan
    })
    
    output$plan_table_rounded <- renderTable({
      lp_round()$plan
    })
    
    output$cost_table <- renderTable({
      calc_plan_costs(lp_orig()$plan, cost_params())
    })
    
    output$cost_table_rounded <- renderTable({
      calc_plan_costs(lp_round()$plan, cost_params())
    })
  })
}
