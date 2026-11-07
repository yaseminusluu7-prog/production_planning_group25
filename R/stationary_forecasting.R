# R/stationary_forecasting.R
library(shiny)

stationary_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Stationary Series Forecasting"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx, .xls, .csv)", accept = c(".xlsx", ".xls", ".csv")),
        selectInput(ns("col"), "Select Demand Column", choices = NULL),
        hr(),
        selectInput(ns("method"), "Select Method",
                    choices = c("Moving Average" = "ma",
                                "Weighted Moving Average" = "wma",
                                "Simple Exponential Smoothing (SES)" = "ses",
                                "Seasonal Factors (Quick & Dirty)" = "qnd")),
        
        # Method-specific inputs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'ma'", ns("method")),
          radioButtons(ns("ma_opt"), "Parameter Selection",
                       choices = c("Manual N" = "manual", "Find Best N" = "auto")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("ma_opt")),
            numericInput(ns("ma_n"), "N (Window Size)", value = 3, min = 1, step = 1)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'auto'", ns("ma_opt")),
            selectInput(ns("ma_crit"), "Optimization Criterion", choices = c("MAD", "MSE", "MAPE"))
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'wma'", ns("method")),
          textInput(ns("wma_weights"), "Weights (comma-separated, newest first or last)", value = "0.5, 0.3, 0.2"),
          helpText("Weights will be normalized automatically to sum to 1.")
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'ses'", ns("method")),
          radioButtons(ns("ses_opt"), "Alpha Selection",
                       choices = c("Manual Alpha" = "manual", "Find Best Alpha" = "auto")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("ses_opt")),
            sliderInput(ns("ses_alpha"), "Smoothing Parameter (Alpha)", min = 0.01, max = 0.99, value = 0.2, step = 0.01)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'auto'", ns("ses_opt")),
            selectInput(ns("ses_crit"), "Criterion to Minimize", choices = c("MSE", "MAD", "MAPE"))
          ),
          radioButtons(ns("ses_init_opt"), "Initial Level (Y0 / L0)",
                       choices = c("Fit Linear Regression" = "reg", "Enter Manually" = "manual", "Use First Observation" = "first")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("ses_init_opt")),
            numericInput(ns("ses_init_val"), "Initial Level Value", value = 100)
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'qnd'", ns("method")),
          numericInput(ns("qnd_season_len"), "Number of Seasons per Cycle (e.g. 4 for quarterly, 12 for monthly)", value = 4, min = 2, step = 1)
        )
      ),
      
      mainPanel(
        h3(textOutput(ns("forecast_summary"))),
        plotOutput(ns("plot")),
        hr(),
        fluidRow(
          column(5,
                 h4("Error Measures"),
                 tableOutput(ns("error_table"))
          ),
          column(7,
                 h4("Forecasts by Period"),
                 div(style = "max-height: 350px; overflow-y: auto;", tableOutput(ns("forecast_table")))
          )
        )
      )
    )
  )
}

stationary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive raw dataset
    raw_data <- reactive({
      req(input$file)
      read_uploaded_series(input$file$datapath, input$file$name)
    })
    
    # Update column choices
    observeEvent(raw_data(), {
      df <- raw_data()
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "col", choices = if(length(num_cols) > 0) num_cols else names(df))
    })
    
    # Extracted numeric demand series
    series <- reactive({
      req(raw_data(), input$col)
      df <- raw_data()
      as.numeric(df[[input$col]])
    })
    
    # Computation logic
    results <- reactive({
      req(series())
      y <- series()
      n_obs <- length(y)
      req(n_obs >= 3)
      
      method <- input$method
      forecast <- rep(NA, n_obs)
      next_forecast <- NA
      param_info <- ""
      
      if (method == "ma") {
        if (input$ma_opt == "manual") {
          N <- max(1, min(as.integer(input$ma_n), n_obs - 1))
        } else {
          crit <- input$ma_crit
          best_n <- 1
          best_val <- Inf
          for (cand in 1:min(10, n_obs - 2)) {
            cand_f <- rep(NA, n_obs)
            for (t in (cand + 1):n_obs) {
              cand_f[t] <- mean(y[(t - cand):(t - 1)])
            }
            err <- calc_error_measures(y, cand_f)
            val <- err$Value[err$Measure == crit]
            if (!is.na(val) && val < best_val) {
              best_val <- val
              best_n <- cand
            }
          }
          N <- best_n
        }
        param_info <- paste0("Moving Average (N = ", N, ")")
        for (t in (N + 1):n_obs) {
          forecast[t] <- mean(y[(t - N):(t - 1)])
        }
        next_forecast <- mean(tail(y, N))
        
      } else if (method == "wma") {
        w_raw <- as.numeric(trimws(unlist(strsplit(input$wma_weights, ","))))
        w_raw <- w_raw[!is.na(w_raw)]
        if (length(w_raw) == 0) w_raw <- c(0.5, 0.3, 0.2)
        w <- w_raw / sum(w_raw)
        k <- length(w)
        param_info <- paste0("Weighted Moving Average (k = ", k, ", Weights: [", paste(round(w, 2), collapse=", "), "])")
        if (n_obs > k) {
          for (t in (k + 1):n_obs) {
            forecast[t] <- sum(y[(t - k):(t - 1)] * rev(w))
          }
          next_forecast <- sum(tail(y, k) * rev(w))
        }
        
      } else if (method == "ses") {
        # Initial value
        init_level <- if (input$ses_init_opt == "reg") {
          calc_initial_level_trend(y)$level
        } else if (input$ses_init_opt == "manual") {
          input$ses_init_val
        } else {
          y[1]
        }
        
        run_ses <- function(alpha_val) {
          f <- rep(NA, n_obs)
          f[1] <- init_level
          for (t in 2:n_obs) {
            f[t] <- alpha_val * y[t - 1] + (1 - alpha_val) * f[t - 1]
          }
          nxt <- alpha_val * y[n_obs] + (1 - alpha_val) * f[n_obs]
          list(f = f, nxt = nxt)
        }
        
        if (input$ses_opt == "manual") {
          alpha <- input$ses_alpha
        } else {
          crit <- input$ses_crit
          grid <- seq(0.01, 0.99, by = 0.02)
          best_alpha <- 0.2
          best_err <- Inf
          for (cand in grid) {
            res <- run_ses(cand)
            err <- calc_error_measures(y, res$f)
            val <- err$Value[err$Measure == crit]
            if (!is.na(val) && val < best_err) {
              best_err <- val
              best_alpha <- cand
            }
          }
          alpha <- best_alpha
        }
        res_final <- run_ses(alpha)
        forecast <- res_final$f
        next_forecast <- res_final$nxt
        param_info <- paste0("SES (Alpha = ", round(alpha, 3), ", Initial Level = ", round(init_level, 2), ")")
        
      } else if (method == "qnd") {
        L <- max(2, as.integer(input$qnd_season_len))
        num_cycles <- floor(n_obs / L)
        if (num_cycles < 1) {
          param_info <- "Quick & Dirty Seasonal Factors (Need at least one full cycle)"
        } else {
          # Cycle averages
          cycle_data <- matrix(y[1:(num_cycles * L)], nrow = num_cycles, byrow = TRUE)
          cycle_means <- rowMeans(cycle_data)
          # Normalized seasonal factors
          season_factors <- colMeans(cycle_data / cycle_means)
          season_factors <- season_factors / mean(season_factors) # normalize
          
          overall_mean <- mean(y)
          # Fitted values
          for (t in 1:n_obs) {
            s_idx <- ((t - 1) %% L) + 1
            forecast[t] <- overall_mean * season_factors[s_idx]
          }
          next_s_idx <- ((n_obs) %% L) + 1
          next_forecast <- overall_mean * season_factors[next_s_idx]
          param_info <- paste0("Seasonal Factors (Quick & Dirty, Cycle Length = ", L, ")")
        }
      }
      
      list(
        actual = y,
        forecast = forecast,
        next_forecast = next_forecast,
        param_info = param_info
      )
    })
    
    output$forecast_summary <- renderText({
      res <- results()
      paste0(res$param_info, " | Next Period Forecast: ", round(res$next_forecast, 2))
    })
    
    output$plot <- renderPlot({
      res <- results()
      y <- res$actual
      f <- res$forecast
      n <- length(y)
      
      all_vals <- c(y, f, res$next_forecast)
      ylim <- range(all_vals, na.rm = TRUE)
      
      plot(1:n, y, type = "b", pch = 16, col = "grey30", lwd = 2,
           xlab = "Period", ylab = "Demand", ylim = ylim,
           main = paste("Actual vs Forecast -", res$param_info))
      lines(1:n, f, type = "b", pch = 17, col = "#2B6CB0", lty = 2, lwd = 2)
      points(n + 1, res$next_forecast, pch = 18, col = "#C53030", cex = 1.8)
      legend("topleft", legend = c("Actual Demand", "Forecast", "Next-Period Forecast"),
             col = c("grey30", "#2B6CB0", "#C53030"), pch = c(16, 17, 18), lty = c(1, 2, NA), bty = "n")
    })
    
    output$error_table <- renderTable({
      res <- results()
      calc_error_measures(res$actual, res$forecast)
    })
    
    output$forecast_table <- renderTable({
      res <- results()
      y <- res$actual
      f <- res$forecast
      n <- length(y)
      e <- y - f
      
      df_tbl <- data.frame(
        Period = as.character(1:n),
        Actual = round(y, 2),
        Forecast = round(f, 2),
        Error = round(e, 2)
      )
      next_row <- data.frame(
        Period = as.character(n + 1),
        Actual = NA,
        Forecast = round(res$next_forecast, 2),
        Error = NA
      )
      rbind(df_tbl, next_row)
    }, na = "-")
  })
}
