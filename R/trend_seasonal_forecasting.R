# R/trend_seasonal_forecasting.R
library(shiny)

trend_seasonal_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Trend & Seasonal Series Forecasting"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx, .xls, .csv)", accept = c(".xlsx", ".xls", ".csv")),
        selectInput(ns("col"), "Select Demand Column", choices = NULL),
        hr(),
        selectInput(ns("method"), "Select Method",
                    choices = c("Holt's Method (Double Exponential)" = "holt",
                                "Seasonal Decomposition (Moving Average)" = "decomp",
                                "Winters' Method (Holt-Winters)" = "winters")),
        
        # Holt inputs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'holt'", ns("method")),
          radioButtons(ns("holt_param_opt"), "Parameters (Alpha, Beta)",
                       choices = c("Manual" = "manual", "Find Best" = "auto")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("holt_param_opt")),
            sliderInput(ns("holt_alpha"), "Level Smoothing (Alpha)", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
            sliderInput(ns("holt_beta"), "Trend Smoothing (Beta)", min = 0.01, max = 0.99, value = 0.1, step = 0.01)
          ),
          radioButtons(ns("holt_init_opt"), "Initial Values (Level & Trend)",
                       choices = c("Fit Linear Regression" = "reg", "Enter Manually" = "manual")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("holt_init_opt")),
            numericInput(ns("holt_init_level"), "Initial Level (L0)", value = 100),
            numericInput(ns("holt_init_trend"), "Initial Trend (T0)", value = 0)
          )
        ),
        
        # Seasonal Decomposition inputs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'decomp'", ns("method")),
          numericInput(ns("decomp_m"), "Seasonality Period (m)", value = 4, min = 2, step = 1),
          selectInput(ns("decomp_type"), "Decomposition Type", choices = c("Multiplicative" = "mult", "Additive" = "add"))
        ),
        
        # Winters inputs
        conditionalPanel(
          condition = sprintf("input['%s'] == 'winters'", ns("method")),
          numericInput(ns("winters_m"), "Seasonality Period (m)", value = 4, min = 2, step = 1),
          selectInput(ns("winters_type"), "Model Type", choices = c("Additive" = "additive", "Multiplicative" = "multiplicative")),
          radioButtons(ns("winters_param_opt"), "Parameters (Alpha, Beta, Gamma)",
                       choices = c("Optimize (stats::HoltWinters)" = "auto", "Manual" = "manual")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("winters_param_opt")),
            sliderInput(ns("w_alpha"), "Alpha (Level)", min = 0.01, max = 0.99, value = 0.2, step = 0.01),
            sliderInput(ns("w_beta"), "Beta (Trend)", min = 0.01, max = 0.99, value = 0.1, step = 0.01),
            sliderInput(ns("w_gamma"), "Gamma (Season)", min = 0.01, max = 0.99, value = 0.1, step = 0.01)
          ),
          radioButtons(ns("winters_init_opt"), "Initial Level & Trend",
                       choices = c("Fit Linear Regression" = "reg", "Enter Manually" = "manual", "Default Heuristic" = "default")),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'manual'", ns("winters_init_opt")),
            numericInput(ns("winters_init_l"), "Initial Level (L0)", value = 100),
            numericInput(ns("winters_init_b"), "Initial Slope (b0)", value = 1)
          )
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

trend_seasonal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    raw_data <- reactive({
      req(input$file)
      read_uploaded_series(input$file$datapath, input$file$name)
    })
    
    observeEvent(raw_data(), {
      df <- raw_data()
      num_cols <- names(df)[sapply(df, is.numeric)]
      updateSelectInput(session, "col", choices = if(length(num_cols) > 0) num_cols else names(df))
    })
    
    series <- reactive({
      req(raw_data(), input$col)
      df <- raw_data()
      as.numeric(df[[input$col]])
    })
    
    results <- reactive({
      req(series())
      y <- series()
      n_obs <- length(y)
      req(n_obs >= 4)
      
      method <- input$method
      forecast <- rep(NA, n_obs)
      next_forecast <- NA
      param_info <- ""
      
      if (method == "holt") {
        # Initial Level & Trend
        if (input$holt_init_opt == "reg") {
          reg_init <- calc_initial_level_trend(y, p = min(6, n_obs))
          L0 <- reg_init$level
          T0 <- reg_init$slope
        } else {
          L0 <- input$holt_init_level
          T0 <- input$holt_init_trend
        }
        
        run_holt <- function(a, b) {
          L <- numeric(n_obs)
          Tt <- numeric(n_obs)
          f <- rep(NA, n_obs)
          
          # t = 1
          f[1] <- L0 + T0
          L[1] <- a * y[1] + (1 - a) * (L0 + T0)
          Tt[1] <- b * (L[1] - L0) + (1 - b) * T0
          
          if (n_obs > 1) {
            for (t in 2:n_obs) {
              f[t] <- L[t - 1] + Tt[t - 1]
              L[t] <- a * y[t] + (1 - a) * (L[t - 1] + Tt[t - 1])
              Tt[t] <- b * (L[t] - L[t - 1]) + (1 - b) * Tt[t - 1]
            }
          }
          nxt <- L[n_obs] + Tt[n_obs]
          list(f = f, nxt = nxt, L = L, Tt = Tt)
        }
        
        if (input$holt_param_opt == "manual") {
          alpha <- input$holt_alpha
          beta <- input$holt_beta
        } else {
          # Grid search for MSE
          best_err <- Inf
          best_a <- 0.2
          best_b <- 0.1
          for (a in seq(0.05, 0.95, by = 0.1)) {
            for (b in seq(0.05, 0.95, by = 0.1)) {
              out <- run_holt(a, b)
              mse <- mean((y - out$f)^2, na.rm = TRUE)
              if (!is.na(mse) && mse < best_err) {
                best_err <- mse
                best_a <- a
                best_b <- b
              }
            }
          }
          alpha <- best_a
          beta <- best_b
        }
        
        h_res <- run_holt(alpha, beta)
        forecast <- h_res$f
        next_forecast <- h_res$nxt
        param_info <- paste0("Holt's Method (Alpha = ", round(alpha, 2), ", Beta = ", round(beta, 2), 
                             ", L0 = ", round(L0, 2), ", T0 = ", round(T0, 2), ")")
        
      } else if (method == "decomp") {
        m <- max(2, as.integer(input$decomp_m))
        req(n_obs >= 2 * m)
        y_ts <- ts(y, frequency = m)
        d_type <- input$decomp_type
        
        # Classical decomposition
        decomp_obj <- decompose(y_ts, type = if(d_type == "mult") "multiplicative" else "additive")
        trend_comp <- decomp_obj$trend
        seas_comp <- decomp_obj$seasonal
        
        # Fit trend line to extrapolate next period
        valid_t <- which(!is.na(trend_comp))
        trend_fit <- lm(trend_comp[valid_t] ~ valid_t)
        fitted_trend <- predict(trend_fit, newdata = data.frame(valid_t = 1:n_obs))
        next_trend <- predict(trend_fit, newdata = data.frame(valid_t = n_obs + 1))
        
        if (d_type == "mult") {
          forecast <- fitted_trend * seas_comp[1:n_obs]
          next_seas <- seas_comp[((n_obs) %% m) + 1]
          next_forecast <- next_trend * next_seas
          param_info <- paste0("Seasonal Decomposition (Multiplicative, m = ", m, ")")
        } else {
          forecast <- fitted_trend + seas_comp[1:n_obs]
          next_seas <- seas_comp[((n_obs) %% m) + 1]
          next_forecast <- next_trend + next_seas
          param_info <- paste0("Seasonal Decomposition (Additive, m = ", m, ")")
        }
        
      } else if (method == "winters") {
        m <- max(2, as.integer(input$winters_m))
        req(n_obs >= 2 * m)
        y_ts <- ts(y, frequency = m)
        w_type <- input$winters_type
        
        # Initial values handling
        l_start <- NULL
        b_start <- NULL
        if (input$winters_init_opt == "reg") {
          reg_init <- calc_initial_level_trend(y, p = 2 * m)
          l_start <- reg_init$level
          b_start <- reg_init$slope
        } else if (input$winters_init_opt == "manual") {
          l_start <- input$winters_init_l
          b_start <- input$winters_init_b
        }
        
        if (input$winters_param_opt == "auto") {
          hw_fit <- HoltWinters(y_ts, seasonal = w_type, l.start = l_start, b.start = b_start)
          a <- hw_fit$alpha
          b <- hw_fit$beta
          g <- hw_fit$gamma
        } else {
          a <- input$w_alpha
          b <- input$w_beta
          g <- input$w_gamma
          hw_fit <- HoltWinters(y_ts, alpha = a, beta = b, gamma = g, seasonal = w_type, l.start = l_start, b.start = b_start)
        }
        
        # Fitted values
        fitted_mat <- hw_fit$fitted
        fitted_len <- nrow(fitted_mat)
        forecast[(n_obs - fitted_len + 1):n_obs] <- fitted_mat[, "xhat"]
        
        pred <- predict(hw_fit, n.ahead = 1)
        next_forecast <- as.numeric(pred)[1]
        param_info <- paste0("Winters' Method (", tools::toTitleCase(w_type), 
                             ", Alpha = ", round(a, 2), ", Beta = ", round(b, 2), ", Gamma = ", round(g, 2), ")")
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
      lines(1:n, f, type = "b", pch = 17, col = "#3182CE", lty = 2, lwd = 2)
      points(n + 1, res$next_forecast, pch = 18, col = "#E53E3E", cex = 1.8)
      legend("topleft", legend = c("Actual Demand", "Forecast", "Next-Period Forecast"),
             col = c("grey30", "#3182CE", "#E53E3E"), pch = c(16, 17, 18), lty = c(1, 2, NA), bty = "n")
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
