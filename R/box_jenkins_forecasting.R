# R/box_jenkins_forecasting.R
library(shiny)

box_jenkins_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Box-Jenkins (ARIMA) Forecasting"),
    sidebarLayout(
      sidebarPanel(
        fileInput(ns("file"), "Upload Data (.xlsx, .xls, .csv)", accept = c(".xlsx", ".xls", ".csv")),
        selectInput(ns("col"), "Select Demand Column", choices = NULL),
        hr(),
        selectInput(ns("bj_model_type"), "Model Family",
                    choices = c("AR(p)" = "ar",
                                "MA(q)" = "ma",
                                "ARMA(p, q)" = "arma",
                                "ARIMA(p, d, q)" = "arima",
                                "Auto ARIMA (Best AIC/BIC)" = "auto")),
        
        # Order specifications
        conditionalPanel(
          condition = sprintf("input['%s'] == 'ar'", ns("bj_model_type")),
          numericInput(ns("order_p_ar"), "AR Order (p)", value = 1, min = 1, max = 5)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'ma'", ns("bj_model_type")),
          numericInput(ns("order_q_ma"), "MA Order (q)", value = 1, min = 1, max = 5)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'arma'", ns("bj_model_type")),
          numericInput(ns("order_p_arma"), "AR Order (p)", value = 1, min = 1, max = 5),
          numericInput(ns("order_q_arma"), "MA Order (q)", value = 1, min = 1, max = 5)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'arima'", ns("bj_model_type")),
          numericInput(ns("order_p_arima"), "AR Order (p)", value = 1, min = 0, max = 5),
          numericInput(ns("order_d_arima"), "Differencing (d)", value = 1, min = 0, max = 2),
          numericInput(ns("order_q_arima"), "MA Order (q)", value = 1, min = 0, max = 5)
        ),
        
        checkboxInput(ns("include_mean"), "Include Intercept / Mean", value = TRUE),
        numericInput(ns("forecast_horizon"), "Forecast Horizon (periods ahead)", value = 3, min = 1, max = 12)
      ),
      
      mainPanel(
        h3(textOutput(ns("model_summary_title"))),
        plotOutput(ns("forecast_plot")),
        hr(),
        h4("Diagnostics: ACF & PACF of Series"),
        plotOutput(ns("acf_pacf_plot"), height = "280px"),
        hr(),
        fluidRow(
          column(6,
                 h4("Parameter Estimates & Model Summary"),
                 verbatimTextOutput(ns("model_estimates")),
                 h4("Error Measures (In-sample Fitted)"),
                 tableOutput(ns("error_table"))
          ),
          column(6,
                 h4("Forecasts Table"),
                 div(style = "max-height: 380px; overflow-y: auto;", tableOutput(ns("forecast_table")))
          )
        )
      )
    )
  )
}

box_jenkins_server <- function(id) {
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
    
    # Model fitting
    fit_model <- reactive({
      req(series())
      y <- series()
      req(length(y) >= 5)
      
      mtype <- input$bj_model_type
      inc_mean <- input$include_mean
      
      if (mtype == "ar") {
        p <- input$order_p_ar
        fit <- arima(y, order = c(p, 0, 0), include.mean = inc_mean)
        m_name <- paste0("AR(", p, ")")
      } else if (mtype == "ma") {
        q <- input$order_q_ma
        fit <- arima(y, order = c(0, 0, q), include.mean = inc_mean)
        m_name <- paste0("MA(", q, ")")
      } else if (mtype == "arma") {
        p <- input$order_p_arma
        q <- input$order_q_arma
        fit <- arima(y, order = c(p, 0, q), include.mean = inc_mean)
        m_name <- paste0("ARMA(", p, ", ", q, ")")
      } else if (mtype == "arima") {
        p <- input$order_p_arima
        d <- input$order_d_arima
        q <- input$order_q_arima
        fit <- arima(y, order = c(p, d, q), include.mean = inc_mean)
        m_name <- paste0("ARIMA(", p, ", ", d, ", ", q, ")")
      } else {
        # Auto selection (search best AIC over grid p in 0:2, d in 0:1, q in 0:2)
        best_aic <- Inf
        best_fit <- NULL
        best_order <- c(1, 0, 0)
        for (p in 0:2) {
          for (d in 0:1) {
            for (q in 0:2) {
              if (p == 0 && q == 0) next
              try({
                cand_fit <- arima(y, order = c(p, d, q), include.mean = inc_mean)
                if (cand_fit$aic < best_aic) {
                  best_aic <- cand_fit$aic
                  best_fit <- cand_fit
                  best_order <- c(p, d, q)
                }
              }, silent = TRUE)
            }
          }
        }
        fit <- if(!is.null(best_fit)) best_fit else arima(y, order = c(1, 0, 0))
        m_name <- paste0("Auto-ARIMA(", best_order[1], ", ", best_order[2], ", ", best_order[3], ")")
      }
      
      # Predictions
      h <- max(1, as.integer(input$forecast_horizon))
      pred <- predict(fit, n.ahead = h)
      
      # Fitted values: y - residuals
      fitted_vals <- y - residuals(fit)
      
      list(fit = fit, model_name = m_name, fitted = fitted_vals, pred = pred, actual = y, h = h)
    })
    
    output$model_summary_title <- renderText({
      res <- fit_model()
      next_val <- round(as.numeric(res$pred$pred)[1], 2)
      paste0(res$model_name, " | Next Period Forecast: ", next_val)
    })
    
    output$forecast_plot <- renderPlot({
      res <- fit_model()
      y <- res$actual
      n <- length(y)
      h <- res$h
      f_vals <- as.numeric(res$pred$pred)
      
      all_vals <- c(y, as.numeric(res$fitted), f_vals)
      ylim <- range(all_vals, na.rm = TRUE)
      
      plot(1:n, y, type = "b", pch = 16, col = "grey30", lwd = 2,
           xlim = c(1, n + h), ylim = ylim,
           xlab = "Period", ylab = "Demand",
           main = paste("Forecasts from", res$model_name))
      lines(1:n, res$fitted, type = "l", col = "#3182CE", lwd = 2, lty = 2)
      lines((n + 1):(n + h), f_vals, type = "b", pch = 18, col = "#E53E3E", lwd = 2, lty = 1)
      
      # 95% Confidence interval for forecast
      se_vals <- as.numeric(res$pred$se)
      lines((n + 1):(n + h), f_vals + 1.96 * se_vals, lty = 3, col = "#E53E3E")
      lines((n + 1):(n + h), f_vals - 1.96 * se_vals, lty = 3, col = "#E53E3E")
      
      legend("topleft", legend = c("Actual Demand", "In-sample Fitted", "Out-of-sample Forecast", "95% Confidence Band"),
             col = c("grey30", "#3182CE", "#E53E3E", "#E53E3E"),
             pch = c(16, NA, 18, NA), lty = c(1, 2, 1, 3), bty = "n")
    })
    
    output$acf_pacf_plot <- renderPlot({
      y <- series()
      par(mfrow = c(1, 2))
      acf(y, main = "Autocorrelation (ACF)", lag.max = min(20, length(y) - 1))
      pacf(y, main = "Partial Autocorrelation (PACF)", lag.max = min(20, length(y) - 1))
    })
    
    output$model_estimates <- renderPrint({
      res <- fit_model()
      cat("Model: ", res$model_name, "\n\n")
      print(summary(res$fit))
      cat("\nCoefficients:\n")
      print(coef(res$fit))
      cat("\nAIC:", round(res$fit$aic, 2), "\n")
    })
    
    output$error_table <- renderTable({
      res <- fit_model()
      calc_error_measures(res$actual, res$fitted)
    })
    
    output$forecast_table <- renderTable({
      res <- fit_model()
      y <- res$actual
      n <- length(y)
      h <- res$h
      
      # Future forecasts
      f_vals <- round(as.numeric(res$pred$pred), 2)
      se_vals <- round(as.numeric(res$pred$se), 2)
      lower95 <- round(f_vals - 1.96 * se_vals, 2)
      upper95 <- round(f_vals + 1.96 * se_vals, 2)
      
      data.frame(
        Period = as.character((n + 1):(n + h)),
        Forecast = f_vals,
        Std_Error = se_vals,
        Lower_95 = lower95,
        Upper_95 = upper95
      )
    })
  })
}
