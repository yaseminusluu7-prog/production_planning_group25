# R/forecasting_helpers.R
library(readxl)

# Data reader helper (supports both .xlsx/.xls and .csv)
read_uploaded_series <- function(file_path, file_name) {
  ext <- tolower(tools::file_ext(file_name))
  if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(file_path))
  } else if (ext == "csv") {
    df <- read.csv(file_path, stringsAsFactors = FALSE)
  } else {
    stop("Unsupported file format. Please upload .xlsx, .xls, or .csv")
  }
  return(df)
}

# Error metrics calculator
calc_error_measures <- function(actual, forecast) {
  valid_idx <- which(!is.na(actual) & !is.na(forecast))
  if (length(valid_idx) == 0) {
    return(data.frame(Measure = c("MAD", "MSE", "MAPE (%)"), Value = c(NA, NA, NA)))
  }
  e <- actual[valid_idx] - forecast[valid_idx]
  mad <- mean(abs(e))
  mse <- mean(e^2)
  mape <- mean(abs(e / actual[valid_idx])) * 100
  data.frame(
    Measure = c("MAD", "MSE", "MAPE (%)"),
    Value = round(c(mad, mse, mape), 3)
  )
}

# Initial Level and Trend Estimator via Simple Linear Regression
# fit y = b0 + b1 * t on early periods
calc_initial_level_trend <- function(series, p = 4) {
  p_eff <- min(length(series), max(2, p))
  t <- 1:p_eff
  y <- series[1:p_eff]
  fit <- lm(y ~ t)
  level_0 <- unname(coef(fit)[1]) # Intercept (t=0)
  slope_0 <- unname(coef(fit)[2]) # Slope
  list(level = level_0, slope = slope_0)
}
