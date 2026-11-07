# R/ap_helpers.R
library(readxl)

# Helper: Read uploaded plan data
read_ap_input_file <- function(file_path, file_name) {
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

# Helper: Compute or retrieve productivity K (units per worker per day)
calc_k_value <- function(k_mode, k_direct, past_units, past_workers, past_days) {
  if (k_mode == "direct") {
    return(max(0.001, as.numeric(k_direct)))
  } else {
    denom <- as.numeric(past_workers) * as.numeric(past_days)
    if (is.na(denom) || denom <= 0) return(1)
    k_calc <- as.numeric(past_units) / denom
    return(max(0.001, k_calc))
  }
}

# Helper: Calculate cumulative net demand
calc_cumulative_net_demand <- function(demand, initial_inv, final_inv_target) {
  n <- length(demand)
  cum_d <- cumsum(demand) - initial_inv
  # Add target safety buffer evenly or at final period
  cum_net <- pmax(0, cum_d)
  return(cum_net)
}

# Helper: Standard AP cost calculation
calc_plan_costs <- function(plan_df, costs) {
  # costs list: c_hire, c_fire, c_hold, c_regular, c_overtime, c_subcontract
  hiring_cost <- sum(plan_df$Hired, na.rm = TRUE) * costs$hire
  firing_cost <- sum(plan_df$Fired, na.rm = TRUE) * costs$fire
  holding_cost <- sum(plan_df$Ending_Inventory, na.rm = TRUE) * costs$hold
  regular_cost <- sum(plan_df$Regular_Prod, na.rm = TRUE) * costs$regular
  overtime_cost <- sum(plan_df$Overtime_Prod, na.rm = TRUE) * costs$overtime
  subcontract_cost <- sum(plan_df$Subcontract_Prod, na.rm = TRUE) * costs$subcontract
  
  total_cost <- hiring_cost + firing_cost + holding_cost + regular_cost + overtime_cost + subcontract_cost
  
  data.frame(
    Cost_Component = c("Hiring Cost", "Firing Cost", "Holding Cost", 
                       "Regular Time Cost", "Overtime Cost", "Subcontracting Cost", 
                       "TOTAL COST"),
    Amount = round(c(hiring_cost, firing_cost, holding_cost, regular_cost, overtime_cost, subcontract_cost, total_cost), 2)
  )
}

# Helper: Standard cumulative plot
plot_cumulative_plan <- function(plan_df, title_suffix = "") {
  periods <- 1:nrow(plan_df)
  cum_demand <- cumsum(plan_df$Demand)
  cum_prod <- cumsum(plan_df$Production)
  
  max_val <- max(c(cum_demand, cum_prod), na.rm = TRUE) * 1.1
  
  plot(periods, cum_demand, type = "b", pch = 16, col = "#E53E3E", lwd = 2,
       xlab = "Period", ylab = "Cumulative Units", ylim = c(0, max_val),
       xaxt = "n", main = paste("Cumulative Demand vs Cumulative Production", title_suffix))
  axis(1, at = periods, labels = plan_df$Period)
  lines(periods, cum_prod, type = "b", pch = 17, col = "#2B6CB0", lwd = 2, lty = 2)
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
  legend("topleft", legend = c("Cumulative Demand", "Cumulative Production"),
         col = c("#E53E3E", "#2B6CB0"), pch = c(16, 17), lty = c(1, 2), lwd = 2, bty = "n")
}
