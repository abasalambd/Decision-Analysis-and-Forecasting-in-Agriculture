# 1) Load the decisionSupport package
library(decisionSupport)

# 2) Read your cleaned input table
input_data <- read.csv("Measurement_Framework6.csv", stringsAsFactors = FALSE)

# 3) Helper to grab one draw of all inputs into the global environment
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (i in colnames(x)) {
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
  }
}

# Optional debug: pull in one random draw
make_variables(as.estimate(input_data), n = 1)

# 4) Define your model function (plainNames syntax)
model_function <- function() {
  n_years <- 7
  
  # Initial investment in Year 1 only
  initial_investment_value <- Crop_Seeds + Training_Capacity
  initial_investment_ts    <- c(initial_investment_value, rep(0, n_years - 1))
  
  # Recurring costs Years 1–6
  recurring_operational_value <- Pest_Weed_Management +
    Crop_Maintenance + Irrigation + Additional_Labor
  recurring_operational_ts <- c(
    vv(var_mean = recurring_operational_value, var_CV = 0, n = n_years - 1),
    0
  )
  
  # Total cost per year
  total_cost_ts <- initial_investment_ts + recurring_operational_ts
  
  #accounting for pests_deaseses
  
  Maize_adjusted_yield <- chance_event(chance = Pest_Disease_Chance_MY, 
                                      value_if = Pest_Disease_Effect_MY,
                                      value_if_not = Maize_Yield,
                                      n = n_year) 
  # Revenue per year from all three crops
  revenue_value <- (Maize_adjusted_yield * Maize_Price) +
    (Cowpea_Yield * Cowpea_Price) +
    Yellow_Beans_Yield * Yellow_Beans_Price
  revenue_ts <- vv(var_mean = revenue_value, var_CV = 0, n = n_years)
  
  # Annual profit
  profit_ts <- revenue_ts - total_cost_ts
  
  
  
  
  
  
  # Discount to NPV
  net_present_value <- discount(
    x             = profit_ts,
    discount_rate = discount_rate,
    calculate_NPV = TRUE
  )
  
  return(list(
    Annual_Profit     = profit_ts,
    Net_Present_Value = net_present_value
  ))
}

# 5) Run the Monte Carlo simulation using your model and data
intercropping_mc_simulation <- mcSimulation(
  estimate          = as.estimate(input_data),
  model_function    = model_function,
  numberOfModelRuns = 10000,
  functionSyntax    = "plainNames"
)

# 6) Plot the NPV distribution
plot_distributions(
  mcSimulation_object = intercropping_mc_simulation,
  vars                = "Net_Present_Value",
  method              = "hist_simple_overlay",
  base_size           = 7
)


plot_distributions(
  mcSimulation_object = intercropping_mc_simulation,
  vars                = "Net_Present_Value",
  method              = "smooth_simple_overlay",
  base_size           = 7
)
