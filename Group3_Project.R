
# 1) Load the decisionSupport package
library(decisionSupport)

# 2) Read your cleaned input table
input_data <- read.csv("Input_File.csv", stringsAsFactors = FALSE)

# 3) Helper to grab one draw of all inputs into the global environment
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (i in colnames(x)) {
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
  }
}

# Optional debug: pull in one random draw of all inputs
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
  
  # — Accounting for pest disease risk on each crop —
  Maize_Adjusted_Yield <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years
  )
  
  Cowpea_Adjusted_Yield <- chance_event(
    chance       = Pest_Disease_Chance_CY,
    value_if     = Cowpea_Yield * (1 - Pest_Disease_Effect_CY),
    value_if_not = Cowpea_Yield,
    n            = n_years
  )
  
  Yellow_Beans_Adjusted_Yield <- chance_event(
    chance       = Pest_Disease_Chance_YB,
    value_if     = Yellow_Beans_Yield * (1 - Pest_Disease_Effect_YBY),
    value_if_not = Yellow_Beans_Yield,
    n            = n_years
  )
  
  # — Accounting for extreme climate risk on each crop —
  Maize_Climate_Adjusted_Yield <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = Maize_Adjusted_Yield * (1 - Extreme_Climate_Events_MY),
    value_if_not = Maize_Adjusted_Yield,
    n            = n_years
  )
  
  Cowpea_Climate_Adjusted_Yield <- chance_event(
    chance       = Extreme_Climate_Chance_CY,
    value_if     = Cowpea_Adjusted_Yield * (1 - Extreme_Climate_Events_CY),
    value_if_not = Cowpea_Adjusted_Yield,
    n            = n_years
  )
  
  Yellow_Beans_Climate_Adjusted_Yield <- chance_event(
    chance       = Extreme_Climate_Chance_YB,
    value_if     = Yellow_Beans_Adjusted_Yield * (1 - Extreme_Climate_Events_YB),
    value_if_not = Yellow_Beans_Adjusted_Yield,
    n            = n_years
  )
  
  # — Accounting for post-harvest logistics risk —
  # — Accounting for post-harvest logistics risk —
  PHL_Sales_Factor <- chance_event(
    chance       = Post_Harvest_Logistics,
    value_if     = 1 - Reduction_Sale_PHL,    # use the CSV name
    value_if_not = 1,
    n            = n_years )
  
  
  # — Accounting for market fluctuation risk —
  MF_Price_Factor <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - Reduction_Sales_MF,
    value_if_not = 1,
    n            = n_years
  )
  
  # Revenue per year from all three crops, after all risks
  revenue_value <- 
    (Maize_Climate_Adjusted_Yield    * Maize_Price) +
    (Cowpea_Climate_Adjusted_Yield   * Cowpea_Price) +
    (Yellow_Beans_Climate_Adjusted_Yield * Yellow_Beans_Price)
  
  # apply post-harvest and market factors
  revenue_ts <- vv(var_mean = revenue_value, var_CV = 0, n = n_years) *
    PHL_Sales_Factor * MF_Price_Factor
  
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

# 7) Optional: smooth overlay
plot_distributions(
  mcSimulation_object = intercropping_mc_simulation,
  vars                = "Net_Present_Value",
  method              = "smooth_simple_overlay",
  base_size           = 7
)











































# 1) Load the decisionSupport package

library(decisionSupport)


# 2) Read your cleaned input table
input_data <- read.csv("Input_File.csv", stringsAsFactors = FALSE)

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
    0 )
  
  # Total cost per year
  total_cost_ts <- initial_investment_ts + recurring_operational_ts
  
  # — Accounting for pest disease risk on each crop —
  Maize_Adjusted_Yield <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years )
  
  Cowpea_Adjusted_Yield <- chance_event(
    chance       = Pest_Disease_Chance_CY,
    value_if     = Cowpea_Yield * (1 - Pest_Disease_Effect_CY),
    value_if_not = Cowpea_Yield,
    n            = n_years )
  
  Yellow_Beans_Adjusted_Yield <- chance_event(
    chance       = Pest_Disease_Chance_YB,
    value_if     = Yellow_Beans_Yield * (1 - Pest_Disease_Effect_YBY),
    value_if_not = Yellow_Beans_Yield,
    n = n_years )
  
  # — Accounting for extreme climate risk on each crop —
  
  Maize_Climate_Adjusted_Yield <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = Maize_Adjusted_Yield * (1 - Extreme_Climate_Events_MY),
    value_if_not = Maize_Adjusted_Yield,
    n            = n_years )
  
  Cowpea_Climate_Adjusted_Yield <- chance_event(
    chance       = Extreme_Climate_Chance_CY,
    value_if     = Cowpea_Adjusted_Yield * (1 - Extreme_Climate_Events_CY),
    value_if_not = Cowpea_Adjusted_Yield,
    n            = n_years )
  
  Yellow_Beans_Climate_Adjusted_Yield <- chance_event(
    chance       = Extreme_Climate_Chance_YB,
    value_if     = Yellow_Beans_Adjusted_Yield * (1 - Extreme_Climate_Events_YB),
    value_if_not = Yellow_Beans_Adjusted_Yield,
    n            = n_years )
  
  
  # — Accounting for post-harvest logistics risk 
  # 1) Simulate a “sales factor” each year:
  #    if a logistics shock occurs, you lose PHL_Sales_Reduction fraction of sales,
  #    otherwise you keep 100% (factor = 1).
  PHL_Sales_Factor <- chance_event(
    chance       = Post_Harvest_Logistics_Chance,
    value_if     = 1 - PHL_Sales_Reduction,  # e.g. 0.8 if you lose 20% of sales
    value_if_not = 1,
    n            = n_years )
  
  # — Accounting for market fluctuation risk —
  
  # 1) Simulate a “price factor” each year:
  #    if a market shock occurs, you receive only (1 - MF_Sales_Reduction) of your post-harvest revenue,
  #    otherwise you get full amount.
  MF_Price_Factor <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - MF_Sales_Reduction,  # e.g. 0.85 if a 15% price drop
    value_if_not = 1,
    n            = n_years )
  
  # Revenue per year from all three crops
  revenue_value <- (Maize_Adjusted_Yield * Maize_Price) +
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






























