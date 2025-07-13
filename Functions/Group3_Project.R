




#### Monoculture####


# 1) Load the decisionSupport package
library(ggplot2)
library(decisionSupport)

# 2) Read monoculture input table
mono_data <- read.csv("Data/RA_Input_Table_Mono.csv", stringsAsFactors = FALSE)

# 3) Helper to inject one draw of all inputs into the global environment
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (nm in colnames(x)) {
    assign(nm, as.numeric(x[1, nm]), envir = .GlobalEnv)
  }
}

# Optional debug: pull one random draw of inputs
make_variables(as.estimate(mono_data), n = 1)

# 4) Define the monoculture model function
model_function_monocrop <- function() {
  # Project length (pulled from CSV)
  # n_years and var_CV created by make_variables()
  
  # Year-1 seed cost only
  init_seed_ts <- c(Maize_Seeds_Cost, rep(0, n_years - 1))
  
  # Recurring costs for Years 1–6
  recur_value   <- Pest_Weed_Management + Crop_Maintenance + Irrigation
  recurring_ts  <- c(
    vv(var_mean = recur_value, var_CV = var_CV, n = n_years - 1),
    0
  )
  
  # Total cost per year
  total_cost_ts <- init_seed_ts + recurring_ts
  
  # Pest outbreak adjustment on maize yield
  maize_pest_ts <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years
  )
  
  # Extreme climate adjustment on pest-adjusted yield
  maize_climate_ts <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = maize_pest_ts * (1 - Extreme_Climate_Events_MY),
    value_if_not = maize_pest_ts,
    n            = n_years
  )
  
  # Base revenue before losses
  revenue_base_ts <- vv(
    var_mean = maize_climate_ts * Maize_Price,
    var_CV   = var_CV,
    n        = n_years
  )
  
  # Post-harvest logistics losses
  phl_factor <- chance_event(
    chance       = Post_Harvest_Losses,
    value_if     = 1 - Reduction_Sale_PHL,
    value_if_not = 1,
    n            = n_years
  )
  
  # Market price fluctuation losses
  mf_factor <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - Reduction_Sales_MF,
    value_if_not = 1,
    n            = n_years
  )
  
  # Final revenue after all losses
  revenue_ts <- revenue_base_ts * phl_factor * mf_factor
  
  # Annual profit time series
  profit_ts <- revenue_ts - total_cost_ts
  
  # Net Present Value of profit stream
  npv <- discount(
    x             = profit_ts,
    discount_rate = discount_rate,
    calculate_NPV = TRUE
  )
  
  return(list(
    Annual_Profit = profit_ts,
    NPV           = npv
  ))
}

# 5) Run the Monte Carlo simulation for monoculture
mono_mc_simulation <- mcSimulation(
  estimate          = as.estimate(mono_data),
  model_function    = model_function_monocrop,
  numberOfModelRuns = 10000,
  functionSyntax    = "plainNames"
)

# 6) Plot the NPV distribution for the monoculture scenario
plot_distributions(
  mcSimulation_object = mono_mc_simulation,
  vars                = "NPV",
  method              = "hist_simple_overlay",
  base_size           = 7
)


# 6) Plot the NPV distribution for the monoculture scenario
plot_distributions(
  mcSimulation_object = mono_mc_simulation,
  vars                = "NPV",
  method              = "smooth_simple_overlay",
  base_size           = 7
)

































#Simulation with Intervention####

# 1) Load the decisionSupport package
library(decisionSupport)

# 2) Read cleaned input table

RA_data <- read.csv("Data/RA_Input_Table.csv", stringsAsFactors = FALSE)


# 3) Helper to grab one draw of all inputs into the global environment
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (i in colnames(x)) {
    assign(i, as.numeric(x[1, i]), envir = .GlobalEnv)
  }
}

# Optional debug: pull in one random draw of all inputs
make_variables(as.estimate(RA_data), n = 1)

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
    value_if     = Yellow_Beans_Yield * (1 - Pest_Disease_Effect_YB),
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
  
  # — Accounting for post-harvest Losses —".....Is it post harvest logistics or losses . I think it should be losses 

  PHL_Sales_Factor <- chance_event(
    chance       = Post_Harvest_Losses,
    value_if     = 1 - Reduction_Sale_PHL,  
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
  estimate          = as.estimate(RA_data),
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



#### Value of Information (EVPI) Analysis ####

# 1) After running your intercropping Monte Carlo simulation:
#    we assume `intercropping_mc_simulation` already exists

# 2) Combine inputs (x) and the NPV output (y) into one data frame
df_evpi <- data.frame(
  intercropping_mc_simulation$x,
  Net_Present_Value = intercropping_mc_simulation$y[, "Net_Present_Value"]
)

# 3) Compute EVPI for every input with respect to Net_Present_Value
EVPI_results <- multi_EVPI(
  mc            = df_evpi,
  first_out_var = "Net_Present_Value"
)

# 4) Inspect the EVPI table
print(EVPI_results)

# 5) Plot the EVPI bars for decision variable
plot_evpi(
  EVPIresults   = EVPI_results,
  decision_vars = "Net_Present_Value"
)
