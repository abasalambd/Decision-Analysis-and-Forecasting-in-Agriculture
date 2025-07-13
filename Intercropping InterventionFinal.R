#### Combined Monoculture vs. Intercropping Simulation ####

# 1) Load required packages
library(decisionSupport)
library(ggplot2)

# 2) Read the unified input table
input_data <- read.csv("data/Input_File.csv", stringsAsFactors = FALSE)

# 3) Convert to estimate object and draw one set of inputs into the environment
estimates <- as.estimate(input_data)
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (nm in colnames(x)) {
    assign(nm, as.numeric(x[1, nm]), envir = .GlobalEnv)
  }
}
make_variables(estimates, n = 1)

# 4) Define a single model function that computes both scenarios
model_function <- function() {
  # -- Monoculture baseline --
  mono_seed_ts <- c(Maize_Seeds_Cost, rep(0, n_years - 1))
  mono_recur_val <- Pest_Weed_Management_Mono +
    Crop_Maintenance_Mono +
    Irrigation_Mono
  mono_recur_ts  <- c(
    vv(var_mean = mono_recur_val, var_CV = var_CV, n = n_years - 1),
    0
  )
  mono_cost_ts   <- mono_seed_ts + mono_recur_ts
  
  mono_pest_ts    <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years
  )
  mono_climate_ts <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = mono_pest_ts * (1 - Extreme_Climate_Events_MY),
    value_if_not = mono_pest_ts,
    n            = n_years
  )
  
  mono_rev_base_ts <- vv(
    var_mean = mono_climate_ts * Maize_Price,
    var_CV   = var_CV,
    n        = n_years
  )
  mono_phl_factor  <- chance_event(
    chance       = Post_Harvest_Losses,
    value_if     = 1 - Reduction_Sale_PHL,
    value_if_not = 1,
    n            = n_years
  )
  mono_mf_factor   <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - Reduction_Sales_MF,
    value_if_not = 1,
    n            = n_years
  )
  mono_rev_ts      <- mono_rev_base_ts * mono_phl_factor * mono_mf_factor
  
  mono_profit_ts <- mono_rev_ts - mono_cost_ts
  mono_npv       <- discount(
    x             = mono_profit_ts,
    discount_rate = discount_rate,
    calculate_NPV = TRUE
  )
  
  # -- Intercropping intervention --
  # initial investment: combined seed cost + training capacity in year 1
  int_initial_investment_val <- Total_Seeds_Cost + Training_Capacity_Int
  int_initial_investment_ts  <- c(int_initial_investment_val, rep(0, n_years - 1))
  
  # recurring operational costs Years 1–6 (adds extra labor)
  int_recur_val <- Pest_Weed_Management_Int +
    Crop_Maintenance_Int +
    Irrigation_Int +
    Additional_Labor_Int
  int_recur_ts  <- c(
    vv(var_mean = int_recur_val, var_CV = var_CV, n = n_years - 1),
    0
  )
  int_cost_ts   <- int_initial_investment_ts + int_recur_ts
  
  maize_adj_ts  <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years
  )
  maize_cl_ts   <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = maize_adj_ts * (1 - Extreme_Climate_Events_MY),
    value_if_not = maize_adj_ts,
    n            = n_years
  )
  cowpea_adj_ts <- chance_event(
    chance       = Pest_Disease_Chance_CY,
    value_if     = Cowpea_Yield * (1 - Pest_Disease_Effect_CY),
    value_if_not = Cowpea_Yield,
    n            = n_years
  )
  cowpea_cl_ts  <- chance_event(
    chance       = Extreme_Climate_Chance_CY,
    value_if     = cowpea_adj_ts * (1 - Extreme_Climate_Events_CY),
    value_if_not = cowpea_adj_ts,
    n            = n_years
  )
  yellow_adj_ts <- chance_event(
    chance       = Pest_Disease_Chance_YB,
    value_if     = Yellow_Beans_Yield * (1 - Pest_Disease_Effect_YB),
    value_if_not = Yellow_Beans_Yield,
    n            = n_years
  )
  yellow_cl_ts  <- chance_event(
    chance       = Extreme_Climate_Chance_YB,
    value_if     = yellow_adj_ts * (1 - Extreme_Climate_Events_YB),
    value_if_not = yellow_adj_ts,
    n            = n_years
  )
  
  int_rev_base_ts <- vv(
    var_mean = (maize_cl_ts * Maize_Price) +
      (cowpea_cl_ts * Cowpea_Price) +
      (yellow_cl_ts * Yellow_Beans_Price),
    var_CV   = var_CV,
    n        = n_years
  )
  int_phl_factor  <- chance_event(
    chance       = Post_Harvest_Losses,
    value_if     = 1 - Reduction_Sale_PHL,
    value_if_not = 1,
    n            = n_years
  )
  int_mf_factor   <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - Reduction_Sales_MF,
    value_if_not = 1,
    n            = n_years
  )
  int_rev_ts      <- int_rev_base_ts * int_phl_factor * int_mf_factor
  
  int_profit_ts <- int_rev_ts - int_cost_ts
  int_npv       <- discount(
    x             = int_profit_ts,
    discount_rate = discount_rate,
    calculate_NPV = TRUE
  )
  
  # return both scenarios
  return(list(
    Monoculture_Profit   = mono_profit_ts,
    Monoculture_NPV      = mono_npv,
    Intercropping_Profit = int_profit_ts,
    Intercropping_NPV    = int_npv
  ))
}

# 5) Run a single Monte Carlo simulation for both scenarios
combined_simulation <- mcSimulation(
  estimate          = estimates,
  model_function    = model_function,
  numberOfModelRuns = 10000,
  functionSyntax    = "plainNames"
)

# 6) Plot both NPV distributions together
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("Monoculture_NPV", "Intercropping_NPV"),
  method              = "hist_simple_overlay",
  base_size           = 7
)
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("Monoculture_NPV", "Intercropping_NPV"),
  method              = "smooth_simple_overlay",
  base_size           = 7
)
