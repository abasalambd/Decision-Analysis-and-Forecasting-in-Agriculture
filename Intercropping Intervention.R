
# 1) Load required packages
library(decisionSupport)
library(ggplot2)

# 2) Read the unified input table (contains both monoculture and intervention parameters)
input_data <- read.csv("Data/Input_Table.csv", stringsAsFactors = FALSE)

# 3) Helper to inject one random draw of all inputs into the global environment
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (nm in colnames(x)) {
    assign(nm, as.numeric(x[1, nm]), envir = .GlobalEnv)
  }
}

# 4) Define single model function that computes both scenarios
model_function <- function() {
  # time horizon and coefficient of variation
  # (created via make_variables() from input_data)
  
  # --- Monoculture baseline ---
  # Year-1 seed cost only
  mono_seed_ts <- c(Maize_Seeds_Cost, rep(0, n_years - 1))
  # Recurring costs (Years 1–6)
  mono_recur_val <- Pest_Weed_Management + Crop_Maintenance + Irrigation
  mono_recur_ts  <- c(vv(var_mean = mono_recur_val, var_CV = var_CV, n = n_years - 1), 0)
  mono_cost_ts   <- mono_seed_ts + mono_recur_ts
  
  # Pest & climate adjustments on maize yield
  mono_pest_ts    <- chance_event(Pest_Disease_Chance_MY,
                                  Maize_Yield*(1 - Pest_Disease_Effect_MY),
                                  Maize_Yield, n = n_years)
  mono_climate_ts <- chance_event(Extreme_Climate_Chance_MY,
                                  mono_pest_ts*(1 - Extreme_Climate_Events_MY),
                                  mono_pest_ts, n = n_years)
  
  # Revenue after losses
  mono_rev_base_ts <- vv(var_mean = mono_climate_ts * Maize_Price,
                         var_CV = var_CV, n = n_years)
  mono_phl_factor  <- chance_event(Post_Harvest_Losses, 1 - Reduction_Sale_PHL, 1, n_years)
  mono_mf_factor   <- chance_event(Market_Fluctuation, 1 - Reduction_Sales_MF, 1, n_years)
  mono_rev_ts      <- mono_rev_base_ts * mono_phl_factor * mono_mf_factor
  
  # Profit and NPV
  mono_profit_ts <- mono_rev_ts - mono_cost_ts
  mono_npv       <- discount(x = mono_profit_ts, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  # --- Intercropping intervention ---
  # Year-1 seed + training cost
  int_seed_val <- Crop_Seeds + Training_Capacity
  int_seed_ts  <- c(int_seed_val, rep(0, n_years - 1))
  # Recurring costs (Years 1–6)
  int_recur_val <- Pest_Weed_Management + Crop_Maintenance + Irrigation + Additional_Labor
  int_recur_ts  <- c(vv(var_mean = int_recur_val, var_CV = var_CV, n = n_years - 1), 0)
  int_cost_ts   <- int_seed_ts + int_recur_ts
  
  # Pest & climate adjustments on all three crops
  maize_adj_ts <- chance_event(Pest_Disease_Chance_MY,
                               Maize_Yield*(1 - Pest_Disease_Effect_MY),
                               Maize_Yield, n = n_years)
  maize_cl_ts  <- chance_event(Extreme_Climate_Chance_MY,
                               maize_adj_ts*(1 - Extreme_Climate_Events_MY),
                               maize_adj_ts, n = n_years)
  
  cowpea_adj_ts <- chance_event(Pest_Disease_Chance_CY,
                                Cowpea_Yield*(1 - Pest_Disease_Effect_CY),
                                Cowpea_Yield, n = n_years)
  cowpea_cl_ts  <- chance_event(Extreme_Climate_Chance_CY,
                                cowpea_adj_ts*(1 - Extreme_Climate_Events_CY),
                                cowpea_adj_ts, n = n_years)
  
  yellow_adj_ts <- chance_event(Pest_Disease_Chance_YB,
                                Yellow_Beans_Yield*(1 - Pest_Disease_Effect_YB),
                                Yellow_Beans_Yield, n = n_years)
  yellow_cl_ts  <- chance_event(Extreme_Climate_Chance_YB,
                                yellow_adj_ts*(1 - Extreme_Climate_Events_YB),
                                yellow_adj_ts, n = n_years)
  
  # Revenue from three crops
  int_rev_base_ts <- vv(var_mean = (maize_cl_ts * Maize_Price) +
                          (cowpea_cl_ts * Cowpea_Price) +
                          (yellow_cl_ts * Yellow_Beans_Price),
                        var_CV = var_CV, n = n_years)
  int_phl_factor  <- chance_event(Post_Harvest_Losses, 1 - Reduction_Sale_PHL, 1, n_years)
  int_mf_factor   <- chance_event(Market_Fluctuation, 1 - Reduction_Sales_MF, 1, n_years)
  int_rev_ts      <- int_rev_base_ts * int_phl_factor * int_mf_factor
  
  # Profit and NPV
  int_profit_ts <- int_rev_ts - int_cost_ts
  int_npv       <- discount(x = int_profit_ts, discount_rate = discount_rate, calculate_NPV = TRUE)
  
  # Return both scenarios
  return(list(
    Monoculture_Profit      = mono_profit_ts,
    Monoculture_NPV         = mono_npv,
    Intercropping_Profit    = int_profit_ts,
    Intercropping_NPV       = int_npv
  ))
}

# 5) Run a single Monte Carlo simulation for both scenarios
combined_simulation <- mcSimulation(
  estimate          = as.estimate(input_data),
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
