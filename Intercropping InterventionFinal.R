#### Combined Monoculture vs. Intercropping Simulation ####

# 1) Load required packages
library(decisionSupport)
library(ggplot2)
library(tidyr)


set.seed()   # I could not make this arguments for that function,
#  I did not understand

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
  
#Mono culture Initial Cost
  
  mono_seed_ts <- vv(Maize_Seeds_Cost, var_CV = var_CV, n = n_years)

#Mono culture recurring Cost  
  mono_recur_val <- Pest_Weed_Management_Mono + Crop_Maintenance_Mono +
                    Irrigation_Mono
  
  pest_mono <- vv(Pest_Weed_Management_Mono, var_CV = var_CV, n = n_years)
  cropm_mono <- vv(Crop_Maintenance_Mono, var_CV = var_CV, n = n_years)
  irr_mono <- vv(Irrigation_Mono, var_CV = var_CV, n = n_years)
  
  mono_recur_ts  <- c( vv(var_mean = mono_recur_val,
                    var_CV = var_CV, n = n_years), 0)
  
  mono_cost_ts   <- mono_seed_ts + pest_mono + cropm_mono + irr_mono
  
  mono_pest_ts    <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years )
  
  mono_climate_ts <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = mono_pest_ts * (1 - Extreme_Climate_Events_MY),
    value_if_not = mono_pest_ts,
    n            = n_years )
  
  mono_rev_base_ts <- vv(
    var_mean = mono_climate_ts * Maize_Price,
    var_CV   = var_CV,
    n        = n_years )
  mono_phl_factor  <- chance_event(
    chance       = Post_Harvest_Losses,
    value_if     = 1 - Reduction_Sale_PHL,
    value_if_not = 1,
    n            = n_years )
  mono_mf_factor   <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - Reduction_Sales_MF,
    value_if_not = 1,
    n            = n_years )
  
  mono_rev_ts      <- mono_rev_base_ts * mono_phl_factor * mono_mf_factor
  
  Cashflow_Monoculture    <- mono_rev_ts - mono_cost_ts #cashflowis vs profit?
  CumCashflow_Monoculture <- cumsum(Cashflow_Monoculture)
  NPV_Monoculture         <- discount(Cashflow_Monoculture,
                                      discount_rate = discount_rate,
                                      calculate_NPV = TRUE)
  
  
  
   
  ## -- Intercropping intervention --
  
  int_seed_ts <- vv(Total_Seeds_Cost, var_CV = var_CV, n = n_years)
  
  ini_int_investment <- Training_Capacity_Int
  
  # Training Capacity reducing from the 2nd year in a relative trend
  int_initial_investment_val <- vv( var_mean      = Training_Capacity_Int,
                                var_CV         = 0,
                                n              = n_years,
                                relative_trend = -40 )
  
  ini_int_cost <- int_initial_investment_val + int_seed_ts
  
  #Intercropping Recurr Start Here
  
  int_recur_val <- Pest_Weed_Management_Int + Crop_Maintenance_Int +
                   Irrigation_Int + Additional_Labor_Int
  
  pest_int <- vv(Pest_Weed_Management_Int, var_CV = var_CV, n = n_years)
  cropm_int <- vv(Crop_Maintenance_Int, var_CV = var_CV, n = n_years)
  irr_int <- vv(Irrigation_Int, var_CV = var_CV, n = n_years)
  addl_int <- vv(Additional_Labor_Int, var_CV = var_CV, n = n_years)
  
  int_recur_ts  <- c( vv(var_mean = int_recur_val, var_CV = var_CV, n = n_years), 0)
  
  int_cost_ts   <- ini_int_cost + pest_int + cropm_int + irr_int + addl_int
  
  
  maize_adj_ts  <- chance_event(
    chance       = Pest_Disease_Chance_MY,
    value_if     = Maize_Yield * (1 - Pest_Disease_Effect_MY),
    value_if_not = Maize_Yield,
    n            = n_years )
  maize_cl_ts   <- chance_event(
    chance       = Extreme_Climate_Chance_MY,
    value_if     = maize_adj_ts * (1 - Extreme_Climate_Events_MY),
    value_if_not = maize_adj_ts,
    n            = n_years )
  cowpea_adj_ts <- chance_event(
    chance       = Pest_Disease_Chance_CY,
    value_if     = Cowpea_Yield * (1 - Pest_Disease_Effect_CY),
    value_if_not = Cowpea_Yield,
    n            = n_years )
  cowpea_cl_ts  <- chance_event(
    chance       = Extreme_Climate_Chance_CY,
    value_if     = cowpea_adj_ts * (1 - Extreme_Climate_Events_CY),
    value_if_not = cowpea_adj_ts,
    n            = n_years )
  yellow_adj_ts <- chance_event(
    chance       = Pest_Disease_Chance_YB,
    value_if     = Yellow_Beans_Yield * (1 - Pest_Disease_Effect_YB),
    value_if_not = Yellow_Beans_Yield,
    n            = n_years )
  yellow_cl_ts  <- chance_event(
    chance       = Extreme_Climate_Chance_YB,
    value_if     = yellow_adj_ts * (1 - Extreme_Climate_Events_YB),
    value_if_not = yellow_adj_ts,
    n            = n_years)
  
  int_rev_base_ts <- vv(
    var_mean = (maize_cl_ts * Maize_Price) +
      (cowpea_cl_ts * Cowpea_Price) +
      (yellow_cl_ts * Yellow_Beans_Price),
    var_CV   = var_CV,
    n        = n_years )
  int_phl_factor  <- chance_event(
    chance       = Post_Harvest_Losses,
    value_if     = 1 - Reduction_Sale_PHL,
    value_if_not = 1,
    n            = n_years )
  int_mf_factor   <- chance_event(
    chance       = Market_Fluctuation,
    value_if     = 1 - Reduction_Sales_MF,
    value_if_not = 1,
    n            = n_years )
  int_rev_ts      <- int_rev_base_ts * int_phl_factor * int_mf_factor
  
  Cashflow_Intercropping    <- int_rev_ts - int_cost_ts
  CumCashflow_Intercropping <- cumsum(Cashflow_Intercropping)
  NPV_Intercropping         <- discount(Cashflow_Intercropping,
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  
  # return both scenarios
  # — Decision difference —
  Cashflow_Decision    <- Cashflow_Intercropping - Cashflow_Monoculture
  CumCashflow_Decision <- cumsum(Cashflow_Decision)
  NPV_Decision         <- NPV_Intercropping - NPV_Monoculture
  
  # 5) Return everything
  return(list(
    NPV_Monoculture        = NPV_Monoculture,
    NPV_Intercropping      = NPV_Intercropping,
    NPV_Decision           = NPV_Decision,
    Cashflow_Monoculture   = Cashflow_Monoculture,
    Cashflow_Intercropping = Cashflow_Intercropping,
    Cashflow_Decision      = Cashflow_Decision,
    CumCashflow_Monoculture   = CumCashflow_Monoculture,
    CumCashflow_Intercropping = CumCashflow_Intercropping,
    CumCashflow_Decision      = CumCashflow_Decision
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
  vars                = c("NPV_Monoculture","NPV_Intercropping"),
  method              = "hist_simple_overlay", base_size = 7
)
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_Monoculture","NPV_Intercropping"),
  method              = "smooth_simple_overlay", base_size = 7
)



# 1) Combine inputs and both NPVs into one data frame
df_evpi <- data.frame(
  combined_simulation$x,
  NPV_Monoculture   = combined_simulation$y[, "NPV_Monoculture"],
  NPV_Intercropping = combined_simulation$y[, "NPV_Intercropping"]
)

# 2) Calculate EVPI for all inputs, starting at Monoculture_NPV
EVPI_results <- multi_EVPI(
  mc            = df_evpi,
  first_out_var = "NPV_Monoculture"
)

# 3) Inspect the EVPI table
print(EVPI_results)

# 4) Plot EVPI for both scenarios side by side
plot_evpi(
  EVPIresults   = EVPI_results,
  decision_vars = c("NPV_Monoculture", "NPV_Intercropping")
)



#### Cashflow (Annual Profit) Comparison for Both Scenarios ####

# 1) Plot the annual profit (“cashflow”) time series for both scenarios
plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("Cashflow_Monoculture", "Cashflow_Intercropping"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (USD/ha)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("Monoculture", "Intercropping")
)

# 2) Identify the exact column names for Year-n_years
mono_cols <- grep("^Cashflow_Monoculture\\.", names(combined_simulation$y), value = TRUE)
int_cols  <- grep("^Cashflow_Intercropping\\.", names(combined_simulation$y), value = TRUE)

final_year_col_mono <- mono_cols[grepl(paste0("\\.", n_years, "$"), mono_cols)][1]
final_year_col_int  <- int_cols[ grepl(paste0("\\.", n_years, "$"), int_cols)][1]

# 3) Extract those columns as numeric vectors
mono_final <- combined_simulation$y[[final_year_col_mono]]
int_final  <- combined_simulation$y[[final_year_col_int ]]

# 4) Compute the 30th and 70th percentiles for Year-n_years
mono_q <- quantile(mono_final, probs = c(0.3, 0.7))
int_q  <- quantile(int_final,  probs = c(0.3, 0.7))

# 5) Print the results
cat("Monoculture Year", n_years, "Profit 30th & 70th percentiles:\n")
print(mono_q)
cat("\nIntercropping Year", n_years, "Profit 30th & 70th percentiles:\n")
print(int_q)


#### Additional Result Graphs ####

# 1) Side-by-side boxplots of NPV
npv_df <- data.frame(
  Monoculture   = combined_simulation$y[, "NPV_Monoculture"],
  Intercropping = combined_simulation$y[, "NPV_Intercropping"]
)
npv_long <- pivot_longer(
  npv_df,
  cols      = everything(),
  names_to  = "Scenario",
  values_to = "NPV"
)
ggplot(npv_long, aes(x = Scenario, y = NPV, fill = Scenario)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal(base_size = 14) +
  labs(
    title = "NPV Distribution: Monoculture vs. Intercropping",
    x     = NULL,
    y     = "Net Present Value (USD/ha)"
  ) +
  scale_fill_manual(values = c("grey40", "forestgreen")) +
  theme(legend.position = "none")


