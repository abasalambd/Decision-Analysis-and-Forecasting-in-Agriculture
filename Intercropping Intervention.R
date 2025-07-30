#### Intercropping in a monoculture farm through Decision Analysis (USD per acre) ####

## Load required packages
library(decisionSupport)
library(ggplot2)
library(dplyr)
library(pls)
## Read  unified input table
input_data <- read.csv("data/Input_File.csv", stringsAsFactors = FALSE)

## Convert to estimate object & draw one random sample
estimates <- as.estimate(input_data)

make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (nm in colnames(x)) {
    assign(nm, as.numeric(x[1, nm]), envir = .GlobalEnv)
  }
}

make_variables(estimates, n = 1)

## Define a single model function that computes both scenarios
model_function <- function() {
  
  #project planning
  n_years <- 7
  ### Monoculture: Initial cost
  
  mono_seed_ts <- vv(Maize_Seeds_Cost, var_CV = var_CV, n = n_years)
  
  ### Monoculture: Recurring costs  
  mono_recur_val <- Pest_Weed_Management_Mono + Crop_Maintenance_Mono +
    Irrigation_Mono
  pest_mono <- vv(Pest_Weed_Management_Mono, var_CV = var_CV, n = n_years)
  cropm_mono <- vv(Crop_Maintenance_Mono, var_CV = var_CV, n = n_years)
  irr_mono <- vv(Irrigation_Mono, var_CV = var_CV, n = n_years)
  mono_recur_ts  <- c( vv(var_mean = mono_recur_val,
                          var_CV = var_CV, n = n_years), 0)
  mono_cost_ts   <- mono_seed_ts + pest_mono + cropm_mono + irr_mono
  
  ### Monoculture: Pest & climate adjustments
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
  
  ### Monoculture: Revenue after losses
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
  
  ### Monoculture: Cashflow & NPV
  Cashflow_Monoculture    <- mono_rev_ts - mono_cost_ts 
  CumCashflow_Monoculture <- cumsum(Cashflow_Monoculture)
  NPV_Monoculture         <- discount(Cashflow_Monoculture,
                                      discount_rate = discount_rate,
                                      calculate_NPV = TRUE)
  
  ## -- Intercropping intervention --
  
  ### Intercropping intervention: Initial investment
  int_seed_ts <- vv(Total_Seeds_Cost, var_CV = var_CV, n = n_years)
  ini_int_investment <- Training_Capacity_Int
  # Training Capacity reducing from the 2nd year in a relative trend
  int_initial_investment_val <- vv( var_mean      = Training_Capacity_Int,
                                    var_CV         = 0,
                                    n              = n_years,
                                    relative_trend = -40 )
  ini_int_cost <- int_initial_investment_val + int_seed_ts
  
  ### Intercropping: Recurring costs
  
  int_recur_val <- Pest_Weed_Management_Int + Crop_Maintenance_Int +
    Irrigation_Int + Additional_Labor_Int
  pest_int <- vv(Pest_Weed_Management_Int, var_CV = var_CV, n = n_years)
  cropm_int <- vv(Crop_Maintenance_Int, var_CV = var_CV, n = n_years)
  irr_int <- vv(Irrigation_Int, var_CV = var_CV, n = n_years)
  addl_int <- vv(Additional_Labor_Int, var_CV = var_CV, n = n_years)
  int_recur_ts  <- c( vv(var_mean = int_recur_val, var_CV = var_CV,
                         n = n_years), 0)
  int_cost_ts   <- ini_int_cost + pest_int + cropm_int + irr_int + addl_int
  
  ### Intercropping: Pest & climate adjustments
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
  
  
  ### Intercropping: Revenue after losses
  int_rev_base_ts <- vv( var_mean = (maize_cl_ts * Maize_Price) +
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
  
  
  ### Intercropping: Cashflow & NPV
  Cashflow_Intercropping    <- int_rev_ts - int_cost_ts
  CumCashflow_Intercropping <- cumsum(Cashflow_Intercropping)
  NPV_Intercropping         <- discount(Cashflow_Intercropping,
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  ### Decision difference: Profit, cashflow, cumulative
  Cashflow_Decision    <- Cashflow_Intercropping - Cashflow_Monoculture
  CumCashflow_Decision <- cumsum(Cashflow_Decision)
  NPV_Decision         <- NPV_Intercropping - NPV_Monoculture
  
  CumCashflow_Monoculture   <- cumsum(Cashflow_Monoculture)
  CumCashflow_Intercropping <- cumsum(Cashflow_Intercropping)
  CumCashflow_Decision      <- cumsum(Cashflow_Decision)
  
  
  ## Return all outputs
  return(list(
    NPV_Monoculture           = NPV_Monoculture,
    NPV_Intercropping         = NPV_Intercropping,
    NPV_Decision              = NPV_Decision,
    Cashflow_Monoculture      = Cashflow_Monoculture,
    Cashflow_Intercropping    = Cashflow_Intercropping,
    Cashflow_Decision         = Cashflow_Decision,
    CumCashflow_Monoculture   = CumCashflow_Monoculture,
    CumCashflow_Intercropping = CumCashflow_Intercropping,
    CumCashflow_Decision      = CumCashflow_Decision
  ))
  
}


# Run a single Monte Carlo simulation for both scenarios
combined_simulation <- mcSimulation(
  estimate          = estimates,
  model_function    = model_function,
  numberOfModelRuns = 100,
  functionSyntax    = "plainNames" )


# Plot distributions 
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = "NPV_Monoculture",
  method              = "boxplot_density", base_size = 7,
  x_axis_name = "Outcome distribution (USD/acre)")

plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = "NPV_Intercropping",
  method              = "boxplot_density", base_size = 7,
  x_axis_name = "Outcome distribution (USD/acre)")


plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = "NPV_Decision",
  method              = "boxplot_density", base_size = 7, 
  x_axis_name = "Outcome distribution (USD/acre)" )

plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_Monoculture","NPV_Intercropping"),
  method              = "smooth_simple_overlay", base_size = 7,
  x_axis_name = "Outcome distribution (USD/acre)")


#### Cashflow (Annual Profit) Comparison for Both Scenarios ####

# Plot the annual profit (“cashflow”) time series for both scenarios
plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("Cashflow_Monoculture", "Cashflow_Intercropping"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (USD/acre)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("Monoculture", "Intercropping"))

plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("CumCashflow_Monoculture", "CumCashflow_Intercropping"),
  x_axis_name         = "Year",
  y_axis_name         = "Cumulative Annual Profit (USD/acre)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("Monoculture", "Intercropping"))


#### EVPI Analysis ####

# Build data frame of inputs + decision NPV difference
df_evpi <- data.frame(
  combined_simulation$x,
  NPV_Decision = combined_simulation$y[ , "NPV_Decision"]
)

# Calculate EVPI (starting from the decision outcome)
EVPI_results <- multi_EVPI(
  mc            = df_evpi,
  first_out_var = "NPV_Decision"
)

# Print EVPI table
print(EVPI_results)

# Plot EVPI for the decision variable
plot_evpi(
  EVPIresults   = EVPI_results,
  decision_vars = "NPV_Decision"
)


#### Variable Importance in Projection (VIP) ####

# Prepare PLS data: inputs + response
pls_data <- data.frame(
  combined_simulation$x,
  NPV_Decision = combined_simulation$y[ , "NPV_Decision"]
) %>%
  select_if(~ var(.) > 0)  # drop constant columns

# Fit PLSR model (10-fold CV, jackknife)
set.seed(123)
pls_model <- plsr(
  NPV_Decision ~ .,
  data       = pls_data,
  scale      = TRUE,
  validation = "CV",
  segments   = 10,
  jackknife  = TRUE
)

# Determine optimal number of components via RMSEP
optimal_comp <- which.min(RMSEP(pls_model)$val[1,,]) - 1

# VIP calculation helper
calculate_vip <- function(model) {
  W  <- model$loading.weights
  Tm <- model$scores
  Q  <- model$Yloadings
  SS <- (Q^2) * colSums(Tm^2)
  W2 <- W^2
  vip <- sqrt(
    nrow(W) *
      rowSums(sweep(W2, 2, SS / colSums(W2), "*")) /
      sum(SS)
  )
  names(vip) <- rownames(W)
  vip
}

# Compute VIP scores & assemble into a tibble
vip_scores <- calculate_vip(pls_model)
vip_df <- tibble(
  Variable = names(vip_scores),
  VIP      = vip_scores
) %>%
  arrange(desc(VIP))

# Plot top 15 VIPs
ggplot(head(vip_df, 15), aes(x = reorder(Variable, VIP), y = VIP)) +
  geom_col(fill = "#E15759", alpha = 0.8, width = 0.7) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(
    title    = "Variable Importance in Projection",
    subtitle = paste("Top 15 variables —", optimal_comp, "PLS components"),
    x        = NULL,
    y        = "VIP Score"
  ) +
  theme_minimal(base_size = 12)

