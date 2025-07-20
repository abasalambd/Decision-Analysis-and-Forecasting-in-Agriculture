#### Intercropping in a monoculture farm through Decision Analysis ####

## Load required packages
library(decisionSupport)
library(ggplot2)

# Read the unified input table
input_data <- read.csv("data/Input_File.csv", stringsAsFactors = FALSE)

# Convert to estimate object and draw one set of inputs into the environment
estimates <- as.estimate(input_data)
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (nm in colnames(x)) {
    assign(nm, as.numeric(x[1, nm]), envir = .GlobalEnv)
  }
}

make_variables(estimates, n = 1)

# Define a single model function that computes both scenarios
model_function <- function() {
  
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
  
  
  ## Return all outputs
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


# Run a single Monte Carlo simulation for both scenarios
combined_simulation <- mcSimulation(
  estimate          = estimates,
  model_function    = model_function,
  numberOfModelRuns = 1000,
  functionSyntax    = "plainNames" )



# Plot distributions 
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = "NPV_Monoculture",
  method              = "boxplot_density", base_size = 7,
  x_axis_name = "Outcome distribution (USD/ha)")

plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = "NPV_Intercropping",
  method              = "boxplot_density", base_size = 7,
  x_axis_name = "Outcome distribution (USD/ha)")


plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = "NPV_Decision",
  method              = "boxplot_density", base_size = 7, 
  x_axis_name = "Outcome distribution (USD/ha)" )

plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_Monoculture","NPV_Intercropping"),
  method              = "smooth_simple_overlay", base_size = 7,
  x_axis_name = "Outcome distribution (USD/ha)")


#### Cashflow (Annual Profit) Comparison for Both Scenarios ####

# Plot the annual profit (“cashflow”) time series for both scenarios
plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("Cashflow_Monoculture", "Cashflow_Intercropping"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (USD/ha)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("Monoculture", "Intercropping"))

plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("CumCashflow_Monoculture", "CumCashflow_Intercropping"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (USD/ha)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("Monoculture", "Intercropping"))

















##EVPI is not completed fro here
# Combine inputs and both NPVs into one data frame
#( Please do understand, complete EVPI what needed )
# 1) Combine inputs and both NPVs into one data frame
df_evpi <- data.frame(
  combined_simulation$x,
  # NPV_Monoculture   = combined_simulation$y[, "NPV_Monoculture"],
  # NPV_Intercropping = combined_simulation$y[, "NPV_Intercropping"],
  NPV_Decision = combined_simulation$y[, "NPV_Decision"]
  
)
df <- data.frame(combined_simulation$x, combined_simulation$y[1:3])


# 2) Calculate EVPI for all inputs, starting at Monoculture_NPV
EVPI_results <- multi_EVPI(
  mc            = df,
  first_out_var = "NPV_Monoculture"
)
plot_evpi(EVPIresults = EVPI_results, decision_vars = "NPV_Decision")
# 3) Inspect the EVPI table
print(EVPI_results)

# 4) Plot EVPI for both scenarios side by side
plot_evpi(
  EVPIresults   = EVPI_results,
  decision_vars = c("NPV_Decision")







df_evpi <- data.frame(
  combined_simulation$x,
  NPV_Monoculture   = combined_simulation$y[, "NPV_Monoculture"],
  NPV_Intercropping = combined_simulation$y[, "NPV_Intercropping"] )

# Calculate EVPI for all inputs, starting at Monoculture_NPV 

EVPI_results <- multi_EVPI(
  mc            = df_evpi,
  first_out_var = "NPV_Monoculture" )

# Inspect the EVPI table
print(EVPI_results)

# Plot EVPI for both scenarios side by side
plot_evpi(
  EVPIresults   = EVPI_results,
  decision_vars = c("NPV_Monoculture", "NPV_Intercropping"))





###Variable Importance in Projection





# Compound figure of NPV and cashflow for monoculture
compound_figure(
  # mcSimulation_object = combined_simulation,
  model = model_function,
  input_table = input_data,
  decision_var_name = "NPV_Monoculture",
  cashflow_var_name = "Cashflow_Monoculture",
  model_runs = 1000, 
  distribution_method = 'smooth_simple_overlay')

compound_figure(
  # mcSimulation_object = combined_simulation,
  model = model_function,
  input_table = input_data,
  decision_var_name = "NPV_Intercropping",
  cashflow_var_name = "Cashflow_Intercropping",
  model_runs = 1000, 
  distribution_method = 'smooth_simple_overlay')

compound_figure(
  # mcSimulation_object = combined_simulation,
  model = model_function,
  input_table = input_data,
  decision_var_name = "NPV_Decision",
  cashflow_var_name = "Cashflow_Decision",
  model_runs = 1000, 
  distribution_method = 'smooth_simple_overlay')
























####Additional Figures####


# Identify the exact column names for Year-n_years
mono_cols <- grep("^Cashflow_Monoculture\\.", names(combined_simulation$y),
             value = TRUE)
int_cols  <- grep("^Cashflow_Intercropping\\.", names(combined_simulation$y),
             value = TRUE)

final_year_col_mono <- mono_cols[grepl(paste0("\\.", n_years, "$"), mono_cols)][1]
final_year_col_int  <- int_cols[ grepl(paste0("\\.", n_years, "$"), int_cols)][1]

# Extract those columns as numeric vectors
mono_final <- combined_simulation$y[[final_year_col_mono]]
int_final  <- combined_simulation$y[[final_year_col_int ]]

# Compute the 30th and 70th percentiles for Year-n_years
mono_q <- quantile(mono_final, probs = c(0.3, 0.7))
int_q  <- quantile(int_final,  probs = c(0.3, 0.7))

# Print the results
cat("Monoculture Year", n_years, "Profit 30th & 70th percentiles:\n")
    print(mono_q)
cat("\nIntercropping Year", n_years, "Profit 30th & 70th percentiles:\n")
    print(int_q)

# Side-by-side boxplots of NPV
npv_df <- data.frame(
          Monoculture   = combined_simulation$y[, "NPV_Monoculture"],
          Intercropping = combined_simulation$y[, "NPV_Intercropping"])

npv_long <- pivot_longer(
            npv_df,
            cols      = everything(),
            names_to  = "Scenario",
            values_to = "NPV")

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


# Grab columns
mono_cols <- grep("^CumCashflow_Monoculture",   names(combined_simulation$y),
                  value = TRUE)
int_cols  <- grep("^CumCashflow_Intercropping", names(combined_simulation$y), 
                  value = TRUE)
dec_cols  <- grep("^CumCashflow_Decision",      names(combined_simulation$y), 
                  value = TRUE)

# Sanity check
if (!(length(mono_cols)==n_years && length(int_cols)==n_years && 
      length(dec_cols)==n_years)) {
      stop(sprintf("Expect %d years, got %d mono, %d int, %d dec",
      n_years, length(mono_cols), length(int_cols), length(dec_cols)))}

# Compute medians per year
mono_med_cum <- apply(combined_simulation$y[, mono_cols], 2, median)
int_med_cum  <- apply(combined_simulation$y[, int_cols],  2, median)
dec_med_cum  <- apply(combined_simulation$y[, dec_cols],  2, median)

# Build long data.frame for just Monoculture & Intercropping
year_seq <- seq_len(n_years)
cum_df <- data.frame(
          Year       = rep(year_seq, times = 2),
          Cumulative = c(mono_med_cum, int_med_cum),
          Scenario   = rep(c("Monoculture","Intercropping"), each = n_years))

# Plot it
ggplot(cum_df, aes(x = Year, y = Cumulative, color = Scenario)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      theme_minimal(base_size = 14) +
      labs(
      title = "Median Cumulative Profit Over Time",
      x     = "Year",
      y     = "Cumulative Profit (USD/ha)"
      ) +
      scale_color_manual(values = c("grey40","forestgreen"))










set.seed()   # I could not make this arguments for that function,
#  I did not understand. th





