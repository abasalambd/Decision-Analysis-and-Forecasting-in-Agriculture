#### Combined Monoculture vs. Intercropping Simulation ####

# 1) Load required packages
library(decisionSupport)
library(ggplot2)
library(tidyr)

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



# 1) Combine inputs and both NPVs into one data frame
df_evpi <- data.frame(
  combined_simulation$x,
  Monoculture_NPV   = combined_simulation$y[, "Monoculture_NPV"],
  Intercropping_NPV = combined_simulation$y[, "Intercropping_NPV"]
)

# 2) Calculate EVPI for all inputs, starting at Monoculture_NPV
EVPI_results <- multi_EVPI(
  mc            = df_evpi,
  first_out_var = "Monoculture_NPV"
)

# 3) Inspect the EVPI table
print(EVPI_results)

# 4) Plot EVPI for both scenarios side by side
plot_evpi(
  EVPIresults   = EVPI_results,
  decision_vars = c("Monoculture_NPV", "Intercropping_NPV")
)



#### Cashflow (Annual Profit) Comparison for Both Scenarios ####

# 1) Plot the annual profit (“cashflow”) time series for both scenarios
plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("Monoculture_Profit", "Intercropping_Profit"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (USD/ha)",
  color_25_75         = "blue",
  color_5_95          = "lightblue",
  color_median        = "darkblue",
  facet_labels        = c("Monoculture", "Intercropping")
)

# 2) Identify the exact column names for Year-n_years
mono_cols <- grep("^Monoculture_Profit\\.", names(combined_simulation$y), value = TRUE)
int_cols  <- grep("^Intercropping_Profit\\.", names(combined_simulation$y), value = TRUE)

final_year_col_mono <- mono_cols[grepl(paste0("\\.", n_years, "$"), mono_cols)][1]
final_year_col_int  <- int_cols[ grepl(paste0("\\.", n_years, "$"), int_cols)][1]

# 3) Extract those columns as numeric vectors (not data.frames)
mono_final <- combined_simulation$y[[ final_year_col_mono ]]
int_final  <- combined_simulation$y[[ final_year_col_int  ]]

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
  Monoculture   = combined_simulation$y[, "Monoculture_NPV"],
  Intercropping = combined_simulation$y[, "Intercropping_NPV"]
)
npv_long <- pivot_longer(
  npv_df,
  cols        = everything(),
  names_to    = "Scenario",
  values_to   = "NPV"
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


# 2) Median cumulative cashflow curves
mono_cols <- grep("Monoculture_Profit", names(combined_simulation$y), value = TRUE)
int_cols  <- grep("Intercropping_Profit", names(combined_simulation$y), value = TRUE)

if (length(mono_cols) != n_years || length(int_cols) != n_years) {
  stop(sprintf(
    "Found %d Monoculture_Profit cols and %d Intercropping_Profit cols; expected %d each",
    length(mono_cols), length(int_cols), n_years
  ))
}

mono_mat <- combined_simulation$y[, mono_cols]
int_mat  <- combined_simulation$y[, int_cols]

mono_med_cum <- apply(mono_mat, 2, median) %>% cumsum()
int_med_cum  <- apply(int_mat,  2, median) %>% cumsum()

year_seq <- seq_len(n_years)
cum_df <- data.frame(
  Year       = rep(year_seq, 2),
  Cumulative = c(mono_med_cum, int_med_cum),
  Scenario   = rep(c("Monoculture", "Intercropping"), each = n_years)
)

ggplot(cum_df, aes(x = Year, y = Cumulative, color = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Median Cumulative Profit over Time",
    x     = "Year",
    y     = "Cumulative Profit (USD/ha)"
  ) +
  scale_color_manual(values = c("grey40", "forestgreen"))
