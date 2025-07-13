#### Combined Monoculture vs. Intercropping Simulation ####

# 1) Load packages
library(decisionSupport)
library(ggplot2)
library(tidyr)

# 2) Read inputs and draw one random set of values
input_data <- read.csv("data/Input_File.csv", stringsAsFactors = FALSE)
estimates  <- as.estimate(input_data)
make_variables <- function(est, n = 1) {
  x <- random(rho = est, n = n)
  for (nm in colnames(x)) {
    assign(nm, as.numeric(x[1, nm]), envir = .GlobalEnv)
  }
}
make_variables(estimates, n = 1)

# 3) Model function
model_function <- function() {
  # -- Monoculture --
  mono_seed_ts <- c(Maize_Seeds_Cost, rep(0, n_years - 1))
  mono_recur_val <- Pest_Weed_Management_Mono +
    Crop_Maintenance_Mono +
    Irrigation_Mono
  mono_recur_ts <- c(vv(var_mean = mono_recur_val, var_CV = var_CV, n = n_years - 1), 0)
  mono_cost_ts  <- mono_seed_ts + mono_recur_ts
  
  mono_yield_pest    <- chance_event(Pest_Disease_Chance_MY,
                                     Maize_Yield * (1 - Pest_Disease_Effect_MY),
                                     Maize_Yield, n = n_years)
  mono_yield_climate <- chance_event(Extreme_Climate_Chance_MY,
                                     mono_yield_pest * (1 - Extreme_Climate_Events_MY),
                                     mono_yield_pest, n = n_years)
  
  mono_rev_ts <- vv(var_mean = mono_yield_climate * Maize_Price,
                    var_CV   = var_CV, n = n_years) *
    chance_event(Post_Harvest_Losses,
                 1 - Reduction_Sale_PHL, 1, n = n_years) *
    chance_event(Market_Fluctuation,
                 1 - Reduction_Sales_MF, 1, n = n_years)
  
  Cashflow_Monoculture    <- mono_rev_ts - mono_cost_ts
  CumCashflow_Monoculture <- cumsum(Cashflow_Monoculture)
  NPV_Monoculture         <- discount(Cashflow_Monoculture,
                                      discount_rate = discount_rate,
                                      calculate_NPV = TRUE)
  
  # -- Intercropping --
  int_seed_ts <- c(Total_Seeds_Cost + Training_Capacity_Int, rep(0, n_years - 1))
  int_recur_val <- Pest_Weed_Management_Int +
    Crop_Maintenance_Int +
    Irrigation_Int +
    Additional_Labor_Int
  int_recur_ts  <- c(vv(var_mean = int_recur_val, var_CV = var_CV, n = n_years - 1), 0)
  int_cost_ts   <- int_seed_ts + int_recur_ts
  
  maize_ts  <- chance_event(Pest_Disease_Chance_MY,
                            Maize_Yield * (1 - Pest_Disease_Effect_MY),
                            Maize_Yield, n = n_years)
  maize_ts  <- chance_event(Extreme_Climate_Chance_MY,
                            maize_ts * (1 - Extreme_Climate_Events_MY),
                            maize_ts, n = n_years)
  cowpea_ts <- chance_event(Pest_Disease_Chance_CY,
                            Cowpea_Yield * (1 - Pest_Disease_Effect_CY),
                            Cowpea_Yield, n = n_years)
  cowpea_ts <- chance_event(Extreme_Climate_Chance_CY,
                            cowpea_ts * (1 - Extreme_Climate_Events_CY),
                            cowpea_ts, n = n_years)
  yellow_ts <- chance_event(Pest_Disease_Chance_YB,
                            Yellow_Beans_Yield * (1 - Pest_Disease_Effect_YB),
                            Yellow_Beans_Yield, n = n_years)
  yellow_ts <- chance_event(Extreme_Climate_Chance_YB,
                            yellow_ts * (1 - Extreme_Climate_Events_YB),
                            yellow_ts, n = n_years)
  
  int_rev_ts <- vv(var_mean = maize_ts*Maize_Price +
                     cowpea_ts*Cowpea_Price +
                     yellow_ts*Yellow_Beans_Price,
                   var_CV   = var_CV, n = n_years) *
    chance_event(Post_Harvest_Losses,
                 1 - Reduction_Sale_PHL, 1, n = n_years) *
    chance_event(Market_Fluctuation,
                 1 - Reduction_Sales_MF, 1, n = n_years)
  
  Cashflow_Intercropping    <- int_rev_ts - int_cost_ts
  CumCashflow_Intercropping <- cumsum(Cashflow_Intercropping)
  NPV_Intercropping         <- discount(Cashflow_Intercropping,
                                        discount_rate = discount_rate,
                                        calculate_NPV = TRUE)
  
  # -- Decision difference (Intercropping minus Monoculture) --
  Cashflow_Decision    <- Cashflow_Intercropping - Cashflow_Monoculture
  CumCashflow_Decision <- cumsum(Cashflow_Decision)
  NPV_Decision         <- NPV_Intercropping - NPV_Monoculture
  
  # 4) Return all named outputs
  return(list(
    NPV_Monoculture       = NPV_Monoculture,
    NPV_Intercropping     = NPV_Intercropping,
    NPV_Decision          = NPV_Decision,
    Cashflow_Monoculture  = Cashflow_Monoculture,
    Cashflow_Intercropping= Cashflow_Intercropping,
    Cashflow_Decision     = Cashflow_Decision,
    CumCashflow_Monoculture   = CumCashflow_Monoculture,
    CumCashflow_Intercropping = CumCashflow_Intercropping,
    CumCashflow_Decision      = CumCashflow_Decision
  ))
}

# 5) Run Monte Carlo
combined_simulation <- mcSimulation(
  estimate          = estimates,
  model_function    = model_function,
  numberOfModelRuns = 10000,
  functionSyntax    = "plainNames"
)

#### Plot NPV distributions ####
plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_Monoculture","NPV_Intercropping","NPV_Decision"),
  method              = "hist_simple_overlay",
  base_size           = 7
)


plot_distributions(
  mcSimulation_object = combined_simulation,
  vars                = c("NPV_Monoculture","NPV_Intercropping","NPV_Decision"),
  method              = "smooth_simple_overlay",
  base_size           = 7
)



#### EVPI ####
df_evpi <- data.frame(
  combined_simulation$x,
  NPV_Monoculture   = combined_simulation$y[,"NPV_Monoculture"],
  NPV_Intercropping = combined_simulation$y[,"NPV_Intercropping"],
  NPV_Decision      = combined_simulation$y[,"NPV_Decision"]
)
EVPI_results <- multi_EVPI(mc = df_evpi, first_out_var = "NPV_Monoculture")
plot_evpi(
  EVPIresults   = EVPI_results,
  decision_vars = c("NPV_Monoculture","NPV_Intercropping","NPV_Decision")
)

#### Cashflow plot ####
plot_cashflow(
  mcSimulation_object = combined_simulation,
  cashflow_var_name   = c("Cashflow_Monoculture",
                          "Cashflow_Intercropping",
                          "Cashflow_Decision"),
  x_axis_name         = "Year",
  y_axis_name         = "Annual Profit (USD/ha)",
  facet_labels        = c("Monoculture","Intercropping","Decision")
)

# 1) Grab columns
mono_cols <- grep("^CumCashflow_Monoculture",   names(combined_simulation$y), value = TRUE)
int_cols  <- grep("^CumCashflow_Intercropping", names(combined_simulation$y), value = TRUE)
dec_cols  <- grep("^CumCashflow_Decision",      names(combined_simulation$y), value = TRUE)

# 2) Sanity check
if (!(length(mono_cols)==n_years && length(int_cols)==n_years && length(dec_cols)==n_years)) {
  stop(sprintf("Expect %d years, got %d mono, %d int, %d dec",
               n_years, length(mono_cols), length(int_cols), length(dec_cols)))
}

# 3) Compute medians per year
mono_med_cum <- apply(combined_simulation$y[, mono_cols], 2, median)
int_med_cum  <- apply(combined_simulation$y[, int_cols],  2, median)
dec_med_cum  <- apply(combined_simulation$y[, dec_cols],  2, median)

# 4) Build long data.frame
year_seq <- seq_len(n_years)
cum_df <- data.frame(
  Year       = rep(year_seq, times = 3),
  Cumulative = c(mono_med_cum, int_med_cum, dec_med_cum),
  Scenario   = rep(c("Monoculture","Intercropping","Decision"), each = n_years)
)

# 5) Plot it
library(ggplot2)
ggplot(cum_df, aes(x = Year, y = Cumulative, color = Scenario)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Median Cumulative Profit Over Time",
    x     = "Year",
    y     = "Cumulative Profit (USD/ha)"
  ) +
  scale_color_manual(values = c("grey40","forestgreen","darkred"))
