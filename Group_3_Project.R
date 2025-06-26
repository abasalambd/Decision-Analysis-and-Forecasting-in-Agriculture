library(decisionSupport)


model_function <- function() {
  
  initial_cost <- vv(( Soil_Organic_Inputs + 
                       Intercrop_Seeds +
                       Training_Capacity +
                       Additional_Labor) * 12,
                       var_CV, n_year)

  recurring_cost <- vv((Soil_Health_Maintenance +
                         Pest_Weed_Management + 
                         Monitoring_Reporting +
                         Irrigation) * 12,
                         var_CV, n_year)

  total_cost <- initial_cost + recurring_cost

  total_revenue <- vv(( Maize_Yield * Maize_Price +
                        Yellow_Beans_Yield * Yellow_Beans_Price +
                        Cowpea_###Price * Cowpea_Price),
                        var_CV, n_year)
  
  
  #calculate yearly profit
  profit_yearly <- ##vv((total_revenue - total_cost), 
                       var_CV, n_year)
                      
  # add risk of variable price
  Price_variability_yes_no <- chance_event(Price_Variability,
                                                 1, 0)

  # risk-adjusted price variability
  adjusted_profit_yearly <- profit_yearly * Price_variability_yes_no 
  
  # Estimate the final results from the model
  final_result <- profit_yearly - recurring_cost
  
  # Calculate NPV with discount rate
  
  NPV_ra <- discount(x = final_result,
                      discount_rate = discount_rate,
                      calculate_NPV = TRUE)
  
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = NPV_ra))
}


input_data <- read.csv("Measurement_Framework2.csv")


# Run the Monte Carlo simulation using the model function
example_mc_simulation <- mcSimulation(estimate = as.estimate(input_data),
                                      model_function = model_function,
                                      numberOfModelRuns = 10000,
                                      functionSyntax = "plainNames")




plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = 'hist_simple_overlay',
                   base_size = 7)
