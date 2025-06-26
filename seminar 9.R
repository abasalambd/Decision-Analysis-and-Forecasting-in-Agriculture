library(ggplot2)
library(decisionSupport)

input_estimates <- data.frame(
  variable = c(
    "sheep_income",
    "sheep_cost",
    "apple_income",
    "apple_cost",
    "discount_rate"
  ),
  lower = c(3000, 1000, 30000, 15000, 10),
  median = NA,
  upper = c(5000, 2500, 60000, 30000, 10),
  distribution = c("posnorm", "posnorm", "posnorm", "posnorm", "const"),
  label = c(
    "Income from sheep (euro/year)",
    "Cost of sheep (euro/year)",
    "Income from apple (euro/year)",
    "Cost of apple (euro/year)",
    "Discount Rate"
  )
)

# show the result
input_estimates

df <- data.frame(input_estimates)
write.csv(df, file = "input_table.csv", row.names = FALSE)






model_function <- function() {
  # Estimate the income in a normal season
  AF_income <- sheep_income + apple_income
  AF_cost <- sheep_cost + apple_cost
  # Estimate the final results from the model
  AF_final_result <- AF_income - AF_cost
  # baseline with sheep only
  sheep_only <- sheep_income - sheep_cost
  # should I plant trees in the sheep pastures? 
  Decision_benefit <- AF_final_result - sheep_only
  #Calculating NPV
  #AF System
  AF_NPV <- discount(AF_final_result,
                     discount_rate = discount_rate,
                     calculate_NPV = TRUE)#NVP of AF system
  
  NPV_sheep_only <- discount(sheep_only,
                             discount_rate = discount_rate,
                             calculate_NPV = TRUE) #NVP of grassland
  
  NPV_decision <- discount(Decision_benefit,
                           discount_rate = discount_rate,
                           calculate_NPV = TRUE)
  # Generate the list of outputs from the Monte Carlo simulation
  return(
    list(
      NPV_Agroforestry_System = AF_NPV,
      NPV_Treeless_System = NPV_sheep_only,
      NPV_decision = NPV_decision
    )
  )
}



apple_sheep_mc_simulation <- mcSimulation(
  estimate = as.estimate(input_estimates),
  model_function = model_function,
  numberOfModelRuns = 80,
  functionSyntax = "plainNames"
)




plot_distributions(
  mcSimulation_object = apple_sheep_mc_simulation,
  vars = c("NPV_Agroforestry_System", "NPV_Treeless_System"),
  method = 'smooth_simple_overlay',
  base_size = 7,
  x_axis_name = "Outcome of AF intervention",
  scale_x_continuous(
    labels = function(x)
      x / 100000
  ),
  ggtitle("Net Present Value of the system"),
  legend.position = "bottom"
)
