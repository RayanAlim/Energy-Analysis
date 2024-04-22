#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)

if (!requireNamespace("rstanarm", quietly = TRUE)) {
  install.packages("rstanarm")
}

library(rstanarm)


#### Read data ####
countries_energy_GDP_data <- read_csv("data/analysis_data/countries_energy_GDP_data.csv")

### Model data ####

# Normalize energy consumption
countries_energy_GDP_data$Log_Energy_Consumption <- log(countries_energy_GDP_data$Total_Energy_Consumption_TWh + 1)  # Log-transform to improve normality

# Bayesian Linear Regression Model
bayesian_model <- stan_glm(
  formula = Average_GDP_Growth ~ Log_Energy_Consumption,
  data = countries_energy_GDP_data,
  family = gaussian(),
  prior = normal(location = 0, scale = 2.5, autoscale = TRUE),  # Prior for coefficients
  prior_intercept = normal(location = 0, scale = 2.5, autoscale = TRUE),  # Prior for the intercept
  prior_aux = exponential(rate = 1, autoscale = TRUE),  # Prior for the error term
  seed = 853,  # For reproducibility
  chains = 4,  # Number of Markov Chain Monte Carlo (MCMC) chains
  iter = 2000  # Number of MCMC iterations per chain
)

## Summary of the model
model_summary <- summary(bayesian_model)

# Print the complete model summary to inspect
print(model_summary)

# Example of accessing the coefficients if 'model_summary' is a list
if(is.list(model_summary)) {
  coef_summary <- model_summary$coefficients
  print(coef_summary)
} else {
  # If it's not a list, directly print it
  print(model_summary)
}


# Replace 'model_summary$coefficients' with 'coef_summary' if it's a list and correctly structured

# Check names of coefficients
if(exists("coef_summary")) {
  print(names(coef_summary))
} else {
  print(colnames(model_summary))
}

# Set color scheme for plots
color_scheme_set("brightblue")

# MCMC Trace Plot
mcmc_trace(
  bayesian_model,
  pars = c("(Intercept)", "Log_Energy_Consumption"),
  n_warmup = 1000
)

# Additional Diagnostic Plots
# Posterior Predictive Checks
pp_check(bayesian_model)

# Plotting residuals
posterior_predictions <- posterior_predict(bayesian_model)
residuals <- countries_energy_GDP_data$Average_GDP_Growth - posterior_predictions

# Basic plot of residuals
plot(residuals, type = "p", main = "Residuals Plot")

# Using pairs plot 
if(requireNamespace("bayesplot", quietly = TRUE)) {
  mcmc_pairs(
    bayesian_model,
    pars = c("(Intercept)", "Log_Energy_Consumption")
  )
}




#### Save model ####
saveRDS(
  bayesian_model,
  file = "../models/bayesian_model.rds"
)


