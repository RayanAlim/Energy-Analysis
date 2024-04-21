#### Preamble ####
# Purpose: Simulates GDP growth and energy consaumption for counttries from 2022 to 2012
# Author: Rayan Awad Alim
# Date: 21 April 2024
# Contact: rayan.alim@mail.utoronto.ca
# License: MIT
# Pre-requisites: N/A ;
# Any other information needed? Data simulated here tries to model these two data sets https://data.worldbank.org/indicator/NY.GDP.MKTP.KD.ZG and https://ourworldindata.org/energy


#### Workspace setup ####
# Install Packages if not downloaded:
install_if_missing <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
    suppressMessages(library(package_name, character.only = TRUE))
  }
}

install_if_missing("dplyr")
install_if_missing("ggplot2")
install_if_missing("tidyverse")

#Load Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)

# Defining some random countries fro simulation
countries <- c("USA", "China", "India", "Australia", "Brazil")
years <- 2012:2022

# Creating a data frame with all combinations of country and year
simulated_data <- expand.grid(Country = countries, Year = years)

## Simulating the Total Energy Consumption (in Petajoules)

# Assuming mean consumption and sd
simulated_data$TotalEnergyConsumption <-
  rnorm(n = nrow(data), mean = 5000, sd = 1000)

## Simulate GDP Growth (annual %)

# Assuming mean GDP growth is 3% and sd is 1%
simulated_data$GDPGrowth <- rnorm(n = nrow(data), mean = 3, sd = 1)

# View the first few rows of the dataset
head(simulated_data)


# Plotting Energy consumption over years for each Country
library(ggplot2)
ggplot(data,
       aes(
         x = Year,
         y = TotalEnergyConsumption,
         group = Country,
         color = Country
       )) +
  geom_line() +
  ggtitle("Total Energy Consumption over Years by Country") +
  xlab("Year") +
  ylab("Total Energy Consumption (PJ)") +
  theme_minimal()
