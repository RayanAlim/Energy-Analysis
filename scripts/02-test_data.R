#### Preamble ####
# Purpose: Tests the clean data (such as summaries, missing values, duplicates, and data type validation), range and integrity checks, outlier detection, and, statistical testing for correlations and autocorrelations 
# Author: Rayan Awad Alim
# Date: 21 April 2024
# Contact: rayan.alim@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# Any other information needed?  N/A


#### Workspace setup ####


# Install Packages if not downloaded:
install_if_missing <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
    suppressMessages(library(package_name, character.only = TRUE))
  }
}

install_if_missing("here")
install_if_missing("ggplot2")
install_if_missing("tidyr")
install_if_missing("readr")


#Load
library(here)
library(ggplot2)
library(tidyr)
library(readr)


# Load data of the countries
data <-
  read.csv(here("data", "analysis_data", "countries_energy_GDP_data.csv"))


# Look at data summary
summary(data)


# Check for missing values in the dataset
sum(is.na(data))


# Check duplicates for a specific key
anyDuplicated(data[c("Entity", "Year")])


#Summarize missing values by column
colSums(is.na(data))


# Check if Year is within expected range
with(data, any(Year < 1993 | Year > 2022))


# Check data types of each column
str(data)

# Check data types and convert if neededd here
data$Year <- as.integer(data$Year)
data$Total_Energy_Consumption_TWh <-
  as.numeric(data$Total_Energy_Consumption_TWh)
data$Average_GDP_Growth <- as.numeric(data$Average_GDP_Growth)

# Re-check structure
str(data)

# Checks for GDP growth and energy consumption
if (any(data$Total_Energy_Consumption_TWh < 0)) {
  warning("There are negative values in Total Energy Consumption which is not expected.")
}

# GDP range is a number that is reasonable (Based on historical data)
if (any(data$Average_GDP_Growth < -1000 |
        data$Average_GDP_Growth > 1000)) {
  warning("GDP Growth rate is outside the expected range of -1000% to 1000%.")
}


# Ensure no GDP growth is provided for years with zero energy consumption since that is theoretically impossible
inconsistent_entries <- data %>%
  filter(Total_Energy_Consumption_TWh == 0 &
           !is.na(Average_GDP_Growth))

if (nrow(inconsistent_entries) > 0) {
  print(
    "There are inconsistent entries where energy consumption is zero but GDP growth is recorded:"
  )
  print(inconsistent_entries)
}


# Identify duplicates
duplicates <- data %>%
  group_by(Entity, Year) %>%
  filter(n() > 1)

if (nrow(duplicates) > 0) {
  print("Duplicate records found:")
  print(duplicates)
}

# Removing duplicates, if needed
data <- data %>%
  distinct(Entity, Year, .keep_all = TRUE)


# Boxplot to visualize outliers in GDP Growth
boxplot(data$Average_GDP_Growth, main = "Boxplot for GDP Growth")

# Identifying outliers using the IQR rule
Q1 <- quantile(data$Average_GDP_Growth, 0.25, na.rm = TRUE)
Q3 <- quantile(data$Average_GDP_Growth, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

outliers <- data %>%
  filter(Average_GDP_Growth < (Q1 - 1.5 * IQR) |
           Average_GDP_Growth > (Q3 + 1.5 * IQR))

print("Outliers in GDP Growth:")
print(outliers)

# Look at corelation
cor.test(data$Total_Energy_Consumption_TWh,
         data$Average_GDP_Growth,
         method = "pearson")

# vb check for Autocorrelation
acf(data$Average_GDP_Growth)
pacf(data$Average_GDP_Growth)
