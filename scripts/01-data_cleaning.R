#### Preamble ####
# Purpose: Cleans the raw daTA AND MERGES THEM INTO A DATSET, THIS DATSET IS THEN BROKEN INTO ONE OF COUNTRIES AND ONE BY INCOME LEVEL CATEGORIES. The resulting 2 clean datsets rewsult- countries_energy_GDP_data.csv and income_group_energy_GDP_data.csv
# Author: Rayan Awad Alim
# Date: 21 April 2024
# Contact: rayan.alim@mail.utoronto.ca
# License: MIT
# Pre-requisites:
# Any other information needed? 2 clean datsets rewsult- countries_energy_GDP_data.csv and income_group_energy_GDP_data.csv

#### Workspace setup ####

# Install Packages if not downloaded:
install_if_missing <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name)
    suppressMessages(library(package_name, character.only = TRUE))
  }
}

install_if_missing("readxl")
install_if_missing("here")
install_if_missing("dplyr")
install_if_missing("ggplot2")
install_if_missing("tidyr")
install_if_missing("caret")
install_if_missing("readr")


#Load
library(readxl)
library(here)
library(dplyr)
library(ggplot2)
library(tidyr)
library(caret)
library(readr)

#### Clean data ####

### GDP Data Loading

# Read the CSV file, skip first 4 rows and checking the structure
gdp_data_raw <-
  read.csv(
    "../data/raw_data/Global_GDP_Growth.csv",
    skip = 4,
    check.names = FALSE
  )

# Clean and normalize column names
colnames(gdp_data_raw) <-
  make.names(colnames(gdp_data_raw), unique = TRUE)

# Specify columns to keep: country details and years 1992 to 2022 only
columns_to_keep <- c("Country.Name", "Country.Code",
                     paste0("X", 1992:2022))  # Creates "X1992", "X1993", ..., "X2022"

# Select the specified columns
gdp_data_selected <- gdp_data_raw %>%
  select(all_of(columns_to_keep))

# Check for and exclude an unwanted 'X' column if it accidentally included
if ("X" %in% colnames(gdp_data_selected)) {
  gdp_data_selected <- gdp_data_selected %>%
    select(-X)
}

# Convert the data from wide to long format
gdp_data_long <- pivot_longer(
  gdp_data_selected,
  cols = starts_with("X"),
  names_to = "Year",
  values_to = "GDP_growth",
  names_prefix = "X"
)  # Remove the 'X' prefix in year column

# Convert Year to numeric
gdp_data_long$Year <- as.numeric(gdp_data_long$Year)


### Energy Consumption Data Loading

# Read the CSV file
energy_data <-
  read_csv("../data/raw_data/primary-energy-cons.csv", show_col_types = FALSE)

# Filter rows where Year is between 1992 and 2022
energy_data_filtered <- energy_data %>%
  filter(Year >= 1992, Year <= 2022)

# Rename columns to facilitate merging
gdp_data_filtered <- gdp_data_long %>%
  rename(Entity = Country.Name, Code = Country.Code)

combined_data <-
  full_join(energy_data_filtered,
            gdp_data_filtered,
            by = c("Entity", "Code", "Year"))

# Define the list of income categories
income_categories <- c(
  "High-income countries",
  "Low-income countries",
  "Lower-middle-income countries",
  "Upper-middle-income countries",
  "High income",
  "Low income",
  "Lower middle income",
  "Upper middle income"
)

# Filter the dataset to include only the specified entities
income_data_subset <- combined_data %>%
  filter(Entity %in% income_categories)

income_region_mapping <- c(
  "Low-income countries" = "Low Income",
  "Lower-middle-income countries" = "Lower Middle Income",
  "Upper-middle-income countries" = "Upper Middle Income",
  "High-income countries" = "High Income"
)

# Standardise names based on the mapping
income_data_subset <- income_data_subset %>%
  mutate(Entity = ifelse(
    Entity %in% names(income_region_mapping),
    income_region_mapping[Entity],
    Entity
  ))

# Aggregate data to combine entries
income_data_subset <- income_data_subset %>%
  group_by(Entity, Year) %>%
  summarise(
    Total_Energy_Consumption_TWh = sum(`Primary energy consumption (TWh)`, na.rm = TRUE),
    Average_GDP_Growth = mean(GDP_growth, na.rm = TRUE),
    .groups = 'drop'
  )

income_data_subset <- income_data_subset %>%
  group_by(Entity, Year) %>%
  summarise(
    Total_Energy_Consumption_TWh = max(Total_Energy_Consumption_TWh, na.rm = TRUE),
    Average_GDP_Growth = max(Average_GDP_Growth, na.rm = TRUE),
    .groups = 'drop'
  )

# Clean data: unify case, replace -Inf with NA
income_data_subset$Entity <-
  tolower(income_data_subset$Entity)  # Make entity names lowercase for consistency
income_data_subset$Average_GDP_Growth[income_data_subset$Average_GDP_Growth == -Inf] <-
  NA  # Replace -Inf with NA

income_data_subset <- income_data_subset %>%
  group_by(Entity, Year) %>%
  summarise(
    Total_Energy_Consumption_TWh = max(Total_Energy_Consumption_TWh, na.rm = TRUE),
    Average_GDP_Growth = max(Average_GDP_Growth, na.rm = TRUE),
    .groups = 'drop'
  )

# Add a new column for standardized entity names
income_data_subset <- income_data_subset %>%
  mutate(
    Standardized_Entity = case_when(
      Entity %in% c("High-income countries", "High income") ~ "High Income",
      Entity %in% c("Low-income countries", "Low income") ~ "Low Income",
      Entity %in% c("Lower-middle-income countries", "Lower middle income") ~ "Lower Middle Income",
      Entity %in% c("Upper-middle-income countries", "Upper middle income") ~ "Upper Middle Income",
      TRUE ~ as.character(Entity)  # Fallback to original if not matched
    )
  )


aggregated_data <- income_data_subset %>%
  group_by(Standardized_Entity, Year) %>%
  summarise(
    Total_Energy_Consumption_TWh = sum(`Primary energy consumption (TWh)`, na.rm = TRUE),
    Average_GDP_Growth = mean(GDP_growth, na.rm = TRUE),
    .groups = 'drop'
  )


# Check for rows where either energy or GDP data might still be missing
aggregated_data %>%
  filter(is.na(Total_Energy_Consumption_TWh) |
           is.na(Average_GDP_Growth))


# View the result
aggregated_Income_data <- aggregated_data

# The list of regions to exclude
entities_to_exclude <- c(
  "Africa",
  "Africa (EI)",
  "Africa (EIA)",
  "Antarctica",
  "Asia",
  "Asia & Oceania (EIA)",
  "Asia Pacific (EI)",
  "Australia and New Zealand (EIA)",
  "CIS (EI)",
  "Central & South America (EIA)",
  "Central America (EI)",
  "Eastern Africa (EI)",
  "Eurasia (EIA)",
  "Europe",
  "Europe (EI)",
  "Europe (EIA)",
  "European Union (27)",
  "High-income countries",
  "Low-income countries",
  "Lower-middle-income countries",
  "Middle Africa (EI)",
  "Middle East (EI)",
  "Middle East (EIA)",
  "Non-OECD (EI)",
  "Non-OECD (EIA)",
  "Non-OPEC (EIA)",
  "North America",
  "North America (EI)",
  "OECD (EI)",
  "OECD (EIA)",
  "OPEC (EIA)",
  "Oceania",
  "Persian Gulf (EIA)",
  "South America",
  "South and Central America (EI)",
  "U.S. Pacific Islands (EIA)",
  "U.S. Territories (EIA)",
  "Upper-middle-income countries",
  "Wake Island (EIA)",
  "Western Africa (EI)",
  "World",
  "Arab World",
  "Caribbean small states",
  "Central Europe and the Baltics",
  "East Asia & Pacific",
  "East Asia & Pacific (excluding high income)",
  "Euro area",
  "Europe & Central Asia",
  "Europe & Central Asia (excluding high income)",
  "European Union",
  "Fragile and conflict affected situations",
  "Heavily indebted poor countries (HIPC)",
  "Latin America & Caribbean",
  "Latin America & Caribbean (excluding high income)",
  "Least developed countries: UN classification",
  "Middle East & North Africa",
  "Middle East & North Africa (excluding high income)",
  "North America",
  "OECD members",
  "Other small states",
  "Pacific island small states",
  "Small states",
  "South Asia",
  "Sub-Saharan Africa",
  "Sub-Saharan Africa (excluding high income)",
  "High income",
  "Low & middle income",
  "Low income",
  "Lower middle income",
  "Middle income",
  "Upper middle income",
  "Africa Eastern and Southern",
  "Africa Western and Central",
  "Early-demographic dividend",
  "IBRD only",
  "IDA & IBRD total",
  "IDA total",
  "IDA blend",
  "IDA only",
  "Not classified",
  "Sub-Saharan Africa (IDA & IBRD countries)",
  "South Asia (IDA & IBRD)",
  "Middle East & North Africa (IDA & IBRD countries)",
  "Latin America & the Caribbean (IDA & IBRD countries)",
  "Europe & Central Asia (IDA & IBRD countries)",
  "East Asia & Pacific (IDA & IBRD countries)",
  "Post-demographic dividend",
  "Pre-demographic dividend",
  "Late-demographic dividend",
  ""
)

# Filter out the specified entities
Country_combined_data <- combined_data %>%
  filter(!Entity %in% entities_to_exclude)

# A named vector where keys are old names and values are the standardized names
name_mapping <- c(
  "Yemen, Rep." = "Yemen",
  "russian federation" = "Russia",
  "Viet Nam" = "Vietnam",
  "Virgin Islands (U.S.)" = "United States Virgin Islands",
  "Venezuela, RB" = "Venezuela",
  "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
  "Turkiye" = "Turkey",
  "East Timor" = "Timor-Leste",
  "Syrian Arab Republic" = "Syria",
  "Macao SAR, China" = "Macao",
  "Saint Lucia" = "St. Lucia",
  "Lao PDR" = "Laos",
  "West Bank and Gaza" = "Palestine",
  "Korea, Dem. People's Rep." = "North Korea",
  "Korea, Rep." = "South Korea",
  "St. Kitts and Nevis" = "Saint Kitts and Nevis",
  "Kyrgyz Republic" = "Kyrgyzstan",
  "Iran, Islamic Rep." = "Iran",
  "Hong Kong SAR, China" = "Hong Kong",
  "Gambia, The" = "Gambia",
  "Micronesia, Fed. Sts." = "Micronesia (country)",
  "Egypt, Arab Rep." = "Egypt",
  "Cabo Verde" = "Cape Verde",
  "Congo, Rep." = "Congo",
  "Congo, Dem. Rep." = "Democratic Republic of Congo",
  "Brunei Darussalam" = "Brunei",
  "Bahamas, The" = "Bahamas"
)

# Standardize names based on the mapping
Country_combined_data <- Country_combined_data %>%
  mutate(Entity = ifelse(Entity %in% names(name_mapping), name_mapping[Entity], Entity))


# Aggregate data by Entity, Code, and Year
Country_combined_data <- Country_combined_data %>%
  group_by(Entity, Code, Year) %>%
  summarise(
    Total_Energy_Consumption_TWh = sum(`Primary energy consumption (TWh)`, na.rm = TRUE),
    Average_GDP_Growth = mean(GDP_growth, na.rm = TRUE),
    .groups = 'drop'
  )

# Filter out rows where either energy or GDP is NA
filtered_Country_combined_data <- Country_combined_data %>%
  filter(!is.na(Total_Energy_Consumption_TWh) &
           !is.na(Average_GDP_Growth))

# Filter out rows where energy consumption is exactly 0.00
filtered_Country_combined_data <- filtered_Country_combined_data %>%
  filter(Total_Energy_Consumption_TWh != 0.00)

#### Save data ####
write.csv(
  filtered_Country_combined_data,
  "../data/analysis_data/countries_energy_GDP_data.csv",
  row.names = FALSE
)
write.csv(
  aggregated_data,
  "../data/analysis_data/income_group_energy_GDP_data.csv",
  row.names = FALSE
)
