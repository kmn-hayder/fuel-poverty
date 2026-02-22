library(rcompanion)
library(reshape2)
library(DescTools)
library(corrr)
library(rlang)
library(ggplot2)
library(dplyr)
library(patchwork)
library(tidyr)
library(readxl)
library(readr)
library(tibble)
library(purrr)
library(data.table)
library(arrow)

summary(clean_master)

#==========Clean for hard errors=========

clean_master <- clean_master %>%
  filter(
    # Positive values only
    income_bhc_2024 > 0,
    avg_fuel_bill_annual_individual_estimate > 0,
    energy_consumption_current > 0,
    CO2_EMISSIONS_CURRENT >= 0,
    
    # Valid EPC bands
    epc_rating_numeric >= 1,
    epc_rating_numeric <= 7,
    
    # Reasonable size and room count
    total_floor_area > 10,
    total_floor_area <= 500,
    NUMBER_HABITABLE_ROOMS >= 1,
    NUMBER_HABITABLE_ROOMS <= 20,
    
    # Valid insulation scale
    insulation_score >= 1,
    insulation_score <= 5,
    
    # No missing key values
    !is.na(income_bhc_2024),
    !is.na(avg_fuel_bill_annual_individual_estimate),
    !is.na(epc_rating_numeric),
    !is.na(insulation_score)
  ) %>%
  # Create fuel cost ratio
  mutate(fuel_cost_ratio = avg_fuel_bill_annual_individual_estimate / income_bhc_2024) %>%
  # Drop invalid cost ratios
  filter(
    fuel_cost_ratio > 0,
    fuel_cost_ratio <= 1
  )

# Identify all numeric columns
numeric_cols <- sapply(clean_master, is.numeric)

# Compute quantiles (0% to 100% by 1%) for each numeric column
percentile_summary <- lapply(clean_master[, numeric_cols], function(col) {
  quantile(col, probs = seq(0, 1, 0.01), na.rm = TRUE)
})


# Convert percentile_summary (list) to a data frame
percentile_df <- bind_rows(percentile_summary, .id = "variable")

# Write to CSV
write_csv(percentile_df, "percentile_summary_all_numeric_columns.csv")

#==========Clean for outliers=========

#Treating anomalies in wall/roof efficiency scores
clean_master <- clean_master %>%
  filter(
    # Remove rows with invalid ordinal ratings
    ROOF_ENERGY_EFF >= 1, ROOF_ENERGY_EFF <= 5,
    WALLS_ENERGY_EFF >= 1, WALLS_ENERGY_EFF <= 5,
    insulation_score >= 1, insulation_score <= 5
  )

# Compute percentiles for capping
p99_energy <- quantile(clean_master$ENERGY_CONSUMPTION_CURRENT, 0.99, na.rm = TRUE)
p99_fuel_bill <- quantile(clean_master$avg_fuel_bill_annual_individual_estimate, 0.99, na.rm = TRUE)
p99_co2 <- quantile(clean_master$CO2_EMISSIONS_CURRENT, 0.99, na.rm = TRUE)

# Apply fixes
clean_master <- clean_master %>%
  # Remove invalid EPC efficiency
  filter(CURRENT_ENERGY_EFFICIENCY <= 100) %>%
  
  # Cap long-tail variables
  mutate(
    ENERGY_CONSUMPTION_CURRENT = pmin(ENERGY_CONSUMPTION_CURRENT, p99_energy),
    avg_fuel_bill_annual_individual_estimate = pmin(avg_fuel_bill_annual_individual_estimate, p99_fuel_bill),
    CO2_EMISSIONS_CURRENT = pmin(CO2_EMISSIONS_CURRENT, p99_co2),
    
    # Cap fuel bill difference to ±20,000
    fuel_bill_difference = case_when(
      fuel_bill_difference > 20000 ~ 20000,
      fuel_bill_difference < -20000 ~ -20000,
      TRUE ~ fuel_bill_difference
    )
  )
# Remove extreme edge case households
clean_master <- clean_master %>%
  filter(fuel_cost_ratio <= 0.75)

#===========Categorical Variables =========

# Identify character or factor variables
cat_vars <- names(clean_master)[sapply(clean_master, function(x) is.character(x) || is.factor(x))]

# Display unique values for each
for (var in cat_vars) {
  cat("\n Variable:", var, "\n")
  print(unique(clean_master[[var]]))
}


# Identify character or factor columns
cat_vars <- names(clean_master)[sapply(clean_master, function(x) is.character(x) || is.factor(x))]

# Check for unique values and missing counts
for (var in cat_vars) {
  cat("\n Variable:", var, "\n")
  
  # Unique values
  print(unique(clean_master[[var]]))
  
  # Count NAs
  cat("🔸 NA values:", sum(is.na(clean_master[[var]])), "\n")
  
  # Count blanks and common placeholders
  cat("🔸 Empty strings:", sum(clean_master[[var]] == "", na.rm = TRUE), "\n")
  cat("🔸 'unknown':", sum(tolower(clean_master[[var]]) == "unknown", na.rm = TRUE), "\n")
  cat("🔸 'no data!':", sum(tolower(clean_master[[var]]) == "no data!", na.rm = TRUE), "\n")
  cat("🔸 'invalid!':", sum(tolower(clean_master[[var]]) == "invalid!", na.rm = TRUE), "\n")
}

#filter out unknown and invalid values
clean_master <- clean_master %>%
  filter(
    tolower(TENURE) != "unknown",
    tolower(CONSTRUCTION_AGE_BAND) != "invalid!"
  )

#derive features
clean_master <- clean_master %>%
  mutate(
    fuel_cost_ratio = avg_fuel_bill_annual_individual_estimate / income_bhc_2024,
    floor_area_per_room = total_floor_area / NUMBER_HABITABLE_ROOMS,
    co2_per_m2 = CO2_EMISSIONS_CURRENT / total_floor_area,
    energy_per_m2 = energy_consumption_current / total_floor_area,
    energy_per_room = energy_consumption_current / NUMBER_HABITABLE_ROOMS
  )

#=========== Clean parquet output =========

# Save updated clean_master
write_parquet(clean_master, "clean_master_final.parquet")

#Save as plain CSV
write.csv(clean_master, "clean_master_final.csv", row.names = FALSE)

#Save as compressed CSV (gzip)
write.csv(clean_master, gzfile("clean_master_final.csv.gz"), row.names = FALSE)

#Save as Parquet (fast binary format)
write_parquet(clean_master, "clean_master_final.parquet", compression = "snappy")

cat("Exported: CSV, Compressed CSV, and Parquet formats.\n")

#==========EDA==========

# Get numeric variables
num_vars <- names(clean_master)[sapply(clean_master, is.numeric)]

# Loop through in batches of 9
for (i in seq(1, length(num_vars), by = 4)) {
  batch <- num_vars[i:min(i + 3, length(num_vars))]
  
  plots <- lapply(batch, function(var) {
    ggplot(clean_master, aes(x = !!sym(var))) +
      geom_density(fill = "steelblue", alpha = 0.5) +
      labs(title = var) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 10, face = "bold")
      )
  })
  
  print(wrap_plots(plots))
}

#=======correlation matrix for feature selection========

# 1. Select only numeric columns
numeric_vars <- clean_master %>%
  select(where(is.numeric))

# 2. Compute full correlation matrix
cor_matrix <- correlate(numeric_vars, use = "pairwise.complete.obs")

# 3. View or export the full correlation matrix
print(cor_matrix)

# Optional: Save as CSV
write.csv(cor_matrix, "correlation_matrix_all_numeric.csv", row.names = FALSE)

clean_master <- read_parquet("clean_master_final.parquet")

# ---- MCMC-based target generation variables ----


# MCMC split variables (LILEE-matching)
mcmc_vars <- c(
  "LMK_KEY","epc_rating_numeric", "urban_rural.y", "region_name", "PROPERTY_TYPE",
  "CONSTRUCTION_AGE_BAND", "TOTAL_FLOOR_AREA", "fuel_category",
  "WALLS_ENERGY_EFF", "TENURE"
)

# ML modeling split variables (non-redundant, avoiding signal leakage)
ml_vars <- c(
  "LMK_KEY",
  "CURRENT_ENERGY_EFFICIENCY", "ENERGY_CONSUMPTION_CURRENT", "CO2_EMISSIONS_CURRENT",
  "BUILT_FORM", "local_authority_code.x", "LODGEMENT_DATE",
  "postcode_district", "insulation_score", "local_authority.x", "net_income_2024", "income_bhc_2024",
  "income_ahc_2024", "perc_low_income_households", "unemployment_rate",
  "imd_avg_2019", "hdd_01_20_median", "tas_winter_01_20_median",
  "tas_winter_1.5_median", "tas_winter_2_median", "tas_winter_2.5_median",
  "tas_winter_5.5_median", "tas_winter_3.5_median", "tas_winter_4_median",
  "min_temp_01_20_median", "population_2024", "child_population",
  "senior_population", "working_age_population", "average_household_size_2023",
  "area_km", "population_density", "age_dependency_ratio", "total_floor_area",
  "electricity_avg_variable_per_kWh_price", "electricity_avg_fixed_cost_annual",
  "gas_avg_variable_per_kWh_price", "gas_avg_fixed_cost_annual",
  "num_households_received_winter_fuel_payment_2023",
  "avg_fuel_bill_annual_individual_estimate",
  "local_authority_mean_domessic_electricity_consumption_kWh_per_household",
  "local_authority_mean_domestic_gas_consumption_kWh_per_meter",
  "avg_fuel_bill_annual_local_average_estimate", "fuel_bill_difference",
  "fuel_cost_ratio", "floor_area_per_room", "co2_per_m2", "energy_per_m2",
  "energy_per_room"
)

# Filter columns
mcmc_data <- clean_master %>% select(all_of(mcmc_vars))
ml_data   <- clean_master %>% select(all_of(ml_vars))

# Save to CSV
write.csv(mcmc_data, "mcmc_split_clean_master.csv", row.names = FALSE)
write.csv(ml_data, "ml_split_clean_master.csv", row.names = FALSE)

# Save as Parquet (optional and efficient)
write_parquet(mcmc_data, "mcmc_split_clean_master.parquet")
write_parquet(ml_data, "ml_split_clean_master.parquet")


